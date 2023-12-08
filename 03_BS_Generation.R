################################################################################
#                                                                              #
# Copyright 2023 datcsv                                                        #
#                                                                              #
# Licensed under the Apache License, Version 2.0 (the "License");              #
# you may not use this file except in compliance with the License.             #
# You may obtain a copy of the License at                                      #
#                                                                              #
#   http://www.apache.org/licenses/LICENSE-2.0                                 #
#                                                                              #  
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS,            #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.     #
# See the License for the specific language governing permissions and          #
# limitations under the License.                                               #
#                                                                              #
################################################################################

# Load income statement data
load(file="data/is.RData")

# Adjust tokenID for easier debugging
is %<>% mutate(., tokenID = str_replace(tokenID, "_", "/"))

# Generate empy balance sheet dataset
bs <- tibble(
  timestamp = POSIXct(),
  asset     = character(),
  quantity  = double(),
  costBasis = double()
)

# Generate empty tax form 8949 dataset
tax_8949 <- tibble(
  Description   = character(),
  Date_Acquired = Date(),
  Date_Sold     = Date(),
  Proceeds      = double(),
  Cost_Basis    = double(),
  Codes         = character(),
  Adjustment    = double(),
  Gain_Loss     = double()
)

# Initialize variables
xtz_income <- 0
xtz_income_data <- is[0, ]

# Loop through rows of income statement to generate tax figures
for (i in 1:nrow(is)) {

  # Initialize variables
  xtz_balance    <- 0
  xtz_cost       <- 0
  xtz_proceeds   <- 0
  token_balance  <- 0
  token_cost     <- 0
  token_proceeds <- 0
  subtract_j     <- 0

  # Isolate row
  is_i <- is[i, ]

  ##############################################################################
  # Adjust Tezos sent/received:                                                #
  #  (1) If Tezos is sent and received in the same transaction, net the values.#
  #  (2) For token transfers, base the calculation on sender/receiver.         #
  #  (3) For all other transactions, base the calculation on highest value.    #
  #  (*) This assumption should be used for estimates only and is not          #
  #      an accurate or long-term solution.                                    #
  ##############################################################################

  # Adjust XTZ sent/received
  if (is_i$xtzSent > 0 & is_i$xtzReceived > 0) {
    if (is_i$xtzReceived >= is_i$xtzSent) {
      is_i$xtzReceived <- is_i$xtzReceived - is_i$xtzSent
      is_i$xtzSent     <- 0
    }
    else {
      is_i$xtzSent     <- is_i$xtzSent - is_i$xtzReceived
      is_i$xtzReceived <- 0
    }
  }

  ##############################################################################
  # Do not calculate gain (loss) for purchases or income:                      #
  #  (1) If 'xtzBuy' is TRUE, mark as purchase.                                #
  #  (2) If Tezos are received but no tokens are sent, mark as income.         #
  ##############################################################################
  
  # Add Tezos purchases to balance sheet
  if (is_i$xtzBuy) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = "xtz",
        quantity  = is_i$xtzReceived,
        costBasis = ifelse(
          is.na(is_i$costBasis), is_i$quote, is_i$costBasis / is_i$xtzReceived
        )
      )
    next
  }

  # Add Tezos income to balance sheet
  else if ((is_i$xtzReceived > 0) & (is_i$tokenSent == 0)) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = "xtz",
        quantity  = is_i$xtzReceived,
        costBasis = is_i$quote
      )
    xtz_income <- xtz_income + is_i$quote * is_i$xtzReceived
    xtz_income_data %<>% bind_rows(., is_i)
    next
  }
  
  ##############################################################################
  # Calculate gain (loss) on Tezos sent.                                       #
  ##############################################################################

  if (is_i$xtzSent > 0) {
    
    # Initialize variables
    xtz_balance <- is_i$xtzSent
    xtz_cost <- 0
    
    if (is_i$xtzProceeds == 0) {
      xtz_proceeds <- is_i$quote * is_i$xtzSent
    } else {
      xtz_proceeds <- is_i$xtzProceeds
    }

    # Subtract Tezos from balance sheet, calculate tax figures
    for (j in 1:nrow(bs)) {
      if (xtz_balance <= 0) break
      if (bs$asset[j] == "xtz" & bs$quantity[j] > 0) {

        # Find how much can be reduced from the balance sheet
        subtract_j <- min(bs$quantity[j], xtz_balance)

        # Reduce the balance sheet accordingly
        bs$quantity[j] <- bs$quantity[j] - subtract_j

        # Calculate remaining transaction balance
        xtz_balance <- xtz_balance - subtract_j

        # Calculate transaction cost basis
        xtz_cost <- xtz_cost + subtract_j * bs$costBasis[j]

        # Update tax form 8949 dataset (Transfers adjusted in 'classify_tx.R')
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(subtract_j, bs$asset[j]),
            Date_Acquired = as_date(bs$timestamp[j]),
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(subtract_j * is_i$quote, 2),
            Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
            Codes         = NA,
            Adjustment    = NA,
            Gain_Loss     = Proceeds - Cost_Basis
          )
      }
    }

    # If balance sheet deficit, issue warning and assume cost basis of zero
    if (xtz_balance > 0) {

      warning(cat("\nTezos deficit, cost basis assumed zero:", i, is_i$id))
      tax_8949 %<>% 
        add_row(.,
          Description   = paste(xtz_balance, bs$asset[j]),
          Date_Acquired = NA,
          Date_Sold     = as_date(is_i$timestamp),
          Proceeds      = round(xtz_balance * (xtz_proceeds / is_i$xtzSent), 2),
          Cost_Basis    = 0,
          Codes         = NA,
          Adjustment    = NA,
          Gain_Loss     = Proceeds
        )

      # Add row to balance sheet for debugging
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = "xtz",
          quantity  = -1 * xtz_balance,
          costBasis = NA
        )
    }

    # Log proceeds and gain (loss) to income statement
    is$xtzProceeds[i]  <- round(xtz_proceeds, 2)
    is$xtzGainLoss[i]  <- round(xtz_proceeds, 2) - round(xtz_cost, 2)
  }

  ##############################################################################
  # Calculate gain (loss) on tokens sent.                                      #
  ##############################################################################

  if (is_i$tokenSent > 0) {

    # Initialize variables
    token_balance <- is_i$tokenSent
    token_cost <- 0

    if (is_i$tokenProceeds == 0) token_proceeds <- is_i$xtzReceived * is_i$quote
    else token_proceeds <- is_i$tokenProceeds

    j <- 1

    # Subtract tokens from balance sheet, calculate tax figures
    while (j <= nrow(bs)) {
      if (token_balance <= 0) break
      if (bs$asset[j] == is_i$tokenID & bs$quantity[j] > 0) {

        # Find how much can be reduced from the balance sheet
        subtract_j <- min(bs$quantity[j], token_balance)

        # Reduce the balance sheet accordingly
        bs$quantity[j] <- bs$quantity[j] - subtract_j

        # Calculate remaining transaction balance
        token_balance <- token_balance - subtract_j

        # Calculate transaction cost basis
        token_cost <- token_cost + subtract_j * bs$costBasis[j]

        # Update tax form 8949 dataset, ignore transfers
        if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))) {
          tax_8949 %<>% 
            add_row(.,
              Description   = paste(subtract_j, bs$asset[j]),
              Date_Acquired = as_date(bs$timestamp[j]),
              Date_Sold     = as_date(is_i$timestamp),
              Proceeds      = (
                round(subtract_j * (token_proceeds / is_i$tokenSent), 2)
              ),
              Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
              Codes         = NA,
              Adjustment    = NA,
              Gain_Loss     = Proceeds - Cost_Basis
            )
        }
      }
      
      j <- j + 1

    }
    
    # If balance sheet deficit, issue warning and assume cost basis of zero
    if (token_balance > 0) {
      
      ##########################################################################
      # Token deficit assumption (In progress, not working):                   #
      #  (1) If there is no record of a token entering the balance sheet,      #
      #      the token is assumed to have a cost basis of zero.                #
      #  (2) For tax form 8949, an acquisition date is required; in order to   #
      #      impute this value, the timestamp value is set to the last time    #
      #      the wallet had a positive token balance update.                   #
      #  (*) This assumption should be used for estimates only and is not      #
      #      an accurate or long-term solution.                                #
      ##########################################################################
      
      warning(cat("\nToken deficit assumption:", is_i$id, is_i$tokenID))
      def_acq <- NA
      
      if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))){
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(token_balance, is_i$tokenID),
            Date_Acquired = def_acq,
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(token_balance * (token_proceeds / is_i$tokenSent), 2),
            Cost_Basis    = 0,
            Codes         = NA,
            Adjustment    = NA,
            Gain_Loss     = Proceeds
          )
      }

      # Add row to balance sheet for debugging
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = is_i$tokenID,
          quantity  = -1 * token_balance,
          costBasis = NA
        )
    }

    # Log proceeds and gain (loss) to income statement
    is$tokenProceeds[i] <- round(token_proceeds, 2)
    is$tokenGainLoss[i] <- round(token_proceeds, 2) - round(token_cost, 2)

    # Add Tezos received in token transactions to balance sheet
    if (is_i$xtzReceived > 0) {
      bs %<>%
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = "xtz",
          quantity  = is_i$xtzReceived,
          costBasis = is_i$quote
        )
    }
  }

  ##############################################################################
  # Log any tokens received to balance sheet.                                  #
  ##############################################################################

  # Calculate total transaction proceeds (as cost basis)
  if (is.na(is_i$costBasis)) {
    is$costBasis[i] <- xtz_proceeds + token_proceeds
  }

  # Add tokens received to balance sheet
  if (!is.na(is_i$tokenReceived)) {
    if (is_i$tokenReceived > 0) {
      bs %<>%
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = is_i$tokenID,
          quantity  = is_i$tokenReceived,
          costBasis = is$costBasis[i] / is_i$tokenReceived
        )
    }
  }

  if (is.na(is_i$tokenReceived)) {
    print(is_i$id)
  }

  # If token sent and token received in same transaction, issue warning
  if ((is_i$tokenSent > 0) & (is_i$tokenReceived > 0)) {
     warning(cat("\nToken sent and received in same transaction!", is_i$id))
  }

  # Balance reconciliation (For debugging)
  # if (i %% 50 == 0) {
  #   is$balTZ[i] <- tzkt_balance(wallets[1], is_i$level)
  #   is$balBS[i] <- sum(select(filter(bs, asset == "xtz"), "quantity"))
  # }
}

# Add token ID column to tax data
tax_8949 %<>%
  mutate(., 
    Token_Quantity = str_split(Description, " ", simplify=TRUE)[, 1],
    Token_ID       = str_split(Description, " ", simplify=TRUE)[, 2],
    Token_ID_Short = ifelse(
      nchar(Token_ID) > 16,
      substr(Token_ID, nchar(Token_ID) - 15, nchar(Token_ID)),
      Token_ID
    ),
    Description    = paste(Token_Quantity, Token_ID_Short)
  ) 

# Save income statement, balance sheet, and tax data
save(is, file="data/is_updated.RData")
save(bs, file="data/bs.RData")
save(tax_8949, file="data/tax_8949.RData")
save(tax_8949, file="data/tax_8949_original.RData")
save(xtz_income_data, file="data/xtz_income_data.RData")
