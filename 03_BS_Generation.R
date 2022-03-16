# Load income statement data
load(file="data/is.RData")

# Adjust tokenID for easier debugging
is %<>% mutate(., tokenID = str_replace(tokenID, "_", "/"))

# is$balTZ <- NA
# is$balBS <- NA


# Fungible token list (Only necessary if 'collectible' is set to TRUE)
fungible <- c(
  "KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW_0", # hDAO
  "KT1Trhji1aVzDtGiAxiCfWNi9T74Kyi49DK1_0", # PURPLE
  "KT1G1cCRNBgQ48mVDjopHjEmTN5Sbtar8nn9_0", # Hedgehoge
  "KT18hYjnko76SBVv6TaCT4kU6B32mJk6JWLZ_0", # MATH
  "KT193D4vozYnhGJQVtw7CoxxqphqUEEwK6Vb_0", # QUIPU
  "KT1AM3PV1cwmGRw28DVTgsjjsjHvmL6z4rGh_0"  # akaDAO
)

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
xtzIncome <- 0

# Loop through rows of income statement to generate tax figures
for (i in 1:nrow(is)) {
  
  # Initialize variables
  xtzBalance    <- 0
  xtzCost       <- 0
  xtzProceeds   <- 0
  tokenBalance  <- 0
  tokenCost     <- 0
  tokenProceeds <- 0
  subtract_j    <- 0
  
  # Isolate row
  is_i <- is[i, ]
  
  ##############################################################################
  # Adjust Tezos sent/received:
  #  (1) If Tezos is sent and received in the same transaction, net the values
  #  (2) For token transfers, base the calculation on sender/receiver
  #  (3) For all other transactions, base the calculation on highest value
  #  (*) This assumption should be used for estimates only and is not 
  #      an accurate or long-term solution
  ##############################################################################
  
  # Adjust XTZ sent/received when tokens are involved
  # if (is_i$tokenSender %in% wallets) {
  #   is_i$xtzReceived <- is_i$xtzReceived - is_i$xtzSent
  #   is_i$xtzSent     <- 0
  # }
  # else if (is_i$tokenReceiver %in% wallets) {
  #   is_i$xtzSent     <- is_i$xtzSent - is_i$xtzReceived
  #   is_i$xtzReceived <- 0
  # }
  
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
  # Do not calculate gain (loss) for purchases or income:
  #  (1) If 'xtzBuy' is TRUE, mark as purchase
  #  (2) If Tezos are received but no tokens are sent, mark as income
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
    xtzIncome <- xtzIncome + is_i$quote * is_i$xtzReceived
    next
  }
  
  ##############################################################################
  # Calculate gain (loss) on Tezos sent
  ##############################################################################

  if (is_i$xtzSent > 0) {
    
    # Initialize variables
    xtzBalance  <- is_i$xtzSent
    xtzCost     <- 0
    xtzProceeds <- is_i$quote * is_i$xtzSent
    
    # Subtract Tezos from balance sheet, calculate tax figures
    for (j in 1:nrow(bs)) {
      if (xtzBalance <= 0) break
      if (bs$asset[j] == "xtz" & bs$quantity[j] > 0) {
        
        # Find how much can be reduced from the balance sheet
        subtract_j <- min(bs$quantity[j], xtzBalance)
        
        # Reduce the balance sheet accordingly
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        
        # Calculate remaining transaction balance
        xtzBalance <- xtzBalance - subtract_j
        
        # Calculate transaction cost basis
        xtzCost <- xtzCost + subtract_j * bs$costBasis[j]
        
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
    if (xtzBalance > 0) {
      
      warning(cat("\nTezos deficit, cost basis assumed zero:", i, is_i$id))
      tax_8949 %<>% 
        add_row(.,
          Description   = paste(xtzBalance, bs$asset[j]),
          Date_Acquired = NA,
          Date_Sold     = as_date(is_i$timestamp),
          Proceeds      = round(xtzBalance * (xtzProceeds / is_i$xtzSent), 2),
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
          quantity  = -1 * xtzBalance,
          costBasis = NA
        )
    }
    
    # Log proceeds and gain (loss) to income statement
    is$xtzProceeds[i]  <- round(xtzProceeds, 2)
    is$xtzGainLoss[i]  <- round(xtzProceeds, 2) - round(xtzCost, 2)
  }
  
  ##############################################################################
  # Calculate gain (loss) on tokens sent
  ##############################################################################
  
  if (is_i$tokenSent > 0) {
    
    # Initialize variables
    tokenBalance  <- is_i$tokenSent
    tokenCost     <- 0
    tokenProceeds <- is_i$xtzReceived * is_i$quote
    j <- 1
    
    # Subtract tokens from balance sheet, calculate tax figures
    while (j <= nrow(bs)) {
      if (tokenBalance <= 0) break
      if (bs$asset[j] == is_i$tokenID & bs$quantity[j] > 0) {
        
        # Find how much can be reduced from the balance sheet
        subtract_j <- min(bs$quantity[j], tokenBalance)
        
        # Reduce the balance sheet accordingly
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        
        # Calculate remaining transaction balance
        tokenBalance <- tokenBalance - subtract_j
        
        # Calculate transaction cost basis
        tokenCost <- tokenCost + subtract_j * bs$costBasis[j]
        
        # Update tax form 8949 dataset, ignore transfers
        if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))) {
          tax_8949 %<>% 
            add_row(.,
              Description   = paste(subtract_j, bs$asset[j]),
              Date_Acquired = as_date(bs$timestamp[j]),
              Date_Sold     = as_date(is_i$timestamp),
              Proceeds      = round(subtract_j * (tokenProceeds / is_i$tokenSent), 2),
              Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
              Codes         = ifelse(is_i$tokenID %in% fungible & collectible, NA, "C"),
              Adjustment    = NA,
              Gain_Loss     = Proceeds - Cost_Basis
            )
        }
      }
      
      # While loop is used to account for the RCS mint assumption
      j <- j + 1
      
      ##########################################################################
      # RCS mint assumption: 
      #  (1) Assume RCS tokens lacking transaction history were minted at 5tz
      #  (2) Minted RCS tokens are treated like fungible tokens
      #  (*) This assumption should be used for estimates only and is not 
      #      an accurate or long-term solution
      ##########################################################################
      if (
        (rcs_mint) & 
        (j == nrow(bs)) &
        (tokenBalance > 0) &
        (str_split(is_i$tokenID, "/")[[1]][1] == "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih")
      ) {
        is_i$tokenID <- "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih/0"
        j <- 1
      }
    }
    
    # If balance sheet deficit, issue warning and assume cost basis of zero
    if (tokenBalance > 0) {
      
      ##########################################################################
      # Token deficit assumption:
      #  (1) If there is no record of a token entering the balance sheet,
      #      the token is assumed to have a cost basis of zero. 
      #  (2) For tax form 8949, an acquisition date is required; in order to 
      #      impute this value, the timestamp value is set to the last time
      #      the wallet had a positive token balance update. 
      #  (*) This assumption should be used for estimates only and is not 
      #      an accurate or long-term solution
      ##########################################################################
      
      warning(cat("\nToken deficit assumption:", is_i$id, is_i$tokenID))
      #def_ops <- tzkt_operations_hash(hash=is_i$hash, quote=currency)
      #def_dif <- def_ops$diffs[[1]][1, ]
      #def_key <- tzkt_bigmap_updates(id=def_dif$bigmap, key=def_dif$content$hash)
      #def_acq <- as_datetime(tail(filter(def_upd, value > 0), 1)$timestamp)
      # Testing
      #def_ops <- tzkt_operations_hash(hash="oozftNvMU6akmx1QaUBnNnA1RiqGTutDR3cpgLwx8AohkvXHNem", quote=currency)
      #def_ops <- def_ops$diffs[[1]]
      def_acq <- NA
      
      if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))){
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(tokenBalance, is_i$tokenID),
            Date_Acquired = def_acq,
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(tokenBalance * (tokenProceeds / is_i$tokenSent), 2),
            Cost_Basis    = 0,
            Codes         = ifelse(is_i$tokenID %in% fungible & collectible, NA, "C"),
            Adjustment    = NA,
            Gain_Loss     = Proceeds
          )
      }
      
      # Add row to balance sheet for debugging
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = is_i$tokenID,
          quantity  = -1 * tokenBalance,
          costBasis = NA
        )
    }
    
    # Log proceeds and gain (loss) to income statement
    is$tokenProceeds[i] <- round(tokenProceeds, 2)
    is$tokenGainLoss[i] <- round(tokenProceeds, 2) - round(tokenCost, 2)
    
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
  # Log any tokens received to balance sheet
  ##############################################################################
  
  # Calculate total transaction proceeds (as cost basis)
  if (is.na(is_i$costBasis)) {
    is$costBasis[i] <- xtzProceeds + tokenProceeds
  }
  
  # Add tokens received to balance sheet
  if (is_i$tokenReceived > 0) {
    bs %<>%
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = is_i$tokenID,
        quantity  = is_i$tokenReceived,
        costBasis = is$costBasis[i] / is_i$tokenReceived
      )
  }
  
  if (is.na(is_i$tokenReceived)) {
    print(is_i$id)
  }
  
  # If token sent and token received in same transaction, issue warning
  if ((is_i$tokenSent > 0) & (is_i$tokenReceived > 0)) {
     warning(cat("\nToken sent and received in same transaction!", is_i$id))
  }
  
  # Balance debugging
  # #if (is_i$level >= 1524418) {
  # if (i %% 100 == 0) {
  #   is$balTZ[i] <- tzkt_balance("tz1L5vaycmTzEDekjDJSFZJ1V8FPwAUCVSDM", is_i$level)
  #   is$balBS[i] <- sum(select(filter(bs, asset == "xtz"), "quantity"))
  #   
  #   #is %>% mutate(., delta = round(balBS - balTZ, 2)) %>% filter(., balTZ > 0) %>% View(.)
  # }
}
