# Load income statement data
load(file="data/is.RData")

# Fungible token list
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

################################################################################

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
  
  # Adjust cases where Tezos is both sent and received
  if (is_i$tokenSender %in% wallets) {
    is_i$xtzReceived <- is_i$xtzReceived - is_i$xtzSent
    is_i$xtzSent     <- 0
  }
  else if (is_i$tokenReceiver %in% wallets) {
    is_i$xtzSent     <- is_i$xtzSent - is_i$xtzReceived
    is_i$xtzReceived <- 0
  }
  
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
  }
  
  # Add Tezos income to balance sheet
  else if (
    (is_i$xtzReceived > 0) & (is_i$tokenSent == 0) & (is_i$xtzSent == 0)
  ) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = "xtz",
        quantity  = is_i$xtzReceived,
        costBasis = is_i$quote
      )
  }
  
  ##############################################################################
  
  # Calculate gain (loss) on Tezos sent
  if (is_i$xtzSent > 0) {
    
    # Initialize variables
    xtzBalance  <- is_i$xtzSent
    xtzCost     <- 0
    xtzProceeds <- is_i$quote * is_i$xtzSent
    
    # Subtract Tezos from balance sheet, calculate tax figures
    for (j in 1:nrow(bs)) {
      if (xtzBalance <= 0) break
      if (bs$asset[j] == "xtz" && bs$quantity[j] > 0) {
        
        subtract_j     <- min(bs$quantity[j], xtzBalance)
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        xtzBalance     <- xtzBalance - subtract_j
        xtzCost        <- xtzCost + subtract_j * bs$costBasis[j]
        
        # Update tax form 8949 dataset
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(subtract_j, bs$asset[j]),
            Date_Acquired = as_date(bs$timestamp[j]),
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(subtract_j * (xtzProceeds / is_i$xtzSent), 2),
            Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
            Codes         = NA,
            Adjustment    = NA,
            Gain_Loss     = Proceeds - Cost_Basis
          )
      }
    }
    
    # If balance sheet deficit, issue warning and assume cost basis of zero
    if (xtzBalance > 0) {
      
      warning(cat("\nNegative XTZ balance, cost basis assumed zero!", is_i$id))
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
    
    # Log proceeds and gain (loss) in income statement
    is$xtzProceeds[i]  <- xtzProceeds
    is$xtzGainLoss[i]  <- xtzProceeds - xtzCost
  }
  
  ##############################################################################
  
  # Calculate gain (loss) on tokens sent
  if (is_i$tokenSent > 0) {
    
    # Initialize variables
    tokenBalance  <- is_i$tokenSent
    tokenCost     <- 0
    tokenProceeds <- is_i$xtzReceived * is_i$quote
    j <- 1
    
    # Subtract tokens from balance sheet, calculate tax figures
    while (j <= nrow(bs)) {
      if (tokenBalance <= 0) break
      if (bs$asset[j] == is_i$tokenID && bs$quantity[j] > 0) {
        
        subtract_j     <- min(bs$quantity[j], tokenBalance)
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        tokenBalance   <- tokenBalance - subtract_j
        tokenCost      <- tokenCost + subtract_j * bs$costBasis[j]
        
        # Update tax form 8949 dataset, ignore transfers
        if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))) {
          tax_8949 %<>% 
            add_row(.,
              Description   = paste(subtract_j, bs$asset[j]),
              Date_Acquired = as_date(bs$timestamp[j]),
              Date_Sold     = as_date(is_i$timestamp),
              Proceeds      = round(subtract_j * (tokenProceeds / is_i$tokenSent), 2),
              Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
              Codes         = ifelse(is_i$tokenID %in% fungible, NA, "C"),
              Adjustment    = NA,
              Gain_Loss     = Proceeds - Cost_Basis
            )
        }
      }
      j <- j + 1
      
      # RCS mint assumption
      if (
        (rcs_mint) & 
        (j == nrow(bs)) & 
        (tokenBalance > 0) & 
        (str_split(is_i$tokenID, "_")[[1]][1] == "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih")
      ) {
        is_i$tokenID <- "RCS_MINT"
        j <- 1
      }
      
    }
    
    # If balance sheet deficit, issue warning and assume cost basis of zero
    if (tokenBalance > 0) {
      
      ##########################################################################
      #
      # (1) Find bigmap/bigmap key from send
      # (2) Pull bigmap storage updates using bigmap/key
      # (3) Find timestamp of first update in bigmap storage
      # (4) Issue warning if this does not work, only
      # !!!: How are multi-edition tokens treated?
      #
      ##########################################################################
      
      warning(cat("\nNegative token balance, cost basis assumed zero!", is_i$id, is_i$tokenID))
      if (!(is_i$case %in% c("Token transfer", "Wallet transfer"))){
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(tokenBalance, bs$asset[j]),
            Date_Acquired = NA,
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(tokenBalance * (tokenProceeds / is_i$tokenSent), 2),
            Cost_Basis    = 0,
            Codes         = ifelse(is_i$tokenID %in% fungible, NA, "C"),
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
    
    # Log proceeds and gain (loss) in income statement
    is$tokenProceeds[i] <- tokenProceeds
    is$tokenGainLoss[i] <- tokenProceeds - tokenCost
  }

  ##############################################################################
  
  # Calculate cost basis
  costBasis <- is_i$xtzProceeds + is_i$tokenProceeds
  if (is.na(is_i$costBasis)) {
    is$costBasis[i] <- xtzProceeds + tokenProceeds
  }
  
  # Add xtz to balance sheet
  if (is_i$xtzReceived > 0) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = "xtz",
        quantity  = is_i$xtzReceived,
        costBasis = is$costBasis[i] / is_i$xtzReceived
      )
  }
  
  if (is_i$tokenReceived > 0) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = is_i$tokenID,
        quantity  = is_i$tokenReceived,
        costBasis = is$costBasis[i] / is_i$tokenReceived
      )
  }
  
  if ((is_i$xtzReceived > 0) & (is_i$tokenReceived > 0)) {
    warning(cat("\nRHS error!", is_i$id))
  }
}
cat("\n")

################################################################################

# Update gain/loss on token transfers
is %<>% mutate(., tokenGainLoss = ifelse(case == "Token transfer", 0, tokenGainLoss))
