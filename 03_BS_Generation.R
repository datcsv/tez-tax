# Load income statement data
load(file="data/is.RData")

# Generate empy balance sheet
bs <- tibble(
  timestamp = POSIXct(),
  asset     = character(),
  quantity  = double(),
  costBasis = double()
)

# Generate 8949 form
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

### NEED TO CALCULATE GAIN ON RECEIVED XTZ ###
### USE TZKT TO FIND MISSING TOKEN DATA    ###

# Generate balance sheet, updated income statement, and form 8949
for (i in 1:nrow(is)) {
  
  # Initiate variables
  xtzBalance    <- 0
  xtzCost       <- 0
  xtzProceeds   <- 0
  tokenBalance  <- 0
  tokenCost     <- 0
  tokenProceeds <- 0
  subtract_j    <- 0
  
  # Isolate row
  is_i <- is[i,]
  
  # Adjust cases where xtz is sent and received
  if (is_i$tokenSender %in% wallets) {
    is_i$xtzReceived <- is_i$xtzReceived - is_i$xtzSent
    is_i$xtzSent     <- 0
  }
  else if (is_i$tokenReceiver %in% wallets) {
    is_i$xtzSent     <- is_i$xtzSent - is_i$xtzReceived
    is_i$xtzReceived <- 0
  }
  
  # Tezos exchange buy
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
  
  # Calculate gain/loss on sent XTZ
  if (is_i$xtzSent > 0) {
    xtzBalance  <- is_i$xtzSent
    xtzCost     <- 0
    xtzProceeds <- is_i$quote * is_i$xtzSent
    for (j in 1:nrow(bs)) {
      if (xtzBalance <= 0) break
      if (bs$asset[j] == "xtz" && bs$quantity[j] > 0) {
        subtract_j     <- min(bs$quantity[j], xtzBalance)
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        xtzBalance     <- xtzBalance - subtract_j
        xtzCost        <- xtzCost + subtract_j * bs$costBasis[j]
      
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
    if (xtzBalance > 0) {
      warning(cat("\nNegative XTZ balance, cost basis assumed zero!", is_i$id))
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = "xtz",
          quantity  = -1 * xtzBalance,
          costBasis = NA
        )
      
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
      
    }
    is$xtzProceeds[i]  <- xtzProceeds
    is$xtzGainLoss[i]  <- xtzProceeds - xtzCost
  } 
  else {
    is$xtzProceeds[i]  <- 0
    is$xtzGainLoss[i]  <- 0
  }
  
  # Calculate gain/loss on sent tokens
  if (is_i$tokenSent > 0) {
    tokenBalance  <- is_i$tokenSent
    tokenCost     <- 0
    tokenProceeds <- is_i$xtzReceived * is_i$quote
    j <- 1
    while (j <= nrow(bs)) {
      if (tokenBalance <= 0) break
      if (bs$asset[j] == is_i$tokenID && bs$quantity[j] > 0) {
        subtract_j     <- min(bs$quantity[j], tokenBalance)
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        tokenBalance   <- tokenBalance - subtract_j
        tokenCost      <- tokenCost + subtract_j * bs$costBasis[j]
        
        tax_8949 %<>% 
          add_row(.,
            Description   = paste(subtract_j, bs$asset[j]),
            Date_Acquired = as_date(bs$timestamp[j]),
            Date_Sold     = as_date(is_i$timestamp),
            Proceeds      = round(subtract_j * (tokenProceeds / is_i$tokenSent), 2),
            Cost_Basis    = round(subtract_j * bs$costBasis[j], 2),
            Codes         = NA,
            Adjustment    = NA,
            Gain_Loss     = Proceeds - Cost_Basis
          )
        
      }
      j <- j + 1
      
      # RCS mint assumption
      if ((j == nrow(bs)) & (tokenBalance > 0) & (rcs_mint) & str_split(is_i$tokenID, "_")[[1]][1] == "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih") {
        is_i$tokenID <- "RCS_MINT"
        j <- 1
      }
      
    }
    if (tokenBalance > 0) {
      warning(cat("\nNegative token balance, cost basis assumed zero!", is_i$id, is_i$tokenID))
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = is_i$tokenID,
          quantity  = -1 * tokenBalance,
          costBasis = NA
        )
      
      tax_8949 %<>% 
        add_row(.,
          Description   = paste(tokenBalance, bs$asset[j]),
          Date_Acquired = NA,
          Date_Sold     = as_date(is_i$timestamp),
          Proceeds      = round(tokenBalance * (tokenProceeds / is_i$tokenSent), 2),
          Cost_Basis    = 0,
          Codes         = NA,
          Adjustment    = NA,
          Gain_Loss     = Proceeds
        )
      
    }
    is$tokenProceeds[i] <- tokenProceeds
    is$tokenGainLoss[i] <- tokenProceeds - tokenCost
  }
  else {
    is$tokenProceeds[i] <- 0
    is$tokenGainLoss[i] <- 0
  }
  
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

# Fungible token list
fungible <- c(
  "KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW_0", # hDAO
  "KT1Trhji1aVzDtGiAxiCfWNi9T74Kyi49DK1_0", # PURPLE
  "KT1G1cCRNBgQ48mVDjopHjEmTN5Sbtar8nn9_0", # Hedgehoge
  "KT18hYjnko76SBVv6TaCT4kU6B32mJk6JWLZ_0", # MATH
  "KT193D4vozYnhGJQVtw7CoxxqphqUEEwK6Vb_0", # QUIPU
  "KT1AM3PV1cwmGRw28DVTgsjjsjHvmL6z4rGh_0"  # akaDAO
)

# Update fungible variable
is %<>% mutate(., fungibleToken = (tokenID %in% fungible))

# Update gain/loss on token transfers
is %<>% mutate(., tokenGainLoss = ifelse(case == "Token transfer", 0, tokenGainLoss))

# Save income statement and balance sheet data
is_updated <- is
save(is_updated, file="data/is_updated.RData")
save(bs, file="data/bs.RData")
save(tax_8949, file="data/tax_8949.RData")
