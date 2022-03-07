
# Load income statement data
load(file="data/is.RData")

# Generate empy balance sheet
bs <- tibble(
  timestamp = POSIXct(),
  asset     = character(),
  quantity  = double(),
  costBasis = double(),
  fungible  = logical()
)

# Generate initial balance sheet
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
        ),
        fungible  = TRUE
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
      }
    }
    if (xtzBalance > 0) {
      warning(cat("\nNegative XTZ balance, cost basis assumed zero!", is_i$id))
      bs %<>% 
        add_row(.,
          timestamp = is_i$timestamp,
          asset     = "xtz",
          quantity  = -1 * xtzBalance,
          costBasis = NA,
          fungible  = TRUE
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
          costBasis = NA,
          fungible  = FALSE
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
        costBasis = is$costBasis[i] / is_i$xtzReceived,
        fungible  = TRUE
      )
  }
  
  if (is_i$tokenReceived > 0) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = is_i$tokenID,
        quantity  = is_i$tokenReceived,
        costBasis = is$costBasis[i] / is_i$tokenReceived,
        fungible  = FALSE
      )
  }
  
  if ((is_i$xtzReceived > 0) & (is_i$tokenReceived > 0)) {
    warning(cat("\nRHS error!", is_i$id))
  }
}

# To Do:
# -Manual adjustments
# -hDAO drops
# -Adjust 'fungible' token list

# Form 8949
# (a) Description of property
# (b) Date acquired
# (c) Date sold or disposal of
# (d) Proceeds (sales price)
# (e) Cost or other basis
# (f) Code(s) from instructions
# (g) Amount of adjustment
# (h) Gain or (loss)
