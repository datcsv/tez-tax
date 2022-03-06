
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
  
  # Isolate row
  is_i <- is[i,]
  
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
    if (xtzBalance > 0) warning(cat("\nNegative XTZ balance!", is_i$id))
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
    for (j in 1:nrow(bs)) {
      if (tokenBalance <= 0) break
      if (bs$asset[j] == is_i$tokenID && bs$quantity[j] > 0) {
        subtract_j     <- min(bs$quantity[j], tokenBalance)
        bs$quantity[j] <- bs$quantity[j] - subtract_j
        tokenBalance   <- tokenBalance - subtract_j
        tokenCost      <- tokenCost + subtract_j * bs$costBasis[j]
      }
    }
    if (tokenBalance > 0) warning(cat("\nNegative token balance!", is_i$id))
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
        costBasis = round(is$costBasis[i] / is_i$xtzReceived, 2),
        fungible  = TRUE
      )
  }
  
  if (is_i$tokenReceived > 0) {
    bs %<>% 
      add_row(.,
        timestamp = is_i$timestamp,
        asset     = is_i$tokenID,
        quantity  = is_i$tokenReceived,
        costBasis = round(is$costBasis[i] / is_i$tokenReceived, 2),
        fungible  = FALSE
      )
  }
  
  if ((is_i$xtzReceived > 0) & (is_i$tokenReceived > 0)) {
    warning(cat("\nRHS error!", is_i$id))
  }
}

# Form 8949
# (a) Description of property
# (b) Date acquired
# (c) Date sold or disposal of 
# (d) Proceeds (sales price)
# (e) Cost or other basis
# (f) Code(s) from instructions
# (g) Amount of adjustment
# (h) Gain or (loss)