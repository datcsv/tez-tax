
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
  
  # Isolate row, adjust xtz sent/received
  is_i <- is[i,]
  if ((is_i$xtzSent > 0) & (is_i$xtzReceived > 0)) {
    is_i$xtzSent <- max(is_i$xtzSent - is_i$xtzReceived, 0)
    is_i$xtzReceived <- max(is_i$xtzReceived - is_i$xtzSent, 0)
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
  else {
    
    # XTZ sent - need to generalize to all tokens?
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
    
    if (xtzBalance > 0) warning(cat("\nAsset < balance!", is[[i, "id"]]))
    if (is.na(is$proceeds[i]))  is$proceeds[i]  <- round(xtzProceeds, 2)
    if (is.na(is$gainLoss[i]))  is$gainLoss[i]  <- round(xtzProceeds - xtzCost, 2)
    if (is.na(is$costBasis[i])) is$costBasis[i] <- round(xtzProceeds, 2)
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