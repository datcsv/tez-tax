
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
  else {
    
    # XTZ sent
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
    is$proceeds[i]  <- round(xtzProceeds, 2)
    is$gainLoss[i]  <- round(xtzProceeds - xtzCost, 2)
    is$costBasis[i] <- round(xtzProceeds, 2)
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