
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
  
  is_i <- is[i,]
  
  # Tezos exchange buy
  if (is_i$xtzBuy) {
    bs %<>% 
      add_row(.,
        timestamp   = is_i$timestamp,
        asset       = "xtz",
        quantity    = is_i$xtzReceived,
        costBasis   = ifelse(
          is.na(is_i$costBasis), is_i$quote, is_i$costBasis / is_i$xtzReceived
        ),
        fungible    = TRUE
      )
  }
  
  # Tezos exchange sell
  else if (is_i$xtzSell) {
    
  }
  
  # Else...
  else {
    
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