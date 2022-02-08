################################################################################
# Notes:
# (1) Download Coinbase data using: https://www.coinbase.com/report
#       Transaction history -> Generate report
#       [YEAR], [XTZ], [All transactions]
#       CSV -> Generate report -> Download
#
################################################################################

# Import Coinbase data
cb <- read_csv(file=cb_data, skip=7)

# Generate empty income statement row
cb_is <- is[0, ]

# For each Coinbase transaction, add row to income statment
for (i in 1:nrow(cb)) {
  
  cb_i   <- cb[i, ]
  x      <- is[0, ]
  x[1, ] <- NA
  
  x$timestamp <- as.character(cb_i$Timestamp)
  x$status    <- "applied"
  x$quote     <- cb_i$`Spot Price at Transaction`
  
  # Coinbase buy
  if (cb_i$`Transaction Type` == "Buy") {
    x$xtzSent     <- 0
    x$xtzReceived <- cb_i$`Quantity Transacted`
    x$costBasis   <- cb_i$`Total (inclusive of fees)`
  }
  
  # Coinbase sell
  else if (cb_i$`Transaction Type` == "Sell") {
    
  }
  
  # Coinbase send
  else if (cb_i$`Transaction Type` == "Send") {
    
  }
  
  # Coinbase receive
  else if (cb_i$`Transaction Type` == "Receive") {
    
  }
  
  # Coinbase convert
  else if (cb_i$`Transaction Type` == "Convert") {
    
  }
  
  # Coinbase income
  else if (cb_i$`Transaction Type` == "Rewards Income") {
    
  }
  
  cb_is %<>% bind_rows(., x)
  
}

