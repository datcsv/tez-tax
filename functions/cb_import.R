################################################################################
# Notes:
# (1) Download Coinbase data using: https://www.coinbase.com/report
#       Transaction history -> Generate report
#       [YEAR], [XTZ], [All transactions]
#       CSV -> Generate report -> Download
#
################################################################################

# Import Coinbase data
cb <- read_csv(file=cb_path, skip=7)
cb %<>% 
  filter(., 
    Asset == "XTZ", 
    Timestamp >= date_span[1], 
    Timestamp <= date_span[2]
  )

# Generate empty income statement row
cb_is <- is[0, ]

# For each Coinbase transaction, add row to income statment
for (i in 1:nrow(cb)) {
  
  cb_i   <- cb[i, ]
  x      <- is[0, ]
  x[1, ] <- NA
  
  x$timestamp <- cb_i$Timestamp
  x$status    <- "applied"
  x$quote     <- cb_i$`Spot Price at Transaction`
  
  # Coinbase buy
  if (cb_i$`Transaction Type` == "Buy") {
    x$xtzSent     <- 0
    x$xtzReceived <- cb_i$`Quantity Transacted`
    x$costBasis   <- cb_i$`Total (inclusive of fees)`
    x$proceeds    <- NA
    x$case        <- "Coinbase buy"
  }
  
  # Coinbase sell
  else if (cb_i$`Transaction Type` == "Sell") {
    x$xtzSent     <- cb_i$`Quantity Transacted`
    x$xtzReceived <- 0
    x$costBasis   <- NA
    x$proceeds    <- cb_i$`Total (inclusive of fees)`
    x$case        <- "Coinbase sell"
  }
  
  # Coinbase send
  else if (cb_i$`Transaction Type` == "Send") {
    x$xtzSent       <- cb_i$`Quantity Transacted`
    x$xtzReceived   <- 0
    x$costBasis     <- NA
    x$proceeds      <- NA
    x$case          <- "Coinbase send"
    x$targetAddress <- substr(cb_i$Notes, nchar(cb_i$Notes)-36, nchar(cb_i$Notes))
  }
  
  # Coinbase receive
  else if (cb_i$`Transaction Type` == "Receive") {
    x$xtzSent       <- 0
    x$xtzReceived   <- cb_i$`Quantity Transacted`
    x$costBasis     <- NA
    x$proceeds      <- NA
    x$case          <- "Coinbase receive"
  }
  
  # Coinbase convert
  else if (cb_i$`Transaction Type` == "Convert") {
    x$xtzSent     <- 0
    x$xtzReceived <- cb_i$`Quantity Transacted`
    x$costBasis   <- cb_i$`Total (inclusive of fees)`
    x$proceeds    <- NA
    x$case        <- "Coinbase convert"
  }
  
  # Coinbase income
  else if (cb_i$`Transaction Type` == "Rewards Income") {
    x$xtzSent     <- 0
    x$xtzReceived <- cb_i$`Quantity Transacted`
    x$costBasis   <- 0
    x$proceeds    <- NA
    x$case        <- "Coinbase income"
  }
  
  cb_is %<>% 
    bind_rows(., x) %>%
    mutate(., walletTx=FALSE) %>%
    filter(., )
  
}

# Combine with income statement
is %<>% 
  bind_rows(., cb_is) %>%
  arrange(., timestamp)
