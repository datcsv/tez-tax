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
  x$xtzBuy    <- FALSE
  x$xtzSell   <- FALSE
  
  # Coinbase buy
  if (cb_i$`Transaction Type` == "Buy") {
    x$xtzSent     <- 0
    x$xtzReceived <- cb_i$`Quantity Transacted`
    x$costBasis   <- cb_i$`Total (inclusive of fees)`
    x$proceeds    <- NA
    x$case        <- "Coinbase buy"
    x$xtzBuy      <- TRUE
  }
  
  # Coinbase sell
  else if (cb_i$`Transaction Type` == "Sell") {
    x$xtzSent     <- cb_i$`Quantity Transacted`
    x$xtzReceived <- 0
    x$costBasis   <- NA
    x$proceeds    <- cb_i$`Total (inclusive of fees)`
    x$case        <- "Coinbase sell"
    x$xtzSell     <- TRUE
  }
  
  # Coinbase send
  else if (cb_i$`Transaction Type` == "Send") {
    x$xtzSent       <- cb_i$`Quantity Transacted`
    x$xtzReceived   <- 0
    x$costBasis     <- NA
    x$proceeds      <- NA
    x$case          <- "Coinbase send"
    x$targetAddress <- substr(cb_i$Notes, nchar(cb_i$Notes)-35, nchar(cb_i$Notes))
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
    x$xtzBuy      <- TRUE
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

# Identify and adjust Coinbase/wallet transfers
drop_rows <- c()
for (i in 1:nrow(cb_is)) {
  
  if (cb_is$case[i] == "Coinbase send") {
    time_i <- cb_is$timestamp[i]
    xtz_i  <- cb_is$xtzSent[i] + cb_is$xtzReceived[i]
    is_i   <- is %>% filter(.,
      between(timestamp, time_i - 300, time_i + 300),
      between(xtzReceived + xtzFee, xtz_i - 0.1, xtz_i + 0.1)
    )
    if (nrow(is_i) == 1) { 
      id_i <- is_i$id[1]
      is[is$id == id_i, "xtzSent"] <- is[is$id == id_i, "xtzFee"]
      is[is$id == id_i, "xtzReceived"] <- 0
      is[is$id == id_i, "case"] <- cb_is$case[i]
      drop_rows <- c(drop_rows, i)
    }
  }
  
  else if (cb_is$case[i] == "Coinbase receive") {
    time_i <- cb_is$timestamp[i]
    xtz_i  <- cb_is$xtzSent[i] + cb_is$xtzReceived[i]
    is_i   <- is %>% filter(.,
      between(timestamp, time_i - 300, time_i + 300),
      between(xtzSent - xtzFee, xtz_i - 0.1, xtz_i + 0.1)
    )
    if (nrow(is_i) == 1) { 
      id_i <- is_i$id[1]
      is[is$id == id_i, "xtzSent"] <- is[is$id == id_i, "xtzFee"]
      is[is$id == id_i, "xtzReceived"] <- 0
      is[is$id == id_i, "case"] <- cb_is$case[i]
      drop_rows <- c(drop_rows, i)
    }
  }
  
}
cb_is <- cb_is[-drop_rows, ]

# Combine with income statement
is %<>% 
  bind_rows(., cb_is) %>%
  arrange(., timestamp)
