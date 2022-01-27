
# Case: Contract signature
else if (nrow(x) == 1 & "sign" %in% x$parameterEntry) {
  x %<>% mutate(., case = "Contract signature")
}






# Case Grouping: OBJKT transactions 
else if (
  ("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z" %in% x$targetAddress) |
  ("KT1FvqJwEDWb1Gwc55Jd1jjTHRVWbYKUUpyq" %in% x$targetAddress) |
  ("KT1XjcRq5MLAzMKQ3UHsrue2SeU2NbxUrzmU" %in% x$targetAddress)
) {
  
  # Case: OBJKT bid
  if ("bid" %in% x$parameterEntry) {
    x %<>% 
      top_n(., n=-1, wt=id) %>%
      mutate(., 
             xtzSent = xtzSent - xtzAmount, 
             case = "OBJKT bid"
      )
  }
  
  # Case: OBJKT retract bid
  else if ("retract_bid" %in% x$parameterEntry) {
    x %<>% 
      top_n(., n=-1, wt=id) %>%
      mutate(., 
             xtzReceived = 0, 
             case = "OBJKT retract bid"
      )
  }
  
  # Case: OBJKT fulfill ask (collect)
  else if (
    ("fulfill_ask" %in% x$parameterEntry) & 
    (x$xtzSent[1] > 0) 
  ) {
    x %<>% 
      filter(., parameterEntry == "transfer") %>% 
      mutate(., case = "OBJKT fulfill ask (collect)")
  }
  
  # Case: OBJKT fulfill ask (trade)
  else if (
    ("fulfill_ask" %in% x$parameterEntry) & 
    (x$xtzSent[1] == 0) 
  ) {
    x %<>% 
      filter(., parameterEntry == "transfer") %>% 
      mutate(., 
             tokenAmount = ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
             case = ifelse(
               xtzCollect > xtzReceived, "OBJKT royalties", "OBJKT fulfill ask (collect)"
             ),
      )
  }
  
  # Case: OBJKT fulfill bid (collect)
  else if (
    ("fulfill_bid" %in% x$parameterEntry) & 
    (x$xtzSent[1] > 0) 
  ) {
    x %<>% 
      filter(., parameterEntry == "transfer") %>% 
      mutate(., case = "OBJKT fulfill bid (collect)")
  }
  
  # Case: OBJKT fulfill bid (trade)
  else if (
    ("fulfill_bid" %in% x$parameterEntry) & 
    (x$xtzSent[1] == 0) 
  ) {
    x %<>% 
      filter(., parameterEntry == "transfer") %>% 
      mutate(., 
             tokenAmount = ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
             case = ifelse(
               xtzCollect > xtzReceived, "OBJKT royalties", "OBJKT fulfill bid (collect)"
             ),
      )
  }
  
}

# Case Grouping: AkaSwap transactions
else if (
  ("KT1HGL8vx7DP4xETVikL4LUYvFxSV19DxdFN" %in% x$targetAddress)
) {
  
}







################################################################################
# Notes:
# (1) Account for inter-address transfers
# (2) Create date filtering
# (3) Add for hDAO/akaDAO airdrop info if FIFO requires consistency for all tx
# (4) Functionalize currency
# (5) Figure out how to work with exchange data
# (6) Swap operations API to transactions API as per suggestion from BCD?
################################################################################

# Generate price and fee features
is %<>%
  mutate(., 
         nftID       = tokenID,
         nftSent     = if_else(
           !is.na(tokenID) & (tokenSender %in% addresses),
           tokenAmount,
           0
         ),
         nftReceived = if_else(
           !is.na(tokenID) & (tokenReceiver %in% addresses),
           tokenAmount,
           0
         ),
         xtzCost     = NA,
         xtzProceeds = NA,
         xtzGainLoss = NA,
         nftCost     = NA,
         nftProceeds = NA,
         nftGainLoss = NA
  )

# Generate balance sheet
bs <- tibble(
  timestamp   = character(),
  asset       = character(),
  quantity    = double(),
  cost        = double(),
  collectible = logical()
)

for (i in 1:nrow(is)) {
  
  # Define variables
  xtzSent     <- is[[i, "xtzSent"]]
  xtzReceived <- is[[i, "xtzReceived"]]
  nftSent     <- is[[i, "nftSent"]]
  nftReceived <- is[[i, "nftReceived"]]
  nftID       <- is[[i, "nftID"]]
  timestamp   <- is[[i, "timestamp"]]
  quote       <- is[[i, "quote"]]
  
  # Subtract sent basis from balance sheet, calculate cost basis of XTZ
  # Note: Cost basis is calculated in USD
  if (xtzSent > 0) {
    xtzBalance  <- xtzSent
    xtzCost     <- 0
    xtzProceeds <- quote * xtzSent
    for (j in 1:nrow(bs)) {
      quantity_j <- bs[[j, "quantity"]]
      asset_j    <- bs[[j, "asset"]]
      cost_j     <- bs[[j, "cost"]] / bs[[j, "quantity"]] # Unit price in USD
      if (asset_j == "xtz" && quantity_j > 0) {
        subtract_j        <- min(quantity_j, xtzBalance)
        bs[j, "quantity"] <- quantity_j - subtract_j
        xtzBalance        <- xtzBalance - subtract_j
        xtzCost           <- xtzCost + subtract_j * cost_j
      }
      if (xtzBalance <= 0) break
    }
    if (xtzBalance > 0) warning(cat("Asset < balance!", is[[i, "id"]]))
    is[[i, "xtzCost"]]     <- xtzCost
    is[[i, "xtzProceeds"]] <- xtzProceeds
    is[[i, "xtzGainLoss"]] <- xtzProceeds - xtzCost
  }
  
  # Subtract sent NFT from balance sheet
  if (nftSent > 0) {
    
  }
  
  # Add received XTZ to balance sheet
  if (xtzReceived > 0) {
    xtzCost <- ifelse(nftSent == 0, 0, quote * xtzReceived)
    bs %<>% 
      add_row(.,
              timestamp   = timestamp,
              asset       = "xtz",
              quantity    = xtzReceived,
              cost        = xtzCost,
              collectible = FALSE
      )
    is[[i, "xtzCost"]]     <- xtzCost
  }
  
  # Add received NFT to balance sheet
  if (nftReceived > 0) {
    bs %<>%
      add_row(.,
              timestamp   = timestamp,
              asset       = nftID,
              quantity    = nftReceived,
              cost        = ifelse(xtzSent == 0, 0, quote * xtzReceived), # Update send amount
              collectible = TRUE
      )
  }
  
}
