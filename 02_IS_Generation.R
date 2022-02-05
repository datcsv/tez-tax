# Clear work space
rm(list=ls())

# Import packages
packages <- c("tidyverse", "jsonlite", "magrittr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only=TRUE))

# Import data from prior step
load(file="addresses.RData")
load(file="operations.RData")

# Define parameter function
list_check <- function(x, check) {
  y <- NA
  if ((class(x) == "list" | class(x) == "data.frame") & length(x) > 0) {
    for (i in 1:length(x)) {
      if ((!is.null(names(x)[i])) && (names(x)[i] %in% check)) y <- x[[i]][[1]]
      else y <- list_check(x[[i]], check) 
      if (!is.na(y)) break
    }
  }
  return(y)
}

# Split nested features in operations data
operations$initiatorAlias   <- operations$initiator$alias
operations$initiatorAddress <- operations$initiator$address
operations$senderAlias      <- operations$sender$alias
operations$SenderAddress    <- operations$sender$address
operations$targetAlias      <- operations$target$alias
operations$targetAddress    <- operations$target$address
operations$parameterEntry   <- operations$parameter$entrypoint
operations$parameterValue   <- operations$parameter$value
operations$quote            <- operations$quote$usd

# Clean operations data
operations %<>% 
  filter(., type == "transaction") %>%
  distinct(., id, hash, .keep_all=TRUE) %>%
  mutate(., 
    xtzAmount      = ifelse(
      (status != "backtracked") & (status != "failed") & (!is.na(amount)),
      amount / 1000000, 0
    ),
    bakerFee       = ifelse(!is.na(bakerFee), bakerFee, 0),
    storageFee     = ifelse(!is.na(storageFee), storageFee, 0),
    allocationFee  = ifelse(!is.na(allocationFee), allocationFee, 0),
    xtzFee         = (bakerFee + storageFee + allocationFee) / 1000000,
    xtzSent        = ifelse(SenderAddress %in% addresses, xtzAmount+xtzFee, 0),
    xtzReceived    = ifelse(targetAddress %in% addresses, xtzAmount, 0),
    parameterValue = ifelse(parameterValue == "NULL", NA, parameterValue),
    tokenID        = NA,
    tokenAmount    = NA,
    tokenSender    = NA,
    tokenReceiver  = NA,
    date           = as.Date(timestamp)
  ) %>%
  filter(., date < as.Date("2022-01-01")) %>%
  select(., 
    -initiator, -sender, -target, -quote, -parameter, -originatedContract, 
    -newDelegate, -block, -nonce, -gasLimit, -gasUsed, -storageLimit, 
    -storageUsed, -hasInternals, -contractBalance, -errors, -bakerFee, 
    -storageFee, -allocationFee, -amount, -type, -level, -counter, -parameters, 
    -initiatorAlias, -senderAlias, -targetAlias, -date
  )


################################################################################
# Notes:
# (1) Tezos Domains, Randomly Common Skeles, Pixel Potus, Geoff Stearns mints 
#     are currently not being registered. The data for these appears to be
#     stored in bigmaps rather than parameter values. 
#
# (2) This code is a bit of a mess - I think case modularization would be a good
#     move for future iterations. 
#
################################################################################

# Create empty income statement
is <- operations[0, ]

# For each operation hash, add a single row to the income statement
operations_hash <- operations %>% distinct(., hash)
for (i in 1:nrow(operations_hash)) {
  
  # Define helper variables
  x <- operations %>% filter(., hash == operations_hash[i, ])
  y <- x
  
  # Scrape token data, where applicable
  if (sum(c("transfer", "mint") %in% x$parameterEntry) > 0) {
    for (i in 1:nrow(x)) {
      if (sum(c("transfer", "mint") %in% x$parameterEntry[i]) > 0) {
        x$tokenID[i]       <- paste0(x$targetAddress[i], "_", list_check(x$parameterValue[i], "token_id"))
        x$tokenSender[i]   <- list_check(x$parameterValue[i], c("address", "from_"))
        x$tokenReceiver[i] <- list_check(x$parameterValue[i], c("to_", "to"))
        x$tokenAmount[i]   <- as.numeric(list_check(x$parameterValue[i], "amount"))
      }
    }
  }
  x$xtzSent     <- sum(x$xtzSent)
  x$xtzReceived <- sum(x$xtzReceived)
  xtzCollect    <- sort(x$xtzAmount, decreasing=TRUE)[2]
  
  # Failed/backtracked transaction
  if (
    ("failed"      %in% x$status) | 
    ("backtracked" %in% x$status)
  ) {
    x %<>%
      top_n(., n=1, wt=id) %>%
      mutate(., case = "Failed/backtracked transaction")
  }
  
  # Standard transaction
  else if (sum(is.na(x$parameterEntry)) == nrow(x)) {
    x %<>%
      filter(., 
        (SenderAddress %in% addresses) |
        (targetAddress %in% addresses)
      ) %>%
      mutate(., case = "Standard transaction")
  }
  
  # Token transfer
  else if (
    (nrow(x) == 1) &
    ("transfer" %in% x$parameterEntry)
  ) {
    x %<>% mutate(., 
      tokenSender = targetAddress,
      case = "Token transfer"
    )
  }
  
  
  
  # Contract signature
  else if (
    (nrow(x) == 1) &
    ("sign" %in% x$parameterEntry)
  ) {
    x %<>% mutate(., case = "Contract signature")
  }
  
  # Hic et Nunc contracts
  else if (
    ("KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9" %in% x$targetAddress) |
    ("KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn" %in% x$targetAddress) |
    ("KT1My1wDZHDGweCrJnQJi3wcFaS67iksirvj" %in% x$targetAddress)
  ) {
    
    # Hic et Nunc mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenSender   = targetAddress,
          case          = "Hic et Nunc mint"
        )
    }
    
    # Hic et Nunc swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "swap") %>% 
        mutate(., case = "Hic et Nunc swap")
    }
    
    # Hic et Nunc cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "cancel_swap") %>% 
        mutate(., case = "Hic et Nunc cancel swap")
    }
    
    # Hic et Nunc trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect <= xtzReceived, as.numeric(list_check(parameterValue, "amount")), 0),
          case        = ifelse(xtzCollect <= xtzReceived, "Hic et Nunc trade", "Hic et Nunc royalties")
        )
    }
   
    # Hic et Nunc collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., case = "Hic et Nunc collect")
    }
     
    # Hic et Nunc curate
    else if ("curate" %in% x$parameterEntry) {
      x %<>%
        top_n(., n=-1, wt=id) %>%
        mutate(., case  = "Hic et Nunc curate")
    }
    
    # Hic et Nunc registry
    else if ("registry" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "registry") %>% 
        mutate(., case  = "Hic et Nunc registry")
    }
    
    # Unidentified
    else {
      x <- y
    }
  }
  
  # QuipuSwap contracts
  else if (
    ("KT1QxLqukyfohPV5kPkw97Rs6cw1DDDvYgbB" %in% x$targetAddress) |
    ("KT1BMEEPX7MWzwwadW3NCSZe9XGmFJ7rs7Dr" %in% x$targetAddress) |
    ("KT1X3zxdTzPB9DgVzA3ad6dgZe9JEamoaeRy" %in% x$targetAddress)
  ) {
    
    # QuipuSwap buy/sell
    if (
      ("tezToTokenPayment" %in% x$parameterEntry) |
      ("tokenToTezPayment" %in% x$parameterEntry)
    ) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "QuipuSwap buy/sell"
        )
    }

    # Unidentified
    else {
      x <- y
    }
    
  }
    
  # Tezos Domains contracts
  else if (
    ("KT1P8n2qzJjwMPbHJfi4o8xu6Pe3gaU3u2A3" %in% x$targetAddress) |
    ("KT191reDVKrLxU9rjTSxg53wRqj6zh8pnHgr" %in% x$targetAddress) |
    ("KT1Mqx5meQbhufngJnUAGEGpa4ZRxhPSiCgB" %in% x$targetAddress) |
    ("KT1GBZmSxmnKJXGMdMLbugPfLyUPmuLSMwKS" %in% x$targetAddress)
  ) {
    
    # Tezos Domains commit
    if ("commit" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "commit") %>%
        mutate(., case = "Tezos Domains commit")
    }
    
    # Tezos Domains buy
    else if ("buy" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "buy") %>%
        mutate(., case = "Tezos Domains buy")
    }
    
    # Tezos Domains update record
    else if ("update_record" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "update_record") %>%
        mutate(., case = "Tezos Domains update record")
    }
    
    # Tezos Domains update reverse record
    else if ("update_reverse_record" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "update_reverse_record") %>%
        mutate(., case = "Tezos Domains update reverse record")
    }
    
    # Unidentified
    else {
      x<- y
    }
    
  }
  
  # OBJKT contracts
  else if (
    ("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z" %in% x$targetAddress) |
    ("KT1FvqJwEDWb1Gwc55Jd1jjTHRVWbYKUUpyq" %in% x$targetAddress) |
    ("KT1XjcRq5MLAzMKQ3UHsrue2SeU2NbxUrzmU" %in% x$targetAddress) |
    ("KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih" %in% x$targetAddress) |
    ("KT1QJ71jypKGgyTNtXjkCAYJZNhCKWiHuT2r" %in% x$targetAddress) |
    ("KT1Aq4wWmVanpQhq4TTfjZXB5AjFpx15iQMM" %in% x$targetAddress)
  ) {
  
    # OBJKT ask
    if ("ask" %in% x$parameterEntry) {
      x %<>%
        filter(parameterEntry == "ask") %>%
        mutate(., case = "OBJKT ask")
    }
    
    # OBJKT retract ask
    else if ("retract_ask" %in% x$parameterEntry) {
      x %<>%
        filter(parameterEntry == "retract_ask") %>%
        mutate(., case = "OBJKT retract ask")
    }
    
    # OBJKT bid
    else if ("bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzReceived = 0,
          xtzSent = ifelse(SenderAddress %in% addresses, xtzSent - xtzAmount, 0),
          case = ifelse(SenderAddress %in% addresses, "OBJKT bid", "OBJKT outbid")
        )
    }
    
    # OBJKT retract bid
    else if ("retract_bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzReceived = 0, 
          case = "OBJKT retract bid"
        )
    }
    
    # OBJKT fulfill ask (trade)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
          case = ifelse(
            xtzCollect > xtzReceived, 
            "OBJKT fulfill ask (royalties)", 
            "OBJKT fulfill ask (trade)"
          ),
        )
    }
    
    # OBJKT fulfill ask (collect)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., case = "OBJKT fulfill ask (collect)")
    }
    
    # OBJKT fulfill bid (trade)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
          case = ifelse(
            xtzCollect > xtzReceived, 
            "OBJKT fulfill bid (royalties)", 
            "OBJKT fulfill bid (trade)"
          ),
        )
    }
    
    # OBJKT fulfill bid (collect)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., case = "OBJKT fulfill bid (collect)")
    }
    
    # OBJKT buy dutch auction
    else if (
      ("buy" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., case = "OBJKT buy via Dutch auction")
    }
    
    # OBJKT create auction
    else if("create_auction" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "create_auction") %>%
        mutate(., case = "OBJKT create auction")
    }
    
    # OBJKT conclude auction
    else if("conclude_auction" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "conclude_auction") %>%
        mutate(., case = "OBJKT conclude auction")
    }
    
    # OBJKT cancel auction
    else if("cancel_auction" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "cancel_auction") %>%
        mutate(., case = "OBJKT cancel auction")
    }
    
    # OBJKT create collection
    else if ("create_artist_collection" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "create_artist_collection") %>%
        mutate(., case = "OBJKT create colletion")
    } 
    
    # OBJKT mint
    else if ("mint_artist" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint") %>%
        mutate(., 
          tokenSender = NA,
          tokenReceiver = initiatorAddress,
          case = "OBJKT mint"
        )
    }
    
    # OBJKT update metadata
    else if ("update_artist_metadata" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "update_artist_metadata") %>%
        mutate(., case = "OBJKT update metadata")
    }
    
    
    # Unidentified
    else {
      x <- y
    }
  }
  
  # akaSwap contract
  else if (
    ("KT1HGL8vx7DP4xETVikL4LUYvFxSV19DxdFN" %in% x$targetAddress) |
    ("KT1NL8H5GTAWrVNbQUxxDzagRAURsdeV3Asz" %in% x$targetAddress) |
    ("KT1ULea6kxqiYe1A7CZVfMuGmTx7NmDGAph1" %in% x$targetAddress) |
    ("KT19QcybJCf8zKCEECRhtMUYTptTwgY9jMKU" %in% x$targetAddress)
  ) {
    
    # akaSwap mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenSender = targetAddress,
          case        = "akaSwap mint"
        )
    }
    
    # akaSwap trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., 
          tokenAmount = ifelse(xtzCollect <= xtzReceived, as.numeric(list_check(parameterValue, "amount")), 0),
          case        = ifelse(xtzCollect <= xtzReceived, "akaSwap trade", "akaSwap royalties")
        )
    }
    
    # akaSwap collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., case = "akaSwap collect")
    }
    
    # akaSwap collect bundle
    else if (
      ("collect_bundle" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>%
        top_n(., n=1, wt=id) %>%
        mutate(., case = "akaSwap collect bundle")
      x_params <- x$parameterValue[[1]][[1]][[1]]
      x_n <- nrow(x_params)
      y <- x
      x <- x[0, ]
      for (i in 1:x_n) {
        x_i <- y
        x_i$tokenReceiver <- x_params$to_[[i]]
        x_i$tokenAmount   <- as.numeric(x_params$amount[[i]])
        x_i$tokenID       <- paste0(x_i$targetAddress, "_", x_params$token_id[[i]])
        x_i$xtzSent       <- x_i$xtzSent / x_n
        x %<>% bind_rows(., x_i)
      }
    }
    
    # akaSwap swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "swap") %>%
        mutate(., case = "akaSwap swap")
    }
    
    # akaSwap cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "cancel_swap") %>%
        mutate(., case = "akaSwap cancel swap")
    }
    
    # akaSwap gachapon royalties
    else if (
      ("default" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% mutate(., case = "akaSwap gachapon royalties")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Tezzardz mint
  else if (
    ("KT1LHHLso8zQWQWg1HUukajdxxbkGfNoHjh6" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint",
        !row_number() == 1,
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Tezzardz mint"
      )
  }
  
  # Gogos mint
  else if (
    ("KT1CExkW5WoKqoiv5An6uaZzN6i2Q3mxcqpW" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint",
        !row_number() == 1,
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Gogos mint"
      )
  }
  
  # Neonz mint
  else if (
    ("KT1QMAN7pWrR7fdiiMZ8mtVMMeFw2nADcVAH" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint",
        !row_number() == 1,
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Neonz mint"
      )
  }
  
  # Geoff Stearns mint
  else if (
    ("KT1Fxz4V3LaUcVFpvF8pAAx8G3Z4H7p7hhDg" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    x %<>% 
      filter(., parameterEntry == "mint") %>%
      mutate(., case = "Geoff Stearns mint")
  }
  
  # fxhash contracts
  else if (
    ("KT1AEVuykWeuuFX7QkEAMNtffzwhe1Z98hJS" %in% x$targetAddress) |
    ("KT1XCoGnfupWk7Sp8536EfrxcP73LmT68Nyr" %in% x$targetAddress) |
    ("KT1Xo5B7PNBAeynZPmca4bRh6LQow4og1Zb9" %in% x$targetAddress) |
    ("KT1Ezht4PDKZri7aVppVGT4Jkw39sesaFnww" %in% x$targetAddress) 
  ) {
    
    # fxhash issue mint
    if ("mint_issuer" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint_issuer") %>%
        mutate(., case = "fxhash issue mint")
    }
    
    # fxhash mint
    else if ("mint" %in% x$parameterEntry) {
      x %<>%
        filter(., 
          parameterEntry == "mint",
          !row_number() == 1
        ) %>%
        mutate(., case = "fxhash mint")
    }
    
    # fxhash offer
    else if ("offer" %in% x$parameterEntry) {
      x %<>%
        filter(., x$parameterEntry == "offer") %>%
        mutate(., case="fxhash offer")
    }
    
    # fxhash cancel offer
    else if ("cancel_offer" %in% x$parameterEntry) {
      x %<>%
        filter(., x$parameterEntry == "cancel_offer") %>%
        mutate(., case="fxhash cancel offer")
    }
    
    # fxhash trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          tokenAmount = ifelse(xtzCollect <= xtzReceived, as.numeric(list_check(parameterValue, "amount")), 0),
          case        = ifelse(xtzCollect <= xtzReceived, "fxhash trade", "fxhash royalties")
        )
    }
    
    # fxhash collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "fxhash collect")
    }
    
    # fxhash update profile
    else if ("update_profile" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "update_profile") %>%
        mutate(., case = "fxhash update profile")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Pixel Potus contracts
  else if ("KT1WGDVRnff4rmGzJUbdCRAJBmYt12BrPzdD" %in% x$targetAddress) {
    
    # Claim
    if ("claim" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "claim") %>%
        mutate(., case = "Pixel Potus claim")
    }
    
    # Trade
    else if ("take_trade" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "Pixel Potus trade")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Rarible contracts
  else if (
    ("KT198mqFKkiWerXLmMCw69YB1i6yzYtmGVrC" %in% x$targetAddress) |
    ("KT18pVpRXKPY2c4U2yFEGSH3ZnhB2kL8kwXS" %in% x$targetAddress)
  ) {
    
    # Rarible collect
    if (
      ("match_orders" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "Rarible collect")
    }
    
    # Rarible update operators
    else if ("update_operators_for_all" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "update_operators_for_all") %>%
        mutate(., case = "Rarible update operators")
    }
    
    # Rarible cancel
    else if ("cancel" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "cancel") %>%
        mutate(., case = "Rarible cancel")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Randomly Common Skeles mint
  else if (
    ("KT1AvxTNETj3U4b3wKYxkX6CKya1EgLZezv8" %in% x$targetAddress) &
    ("buy" %in% x$parameterEntry)
  ){
    x %<>% mutate(., case = "RCS mint")
  }
  
  # Unidentified
  else {
    x <- y
  }
  
  is %<>% bind_rows(., x)
  
}

# Debugging filter
#is %<>% filter(., row_number() > 3500)
is %<>% filter(., is.na(case))
#is %<>% filter(., case == "Rarible update operators")
#t <- operations %>% filter(., hash == "oneQ3pHjpfbJ8GCGQF7SQqtkEtCTbWjykYgnCPudCuAe4HwkdPy")
