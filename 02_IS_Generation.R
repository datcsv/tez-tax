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
    tokenReceiver  = NA
  ) %>%
  select(., 
    -initiator, -sender, -target, -quote, -parameter, -originatedContract, 
    -newDelegate, -block, -nonce, -gasLimit, -gasUsed, -storageLimit, 
    -storageUsed, -hasInternals, -contractBalance, -errors, -bakerFee, 
    -storageFee, -allocationFee, -amount, -type, -level, -counter, -parameters, 
    -initiatorAlias, -senderAlias, -targetAlias
  )

################################################################################
# Notes:
# (1) Tezos domains are not registered to income statement, but payments and
#     gas fees are still logged.
#
# (2) Current filtering methodology requires whitelisting of contract addresses.
#     Assumptioms could probably be loosened in some cases without much risk.
#
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
  x$xtzSent     <- sum(x$xtzSent)
  x$xtzReceived <- sum(x$xtzReceived)
  xtzCollect    <- sort(x$xtzAmount, decreasing=TRUE)[2]
  
  # Scrape token data, where applicable
  if ("transfer" %in% x$parameterEntry) {
    x <- y %>% 
      mutate(., 
        tokenID       = paste0(targetAddress, "_", list_check(parameterValue, "token_id")),
        tokenSender   = list_check(parameterValue, "from_"),
        tokenReceiver = list_check(parameterValue, "to_"),
        tokenAmount   = as.numeric(list_check(parameterValue, "amount"))
      )
  }
  
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
  
  # Hic et Nunc contracts
  else if (
    ("KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9" %in% x$targetAddress) |
    ("KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn" %in% x$targetAddress) |
    ("KT1My1wDZHDGweCrJnQJi3wcFaS67iksirvj" %in% x$targetAddress) |
    ("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton" %in% x$targetAddress)
  ) {
    
    # Hic et Nunc mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenID       = paste0(targetAddress, "_", list_check(parameterValue, "token_id")),
          tokenSender   = targetAddress,
          tokenReceiver = list_check(parameterValue, "address"),
          tokenAmount   = as.numeric(list_check(parameterValue, "amount")),
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
        mutate(., case  = "Hic et Nunc cancel swap")
    }
    
    # Hic et Nunc trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (x$xtzSent[1] == 0)
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
      (x$xtzSent[1] > 0)
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
    
    # Hic et Nunc transfer
    else if ("transfer" %in% x$parameterEntry) {
      x %<>% mutate(., case = "Hic et Nunc transfer")
    }
    
    # Unidentified
    else {
      x <- y
    }
  }
  
  # QuipuSwap contracts
  else if (
    ("KT1QxLqukyfohPV5kPkw97Rs6cw1DDDvYgbB" %in% x$targetAddress)
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
    ("KT1XjcRq5MLAzMKQ3UHsrue2SeU2NbxUrzmU" %in% x$targetAddress)
  ) {
  
    # OBJKT bid
    if ("bid" %in% x$parameterEntry) {
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
    
    ###########################################################################
    # NOTE: WIP, TOKEN_ID INCORRECTLY POPULATED
    ###########################################################################
    # OBJKT fulfill ask (collect)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (x$xtzSent[1] > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(.,case = "OBJKT fulfill ask (collect)")
    }
    
    # Unidentified
    else {
      x <- y
    }
  }
    
  # Unidentified
  else {
    x <- y
  }
  
  is %<>% bind_rows(., x)
  
}

# Debugging filter
#is %<>% filter(., row_number() > 3500)
#is %<>% filter(., is.na(case))
is %<>% filter(., case == "OBJKT fulfill ask (collect)")
