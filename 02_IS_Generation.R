# Clear work space
rm(list=ls())

# Load packages
packages <- c("tidyverse", "jsonlite", "magrittr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only=TRUE))

# Load data from prior step
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
    xtzAmount      = if_else(
      status != "backtracked" & status != "failed",
      if_else(!is.na(amount), amount / 1000000, 0),
      0
    ),
    bakerFee       = ifelse(!is.na(bakerFee), bakerFee, 0),
    storageFee     = ifelse(!is.na(storageFee), storageFee, 0),
    allocationFee  = ifelse(!is.na(allocationFee), allocationFee, 0),
    xtzFee         = (bakerFee + storageFee + allocationFee) / 1000000,
    xtzSent        = if_else(SenderAddress %in% addresses, xtzAmount+xtzFee, 0),
    xtzReceived    = if_else(targetAddress %in% addresses, xtzAmount, 0),
    parameterValue = ifelse(parameterValue == "NULL", NA, parameterValue)
  ) %>%
  select(., 
    -initiator, -sender, -target, -quote, -parameter, -originatedContract, 
    -newDelegate, -block, -nonce, -gasLimit, -gasUsed, -storageLimit, 
    -storageUsed, -hasInternals, -contractBalance, -errors, -bakerFee, 
    -storageFee, -allocationFee, -amount, -type, -level, -counter, -parameters, 
    -initiatorAlias, -senderAlias, -targetAlias
  )

# Generate operations contract features
calls <- c("mint", "collect", "transfer")
for (i in 1:nrow(operations)) {
  if (operations[[i, "parameterEntry"]] %in% calls) {
    
    # Parse out standard contract variables
    operations_i <- operations[[i, "parameterValue"]]
    operations[i, "tokenID"] <- paste0(
      operations[i, "targetAddress"], 
      "_", 
      list_check(operations_i, 
      c("token_id", "objkt_id"))
    )
    if (!is.na(operations[i, "tokenID"])) {
      operations[i, "tokenAmount"]   <- as.numeric(
        list_check(operations_i, c("token_amt", "objkt_amount", "amount"))
      )
      operations[i, "tokenSender"]   <- list_check(operations_i, "from_")
      operations[i, "tokenReceiver"] <- list_check(operations_i, "to_")
    }
    
    # Adjust minting
    if (operations[[i, "parameterEntry"]] == "mint") {
      operations[i, "tokenReceiver"] <- operations[i, "initiatorAddress"]
    }
    
    # Adjust failed/backtracked transactions
    if (
      (operations[i, "status"] == "backtracked") | 
      (operations[i, "status"] == "failed")
    ) {
      operations[i, "tokenID"]       <- NA
      operations[i, "tokenAmount"]   <- NA
      operations[i, "tokenSender"]   <- NA
      operations[i, "tokenReceiver"] <- NA
    }
    
  }
}

# Generate income statement
is <- operations[0, ]


################################################################################
# Notes:
# (1) Tezos domains are not registered to income statement, but payments and
#     gas fees are still logged.
#
################################################################################

# For each operation hash, add a single row to the income statement
operations_hash <- operations %>% distinct(., hash)
for (i in 1:nrow(operations_hash)) {
  
  x <- operations %>% filter(., hash == operations_hash[i, ])
  y <- x # Back up data in case no cases are hit
  x$xtzAmount   <- as.vector(x$xtzAmount)
  x$xtzSent     <- sum(x$xtzSent)
  x$xtzReceived <- sum(x$xtzReceived)
  xtzCollect    <- sort(x$xtzAmount, decreasing=TRUE)[2]
  
  # Case: Failed/backtracked transaction
  if (
    ("failed" %in% x$status) |
    ("backtracked" %in% x$status)
  ) {
    x %<>%
      top_n(., n=1, wt=id) %>%
      mutate(., case = "Failed/backtracked transaction")
  }
  
  # Case: Standard transaction
  if (nrow(x) == 1 & is.na(x$parameterEntry[1])) {
    x %<>% 
      mutate(., case = "Standard transaction")
  }
  
  # Case: Token transfer
  else if (nrow(x) == 1 & "transfer" %in% x$parameterEntry) {
    x %<>% 
      mutate(., case = "Token transfer")
  }
  
  # Case: Contract signature
  else if (nrow(x) == 1 & "sign" %in% x$parameterEntry) {
    x %<>% mutate(., case = "Contract signature")
  }
  
  
  # Case: Multiple address transaction
  else if (sum(is.na(x$parameterEntry)) == nrow(x)) {
    x %<>%
      filter(., 
        (SenderAddress %in% addresses) |
        (targetAddress %in% addresses)  
      ) %>%
      mutate(., case = "Multiple address transaction")
  }
  
  # Case Grouping: Hic et Nunc transaction
  else if (
    ("KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9" %in% x$targetAddress) |
    ("KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn" %in% x$targetAddress) |
    ("KT1My1wDZHDGweCrJnQJi3wcFaS67iksirvj" %in% x$targetAddress)
  ) {
  
    # Case: Hic et Nunc mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenSender = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton",
          case = "Hic et Nunc mint"
        )
    }
    
    # Case: Hic et Nunc swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "swap") %>% 
        mutate(., case = "Hic et Nunc swap")
    }
    
    # Case: Hic et Nunc cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "cancel_swap") %>% 
        mutate(., case = "Hic et Nunc cancel swap")
    }
    
    # Case: Hic et Nunc collect
    else if (("collect" %in% x$parameterEntry) & (x$xtzSent[1] > 0)) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., case = "Hic et Nunc collect")
    }
    
    # Case: Hic et Nunc trade
    else if (("collect" %in% x$parameterEntry) & (x$xtzSent[1] == 0)) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
          case = ifelse(xtzCollect > xtzReceived, "Hic et Nunc royalties", "Hic et Nunc trade")
        )
    }
    
    # Case: Hic et Nunc curate
    else if ("curate" %in% x$parameterEntry) {
      x %<>%
        top_n(., n=-1, wt=id) %>%
        mutate(., case = "Hic et Nunc curate")
    }
    
    # Case: Hic et Nunc registry
    else if ("registry" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "registry") %>% 
        mutate(., case = "Hic et Nunc registry")
    }
  
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
  
  # Case: QuipuSwap buy
  else if ("tezToTokenPayment" %in% x$parameterEntry) {
    x %<>%
      filter(., parameterEntry == "transfer") %>%
      mutate(., case = "QuipuSwap buy")
  }
  
  # Case: QuipuSwap sell
  else if ("tokenToTezPayment" %in% x$parameterEntry) {
    x %<>%
      filter(., parameterEntry == "transfer") %>%
      mutate(., case = "QuipuSwap sell")
  }
  
  # Case: Tezos Domains commit
  else if (
    ("commit" %in% x$parameterEntry) &
    ("KT1P8n2qzJjwMPbHJfi4o8xu6Pe3gaU3u2A3" %in% x$targetAddress) 
  ) {
    x %<>%
      filter(., parameterEntry == "commit") %>%
      mutate(., case = "Tezos Domains commit")
  }
  
  # Case: Tezos Domains buy
  else if (
    ("buy" %in% x$parameterEntry) &
    ("KT191reDVKrLxU9rjTSxg53wRqj6zh8pnHgr" %in% x$targetAddress) 
  ) {
    x %<>%
      filter(., parameterEntry == "buy") %>%
      mutate(., case = "Tezos Domains buy")
  }
  
  # Case: Tezos Domains update record
  else if (
    ("update_record" %in% x$parameterEntry) &
    ("KT1H1MqmUM4aK9i1833EBmYCCEfkbt6ZdSBc" %in% x$targetAddress) 
  ) {
    x %<>%
      filter(., parameterEntry == "update_record") %>%
      mutate(., case = "Tezos Domains update record")
  }
  
  # Case: Tezos Domains update reverse record
  else if (
    ("update_reverse_record" %in% x$parameterEntry) &
    ("KT1J9VpjiH5cmcsskNb8gEXpBtjD4zrAx4Vo" %in% x$targetAddress) 
  ) {
    x %<>%
      filter(., parameterEntry == "update_reverse_record") %>%
      mutate(., case = "Tezos Domains update reverse record")
  }
  
  # Case: Unidentified
  else {
    x <- y
  }
  
  is %<>% bind_rows(., x)
  
}

# Filter export for debugging
#is %<>% filter(., row_number() > 3500)

