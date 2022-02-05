# Clear workspace
rm(list=ls())

# Load function
source("functions/load_packages.R")
source("functions/tzkt_operations.R")
source("functions/tzkt_operations_hash.R")
source("functions/list_check.R")

# Load packages
load_packages(c("tidyverse", "jsonlite", "magrittr"))

# Load data
load(file="data/addresses.RData")
load(file="data/currency.RData")
load(file="data/date_span.RData")
load(file="data/operations.RData")

# Split nested features in operations data
operations$initiatorAlias   <- operations$initiator$alias
operations$initiatorAddress <- operations$initiator$address
operations$senderAlias      <- operations$sender$alias
operations$SenderAddress    <- operations$sender$address
operations$targetAlias      <- operations$target$alias
operations$targetAddress    <- operations$target$address
operations$parameterEntry   <- operations$parameter$entrypoint
operations$parameterValue   <- operations$parameter$value
operations$quote            <- operations$quote$usd  ## UPDATE THIS!

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
    tokenReceiver  = NA
  ) %>%
  select(., 
    -initiator, -sender, -target, -parameter, -originatedContract, 
    -newDelegate, -block, -nonce, -gasLimit, -gasUsed, -storageLimit, 
    -storageUsed, -hasInternals, -contractBalance, -errors, -bakerFee, 
    -storageFee, -allocationFee, -amount, -type, -level, -counter, -parameter, 
    -initiatorAlias, -senderAlias, -targetAlias
  )

# Classify and filter transactions
source("functions/classify_tx.R")

# Debugging filter
#is %<>% filter(., is.na(case))
#is %<>% filter(., case == "OBJKT conclude auction")
