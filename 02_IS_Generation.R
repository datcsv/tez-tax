# Load operations data
load(file="data/operations.RData")

# Split nested features in operations data (Better way using tidyverse?)
operations$initiatorAlias   <- operations$initiator$alias
operations$initiatorAddress <- operations$initiator$address
operations$senderAlias      <- operations$sender$alias
operations$SenderAddress    <- operations$sender$address
operations$targetAlias      <- operations$target$alias
operations$targetAddress    <- operations$target$address
operations$parameterEntry   <- operations$parameter$entrypoint
operations$parameterValue   <- operations$parameter$value
operations$quote            <- operations$quote[[1]]

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
    xtzSent        = ifelse(SenderAddress %in% wallets, xtzAmount+xtzFee, 0),
    xtzReceived    = ifelse(targetAddress %in% wallets, xtzAmount, 0),
    parameterValue = ifelse(parameterValue == "NULL", NA, parameterValue),
    tokenID        = NA,
    tokenAmount    = NA,
    tokenSender    = NA,
    tokenReceiver  = NA,
    walletTx       = TRUE,
    xtzBuy         = FALSE,
    xtzSell        = FALSE
  ) %>%
  select(., 
    -initiator, -sender, -target, -parameter, -originatedContract,
    -newDelegate, -block, -nonce, -gasLimit, -gasUsed, -storageLimit, 
    -storageUsed, -hasInternals, -contractBalance, -errors, -bakerFee, 
    -storageFee, -allocationFee, -amount, -type, -level, -counter, -parameter, 
    -initiatorAlias, -senderAlias, -targetAlias
  )

# Generate income statement from operation data:
source("functions/classify_tx.R")

# Clean income statement data
is %<>% 
  mutate(., timestamp = as_datetime(timestamp)) %>%
  select(., -xtzAmount) %>%
  arrange(., timestamp)

# Add exchange data:
if (!is.na(cb_path)) source("functions/cb_import.R")

# Save income statement data
save(is, file="data/is.RData")
