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
    tokenAmount    = 0,
    tokenSender    = NA,
    tokenReceiver  = NA,
    tokenSent      = 0,
    tokenReceived  = 0,
    walletTx       = TRUE,
    xtzBuy         = FALSE,
    xtzProceeds    = 0, # Proceeds on xtz sent
    xtzGainLoss    = 0, # Gain (loss) on xtz sent
    tokenProceeds  = 0, # Proceeds on token sent
    tokenGainLoss  = 0, # Gain (loss) on token sent
    costBasis      = NA # Cost basis of all xtz/tokens received
  ) %>%
  select(., any_of(c(
    "id", "timestamp", "hash", "status", "quote", "initiatorAddress", 
    "SenderAddress", "targetAddress", "parameterEntry", "parameterValue", 
    "xtzAmount", "xtzFee", "xtzSent", "xtzReceived", "tokenID", "tokenAmount",
    "tokenSender", "tokenReceiver", "tokenSent", "tokenReceived", "walletTx", 
    "xtzBuy", "xtzProceeds", "xtzGainLoss",  "tokenProceeds", "tokenGainLoss", 
    "costBasis"
  )))

# Generate income statement from operations data:
source("functions/classify_tx.R")

# Adjust data
is %<>% 
  mutate(., 
    timestamp     = as_datetime(timestamp),
    tokenSent     = ifelse(tokenSender %in% wallets, tokenAmount, 0),
    tokenReceived = ifelse(tokenReceiver %in% wallets, tokenAmount, 0)
  ) %>%
  select(., -xtzAmount, -tokenAmount) %>%
  arrange(., timestamp)

# Add exchange data:
if (!is.na(cb_path)) source("functions/cb_import.R")

# Save income statement data
save(is, file="data/is.RData")
