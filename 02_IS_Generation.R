################################################################################
#                                                                              #
# Copyright 2022 datcsv                                                        #
#                                                                              #
# Licensed under the Apache License, Version 2.0 (the "License");              #
# you may not use this file except in compliance with the License.             #
# You may obtain a copy of the License at                                      #
#                                                                              #
#   http://www.apache.org/licenses/LICENSE-2.0                                 #
#                                                                              #  
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS,            #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.     #
# See the License for the specific language governing permissions and          #
# limitations under the License.                                               #
#                                                                              #
################################################################################

# Load operations data
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
operations$quote            <- operations$quote[[1]]

# Clean operations data
operations %<>%
  group_by(., hash) %>%
  mutate(.,
    sumBakerFee      = sum(bakerFee, na.rm=TRUE),
    sumStorageFee    = sum(storageFee, na.rm=TRUE),
    sumAllocationFee = sum(allocationFee, na.rm=TRUE)
  ) %>%
  ungroup(.)

operations %<>% 
  filter(., (type == "transaction") | (SenderAddress %in% wallets)) %>%
  distinct(., id, hash, .keep_all=TRUE) %>%
  mutate(., 
    xtzAmount      = ifelse(
      (status != "backtracked") & (status != "failed") & 
      (!is.na(amount)) &  (type == "transaction"),
      amount / 1000000, 0
    ),
    xtzFee         = ifelse(
      (status != "backtracked") & (status != "failed"),
      (sumBakerFee + sumStorageFee + sumAllocationFee) / 1000000,
      (sumBakerFee) / 1000000
    ),
    xtzSent        = ifelse(SenderAddress %in% wallets, xtzAmount + xtzFee, 0),
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
    "id", "level", "timestamp", "hash", "type", "status", "quote",
    "initiatorAddress", "SenderAddress", "targetAddress", "parameterEntry",
    "parameterValue", "xtzAmount", "xtzFee", "xtzSent", "xtzReceived",
    "tokenID", "tokenAmount", "tokenSender", "tokenReceiver", "tokenSent",
    "tokenReceived", "walletTx", "xtzBuy", "xtzProceeds", "xtzGainLoss",
    "tokenProceeds", "tokenGainLoss", "costBasis"
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
save(is, file="data/is_exchange.RData")
if (!is.na(cb_path)) source("functions/cb_import.R")

# Save income statement data
save(is, file="data/is.RData")
save(is, file="data/is_original.RData")
