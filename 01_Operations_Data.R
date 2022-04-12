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

# Get account operations: First pass
limit_ops <- 1000
for (i in 1:length(wallets)) {
  operations_i <- tzkt_operations(
    address=wallets[i], limit=limit_ops, span=date_span, quote=currency
  )
  while ((nrow(operations_i) > 0) & (nrow(operations_i) %% limit_ops) == 0) {
    # Add one to level to ensure overlap between pulls
    level <- min(operations_i$level + 1)
    operations_i %<>% bind_rows(.,
      tzkt_operations(address=wallets[i], level=level, limit=limit_ops, span=date_span, quote=currency)
    )
  }
  if (i == 1) operations <- operations_i
  else operations %<>% bind_rows(., operations_i)
}

# Drop potential excess variables
op_names <- c(
  "type", "id", "level", "timestamp", "block", "hash", "counter", "sender",            
  "gasLimit", "gasUsed", "storageLimit", "storageUsed", "bakerFee", 
  "storageFee", "allocationFee", "target", "amount", "parameter", "status",
  "hasInternals", "quote", "initiator", "nonce", "errors", "contractBalance",
  "originatedContract"
)
operations %<>% select(., any_of(op_names))

# Get account operations: Second pass (Get operations by hash)
operations_hash <- operations %>% 
  filter(., target[[2]] %in% wallets | target[[1]] %in% wallets) %>%
  distinct(., hash)

if (nrow(operations_hash) > 0) {
  for (i in 1:nrow(operations_hash)) {
    operations_i <- tzkt_operations_hash(operations_hash[i, ], quote=currency)
    operations_i %<>% select(., any_of(op_names))
    if ("parameter" %in% names(operations_i)) {
      if ("value" %in% names(operations_i$parameter)) {
        if (class(operations_i$parameter$value) != "list") {
          operations_i$parameter$value <- list(operations_i$parameter$value)
        }
      }
    }
    operations %<>% bind_rows(., operations_i)
  }
}
else {
  warning(cat("\nNo contract operations found."))
}

# Get account operations: OBJKT v1 contract (Early auction data)
if (objkt_v1) {
  contracts <- c("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z")
  for (i in 1:length(contracts)) {
    operations_i <- tzkt_operations(
      address=contracts[i], limit=limit_ops, span=date_span, quote=currency
    )
    while (nrow(operations_i) > 0 & (nrow(operations_i) %% limit_ops) == 0) {
      level <- min(operations_i$level + 1)
      operations_i %<>% bind_rows(., 
        tzkt_operations(address=contracts[i], level=level, limit=limit_ops)
      )
    }
    if (i == 1) objkt_operations <- operations_i
    else objkt_operations %<>% bind_rows(., operations_i)
  }
  objkt_operations_hash <- objkt_operations %>% distinct(., hash)
  for (i in 1:nrow(objkt_operations_hash)) {
    operations_i <- filter(objkt_operations, hash == objkt_operations_hash[i, ])
    parameter_entry <- operations_i$parameter$entrypoint
    parameter_value <- operations_i$parameter$value
    token_receiver <- list_check(operations_i$parameter$value, "to_")
    if (("swap" %in% parameter_entry > 0) & (token_receiver %in% wallets)) {
      operations_i %<>% select(., any_of(op_names))
      operations %<>% bind_rows(., operations_i)
    }
  }
}

# Combine and clean operations data
operations %<>%
  arrange(., id, hash) %>%
  distinct(.)

# Save transaction data
save(operations, file="data/operations.RData")
