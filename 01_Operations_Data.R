# Get account operations: First pass 
limit_ops <- 1000
for (i in 1:length(wallets)) {
  operations_i <- tzkt_operations(
    address=wallets[i], limit=limit_ops, span=date_span, quote=currency
  )
  while (nrow(operations_i) > 0 & (nrow(operations_i) %% limit_ops) == 0) {
    level <- min(operations_i$level + 1)
    operations_i <- bind_rows(
      operations_i,
      tzkt_operations(address=wallets[i], level=level, limit=limit_ops)
    )
  }
  if (i == 1) operations <- operations_i
  else operations %<>% bind_rows(., operations_i)
}

# Get account operations: Second pass (Get operations by hash)
operations_hash <- operations %>% 
  filter(., target[[2]] %in% wallets) %>%
  distinct(., hash)

for (i in 1:nrow(operations_hash)) {
  operations_i <- tzkt_operations_hash(operations_hash[i, ], quote=currency)
  if ("storage" %in% names(operations_i)) operations_i %<>% select(., -storage)
  if ("diffs" %in% names(operations_i))   operations_i %<>% select(., -diffs)
  if ("parameter" %in% names(operations_i)) {
    if ("value" %in% names(operations_i$parameter)) {
      if (class(operations_i$parameter$value) != "list") {
        operations_i$parameter$value <- list(operations_i$parameter$value)
      }
    }
  }
  operations %<>% bind_rows(., operations_i)
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
      operations_i <- bind_rows(
        operations_i,
        tzkt_operations(address=wallets[i], level=level, limit=limit_ops)
      )
    }
    if (i == 1) objkt_operations <- operations_i
    else objkt_operations %<>% bind_rows(., operations_i)
  }
  
  objkt_operations_hash <- objkt_operations %>% distinct(., hash)
  for (i in 1:nrow(operations_hash)) {
    operations_i <- filter(objkt_operations, hash == objkt_operations_hash[i, ])
    parameter_entry <- operations_i$parameter$entrypoint
    parameter_value <- operations_i$parameter$value
    token_receiver <- list_check(operations_i$parameter$value, "to_")
    if (("swap" %in% parameter_entry > 0) & (token_receiver %in% wallets)) {
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
