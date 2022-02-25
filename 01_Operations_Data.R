
# Get account operations: First pass 
limit_ops <- 1000
contracts <- c("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z") # OBJKT v1 contract
accounts <- c(wallets, contracts)
for (i in 1:length(accounts)) {
  operations_i <- tzkt_operations(
    address=accounts[i], limit=limit_ops, span=date_span, quote=currency
  )
  while (nrow(operations_i) > 0 & (nrow(operations_i) %% limit_ops) == 0) {
    level <- min(operations_i$level + 1)
    operations_i <- bind_rows(
      operations_i,
      tzkt_operations(address=accounts[i], level=level, limit=limit_ops)
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

# Combine and clean operations data
operations %<>% 
  arrange(., id, hash) %>%
  distinct(.)

# Save transaction data
save(operations, file="data/operations.RData")
