
# tzkt API function: Pull operations by address
tzkt_operations <- function(address, level=NA, limit=NA, span=NA, quote="usd", base="https://api.tzkt.io/") {
  sfx <- paste0("v1/accounts/", address, "/operations?quote=", quote)
  if (!is.na(level)) sfx <- paste0(sfx, "&level.lt=", level)
  if (!is.na(limit)) sfx <- paste0(sfx, "&limit=", limit)
  if (!is.na(span)[1]) {
    sfx <- paste0(sfx, "&timestamp.ge=", span[1], "&timestamp.le=", span[2])
  }
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}

# tzkt API function: Pull operations by hash
tzkt_operations_hash <- function(hash, quote="usd", base="https://api.tzkt.io/") {
  sfx <- paste0("v1/operations/", hash, "?quote=", quote)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}

# tzkt API function: Pull bigmap by key and ID
tzkt_bigmap <- function(id, key, base="https://api.tzkt.io/") {
  sfx <- paste0("v1/bigmaps/", id, "/keys/", key)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}

# tzkt API function - Pull quote by block level
tzkt_quote <- function(level, quote="usd", base="https://api.tzkt.io/") {
  sfx <- paste0("v1/quotes?level=", level)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}
