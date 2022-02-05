
# tzkt API function - pull operations by hash
tzkt_operations_hash <- function(  hash, quote="usd", base="https://api.tzkt.io/") {
  sfx <- paste0("v1/operations/", hash, "?quote=", quote)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}
