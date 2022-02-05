
# tzkt API function - pull operations by address
tzkt_operations <- function(
  address, level=NA, limit=NA, span=NA, quote="usd", base="https://api.tzkt.io/"
) {
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
