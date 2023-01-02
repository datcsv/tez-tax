################################################################################
#                                                                              #
# Copyright 2023 datcsv                                                        #
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

################################################################################
# Notes:                                                                       #
# (1) TzKT Explorer provides free REST API and WebSocket API for accessing     #
#     detailed Tezos blockchain data and helps developers build more services  #
#     and applications on top of Tezos. The following functions were built for #
#     TzKT API (1.7.0).                                                        #
#                                                                              #
#     Additional API documentation is available at: https://api.tzkt.io/       #
#                                                                              #
################################################################################

# Get account operations, https://api.tzkt.io/#operation/Accounts_GetOperations
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
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

# Get operations by hash, https://api.tzkt.io/#operation/Operations_GetByHash
tzkt_operations_hash <- function(
  hash, quote="usd", base="https://api.tzkt.io/"
) {
  sfx <- paste0("v1/operations/", hash, "?quote=", quote)
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

# Get bigmap by ID, https://api.tzkt.io/#operation/BigMaps_GetBigMapById
tzkt_bigmap <- function(id, key, base="https://api.tzkt.io/") {
  sfx <- paste0("v1/bigmaps/", id, "/keys/", key)
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

# Get bigmap key updates, https://api.tzkt.io/#operation/BigMaps_GetKeyUpdates
tzkt_bigmap_updates <- function(id, key, base="https://api.tzkt.io/") {
  sfx <- paste0("v1/bigmaps/", id, "/keys/", key, "/updates")
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

# Get quotes, https://api.tzkt.io/#operation/Quotes_Get
tzkt_quote <- function(level, quote="usd", base="https://api.tzkt.io/") {
  sfx <- paste0("v1/quotes?level=", level)
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

# Get balance at level, https://api.tzkt.io/#operation/Accounts_GetBalanceAtLevel
tzkt_balance <- function(address, level, base="https://api.tzkt.io/") {
  sfx <- paste0("v1/accounts/", address, "/balance_history/", level)
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  x <- x / 1000000
  return(x)
}

# Get block by hash, https://api.tzkt.io/#operation/Blocks_GetByHash
tzkt_block <- function(level, base="https://api.tzkt.io/") {
  sfx <- paste0("v1/blocks/", level)
  url <- paste0(base, sfx)
  x <- tryCatch(
    fromJSON(url),
    error=function(e) {
      message(e)
      message("Retrying in 10 seconds...")
      Sys.sleep(10)
      return(fromJSON(url))
    }
  )
  return(x)
}

