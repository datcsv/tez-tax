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

# Non-FA2 tokens
nfa2 <- c(
  "KT1G1cCRNBgQ48mVDjopHjEmTN5Sbtar8nn9",
  "KT1Trhji1aVzDtGiAxiCfWNi9T74Kyi49DK1"
)

# HEN contracts
hen_contracts <- c(
  "KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9", 
  "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn", 
  "KT1My1wDZHDGweCrJnQJi3wcFaS67iksirvj",
  "KT1TybhR7XraG75JFYKSrh7KnxukMBT5dor6",
  "KT1PHubm9HtyQEJ4BBpMTVomq6mhbfNZ9z5w" # Teia
)

# QuipuSwap contracts
quipu_contracts <- c(
  "KT1QxLqukyfohPV5kPkw97Rs6cw1DDDvYgbB",
  "KT1BMEEPX7MWzwwadW3NCSZe9XGmFJ7rs7Dr",
  "KT1X3zxdTzPB9DgVzA3ad6dgZe9JEamoaeRy",
  "KT1FHiJmJUgZMPtv5F8M4ZEa6cb1D9Lf758T",
  "KT1JyPE1BWdYoRGBvvKhEPbcVRd3C9NCCwQC",
  "KT1NEa7CmaLaWgHNi6LkRi5Z1f4oHfdzRdGA",
  "KT1FG63hhFtMEEEtmBSX2vuFmP87t9E7Ab4t",
  "KT1V41fGzkdTJki4d11T1Rp9yPkCmDhB7jph",
  "KT1Qm3urGqkRsWsovGzNb2R81c3dSfxpteHG",
  "KT1U2hs5eNdeCpHouAvQXGMzGFGJowbhjqmo",
  "KT1X6dAh8fwQMkWC9yh4yuvkJaS5NjqY4NvW",
  "KT1NXdxJkCiPkhwPvaT9CytFowuUoNcwGM1p",
  "KT1JzZtBeHDBS9XyEqUc9nZhsqcfeNLxVV1T",
  "KT1X6MJFtzypK6yuMXzg4KQ9FJAccH8PazKz",
  "KT1C755xS3TLH4HEWaCJvWuoHTcytTBdEjgS",
  "KT1Ji39TrkVBd6L2SL7H4u9yvA1sPuSygthq",
  "KT1K4EwTpbvYN9agJdjpyJm4ZZdhpUNKB3F6",
  "KT1R8eiRrSoSHufHXPqevZZYEUVzBxhcdenP",
  "KT1XmsFAdPhsDnhGzQLqQQiGkCGoyekGFC8B",
  "KT1ULad9tYBY39FT8K1Dgtha7jD3qpygbLPG",
  "KT1LZqFas2GEUe5z5CFkL9Wqq6tZFzQbopm8",
  "KT1Gdix8LoDoQng7YqdPNhdP5V7JRX8FqWvM",
  "KT1WREc3cpr36Nqjvegr6WSPgQKwDjL7XxLN",
  "KT1VEKgCCYCudjfvLZaEh4havFiJMmxMgNdX",
  "KT1Eg2QesN1tCzScTrvoeKm5W67GgjV32McR",
  "KT1Vjp7tHoNXRPpd9BbGrq9pbxF1FxMFUFTE",
  "KT1GFDagtGGQS1gmC3S6Noqns1svCTzL23By",
  "KT1DksKXvCBJN7Mw6frGj6y6F3CbABWZVpj1",
  "KT1ANY7962FTf2RqJMMF4paZkuTQA77994yv",
  "KT1RRgK6eXvCWCiEGWhRZCSVGzhDzwXEEjS4",
  "KT1ANEFHasacTTZxGPmQmZo7spj5YuLE6TL4",
  "KT1X1LgNkQShpF9nRLYw3Dgdy4qp38MX617z",
  "KT1A91CcMx1izXcbBwyH3z8Do3vnEdKpbde2",
  "KT1BgezWwHBxA9NrczwK9x3zfgFnUkc7JJ4b",
  "KT1Evsp2yA19Whm24khvFPcwimK6UaAJu8Zo",
  "KT1DuYujxrmgepwSDHtADthhKBje9BosUs1w",
  "KT19g5hey69CiXRbhRzJEwvuJ95RgVLzS3TP",
  "KT1RKdp1rL3c3wxy6XWE8ZdUXdihrGjb4eGB",
  "KT1VXBX6NwapYf9Sq6LsQVr4SdsDq3ta1nss",
  "KT18fp5rcTW7mbWDmzFwjLDUhs5MeJmagDSZ",
  "KT1FptuULGK69mZRsBz62CSFdRs52etEb6Ah",
  "KT1V4jaZpCwhfitTnUucY1EHiRfz3bjqznAU",
  "KT1V4jaZpCwhfitTnUucY1EHiRfz3bjqznAU"
)

# Crunchy contracts
crunchy_contracts <- c(
  "KT1KnuE87q1EKjPozJ5sRAjQA24FPsP57CE3"
)

# Tezos Domains contracts
td_contracts <- c(
  "KT1P8n2qzJjwMPbHJfi4o8xu6Pe3gaU3u2A3",
  "KT191reDVKrLxU9rjTSxg53wRqj6zh8pnHgr",
  "KT1Mqx5meQbhufngJnUAGEGpa4ZRxhPSiCgB",
  "KT1GBZmSxmnKJXGMdMLbugPfLyUPmuLSMwKS",
  "KT1Evxe1udtPDGWrkiRsEN3vMDdB6gNpkMPM",
  "KT1EVYBj3f1rZHNeUtq4ZvVxPTs77wuHwARU"
)

# OBJKT contracts
objkt_contracts <- c(
  "KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z",
  "KT1FvqJwEDWb1Gwc55Jd1jjTHRVWbYKUUpyq",
  "KT1XjcRq5MLAzMKQ3UHsrue2SeU2NbxUrzmU",
  "KT1QJ71jypKGgyTNtXjkCAYJZNhCKWiHuT2r",
  "KT1Aq4wWmVanpQhq4TTfjZXB5AjFpx15iQMM",
  "KT1Wvk8fon9SgNEPQKewoSL2ziGGuCQebqZc"
)

# OBJKT v2 contracts
objkt_v2_contracts <- c(
  "KT1WvzYHCNBvDSdwafTHv7nJ1dWmZ8GCYuuC",
  "KT18p94vjkkHYY3nPmernmgVR7HdZFzE7NAk",
  "KT1TjnZYs5CGLbmV6yuW169P8Pnr9BiVwwjz"
)

# akaSwap contracts
aka_contracts <- c(
  "KT1HGL8vx7DP4xETVikL4LUYvFxSV19DxdFN",
  "KT1NL8H5GTAWrVNbQUxxDzagRAURsdeV3Asz",
  "KT1ULea6kxqiYe1A7CZVfMuGmTx7NmDGAph1",
  "KT19QcybJCf8zKCEECRhtMUYTptTwgY9jMKU",
  "KT1Dn3sambs7KZGW88hH2obZeSzfmCmGvpFo",
  "KT1J2C7BsYNnSjQsGoyrSXShhYGkrDDLVGDd"
)

# typed contracts
typed_contracts <- c(
  "KT1VoZeuBMJF6vxtLqEFMoc4no5VDG789D7z"
)

# 8scribo contracts
scribo_contracts <- c(
  "KT19vw7kh7dzTRxFUZNWu39773baauzNWtzj"
)

# C-VERSO contracts
cverso_contracts <- c(
  "KT1BJaN9oY2SuUzwACxSegGJynkrRbQCEEfX"
)

# fxhash contracts
fx_contracts <- c(
  "KT1AEVuykWeuuFX7QkEAMNtffzwhe1Z98hJS",
  "KT1XCoGnfupWk7Sp8536EfrxcP73LmT68Nyr",
  "KT1Xo5B7PNBAeynZPmca4bRh6LQow4og1Zb9",
  "KT1Ezht4PDKZri7aVppVGT4Jkw39sesaFnww" 
)

#fxhash v2 contracts
fx_v2_contracts <- c(
  "KT1BJC12dG17CVvPKJ1VYaNnaT5mzfnUTwXv",
  "KT1GbyoDi7H1sfXmimXpptZJuCdHMh66WS9u",
  "KT1M1NyU9X4usEimt2f3kDaijZnDMNBu42Ja" #fxtext
)

# Rarible contracts
rari_contracts <- c(
  "KT198mqFKkiWerXLmMCw69YB1i6yzYtmGVrC",
  "KT18pVpRXKPY2c4U2yFEGSH3ZnhB2kL8kwXS"
)

# NFTbutton contracs
nftbutton_contracts <- c(
  "KT1Ax9VNx2fqnknLXDAgXt3b3amkBQTR62Tj", 
  "KT1RtfsNjwQoNATiM53ZD621disELccxpyjE",
  "KT1Ax9VNx2fqnknLXDAgXt3b3amkBQTR62Tj"
)

wrap_contracts <- c(
  "KT1DLif2x9BtK6pUq9ZfFVVyW5wN2kau9rkW",
  "KT1MTnKjFSN2u4myPRBqnFuCYCPX8kTdUEnv"
)

kolibri_contracts <- c(
  "KT1Mgy95DVzqVBNYhsW93cyHuB57Q94UFhrh",
  "KT1DLaeYVgg4X21BFyFgJ8gjcR3AnPNM8ZCY"
  
)

versum_contracts <- c(
  "KT1GyRAJNdizF1nojQz62uGYkx8WFRUJm9X5",
  "KT1KRvNVubq64ttPbQarxec5XdS6ZQU4DVD2",
  "KT1NUrzs7tiT4VbNPqeTxgAFa4SXeV1f3xe9"
)

minterpop_contracts <- c(
  "KT1DgUawhCMBixK8Nt24uvMxFdjYRRbjiNGi"
)

eightbidou_contracts <- c(
  "KT1BvWGFENd4CXW5F3u4n31xKfJhmBGipoqF"
)

endless_ways_contracts <- c(
  "KT1VdCrmZsQfuYgbQsezAHT1pXvs6zKF8xHB"
)

cverso_contracts <- c(
  "KT1BJaN9oY2SuUzwACxSegGJynkrRbQCEEfX"
)

# Create null income statement
is <- operations[0, ]

# Identify unique operation groups
operations_hash <- operations %>% distinct(., hash)

# Adjust operation data
operations %<>% mutate(., bidKey = "")

# Iterate over unique operation groups to build income statement
for (i in 1:nrow(operations_hash)) {
  
  ##############################################################################
  # Define variables, as necessary
  ##############################################################################
  
  # Helper variables, hash adjustment, etc
  x <- operations %>% 
    filter(., hash == operations_hash[[1]][i]) %>%
    mutate(., hash = str_split(hash, "_", simplify=TRUE)[, 1])
  y <- x
  
  # Update token data
  if (sum(c("transfer", "mint") %in% x$parameterEntry) > 0) {
    for (i in 1:nrow(x)) {
      
      # Add token metadata, if possible
      if (sum(c("transfer", "mint") %in% x$parameterEntry[i]) > 0) {
        x$tokenID[i]       <- paste0(x$targetAddress[i], "_", list_check(x$parameterValue[i], c("token_id", "itokenid")))
        x$tokenSender[i]   <- list_check(x$parameterValue[i], c("address", "from_", "from"))
        x$tokenReceiver[i] <- list_check(x$parameterValue[i], c("to_", "to"))
        x$tokenAmount[i]   <- as.numeric(list_check(x$parameterValue[i], c("amount", "value")))
      }
    }
  }
  x$xtzSent      <- max(x$xtzSent, na.rm=TRUE)
  x$xtzReceived  <- sum(x$xtzReceived)
  xtzCollect     <- sort(x$xtzAmount, decreasing=TRUE)[2]
  xtzCollect_bid <- sort(x$xtzAmount, decreasing=TRUE)[1]
  
  ##############################################################################
  # Identify operation groups, filter and classify as necessary
  ##############################################################################
  
  # Adjust reveals
  if (("reveal" %in% x$type) & (nrow(x) > 1)) {
    x %<>% filter(., type != "reveal")
  }
  
  # Failed transaction
  if (sum(c("failed", "backtracked") %in% x$status) > 0) {
    x %<>% quick_case(., case="Failed transaction", type=2)
    x %<>% mutate(., tokenAmount=0)
  }
  
  # Adjust delegation operations
  else if ("delegation" %in% x$type) {
    x %<>% quick_case(., case="Delegation", type=2) 
  }
  
  # Adjust other non-transactionary operations
  else if (!("transaction" %in% x$type)) {
    x %<>% quick_case(., case="Non-transaction", type=2)
  }
  
  # Standard transaction
  else if (sum(is.na(x$parameterEntry)) == nrow(x)) {
    x %<>%
      filter(., (SenderAddress %in% wallets) | (targetAddress %in% wallets)) %>%
      mutate(., case = "Standard transaction")
    
    # Adjust wallet-to-wallet transfers
    if (
      (sum(x$SenderAddress %in% wallets) > 0) & 
      (sum(x$targetAddress %in% wallets) > 0)
    ) {
      x %<>% mutate(., xtzSent=xtzFee, xtzReceived=0, case="Wallet transfer")
    }
    
  }
  
  # Token transfer
  else if (nrow(x) == sum(x$parameterEntry == "transfer", na.rm=TRUE)) {
    x %<>% mutate(., tokenSender = SenderAddress, case = "Token transfer")
    
    # Adjust fxhash v2 batch transfers
    x_temp <- x
    for (i in 1:nrow(x)) {
      if (x$targetAddress[i] == "KT1U6EHmNxJTkvaWJ4ThczG4FSDaHC21ssvi") {
        x_i <- x[i,]
        tx_i = x_i$parameterValue[[1]]$txs[[1]]
        for (j in 1:nrow(tx_i)) {
          x_j <- x_i
          tx_j <- tx_i[j,]
          x_j$tokenID = str_c(x_j$targetAddress, "_", tx_j$token_id)
          x_j$tokenAmount = as.numeric(tx_j$amount)
          x_temp <- bind_rows(x_temp, x_j)
        }
      }
    }
    x <- x_temp
    x %<>% mutate(., 
      xtzFee = xtzFee / nrow(.), 
      xtzSent = xtzFee,
      tokenAmount = ifelse(is.na(tokenAmount), 0, tokenAmount)
    )
    
    # Adjust wallet-to-wallet transfers
    for (i in 1:nrow(x))
      if ((x$tokenSender[i] %in% wallets) & (x$tokenReceiver[i] %in% wallets)) {
        x$tokenAmount[i] = 0
        x$case[i] = "Wallet transfer"
      }
    
  }
  
  # Contract signature
  else if ((nrow(x) == 1) & ("sign" %in% x$parameterEntry)) {
    x %<>% mutate(., case="Contract signature")
  }
  
  # HEN contracts
  else if (sum(hen_contracts %in% x$targetAddress) > 0) {
    
    # HEN mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenSender = NA, 
          tokenReceiver = initiatorAddress, 
          case="HEN mint"
        )
    }
    
    # HEN swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="swap", case="HEN swap")
    }
    
    # HEN cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="cancel_swap", case="HEN cancel swap")
    }
    
    # HEN trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      n_collect <- sum(x$parameterEntry == "collect", na.rm=TRUE)
      if (n_collect == 1) {
        token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
        x %<>%
          filter(., parameterEntry == "transfer") %>%
          mutate(.,
            tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
            tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
            case = ifelse(
              xtzCollect != xtzReceived,
              "HEN collect (sales/royalties)",
              "HEN collect (trade)"
            )
          )
      }
      else {
        x <- y
      }
    }
    
    # HEN collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="HEN collect")
    }
    
    # HEN curate
    else if ("curate" %in% x$parameterEntry) {
      x %<>% 
        quick_case(., case="HEN curate", type=2) %>%
        mutate(., 
          tokenID = "KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW_0",
          tokenAmount = as.numeric(list_check(parameterValue, "hDAO_amount")),
          tokenSender = initiatorAddress
        )
    }
    
    # HEN claim hDAO
    else if ("claim_hDAO" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="HEN claim hDAO")
    }
  
    # HEN registry
    else if ("registry" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="registry", case="HEN registry")
    }
    
    # HEN unidentified
    else {
      x <- y
    }
  }
  
  # Crunchy contracts
  else if (sum(crunchy_contracts %in% x$targetAddress) > 0) {
    
    # Crunchy harvest
    if ("harvest" %in% x$parameterEntry) {
      
      if ("transfer" %in% x$parameterEntry) {
        x %<>% quick_case(., entry="transfer", case="Crunchy harvest")
      }
      else {
        x %<>% quick_case(., case="Crunchy harvest (No reward)", type=2)
      }
      
    }
    
    # Crunchy deposit
    else if ("deposit" %in% x$parameterEntry) {
      
      n_transfers <- nrow(filter(x, parameterEntry == "transfer"))
      
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(.,
          xtzSent = xtzFee / n_transfers,
          tokenAmount = ifelse(tokenSender %in% wallets, 0, tokenAmount), 
          case = "Crunchy deposit",
        )
      
    }
    
    # Crunchy withdrawal
    else if ("withdrawal" %in% x$parameterEntry) {
      
      n_transfers <- nrow(filter(x, parameterEntry == "transfer"))
      
      x1 <- x %>%
        filter(., parameterEntry == "transfer") %>%
        mutate(.,
          xtzSent = xtzFee / n_transfers,
          tokenAmount = ifelse(row_number() == 1, 0, tokenAmount),
          case = "Crunchy withdrawal"
        )
      
      if (x$xtzReceived[1] > 0) {
        x2 <- x %>%
          filter(., is.na(parameterEntry)) %>%
          mutate(., 
            xtzSent = 0,
            case = "Crunchy baking payment"
          )
        x <- bind_rows(x1, x2)
      }
      else {
        x <- x1
      }
      
    }
    
    # Crunchy unidentified
    else {
      x <- y
    }
    
  }
  
  # QuipuSwap contracts
  else if (sum(quipu_contracts %in% x$targetAddress) > 0) {
    
    # Drop extra approval data
    if (("approve" %in% x$parameterEntry) & (nrow(x) > 1)) {
      x %<>% mutate(., xtzSent = xtzSent - xtzFee / 2)
    }
    
    # QuipuSwap trade
    if (sum(c("tezToTokenPayment", "tokenToTezPayment") %in% x$parameterEntry) > 0) {
      x %<>% quick_case(., entry="transfer", case="QuipuSwap trade") 
    }
    
    # QuipuSwap invest liquidity
    else if ("investLiquidity" %in% x$parameterEntry) {
      
      x1 <- x %>% 
        filter(., parameterEntry == "investLiquidity") %>%
        mutate(., 
          xtzProceeds = quote * (xtzSent - (xtzFee / 2.0)),
          xtzSent = xtzSent - (xtzFee / 2.0),
          case = "QuipuSwap invest liquidity"
        )
      
      x2 <- x %>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          tokenProceeds = quote * (xtzSent - (xtzFee / 2.0)),
          xtzSent = xtzFee / 2.0,
          case = "QuipuSwap invest liquidity"
        )
      
      # Find LP token amount
      tx_id <- x1$id[1]
      tx_hash <- x1$hash[1]
      tx_operations <- tzkt_operations_hash(tx_hash, quote=currency)
      tx_operations %<>% filter(., id == tx_id)
      
      if (tx_operations$diffs[[1]][2,]$action == "add_key") {
        token_received <- as.numeric(tx_operations$diffs[[1]][2,]$content$value$balance)
      }
      else {
        
        id <- tx_operations$diffs[[1]][2, ]$bigmap
        key <- tx_operations$diffs[[1]][2, ]$content$key
        bigmap <- tzkt_bigmap_updates(id=id, key=key)
        
        bigmap$balance <- as.numeric(bigmap$value$balance)
        bigmap$frozen_balance <- as.numeric(bigmap$value$frozen_balance)
        
        bigmap %<>% 
          mutate(., balance = balance + frozen_balance) %>%
          group_by(., level) %>%
          filter(., row_number() >= (n() - 1)) %>%
          ungroup(.) %>%
          mutate(., delta = balance - lag(balance)) %>%
          filter(., level == x1$level[1])
          
        token_received <- bigmap$delta[1]
        
      }
      
      x3 <- x %>%
        filter(., parameterEntry == "investLiquidity") %>%
        mutate(., 
          costBasis = 2.0 * quote * (xtzSent - (xtzFee / 2.0)),
          xtzSent = 0,
          tokenAmount = token_received,
          tokenReceiver = x2$initiatorAddress[1],
          tokenID = paste0(x2$tokenID[1], "_LP"),
          case = "QuipuSwap invest liquidity"
        )
      
      x <- bind_rows(x1, x2, x3)
      
    } 
    
    # QuipuSwap divest liquidity
    else if ("divestLiquidity" %in% x$parameterEntry) {
      
      x1 <- x %>%
        filter(., is.na(parameterEntry)) %>%
        mutate(., 
          xtzBuy = TRUE,
          case = "QuipuSwap divest liquidity"
        )
      
      x2 <- x %>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          xtzSent = 0,
          xtzReceived = 0,
          costBasis = quote * x1$xtzReceived[1],
          case = "QuipuSwap divest liquidity"
        )
      
      x3 <- x %>%
        filter(., parameterEntry == "divestLiquidity") %>%
        mutate(., 
          xtzSent = 0,
          xtzReceived = 0,
          tokenSender = SenderAddress,
          tokenAmount = as.numeric(list_check(parameterValue, "shares")),
          tokenID = paste0(x2$tokenID[1], "_LP"),
          tokenProceeds = 2 * quote * x1$xtzReceived[1],
          case = "QuipuSwap divest liquidity"
        )
      
      x <- bind_rows(x1, x2, x3)
      
    }
    
    # QuipuSwap vote
    else if ("vote" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="vote", case="QuipuSwap vote") 
    }
    
    # Quipuswap unidentified
    else {
      x <- y
    }
  }
  
  #0xSelfie contract
  else if (sum("KT1LMrt6NKe86GUeCxHXjXkf5UD4e5uUTSkP" %in% x$targetAddress) > 0) {
    
    # 0xSelfie mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% quick_case(., case="0xSelfie mint", type=2)
    }
    
    # 0xSelfie access
    else if ("request_access" %in% x$parameterEntry) {
      x %<>% quick_case(., case="0xSelfie access", type=2)
    }
    
    # 0xSelfie unidentified
    else {
      x <- y
    }
    
  }
  
  # Marina hero mint
  else if (sum("KT1Q2jUJnrvrrhi4gBpZVLm37nyCqaFNtK7X" %in% x$targetAddress) > 0) {

    # Marina pay
    if ("default" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="default", case="Marina payment")
    }
    
    # Marina unidentified
    else {
      x <- y
    }
    
  }
  
  # Tezos Domains contracts
  else if (sum(td_contracts %in% x$targetAddress) > 0) {
    
    # TD commit
    if ("commit" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="commit", case="TD commit")
    }
    
    # TD buy
    else if ("buy" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="buy", case="TD buy")
    }
    
    # TD sell
    else if (
      ("execute_offer" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="TD sell (market)")
    }
    
    # TD buy
    else if (
      ("execute_offer" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="TD buy (market)")
    }
    
    # TD update record
    else if ("update_record" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_record", case="TD update record")
    }
    
    # TD update reverse record
    else if ("update_reverse_record" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_reverse_record", case="TD update reverse record")
    } 
    
    # TD place offer
    else if ("place_offer" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="place_offer", case="TD place offer")
    }
    
    # TD renew
    else if ("renew" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="renew", case="TD renew")
    }
    
    # TD remove offer
    else if ("remove_offer" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="remove_offer", case="TD remove offer")
    }
    
    # TD update operators
    else if ("update_operators" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_operators", case="TD update operators")
    }
    
    # TD unidentified
    else {
      x<- y
    }
  }
  
  # OBJKT contracts
  else if (sum(objkt_contracts %in% x$targetAddress) > 0) {
    
    # OBJKT ask
    if ("ask" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="ask", case="OBJKT ask")
    }
    
    # OBJKT retract ask
    else if ("retract_ask" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="retract_ask", case="OBJKT retract ask")
    }
    
    # OBJKT retract bid
    else if ("retract_bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          tokenAmount = 0, 
          xtzSent = xtzSent / 2,
          xtzFee = xtzFee / 2,
          xtzReceived = 0,
          case = "OBJKT retract bid"
        )
      
      # Check if bid recorded, zero out if so
      tx_id <- x$id[1]
      tx_hash <- x$hash[1]
      tx_operations <- tzkt_operations_hash(tx_hash, quote=currency)
      tx_operations %<>% filter(., type == "transaction")
      id <- tx_operations$diffs[[1]]$bigmap
      key <- tx_operations$diffs[[1]]$content$hash
      
      if ((!is.na(key)) & (key %in% is$bidKey)) {
        is %<>% mutate(.,
          tokenAmount = ifelse(bidKey == key & case != "OBJKT win auction (6210)", 0, tokenAmount), 
          xtzSent = ifelse(bidKey == key & case != "OBJKT win auction (6210)", 0, xtzSent),
          xtzReceived = ifelse(bidKey == key & case != "OBJKT win auction (6210)", 0, xtzReceived),
          case = ifelse(bidKey == key & case != "OBJKT win auction (6210)", "OBJKT retract bid", case)
        )
      }
      
    }
    
    # OBJKT bid
    else if ("bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzReceived = ifelse(SenderAddress %in% wallets, 0, 0),
          xtzSent = ifelse(SenderAddress %in% wallets, xtzSent - xtzAmount, 0),
          case = ifelse(SenderAddress %in% wallets, "OBJKT bid", "OBJKT outbid")
        )
      
      # Check if auction was won
      tx_id <- x$id[1]
      tx_hash <- x$hash[1]
      tx_operations <- tzkt_operations_hash(tx_hash, quote=currency)
      tx_operations %<>% filter(., id == tx_id)
      id  <- tx_operations$diffs[[1]]$bigmap
      key <- tx_operations$diffs[[1]]$content$key
      bigmap <- tzkt_bigmap(id=id, key=key)
      
      # Omit early OBJKT contracts
      if (x$targetAddress[1] != "KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z") {
        
        if (id == 6210) {
          state    <- as.numeric(bigmap$value$state)
          price    <- as.numeric(bigmap$value$current_price) / 1000000
          buyer    <- bigmap$value$highest_bidder
          time     <- bigmap$value$end_time
          token_id <- paste0(bigmap$value$fa2, "_", bigmap$value$objkt_id)
          bidHash  <- NA
          bidCase  <- "OBJKT win auction (6210)"
        }
        else if (id == 5910) {
          state    <- ifelse(bigmap$active == "FALSE", 2, 1)
          price    <- as.numeric(bigmap$value$xtz_per_objkt) / 1000000
          buyer    <- bigmap$value$issuer
          time     <- tzkt_block(bigmap$lastLevel)$timestamp
          token_id <- paste0(bigmap$value$fa2, "_", bigmap$value$objkt_id)
          bidHash  <- tx_operations$diffs[[1]]$content$hash
          bidCase  <- "OBJKT win auction (5910)"
        }
        else {
          warning(cat("\nOBJKT auction contract not recognized:", tx_hash))
          state <- NA
        }
        
        if (
          (state == 2) &
          (as.Date(time) <= as.Date(date_span[2])) &
          (buyer %in% wallets) &
          (price == x$xtzAmount[[1]])
        ) {
          x2 <- mutate(x,
            timestamp    = time,
            quote        = tx_operations$quote[[1]],
            xtzSent      = price,
            xtzReceived  = 0,
            tokenID      = token_id,
            tokenAmount  = 1,
            tokenReceiver= buyer,
            case         = bidCase,
            bidKey       = bidHash
          )
          x %<>% bind_rows(., x2)
        }
      }
    }
    
    # OBJKT win auction (old contract)
    else if (
      ("swap" %in% x$parameterEntry) & 
      ("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z" %in% x$targetAddress)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT win auction (old)")
      tx_hash <- x$hash[1]
      tx_operations <- tzkt_operations_hash(tx_hash, quote=currency)
      tx_operations %<>% filter(., tx_operations$parameter$entrypoint == "swap")
      price <- as.numeric(tx_operations$diffs[[1]]$content$value$xtz_per_objkt) / 1000000
      x %<>% mutate(., xtzSent = price)
    }
    
    # OBJKT fulfill ask (trade)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
          tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
          case = ifelse(
              xtzCollect != xtzReceived, 
              "OBJKT fulfill ask (sales/royalties)", 
              "OBJKT fulfill ask (trade)"
            )
        )
    }
    
    # OBJKT fulfill ask (collect)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT fulfill ask (collect)")
    }
    
    # OBJKT fulfill bid (trade)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      token_sender <- x$initiatorAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect_bid != xtzReceived, 0, tokenAmount),
          tokenSender = ifelse(xtzCollect_bid != xtzReceived, NA, token_sender),
          xtzSent = xtzFee,
          case = ifelse(
            xtzCollect != xtzReceived, 
            "OBJKT fulfill bid (sales/royalties)", 
            "OBJKT fulfill bid (trade)"
          )
        )
    }
    
    # OBJKT fulfill bid (collect)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT fulfill bid (collect)")
    }
    
    # OBJKT buy dutch auction
    else if (
      ("buy" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT buy dutch auction")
    }
    
    # OBJKT create auction
    else if("create_auction" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="create_auction", case="OBJKT create auction")
    }
    
    # OBJKT conclude auction
    else if("conclude_auction" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="conclude_auction", case="OBJKT conclude auction") 
    }
    
    # OBJKT cancel auction
    else if("cancel_auction" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="cancel_auction", case="OBJKT cancel auction")
    }
    
    # OBJKT create collection
    else if ("create_artist_collection" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="create_artist_collection", case="OBJKT create collection")
    } 
    
    # OBJKT mint
    else if ("mint_artist" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint") %>%
        mutate(., 
          tokenSender = NA,
          tokenReceiver = initiatorAddress, 
          case = "OBJKT mint"
        )
    }
    
    # OBJKT update metadata
    else if ("update_artist_metadata" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_artist_metadata", case="OBJKT update metadata")
    }
    
    # OBJKT unidentified
    else {
      x <- y
    }
  }
  
  # OBJKT v2 contracts
  else if (sum(objkt_v2_contracts %in% x$targetAddress) > 0) {
    
    # OBJK v2 ask
    if ("ask" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="ask", case="OBJKT v2 ask")
    }
    
    # OBJKT v2 retract ask
    else if ("retract_ask" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="retract_ask", case="OBJKT v2 retract ask")
    }
    
    # OBJKT v2 offer
    else if ("offer" %in% x$parameterEntry) {
      x %<>% mutate(.,
        xtzSent = xtzFee,
        case = "OBJKT v2 offer"
      )
    }
    
    # OBJKT v2 bid
    else if ("bid" %in% x$parameterEntry) {
      x %<>%
        filter(., 
          parameterEntry == "transfer", 
          tokenReceiver == "KT18p94vjkkHYY3nPmernmgVR7HdZFzE7NAk"
        ) %>%
        mutate(., case = "OBJKT v2 bid")
    }

    # OBJKT v2 unwrap
    else if ("unwrap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="unwrap", case="OBJKT v2 unwrap") %>%
        mutate(., 
          tokenAmount = as.numeric(parameterValue[[1]]), 
          tokenID = "KT1TjnZYs5CGLbmV6yuW169P8Pnr9BiVwwjz_0",
          tokenSender = SenderAddress
        )
    }
    
    # OBJKT v2 fulfill offer (trade)
    else if (
      ("fulfill_offer" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
               tokenAmount = ifelse(xtzCollect_bid != xtzReceived, 0, tokenAmount),
               tokenSender = ifelse(xtzCollect_bid != xtzReceived, NA, token_sender),
               case = ifelse(
                 xtzCollect_bid != xtzReceived, 
                 "OBJKT v2 fulfill offer (sales/royalties)", 
                 "OBJKT v2 fulfill offer (trade)"
               )
        )
      
      if (sum("OBJKT v2 fulfill offer (sales/royalties)" %in% x$case) > 0) {
        print(x$xtzReceived)
        print(x$xtzFee)
        print( round(x$xtzReceived + x$xtzFee, 2))
        print(xtzCollect)
        print(xtzCollect_bid)
      }
      
    }
    
    # OBJKT v2 fulfill offer (collect)
    else if (
      ("fulfill_offer" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT v2 fulfill offer (collect)")
    }
    
    # OBJKT v2 fulfill ask (trade)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
          tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
          case = ifelse(
          xtzCollect != xtzReceived, 
            "OBJKT v2 fulfill ask (sales/royalties)", 
            "OBJKT v2 fulfill ask (trade)"
          )
        )
    }
    
    # OBJKT v2 fulfill ask (collect)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT v2 fulfill ask (collect)")
    }
    
    # OBJKT retract offer
    else if ("retract_offer" %in% x$parameterEntry) {
      x %<>% 
        quick_case(., entry="retract_offer", case="OBJKT v2 retract offer") %>%
        mutate(., xtzSent = xtzFee, xtzReceived = 0)
    }
    
    # OBJKT v2 unidentified
    else {
      x <- y
    }
  }
  
  # C-Verso contracts
  else if (sum(cverso_contracts %in% x$targetAddress) >0) {
    
    # C-Verso mint
    if ("mint_token" %in% x$parameterEntry) {
      x_hash <- tzkt_operations_hash(x$hash[1])
      token_id <- as.numeric(x_hash[2,]$storage$last_token_id) - 1
      token_id <- paste0(x_hash[2,]$target$address, "_", token_id)
      x$tokenID <- token_id
      x %<>% 
        quick_case(., entry="mint", case="C-Verso mint") %>%
        mutate(., tokenAmount = 1)
    }
    
    # C-Verso unidentified
    else {
      x <- y
    }
    
  }

  # typed contracts
  else if (sum(typed_contracts %in% x$targetAddress) > 0) {
    
    # typed trade
    if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., 
          tokenAmount = ifelse(
            xtzCollect != xtzReceived, 
            0, 
            as.numeric(list_check(parameterValue, "amount"))
          ),
            tokenSender = ifelse(
            xtzCollect != xtzReceived, 
            NA, 
            token_sender
          ),
          case = ifelse(
            xtzCollect != xtzReceived, 
            "typed collect (sales/royalties)", 
            "typed collect (trade)"
          )
        )
    }
    
    # typed collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., case="typed collect", type=2)
    }
    
    # typed unidentified
    else {
      x <- y
    }
    
  }
  
  # 8scribo contracts
  else if (sum(scribo_contracts %in% x$targetAddress) > 0) {
    
    # 8scribo trade
    if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., 
          tokenAmount = ifelse(
            xtzCollect != xtzReceived, 
            0, 
            as.numeric(list_check(parameterValue, "amount"))
          ),
          tokenSender = ifelse(
            xtzCollect != xtzReceived, 
            NA, 
            token_sender
          ),
          case = ifelse(
            xtzCollect != xtzReceived, 
            "8scribo collect (sales/royalties)", 
            "8scribo collect (trade)"
          )
        )
    }
    
    # 8scribo collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., case="8scribo collect", type=2)
    }
    
    # 8scribo unidentified
    else {
      x <- y
    }
    
  }
    
  # akaSwap contracts
  else if (sum(aka_contracts %in% x$targetAddress) > 0) {
    
    # akaSwap mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
          tokenSender = NA, 
          tokenReceiver = initiatorAddress, 
          case = "akaSwap mint"
        )
    }
    
    # akaSwap trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., 
          tokenAmount = ifelse(
            xtzCollect != xtzReceived, 
            0, 
            as.numeric(list_check(parameterValue, "amount"))
          ),
          tokenSender = ifelse(
            xtzCollect != xtzReceived, 
            NA, 
            token_sender
          ),
          case = ifelse(
            xtzCollect != xtzReceived, 
            "akaSwap collect (sales/royalties)", 
            "akaSwap collect (trade)"
          )
        )
    }
    
    # akaSwap collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., case="akaSwap collect", type=2)
    }
    
    # akaSwap collect bundle
    else if (
      ("collect_bundle" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., case="akaSwap collect bundle", type=2)
      x_params <- x$parameterValue[[1]][[1]][[1]]
      x_n <- nrow(x_params)
      y <- x
      x <- x[0, ]
      for (i in 1:x_n) {
        x_i <- y
        x_i$tokenReceiver <- x_params$to_[[i]]
        x_i$tokenAmount <- as.numeric(x_params$amount[[i]])
        x_i$tokenID <- paste0(x_i$targetAddress, "_", x_params$token_id[[i]])
        x_i$xtzSent <- x_i$xtzSent / x_n
        x %<>% bind_rows(., x_i)
      }
    }
    
    # akaSwap swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="swap", case="akaSwap swap")
    }
    
    # akaSwap cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>%  quick_case(., entry="cancel_swap", case="akaSwap cancel swap")
    }
    
    # akaSwap gachapon royalties
    else if (
      ("default" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      target_address <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
      x %<>% 
        filter(., targetAddress == target_address) %>%
        mutate(., case = "akaSwap gachapon royalties")
    }
    
    # akaSwap offer
    else if ("make_offer" %in% x$parameterEntry) {
      x %<>% mutate(., 
        xtzSent = xtzFee,
        case = "akaSwap offer"              
      )
    }
    
    # akaSwap cancel offer
    else if ("cancel_offer" %in% x$parameterEntry) {
      x %<>% mutate(., 
        xtzReceived = 0,
        case = "akaSwap cancel offer"              
      )
    }
    
    # akaSwap unidentified
    else {
      x <- y
    }
  }
  
  # 8bidou contracts
  else if (sum(eightbidou_contracts %in% x$targetAddress) > 0) {
    
    # 8bidou buy
    if (
      ("buy" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="8bidou buy")
    }
    
    # 8bidou unidentified
    else {
      x <- y
    }
  }
  
  # Endless Ways contracts
  else if (sum(endless_ways_contracts %in% x$targetAddress) > 0) {
    
    # 8bidou buy
    if (
      ("mint_and_purchase" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="mint_and_purchase", case="Endless ways mint")
    }
    
    # 8bidou unidentified
    else {
      x <- y
    }
  }
  
  # Mooncakes mint
  else if (
    ("KT1WvV2rPBQUFUqtCWmnnj8JX2gkmDtMBzQi" %in% x$targetAddress) &
    ("mint" %in% x$parameterEntry)
  ) {
    x %<>% quick_case(., entry="mint", case="mooncakes mint")
  }
  
  # Tezzardz mint
  else if (
    ("KT1LHHLso8zQWQWg1HUukajdxxbkGfNoHjh6" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint",
        !row_number() == 1
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Tezzardz mint"
      )
  }
  
  # Gogos mint
  else if (
    ("KT1CExkW5WoKqoiv5An6uaZzN6i2Q3mxcqpW" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint",
        !row_number() == 1
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Gogos mint"
      )
  }
  
  # Neonz mint
  else if (
    ("KT1QMAN7pWrR7fdiiMZ8mtVMMeFw2nADcVAH" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint", 
        !row_number() == 1
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Neonz mint"
      )
  }
  
  # Ziggurats mint
  else if (
    ("KT1NC4pPLbhcw5JRq89NnHgwXg9uGztSZm1k" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    tz <- as.numeric(x$parameterValue[[1]])
    x %<>%
      filter(., 
        parameterEntry == "mint", 
        !row_number() == 1
      ) %>% 
      mutate(., 
        xtzSent = xtzSent / tz,
        tokenSender = targetAddress,
        tokenReceiver = list_check(parameterValue, "address"),
        case = "Ziggurats mint"
      )
  }
  
  # Hash Three Points mint
  else if (
    ("KT1Fxz4V3LaUcVFpvF8pAAx8G3Z4H7p7hhDg" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    x %<>% quick_case(., entry="mint", case="H3P mint")
    token_id <- as.numeric(tzkt_operations_hash(x$hash[1])$storage$next_id) - 1
    token_id <- paste0(x$targetAddress[1], "_", token_id)
    x$tokenID <- token_id
  }

  # Randomly Common Skeles mint
  else if (
    ("KT1AvxTNETj3U4b3wKYxkX6CKya1EgLZezv8" %in% x$targetAddress) &
    ("buy" %in% x$parameterEntry)
  ){
    x %<>% quick_case(., entry="buy", case="RCS mint")
    
    # RCS mint assumption logic
    if (rcs_mint) {
      x %<>% 
        mutate(., 
          tokenID = "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih_0",
          tokenReceiver = SenderAddress,
          tokenAmount = round((xtzSent - xtzFee) / 5.00, 0)
        )
    }
    
  }
  
  # Pixel Potus contracts
  else if ("KT1WGDVRnff4rmGzJUbdCRAJBmYt12BrPzdD" %in% x$targetAddress) {
    
    # Pixel Potus claim
    if ("claim" %in% x$parameterEntry) {
      claim_paid <- sum(filter(x, parameterEntry == "claim_paid")$xtzAmount)
      x %<>% 
        quick_case(., entry="claim", case="Pixel Potus claim") %>%
        mutate(., xtzSent = claim_paid + xtzFee)
    }
    
    # Pixel Potus trade
    else if ("take_trade" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="Pixel Potus trade")
    }
    
    # Pixel Potus unidentified
    else {
      x <- y
    }
  }
  
  # DKRBT homebase
  else if ("KT1DNHADdFxHM6mRKTgyJmchW5ELxcoW1aSh" %in% x$targetAddress) {

    # DKRBT freeze (staking)
    if ("freeze" %in% x$parameterEntry) {
      x %<>% quick_case(., case="DKRBT freeze", type=2)
      x %<>% mutate(., tokenAmount = 0)
    }

    # DKRBT unfreeze (unstake)
    ############################################################################
    # NOTE: Unfreeze classification does not account for DKRBT earned through
    # staking. That is, output will not properly classify earned tokens as
    # income, but instead will assume that the tokens were acquired at a cost
    # basis of 0 when the tokens are sold.
    ############################################################################
    else if ("unfreeze" %in% x$parameterEntry) {
      x %<>% quick_case(., case="DKRBT unfreeze", type=2)
      x %<>% mutate(., tokenAmount = 0)
    }

    # DKRBT unidentified
    else {
      x <- y
    }
  }
  
  # fxhash contracts
  else if (sum(fx_contracts %in% x$targetAddress) > 0) {
    
    # fxhash issue mint
    if ("mint_issuer" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="mint_issuer", case="fxhash mint issue")
    }
    
    # fxhash mint
    else if ("mint" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint", !row_number() == 1) %>%
        mutate(., 
          case = "fxhash mint", 
          tokenAmount = 1
        )
      
      if (x$tokenSender %in% wallets) {
        x %<>% mutate(., 
          case = "fxhash self-mint",
          tokenReceiver = tokenSender,
          tokenSender = NA
        )
      }
      
    }
    
    # fxhash offer
    else if ("offer" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="offer", case="fxhash offer")
      x <- x[1, ]
    }
    
    # fxhash cancel offer
    else if ("cancel_offer" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="cancel_offer", case="fxhash cancel offer")
      x <- x[1, ]
    }
    
    # fxhash trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          tokenAmount = ifelse(
            xtzCollect != xtzReceived, 
            0,
            as.numeric(list_check(parameterValue, "amount"))
          ),
          tokenSender = ifelse(
            xtzCollect != xtzReceived, 
            NA,
            wallets[1]
          ),
          case = ifelse(
            xtzCollect != xtzReceived, 
            "fxhash collect (sales/royalties)",
            "fxhash collect (trade)"
          )
        )
    }
    
    # fxhash collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="fxhash collect")
    }
    
    # fxhash update profile
    else if ("update_profile" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_profile", case="fxhash update profile")
    }
    
    # fxhash unidentified
    else {
      x <- y
    }
  }
  
  # fxhash v2 contracts
  else if (sum(fx_v2_contracts %in% x$targetAddress) > 0) {
    
    # fxhash v2 mint
    if ("mint" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint", !row_number() == 1) %>%
        mutate(., 
               case = "fxhash v2 mint", 
               tokenAmount = 1
        )
      
      if (x$tokenSender %in% wallets) {
        x %<>% mutate(., 
          case = "fxhash v2 self-mint",
          tokenReceiver = tokenSender,
          tokenSender = NA
        )
      }
      
    }
    
    # fxhash v2 listing
    else if ("listing" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="listing", case="fxhash v2 listing")
    }
    
    # fxhash v2 trade
    else if (
      ("listing_accept" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
               tokenAmount = ifelse(
                 xtzCollect != xtzReceived, 
                 0,
                 as.numeric(list_check(parameterValue, "amount"))
               ),
               tokenSender = ifelse(
                 xtzCollect != xtzReceived, 
                 NA,
                 wallets[1]
               ),
               case = ifelse(
                 xtzCollect != xtzReceived, 
                 "fxhash v2 collect (sales/royalties)",
                 "fxhash v2 collect (trade)"
               )
        )

    }
    
    # fxhash v2 collect
    else if (
      ("listing_accept" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="fxhash v2 collect")
    }
    
    # fxhash v2 accept offer
    else if (
      ("offer_accept" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          tokenAmount = ifelse(
            xtzCollect_bid != xtzReceived, 
            0,
            as.numeric(list_check(parameterValue, "amount"))
          ),
          tokenSender = ifelse(
            xtzCollect_bid != xtzReceived, 
            NA,
            wallets[1]
          ),
            case = ifelse(
              xtzCollect_bid != xtzReceived, 
              "fxhash v2 accept offer (sales/royalties)",
              "fxhash v2 accept offer (trade)"
            )
        )
    }
    
    # fxhash v2 offer
    else if ("offer" %in% x$parameterEntry) {
      x %<>% mutate(.,
        xtzSent = xtzFee,
        case="fxhash v2 offer"
      )
    }
    
    # fxhash v2 cancel offer
    else if ("offer_cancel" %in% x$parameterEntry) {
      x %<>% mutate(.,
        xtzReceived = 0,
        case="fxhash v2 cancel offer"
      )
    }
    
    # fxhash v2 listing cancel
    else if ("listing_cancel" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="listing_cancel", case="fxhash v2 listing cancel")
    }
    
    # fxhash v2 unidentified
    else {
      x <- y
    }
  }
  
  # Versum contracts
  else if (sum(versum_contracts %in% x$targetAddress) > 0) {
    
    # Versum make offer
    if ("make_offer" %in% x$parameterEntry) {
      x_hash <- tzkt_operations_hash(hash=x$hash[1], quote=currency)
      token_id <- str_c(x_hash[1,]$parameter$value$token$address, "_", x_hash[1,]$parameter$value$token$nat)
      token_amount <- as.numeric(x_hash[1,]$parameter$value$token_amount)
      
      bm_id <- x_hash[1,]$diffs[[1]]$bigmap
      bm_key <- x_hash[1,]$diffs[[1]]$content$key
      bm_updates <- tzkt_bigmap_updates(bm_id, bm_key)
      
      bm_last_update <- bm_updates[nrow(bm_updates),]
      bm_last_action <- bm_last_update$action
      bm_last_action_date <- as_datetime(bm_last_update$timestamp)
      bm_last_level  <- bm_last_update$level
      
      ##########################################################################
      # Check for key removal at last update level
      # Ideally, we should validate the key removal operation, but the 
      # probabilty of removing another key, when a bid is accepted, should
      # be sufficiently low
      ##########################################################################
      key_removed <- operations %>%
        filter(., level == bm_last_level, sum(c("cancel_offer", "make_offer") %in% x$parameterEntry) > 0) %>%
        nrow(.) > 0
      
      # If the key has beem removed within the time window...
      offer_removed <- bm_last_action == "remove_key" & bm_last_action_date <= date_span[2]
      if (offer_removed & !key_removed) {
        x %<>% mutate(., 
          tokenID = token_id,
          tokenAmount = token_amount,
          tokenReceiver = SenderAddress,
          case = "Versum offer purchase"
        )
      }
      else {
        x %<>% mutate(.,
          xtzSent = xtzFee,
          xtzReceived = 0,
          case = "Versum make offer"
        )
      }
    }
    
    # Versum bid
    else if ("bid" %in% x$parameterEntry) {
      x %<>% mutate(., 
        xtzSent = xtzFee,
        xtzReceived = 0,
        case = "Versum bid"
      )
    }
    
    
    # Versum cancel offer
    else if ("cancel_offer" %in% x$parameterEntry) {
      x %<>% mutate(., 
        xtzSent = xtzFee,
        xtzReceived = 0,
        case = "Versum cancel offer"
      )
    }
    
    # Versum collect (buy)
    else if ("collect_swap" %in% x$parameterEntry & (sum(wallets %in% x$initiatorAddress) > 0)) {
      x %<>% quick_case(., entry="transfer", case="Versum collect (buy)")
    }
    
    # Versum collect (sell)
    else if (
      ("collect_swap" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      xtzCollect <- sort(x$xtzAmount, decreasing=TRUE)[4]
      n_collect <- sum(x$parameterEntry == "collect_swap", na.rm=TRUE)
      if (n_collect == 1) {
        token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
        x %<>%
          filter(., parameterEntry == "transfer") %>%
          mutate(.,
            tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
            tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
            case = ifelse(
            xtzCollect != xtzReceived,
            "Versum collect (sales/royalties)",
            "Versum collect (trade)"
            )
          )
      }
      else {
        x <- y
      }
    }
    
    # Versum claim verification
    else if ("claim_verification" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="claim_verification", case="Versum claim verification")
    }
    
    # Versum claim Materia
    else if ("claim_materia" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="Versum claim Materia")
    }
    
    # Versum swap
    else if ("create_swap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="create_swap", case="Versum swap")
    }
    
    # Versum swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="cancel_swap", case="Versum cancel swap")
    }
    
    # Versum unidentified
    else {
      x <- y
    }
    
  }
  
  # Minterpop contracts
  else if (sum(minterpop_contracts %in% x$targetAddress) > 0) {
    
    # Minterpop buy
    if ("buy" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="Minterpop buy")
    }
    
    # Minterpop unidentified
    else {
      x <- y
    }
    
  }
  
  # Emergent Properties contracts
  else if (sum(c("KT1AML4jv2jMLKESGTApRZVoga5V5cSAMD2E") %in% x$targetAddress) > 0) {
    
    # Emergent Properties trade
    if (
      ("create_sale" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      n_collect <- sum(x$parameterEntry == "create_sale", na.rm=TRUE)
      if (n_collect == 1) {
        token_sender <- x$targetAddress[which(x$targetAddress %in% wallets)][1]
        x %<>%
          filter(., 
            parameterEntry == "create_sale", 
            SenderAddress == "KT1AML4jv2jMLKESGTApRZVoga5V5cSAMD2E"
          ) %>%
          mutate(.,
            tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
            tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
            case = ifelse(
              xtzCollect != xtzReceived,
              "Emergent Properties collect (sales/royalties)",
              "Emergent Properties collect (trade)"
            ),
            tokenID = str_c(targetAddress, "_", parameterValue[[1]]$token_id) 
          )
      }
      else {
        x <- y
      }
    }
    
    # Emergent Properties collect
    else if (
      ("create_sale" %in% x$parameterEntry) & 
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        quick_case(., entry="create_sale", case="Emergent Properties collect") %>%
        filter(., SenderAddress == "KT1AML4jv2jMLKESGTApRZVoga5V5cSAMD2E") %>%
        mutate(., 
          tokenAmount = 1,
          tokenReceiver = initiatorAddress,
          tokenID = str_c(targetAddress, "_", parameterValue[[1]]$token_id) 
        )
    }
    
    # Emergent Properties list
    else if ("list_token" %in% x$parameterEntry) {
      x %<>% quick_case(., type=2, case="Emergent Properties list")
    }
    
    # Emergent Properties unlist
    else if ("unlist_token" %in% x$parameterEntry) {
      x %<>% quick_case(., type=2, case="Emergent Properties unlist")
    }
    
    # Emergent Properties undefined
    else {
      x <- y
    }
    
  }
  
  # NFTbutton contracts
  else if (sum(nftbutton_contracts %in% x$targetAddress) > 0) {
    
    # NFTbutton bid
    if ("bid" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="NFTbutton bid")
    }
    
    # NFTbutton mint
    else if ("mint" %in% x$parameterEntry) {
      x %<>% mutate(., case = "NFTbutton mint")
    }
    
    # NFTbutton resolve
    else if ("resolve" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="NFTbutton resolve")
    }
    
    # NFTbutton unidentified
    else {
      x <- y
    } 
    
  }
  
  # NFTbutton old contract
  else if (sum("KT1Tde5fNg9AZqyW8zjPxfAXhAveSasnp2Dq" %in% x$targetAddress) > 0) {
    
    # NFTbutton old collect
    if ("collect" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="transfer", case="NFTbutton old collect")
    }
    
    # NFTbutton old unidentified
    else {
      x <- y
    }
    
  }
  
  # Tezos Mandala contracts
  else if (sum("KT1DKBvxiwDR7qazNuuxCxY2AaXnoytmDE7H" %in% x$targetAddress) > 0) {
    
    # Tezos Mandala mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="mint", case="Tezos Mandala Mint")
    }
    
    # Tezos Mandala wrap
    else if ("wrap" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint") %>%
        mutate(.,
          case = "Tezos Mandala Wrap"       
        )
    }
    
    # Tezos Mandala unidentified
    else {
      x <- y
    }
    
  }
  
  # Editart mint ccontract
  else if (sum("KT1D7Ufx21sz9yDyP4Rs1WBCur9XhaZ9JwNE" %in% x$targetAddress) > 0) {
    if ("mint" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="mint", case="Editart mint")
    }
    else {
      x <- y
    }
  }
  
  # Knights of Tezonia contracts
  else if (sum("KT1VuqyJmkcSk2m5L9gEgAs69t6CCDDxLtiz" %in% x$targetAddress) > 0) {
    
    # Tezonia collect
    if ("collect" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="mint", case="Knights of Tezonia mint")
    }
    
    # Tezonia unidentified
    else {
      x <- y
    }
    
  }
  
  # Rarible contracts
  else if (sum(rari_contracts %in% x$targetAddress) > 0) {
    
    # Rarible collect
    if (
      ("match_orders" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="Rarible collect")
    }
    
    # Rarible trade
    else if (
      ("match_orders" %in% x$parameterEntry) &
      (sum(wallets %in% x$initiatorAddress) == 0)
    ) {
      xtzCollect <- sort(x$xtzAmount, decreasing=TRUE)[5]
      x %<>% 
        quick_case(., entry="transfer", case="Rarible trade") %>%
        mutate(.,
          tokenAmount = ifelse(xtzCollect != xtzReceived, 0, tokenAmount),
          tokenSender = ifelse(xtzCollect != xtzReceived, NA, token_sender),
          case != ifelse(
            xtzCollect > xtzReceived,
            "Rarible collect (sales/royalties)",
            "Rarible collect (trade)"
          )
        )
    }
    
    # Rarible mint
    else if ("mint" %in% x$parameterEntry) {
      x %<>% mutate(., case = "Rarible mint")
    }
    
    # Rarible update operators
    else if ("update_operators_for_all" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_operators_for_all", case="Rarible update operators")
    }
    
    # Rarible cancel
    else if ("cancel" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="cancel", case="Rarible cancel")
    }
    
    # Rarible cancel
    else if ("burn" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="burn", case="Token transfer")
      x %<>% mutate(.,
        tokenAmount = 1,
        tokenSender = SenderAddress,
        tokenReceiver = "",
        tokenID = str_c(targetAddress, parameterValue[[1]]$itokenid, sep="_")
      )
    }
    
    # Rarible unidentified
    else {
      x <- y
    }
  }
  
  # Goren tokens
  else if ("KT1JBThDEqyqrEHimhxoUBCSnsKAqFcuHMkP" %in% x$targetAddress) {
    
    # Goren mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>%
        mutate(., 
          tokenReceiver = SenderAddress,
          tokenAmount = 1,
          case = "Goren mint"
        )
    }
    
    # Goren update
    else if ("update_operators" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="update_operators", case="Goren update")
    }
    
    # Goren sell
    else if ("sell" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "Goren sell")
    }

    # Goren unidentified
    else {
      x <- y
    }
  }
  
  # WRAP tokens
  else if (sum(wrap_contracts %in% x$targetAddress) > 0) {
    
    # WRAP mint
    if ("minter" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint_tokens") %>%
        mutate(., 
          tokenReceiver = initiatorAddress,
          case = "WRAP mint"
        )
      parameter_value <- filter(x[1, ]$parameterValue[[1]], owner %in% wallets)
      token_id <- str_c(x[1,]$targetAddress, "_", parameter_value[1, ]$token_id)
      token_amount <- parameter_value[1, ]$amount
      x %<>% 
        mutate(.,
          tokenID = token_id, 
          tokenAmount = as.numeric(token_amount)
        )
    }
    
    # WRAP unwrap
    else if ("unwrap_erc20" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "burn_tokens") %>%
        mutate(., 
          tokenSender = initiatorAddress,
          case        = "WRAP unwrap"
        )
      parameter_value <- filter(x[1, ]$parameterValue[[1]], owner %in% wallets)
      token_id <- str_c(x[1,]$targetAddress, "_", parameter_value[1, ]$token_id)
      token_amount <- parameter_value[1, ]$amount
      x %<>% 
        mutate(., 
          tokenID = token_id, 
          tokenAmount = as.numeric(token_amount)
        )
    }
    
    # WRAP unidentified
    else {
      x <- y
    }
  }
  
  # WTZ swap
  else if ("KT1Pyd1r9F4nMaHy8pPZxPSq6VCn9hVbVrf4" %in% x$targetAddress) {
   
    # WTZ unwrap
    if ("unwrap" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "unwrap") %>%
        mutate(.,
          tokenSender = SenderAddress,
          tokenID = "KT1Pyd1r9F4nMaHy8pPZxPSq6VCn9hVbVrf4_0",
          tokenAmount = as.numeric(list_check(parameterValue, "nat")),
          case = "WTZ unwrap"
        )
    }
    
    # WTZ unidentified
    else {
      x <- y
    }
     
  }
  
  # Gogos contract
  else if (sum("KT1Xf44LpwrA7oBcB3VwWTtUBP1eNRaNnWeh" %in% x$targetAddress) > 0) {
    
    # Gogos consume
    if ("consume" %in% x$parameterEntry) {
      x %<>% quick_case(., case="Gogos consume", type=2)
    }
    
    # Gogos return
    else if ("return_item" %in% x$parameterEntry) {
      x %<>% quick_case(., case="Gogos return", type=2)
    }
    
    # Gogos unidentified
    else {
      x <- y
    }
    
  }
  
  # Kolibri oven
  else if (sum(kolibri_contracts %in% x$targetAddress) > 0) {
    
    # Kolibri oven deposit
    if ("deposit" %in% x$parameterEntry) {
      x %<>%
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzSent = xtzFee,
          xtzReceived = 0,
          case = "Kolibri oven deposit"
        )
    }
    
    # Kolibri oven borrow
    else if ("borrow" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint") %>%
        mutate(., 
          costBasis = tokenAmount / 1000000000000000000,
          case = "Kolibri oven borrow"
        )
    }
    
    # Kolibri oven repay
    else if ("repay" %in% x$parameterEntry) {
      x %<>%
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          tokenAmount = as.numeric(parameterValue),
          tokenReceiver = SenderAddress,
          tokenID = "KT1K9gCRgaLRFKTErYt1wVxA3Frb9FjasjTV_NA",
          tokenProceeds = tokenAmount / 1000000000000000000,
          case = "Kolibri oven repay"
        )
    }
    
    # Kolibri oven withdraw
    else if ("withdraw" %in% x$parameterEntry) {
      x %<>%
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzSent = xtzFee,
          xtzReceived = 0,
          case = "Kolibri oven withdraw"
        )
    }

    # Kolibri make oven
    else if ("makeOven" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="makeOven", case="Kolibri make oven")
    }
        
    # Kolibri oven unidentified
    else {
      x <- y
    }
    
  }
  
  # Glitch Forge contracts
  else if (sum("KT1NJXj24i8GDz6ZAb8XdP8RgkE2mo5s9nEG" %in% x$targetAddress) > 0) {
    
    # Glitch Forge purchase
    if ("purchase" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="purchase", case="Glitch Forge purchase")
    }
    
    # Glitch Forge unidentified
    else {
      x <- y
    }
  }
  
  # tzprofiles default call
  else if ("default" %in% x$parameterEntry & nrow(x) == 1) {
    x %<>% quick_case(., entry="default", case="tzprofiles update")
  }
  
  # C-verso Contracts
  else if(sum(cverso_contracts %in% x$targetAddress) > 0) {
    
    # C-verso mint
    if ("mint_token" %in% x$parameterEntry) {
      x %<>% quick_case(., entry="mint")
    }
    
    # C-verso unidentified
    else {
      x <- y
    }
  }
  
  # Unidentified
  else {
    x <- y
  }
  
  # Check for hDAO airdrop
  if ("hDAO_batch" %in% y$parameterEntry) {
    x_h         <- filter(y, parameterEntry == "hDAO_batch")
    y2 <-filter(y, type == "transaction")
    x_h_wallets <- y2$parameterValue[[5]][[1]]
    x_h_amount  <- as.numeric(y2$parameterValue[[5]][[2]])
    x_h_index   <- which(x_h_wallets %in% wallets)
    if (length(x_h_index) > 0) {
      x_h %<>% 
        mutate(.,
          xtzSent       = 0,
          xtzReceived   = 0,
          tokenID       = "KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW_0",
          tokenReceiver = x_h_wallets[x_h_index][1],
          tokenAmount   = sum(x_h_amount[x_h_index]),
          case          = "hDAO airdrop"
        )
        if (sum(c("failed", "backtracked") %in% x$status) > 0) {
          x_h %<>% mutate(., tokenAmount=0, case="Failed transaction")
        }
      x %<>% add_row(., x_h)
    }
  }
  
  # Add row(s) to income statement
  if (nrow(x) > 0) {
    is %<>% bind_rows(., x)  
  }
}

# Adjust income statement data
is %<>% select(., -bidKey)
