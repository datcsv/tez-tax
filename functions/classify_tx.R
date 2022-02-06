################################################################################
# Notes:
# (1) Tezos Domains, Randomly Common Skeles, Pixel Potus, Geoff Stearns mints 
#     are currently not being registered. The data for these appears to be
#     stored in bigmaps rather than parameter values. 
#
# (2) The code is a huge mess - Modularization and funcionalization would be 
#     a very good idea; there is definitely a better way to go about this,
#     but it works for now. 
#
# (3) hDAO and akaDAO airdrop tokens are not currently accounted for. Givven 
#     that they have a cost-basis of zero, this should not be a huge issue.
#     Nevertheless, it would not be terrible hard to add these in later. 
#
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
  "KT1My1wDZHDGweCrJnQJi3wcFaS67iksirvj"
)

# QuipuSwap contracts
quipu_contracts <- c(
  "KT1QxLqukyfohPV5kPkw97Rs6cw1DDDvYgbB",
  "KT1BMEEPX7MWzwwadW3NCSZe9XGmFJ7rs7Dr",
  "KT1X3zxdTzPB9DgVzA3ad6dgZe9JEamoaeRy"
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
  "KT1HZVd9Cjc2CMe3sQvXgbxhpJkdena21pih",
  "KT1QJ71jypKGgyTNtXjkCAYJZNhCKWiHuT2r",
  "KT1Aq4wWmVanpQhq4TTfjZXB5AjFpx15iQMM"
)

# Create null income statement
is <- operations[0, ]

# Identify unique operation groups
operations_hash <- operations %>% distinct(., hash)

# Iterate over unique operation groups to build income statement
for (i in 1:nrow(operations_hash)) {
  
  
  ##############################################################################
  # Define variables, as necessary
  ##############################################################################
  
  # Helper variables, etc
  x <- filter(operations, hash == operations_hash[i, ])
  y <- x
  if (sum(c("transfer", "mint") %in% x$parameterEntry) > 0) {
    for (i in 1:nrow(x)) {
      
      # Add token metadata, if possible
      if (sum(c("transfer", "mint") %in% x$parameterEntry[i]) > 0) {
        x$tokenID[i]       <- paste0(x$targetAddress[i], "_", list_check(x$parameterValue[i], "token_id"))
        x$tokenSender[i]   <- list_check(x$parameterValue[i], c("address", "from_"))
        x$tokenReceiver[i] <- list_check(x$parameterValue[i], c("to_", "to"))
        x$tokenAmount[i]   <- as.numeric(list_check(x$parameterValue[i], "amount"))
      }
      
      # Non-FA2 tokens - assuming a token ID of 0
      if (sum(nfa2 %in% x$targetAddress[i]) > 0) {
        x$tokenID[i]      <- paste0(x$targetAddress[i], "_0")
        x$tokenAmount[i]  <- as.numeric(list_check(x$parameterValue[i], "value"))
      }
      
    }
  }
  x$xtzSent     <- sum(x$xtzSent)
  x$xtzReceived <- sum(x$xtzReceived)
  xtzCollect    <- sort(x$xtzAmount, decreasing=TRUE)[2]
  
  ##############################################################################
  # Identify operation groups, filter and classify as necessary
  ##############################################################################
  
  # Failed transaction
  if (sum(c("failed", "backtracked") %in% x$status) > 0) {
    x %<>% quick_case(., case="Failed transaction", type=2)
  }
  
  # Standard transaction
  else if (sum(is.na(x$parameterEntry)) == nrow(x)) {
    x %<>%
      filter(., (SenderAddress %in% addresses) | (targetAddress %in% addresses)) %>%
      mutate(., case="Standard transaction")
  }
  
  # Token transfer
  else if ((nrow(x) == 1) & ("transfer" %in% x$parameterEntry)) {
    x %<>% mutate(., tokenSender=SenderAddress, case="Token transfer")
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
          tokenSender=targetAddress, 
          tokenReceiver=initiatorAddress,
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
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% addresses)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount=ifelse(
            xtzCollect <= xtzReceived, 
            as.numeric(list_check(parameterValue, "amount")), 0
          ),
          tokenSender=ifelse(xtzCollect <= xtzReceived, token_sender, NA),
          case=ifelse(xtzCollect <= xtzReceived, "HEN trade", "HEN royalties")
        )
    }
    
    # HEN collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="HEN collect")
    }
    
    # HEN curate
    else if ("curate" %in% x$parameterEntry) {
      x %<>% 
        quick_case(., case="HEN curate", type=2) %>%
        mutate(., 
          tokenID="KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW_0",
          tokenAmount=as.numeric(list_check(parameterValue, "hDAO_amount")),
          tokenSender=SenderAddress
        )
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
  
  # QuipuSwap contracts
  else if (sum(quipu_contracts %in% x$targetAddress) > 0) {
    
    # QuipuSwap trade
    if (sum(c("tezToTokenPayment", "tokenToTezPayment") %in% x$parameterEntry) > 0) {
      x %<>% quick_case(., entry="transfer", case="QuipuSwap trade")
    }
    
    # Quipuswap unidentified
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
    
    # OBJKT bid
    else if ("bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., 
          xtzReceived=0,
          xtzSent=ifelse(SenderAddress %in% addresses, xtzSent - xtzAmount, 0),
          case=ifelse(SenderAddress %in% addresses, "OBJKT bid", "OBJKT outbid")
        )
      
      ## Additional logic should be added here using bigmap API ##
      
    }
    
    # OBJKT retract bid
    else if ("retract_bid" %in% x$parameterEntry) {
      x %<>% 
        top_n(., n=-1, wt=id) %>%
        mutate(., xtzReceived=0, case="OBJKT retract bid")
    }
    
    # OBJKT fulfill ask (trade)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% addresses)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
        tokenAmount=ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
        tokenSender=ifelse(xtzCollect <= xtzReceived, token_sender, NA),
        case=ifelse(
          xtzCollect > xtzReceived, 
          "OBJKT fulfill ask (royalties)", 
          "OBJKT fulfill ask (trade)")
        )
    }
    
    # OBJKT fulfill ask (collect)
    else if (
      ("fulfill_ask" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT fulfill ask (collect)")
    }
    
    # OBJKT fulfill bid (trade)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      token_sender <- x$targetAddress[which(x$targetAddress %in% addresses)][1]
      x %<>% 
        filter(., parameterEntry == "transfer") %>% 
        mutate(., 
          tokenAmount=ifelse(xtzCollect > xtzReceived, 0, tokenAmount),
          tokenSender=ifelse(xtzCollect <= xtzReceived, token_sender, NA),
          case=ifelse(
            xtzCollect > xtzReceived, 
            "OBJKT fulfill bid (royalties)", 
            "OBJKT fulfill bid (trade)"
          )
        )
    }
    
    # OBJKT fulfill bid (collect)
    else if (
      ("fulfill_bid" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% quick_case(., entry="transfer", case="OBJKT fulfill bid (collect)")
    }
    
    # OBJKT buy dutch auction
    else if (
      ("buy" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
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
          tokenSender=NA,
          tokenReceiver=initiatorAddress, 
          case="OBJKT mint"
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
  
  # akaSwap contract
  else if (
    ("KT1HGL8vx7DP4xETVikL4LUYvFxSV19DxdFN" %in% x$targetAddress) |
    ("KT1NL8H5GTAWrVNbQUxxDzagRAURsdeV3Asz" %in% x$targetAddress) |
    ("KT1ULea6kxqiYe1A7CZVfMuGmTx7NmDGAph1" %in% x$targetAddress) |
    ("KT19QcybJCf8zKCEECRhtMUYTptTwgY9jMKU" %in% x$targetAddress)
  ) {
    
    # akaSwap mint
    if ("mint" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "mint") %>% 
        mutate(., 
               tokenSender = targetAddress,
               case        = "akaSwap mint"
        )
    }
    
    # akaSwap trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., 
               tokenAmount = ifelse(xtzCollect <= xtzReceived, as.numeric(list_check(parameterValue, "amount")), 0),
               case        = ifelse(xtzCollect <= xtzReceived, "akaSwap trade", "akaSwap royalties")
        )
    }
    
    # akaSwap collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        top_n(., n=1, wt=id) %>%
        mutate(., case = "akaSwap collect")
    }
    
    # akaSwap collect bundle
    else if (
      ("collect_bundle" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>%
        top_n(., n=1, wt=id) %>%
        mutate(., case = "akaSwap collect bundle")
      x_params <- x$parameterValue[[1]][[1]][[1]]
      x_n <- nrow(x_params)
      y <- x
      x <- x[0, ]
      for (i in 1:x_n) {
        x_i <- y
        x_i$tokenReceiver <- x_params$to_[[i]]
        x_i$tokenAmount   <- as.numeric(x_params$amount[[i]])
        x_i$tokenID       <- paste0(x_i$targetAddress, "_", x_params$token_id[[i]])
        x_i$xtzSent       <- x_i$xtzSent / x_n
        x %<>% bind_rows(., x_i)
      }
    }
    
    # akaSwap swap
    else if ("swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "swap") %>%
        mutate(., case = "akaSwap swap")
    }
    
    # akaSwap cancel swap
    else if ("cancel_swap" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "cancel_swap") %>%
        mutate(., case = "akaSwap cancel swap")
    }
    
    # akaSwap gachapon royalties
    else if (
      ("default" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% mutate(., case = "akaSwap gachapon royalties")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
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
  
  # Geoff Stearns mint
  else if (
    ("KT1Fxz4V3LaUcVFpvF8pAAx8G3Z4H7p7hhDg" %in% x$targetAddress) & 
    ("mint" %in% x$parameterEntry)
  ) {
    x %<>% 
      filter(., parameterEntry == "mint") %>%
      mutate(., case = "Geoff Stearns mint")
  }
  
  # fxhash contracts
  else if (
    ("KT1AEVuykWeuuFX7QkEAMNtffzwhe1Z98hJS" %in% x$targetAddress) |
    ("KT1XCoGnfupWk7Sp8536EfrxcP73LmT68Nyr" %in% x$targetAddress) |
    ("KT1Xo5B7PNBAeynZPmca4bRh6LQow4og1Zb9" %in% x$targetAddress) |
    ("KT1Ezht4PDKZri7aVppVGT4Jkw39sesaFnww" %in% x$targetAddress) 
  ) {
    
    # fxhash issue mint
    if ("mint_issuer" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "mint_issuer") %>%
        mutate(., case = "fxhash issue mint")
    }
    
    # fxhash mint
    else if ("mint" %in% x$parameterEntry) {
      x %<>%
        filter(., 
          parameterEntry == "mint",
          !row_number() == 1
        ) %>%
        mutate(., case = "fxhash mint")
    }
    
    # fxhash offer
    else if ("offer" %in% x$parameterEntry) {
      x %<>%
        filter(., x$parameterEntry == "offer") %>%
        mutate(., case="fxhash offer")
    }
    
    # fxhash cancel offer
    else if ("cancel_offer" %in% x$parameterEntry) {
      x %<>%
        filter(., x$parameterEntry == "cancel_offer") %>%
        mutate(., case="fxhash cancel offer")
    }
    
    # fxhash trade
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) == 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., 
          tokenAmount = ifelse(xtzCollect <= xtzReceived, as.numeric(list_check(parameterValue, "amount")), 0),
          case        = ifelse(xtzCollect <= xtzReceived, "fxhash trade", "fxhash royalties")
        )
    }
    
    # fxhash collect
    else if (
      ("collect" %in% x$parameterEntry) & 
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>% 
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "fxhash collect")
    }
    
    # fxhash update profile
    else if ("update_profile" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "update_profile") %>%
        mutate(., case = "fxhash update profile")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Pixel Potus contracts
  else if ("KT1WGDVRnff4rmGzJUbdCRAJBmYt12BrPzdD" %in% x$targetAddress) {
    
    # Claim
    if ("claim" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "claim") %>%
        mutate(., case = "Pixel Potus claim")
    }
    
    # Trade
    else if ("take_trade" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "Pixel Potus trade")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Rarible contracts
  else if (
    ("KT198mqFKkiWerXLmMCw69YB1i6yzYtmGVrC" %in% x$targetAddress) |
    ("KT18pVpRXKPY2c4U2yFEGSH3ZnhB2kL8kwXS" %in% x$targetAddress)
  ) {
    
    # Rarible collect
    if (
      ("match_orders" %in% x$parameterEntry) &
      (sum(addresses %in% x$initiatorAddress) > 0)
    ) {
      x %<>%
        filter(., parameterEntry == "transfer") %>%
        mutate(., case = "Rarible collect")
    }
    
    # Rarible update operators
    else if ("update_operators_for_all" %in% x$parameterEntry) {
      x %<>% 
        filter(., parameterEntry == "update_operators_for_all") %>%
        mutate(., case = "Rarible update operators")
    }
    
    # Rarible cancel
    else if ("cancel" %in% x$parameterEntry) {
      x %<>%
        filter(., parameterEntry == "cancel") %>%
        mutate(., case = "Rarible cancel")
    }
    
    # Unidentified
    else {
      x <- y
    }
    
  }
  
  # Randomly Common Skeles mint
  else if (
    ("KT1AvxTNETj3U4b3wKYxkX6CKya1EgLZezv8" %in% x$targetAddress) &
    ("buy" %in% x$parameterEntry)
  ){
    x %<>% mutate(., case = "RCS mint")
  }
  
  # Unidentified
  else {
    x <- y
  }
  
  is %<>% bind_rows(., x)
  
}
