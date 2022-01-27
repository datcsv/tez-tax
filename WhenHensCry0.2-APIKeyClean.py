#pip install pytezos
#pip install -U discord.py
#pip install asyncio
#pip install nest_asyncio
#pip install requests
#pip install time
#pip install datetime
import pytezos
import time
import datetime
import requests
import json
import discord
from discord import Webhook, RequestsWebhookAdapter
import discord
import asyncio
import nest_asyncio
import dateutil.parser as dp


purchasingWhiteList = [['tz1TxDL7npfYDSyvrZFC4deUPMySvsL6xARU',"p1xelfool",26],
                       ['tz1duZMsESvhr88Xa4fYzC2WLCXGRJnxhwDd',"bouncyman",28],
                       ['tz1Zap44HJtRZQY6aczqSaQb5FGdLVX7YsAK',"twitterDude",15],
                       ['tz1Zs2WDRv9549rnHkjAjdMaWXVnreixXVKD',"skullPictureGuy",26],
                       ['tz1SbhKLqMa1h9eS6LdwL7dduz1qe41SraKQ', "meAlt", 2]]

#['tz1WJj8vkuysLVT21V9auP8WUJuttygRNiq5', "gio", 4],

nest_asyncio.apply()

class MyClient(discord.Client):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    async def on_ready(self):
        print('Logged in as')
        print(self.user.name)
        print(self.user.id)
        print('------')

        # create the background task and run it in the background
        self.bg_task = self.loop.create_task(self.my_background_task())

    async def my_background_task(self):
        counter = 0
        channel = self.get_channel(847192262919192606) # channel ID goes here
        while not self.is_closed():
            counter += 1
            await channel.send(counter)
            await asyncio.sleep(10) # task runs every 10 seconds

def getArtistRecords(address):
    url = "https://api.tzkt.io/v1/operations/transactions?sender="+ address +"&quote=usd&sort.desc=id&limit=10"
    req = requests.get(url)
    artistRecords = req.json()
    return artistRecords

def investigateCreator(address):
    datetime_object = datetime.datetime.now()
    startEpochTime = (datetime.datetime.timestamp(datetime_object))
    artistRecords = getArtistRecords(address)
    #print(artistRecords)
    l = len(artistRecords)
    projectedPrice , TxHash , TxBlock , timestamp , interactionOfintrest = '','','','',''
    while True:
        for i in range (0,l,1):
            try:
                if artistRecords[i]['parameter']['entrypoint'] == 'swap':
                    projectedPrice = artistRecords[i]['parameter']['value']['xtz_per_objkt']
                    timestamp = artistRecords[i]['timestamp']
                    TxBlock = artistRecords[i]['block']
                    TxHash = artistRecords[i]['hash']

                    parsed_t = dp.parse(timestamp)
                    epochTime = parsed_t.timestamp()
                    datetime_object = datetime.datetime.now()
                    currentEpochTime = (datetime.datetime.timestamp(datetime_object))


                    if int(currentEpochTime-startEpochTime) > (60*12):
                        return "0", "0", "0", "0", "0"
                    if int(currentEpochTime - epochTime) < 800:


                        interactionOfintrest = artistRecords[i]
                        return projectedPrice, TxBlock, TxHash, timestamp, interactionOfintrest
                    artistRecords[i]['parameter']['entrypoint'] = 'mint_OBJKT'

                if artistRecords[i]['parameter']['entrypoint'] == 'mint_OBJKT':
                    #recall data and reset
                    time.sleep(1)
                    artistRecords = getArtistRecords(address)

            except KeyError:
                uhh = "do nothing"


                break

def getNearMostRecentBlock():
    url = "https://api.tzstats.com/explorer/tip"
    req = requests.get(url)
    webpage = req.json()
    block = webpage["status"]["blocks"]
    return (block)

def backroundActivityCheck():
    #url = "https://rpc.tzkt.io/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9/operations/received"
    #url = 'https://api.better-call.dev/v1/contract/mainnet/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9/operations'
    #url = "https://api.tzkt.io/v1/operations/transactions?recived=KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9&quote=usd&sort.desc=id&limit=50"
    url = "https://api.tzkt.io/v1/operations/transactions?target=KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9&quote=usd&sort.desc=id&limit=50"
    req = requests.get(url)
    webpage = req.json()
    #print(webpage)
    #operations = webpage['operations']

    return webpage

def blockLock(block):
    url = 'https://rpc.tzkt.io/mainnet/chains/main/blocks/' + str(block) + '/operations'
    url2 = 'https://rpc.tzkt.io/mainnet/chains/main/blocks/' + str(block + 1) + '/operations'
    req = requests.get(url)
    time.sleep(1)
    reqNext = requests.get(url2)
    #print(str(req))
    if str(reqNext) == "<Response [404]>" and str(req) == "<Response [200]>":
        curentblock = block
    elif str(reqNext) == "<Response [404]>" and str(req) == "<Response [404]>":
        curentblock = block - 1
        time.sleep(1)
        return blockLock(curentblock)
    elif str(reqNext) == "<Response [200]>" and str(req) == "<Response [200]>":
        curentblock = block + 1
        time.sleep(1)
        return blockLock(curentblock)
    else:
        time.sleep(3)
        return blockLock(block)
    return curentblock, req

def blockHunter(blockJson, BLhash):
    #url = 'https://api.tzkt.io/v1/blocks/'+ str(BLhash) +'?operations=true'
    url2 = 'https://rpc.tzkt.io/mainnet/chains/main/blocks/' + str(BLhash) + '/operations'
    try:
        reqNext = requests.get(url2)
    except requests.exceptions.ConnectionError or requests.exceptions.Timeout or requests.exceptions.RequestException:
        time.sleep(1)
        return blockHunter(blockJson, BLhash)
    #print(str(req))
    if str(reqNext) != "<Response [200]>":
        return blockJson
    else:
        blockJson = reqNext.json()
        return blockJson

def blockActivityCheck(blockjson):
    print(blockjson)
    #operations = webpage['operations']

    return blockjson

def getArtistSafeList(artistID):
    values = {"query":"\nquery myQuery($address: String!) {\n  hic_et_nunc_token(where: {creator: {address: {_eq: $address}}}, order_by: {id: desc}) {\n    id\n}\n  hic_et_nunc_swap(where: {token: {creator: {address: {_eq: $address}}}, status: {_in: [0, 1]}}, order_by: {token_id: desc, timestamp: desc}) {\n    token_id\n    status\n    price\n    amount\n    amount_left\n    creator {\n      address\n    }\n    timestamp\n  }\n}\n","variables":{"address":artistID},"operationName":"myQuery"}
    url = "https://api.hicdex.com/v1/graphql"
    req = requests.post(url, data=json.dumps(values))
    needstest = req.json()
    # print(page)
    objktWhitelist = []
    for i in range(0,len(needstest['data']['hic_et_nunc_token']),1):
        objktWhitelist.append(needstest['data']['hic_et_nunc_token'][i]['id'])
    return objktWhitelist

def identifyBlueChipActivity(operations, webhook, lastArtistTag):
    bluechip = ['tz1MCwsJvw4RYbN2iJAaALrTb6yQuFpgwPmq',
                'tz1biKU3xnysM6WXAYN9Qsp9siv8Ttz5f7UY',
                'tz1aQKoJYGJTZRNDjunJVuPmJeZPKsrRvpBZ',
                'tz1Zs2WDRv9549rnHkjAjdMaWXVnreixXVKD',
                'tz1XuPRJJEdEumLcuJc4pdzejqs58qBFJSCW',
                'tz1WC3dUzy19mbkGwz4J3KhuSonE7AQxbZuF',
                'tz1TxDL7npfYDSyvrZFC4deUPMySvsL6xARU',
                'tz1gqaKjfQBhUMCE6LhbkpuittRiWv5Z6w38',
                'tz1duZMsESvhr88Xa4fYzC2WLCXGRJnxhwDd',
                'tz1XHADaUcMSkTN9gdmtRqcnrrZfs4tNkCPg',
                'tz1hfuVWgcJ89ZE75ut9Qroi3y7GFJL5Lf2K',
                'tz1hb9PiWxQEf6J9xevPsUM6dkuCLnhDMvsp',
                'tz1cXUzuq1JN48DPHuaR4ZsYdLeq91zDzN4T',
                'tz1PSDQizcBtXrLJ1NWM3jT7z7okXu1oeEdF',
                'tz1PbL2VDajgaVxVYQ4K5NoPmdheqwkmZSMR',
                'tz1WJj8vkuysLVT21V9auP8WUJuttygRNiq5',
                'tz1SbhKLqMa1h9eS6LdwL7dduz1qe41SraKQ',
                'tz1iimuCRajJPgHsMAtxUToEUiggn3uyMj4Y',
                'tz1Zap44HJtRZQY6aczqSaQb5FGdLVX7YsAK',
                'tz1RNDoqLh5VCqBUXrgmzhwxqcR7ptAfvfNh',
                'tz1gwmdJruRtdLsL3jwxaSSLD3p9hp2waXU1',
                'tz1Q8w7g1XncTjXit974x5Xy5DSERm4PU9UB',
                "tz1ar1j9Lwoh668eXYmJggHth35bypj6c9s8",
                "tz1biKU3xnysM6WXAYN9Qsp9siv8Ttz5f7UY",
                "tz1ZHqZyigiw8u5WJjRuj2Q6Dz6r45cjaitu",
                "tz1aLCW57GTwnmBNcrwWUR2y4YUEQAFGQYtU",
                "tz1Kj4gL5Di1344EDdzz7StZwdedA89JB5Td",
                "tz1KjuVj6gJXmUeTVz2GyThsGMVSvhMZY3UZ"]
    l = len(operations)
    for i in range (0,l,1):
        try:
            #print(operations[i]['parameter']['entrypoint'])
            if operations[i]['parameter']['entrypoint'] == 'mint_OBJKT':
                print(operations[i]['parameter']['entrypoint'] + " " + operations[i]['parameter']['value']["address"])
                time.sleep(100)
            if operations[i]['parameter']['entrypoint'] == 'mint_OBJKT' and operations[i]['parameter']['value']["address"] != lastArtistTag and operations[i]['parameter']['value']["address"] in bluechip:
                datetime_object = datetime.datetime.now()
                webhook.send(str(datetime_object) + ": @here incoming bluechip drop recorded at " + str(operations[i]['timestamp']) +" from https://www.hicetnunc.xyz/tz/" + operations[i]['parameter']['value']["address"])
                return operations[i]['parameter']['value']["address"]
        except KeyError:
            "do nothing"
    return "0"

def crownJewlHunter(blockJson,TxHash): #hunts for the SwapID, which is the crown jewl
    l = len(blockJson)
    for i in range (0,l,1):
        m = len(blockJson[i])
        if i > 0:
            n = len(blockJson[l-1])
        for j in range(0, m, 1):
            if blockJson[i][j]['hash'] == str(TxHash):
                try:
                    SwapID = blockJson[i][j]['contents'][0]['metadata']['operation_result']['storage'][3]['int']
                    return SwapID
                except KeyError:
                    "do nothing"
    return "0"

def commitCollectContractInteraction(PRIVATE_KEY,salePrice,swapID):
    #from pytezos.crypto import Key
    #other account I own "tz1SbhKLqMa1h9eS6LdwL7dduz1qe41SraKQ"
    sk = pytezos.crypto.key.Key.from_encoded_key(key=PRIVATE_KEY)
    print(sk.public_key_hash())
    client = pytezos.client.PyTezosClient()
    client = client.using(shell='mainnet',key=sk)
    # print(client.account())
    # print(client.key.public_key())
    # print(client.key.public_key_hash())
    # print("tz1M8fbyxuBvLxacppqwjBP1w52TtW7UhxYZ")
    # print(client.balance())
    # print(client)
    prep = client.transaction(amount=salePrice,destination="KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9",parameters={ "entrypoint": "collect", "value": { "prim": "Pair", "args": [ { "int": "1" }, { "int": swapID } ] } })
    prep = prep.autofill()
    prep = prep.sign()
    ################################################
    print(prep.send()) #uncomment me to send THIS COST MONEY TO RUN $$$$$
    ################################################
    print("purchase commited")

def purchasePreAuth(artistTag, price):
    purchasingShortList = ['tz1RNDoqLh5VCqBUXrgmzhwxqcR7ptAfvfNh',
                           'tz1TxDL7npfYDSyvrZFC4deUPMySvsL6xARU',
                           'tz1duZMsESvhr88Xa4fYzC2WLCXGRJnxhwDd',
                           'tz1Zap44HJtRZQY6aczqSaQb5FGdLVX7YsAK',
                           'tz1Zs2WDRv9549rnHkjAjdMaWXVnreixXVKD',
                           'tz1biKU3xnysM6WXAYN9Qsp9siv8Ttz5f7UY',
                           'tz1Q8w7g1XncTjXit974x5Xy5DSERm4PU9UB']

    purchasingWhiteList = [['tz1TxDL7npfYDSyvrZFC4deUPMySvsL6xARU', "p1xelfool", 30],
                           ['tz1duZMsESvhr88Xa4fYzC2WLCXGRJnxhwDd', "bouncyman", 30],
                           ['tz1Zap44HJtRZQY6aczqSaQb5FGdLVX7YsAK', "twitterDude", 21],
                           ['tz1Zs2WDRv9549rnHkjAjdMaWXVnreixXVKD', "skullPictureGuy", 26],
                           ['tz1biKU3xnysM6WXAYN9Qsp9siv8Ttz5f7UY', "Omgidrawedit", 20],
                           ['tz1Q8w7g1XncTjXit974x5Xy5DSERm4PU9UB', 'fawks',11]]

    pricingShortList = [21000000,31000000,31000000,21000000,26000000,21000000,11000000]

    if artistTag in purchasingShortList:
        order = purchasingShortList.index(artistTag)
        if int(price) < int(pricingShortList[order]):
            return True
    return False

def main():

    PRIVATE_KEY = "Almost forgot to delete this lmao"
    SwapID = '0'
    lastArtistTag = "0"
    artistTag = '0'
    blockJson = ""
    block = getNearMostRecentBlock()
    #webhook = Webhook.from_url("https://discord.com/api/webhooks/850207796598538291/X_gXx7LPLb0FBgSjXTTYhaB156nkYhAP0WLV7mV76lvwtnKLV7Qi9xIm-KGVk_Dw0BJj", adapter=RequestsWebhookAdapter())
    # dev Channel #webhook = Webhook.from_url("https://discord.com/api/webhooks/847512534332211250/1eM_XmfgWjqFt-BG_61b24y7yYER7pCBlWvHm7jUCzmEVv8PySsGJFgbSHHrZdWmrZzR", adapter=RequestsWebhookAdapter())
    webhook = Webhook.from_url(
        "https://discord.com/api/webhooks/this one too",
        adapter=RequestsWebhookAdapter())


    webhook.send("bot going LIVE \n ♩ ♪ This is what it sounds like whenHensCry-0.2.1 ♫ ♬")
    print("bot going LIVE")
    while True:

        try:
            operations = backroundActivityCheck()
            # block, req = blockLock(block) # not really used due to block hunter?
            block = getNearMostRecentBlock()
            # blockJson = req.json()
            artistTag = identifyBlueChipActivity(operations, webhook, lastArtistTag)
            # artistTag = ("tz1gqaKjfQBhUMCE6LhbkpuittRiWv5Z6w38")
        except requests.exceptions.ConnectionError or requests.exceptions.Timeout or requests.exceptions.RequestException:
            time.sleep(5)

        print("sleepMode")
        #artistTag = "tz1SbhKLqMa1h9eS6LdwL7dduz1qe41SraKQ" # commient me in operation
        if artistTag != "0":
            saftylist = []
            try:
                time.sleep(15)
                saftylist = getArtistSafeList(artistTag)
            except:
                webhook.send("OJKT verfification check fault")

            print("responding to inqury")
            projectedPrice, TxBlock, TxHash, timestamp, interactionOfintrest = investigateCreator(artistTag)
            objkt_id = interactionOfintrest['parameter']['value']['objkt_id']

            if objkt_id not in saftylist and saftylist != []:
                webhook.send("Artist swapped non creation! double checking...")
                saftylist = getArtistSafeList(artistTag)
                if objkt_id not in saftylist and saftylist != []:
                    webhook.send("Artist swapped non creation! Verified Reboot...")
                    print("reboot...")
                    main()
                else:
                    webhook.send("False Positive. proceeding")

            if TxBlock == "0":
                print("reboot...")
                webhook.send("Artist did not swap for 12 min! Rebooting...")
                main()

            datetime_object = datetime.datetime.now()
            webhook.send("Swap Process identified, sale begins in 1< min! " + str(datetime_object))
            while SwapID == '0':
                blockJson = blockHunter(blockJson, TxBlock)
                SwapID = crownJewlHunter(blockJson, TxHash)
            datetime_object = datetime.datetime.now()
            timestamp = timestamp[11:-6] + str(int(timestamp[14:-4]) + 1) + timestamp[16:-1]
            SwapID = str(int(SwapID) - 1)
            # webhook.send("The Required info for Drop! \n" +  "\n SwapID: " + str(SwapID) +'\nprice: '+ str(projectedPrice) + "\nRelease time: " + str(timestamp)+ " UTC  \n \nCurrent time: " + str(datetime_object) +"Fast Links: \n \nhttps://better-call.dev/mainnet/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9/interact?entrypoint=collect\n \nhttps://time.gov/?t=24")

            ######################
            PowerOfPurse = purchasePreAuth(artistTag,projectedPrice)
            #PowerOfPurse = True
            if PowerOfPurse:
                commitCollectContractInteraction(PRIVATE_KEY,projectedPrice,SwapID)
            ######################

            webhook.send("\nProjected price IN TEZ: " + str(
                int(projectedPrice) / 1000000) + "\nThe Required info for Drop! \n" + "\nSwapID: " + str(
                SwapID) + '\nprice: ' + str(projectedPrice) + "\nRelease time: " + str(timestamp) + " UTC")

            print("Projected price IN TEZ: " + str(
                int(projectedPrice) / 1000000) + "\nThe Required info for Drop! \n" + "\nSwapID: " + str(SwapID) + '\nprice: ' + str(projectedPrice) + "\nRelease time: " + str(timestamp) + " UTC  \n \nCurrent time: " + str(datetime_object) + "Fast Links: \n \nhttps://better-call.dev/mainnet/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9/interact?entrypoint=collect\n \nhttps://time.gov/?t=24")
            # print("Projected price IN TEZ: "  + str(int(projectedPrice)/1000000) + "\nThe Required info for Drop! \n" +  "\nSwapID: " + str(SwapID) +'\nprice: '+ str(projectedPrice) + "\nRelease time: " + str(timestamp)+ " UTC  \n \nCurrent time: " + str(datetime_object) +"Fast Links: \n \nhttps://better-call.dev/mainnet/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9/interact?entrypoint=collect\n \nhttps://time.gov/?t=24")



            lastArtistTag = artistTag
            artistTag = "0"
            SwapID = '0'
            # time.sleep(40) #to prevent the mint opperation that just went through triggering again.

        # blockJson = blockActivityCheck(blockJson)

        # print(operations[0]['entrypoint'])
        # print(operations[0]['source'])

        time.sleep(15)


if __name__ == '__main__':
   main()

# POST /v1/graphql HTTP/1.1
# Host: api.hicdex.com
# Connection: close
# Content-Length: 4555
# User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.89 Safari/537.36
# Content-Type: text/plain;charset=UTF-8
# Accept: */*
# Origin: https://nftbiker.com
# Sec-Fetch-Site: cross-site
# Sec-Fetch-Mode: cors
# Sec-Fetch-Dest: empty
# Referer: https://nftbiker.com/
# Accept-Encoding: gzip, deflate
# Accept-Language: en-US,en;q=0.9
#
# {"query":"\nquery myQuery($address: String!) {\n  hic_et_nunc_token(where: {creator: {address: {_eq: $address}}}, order_by: {id: desc}) {\n    id\n}\n  hic_et_nunc_swap(where: {token: {creator: {address: {_eq: $address}}}, status: {_in: [0, 1]}}, order_by: {token_id: desc, timestamp: desc}) {\n    token_id\n    status\n    price\n    amount\n    amount_left\n    creator {\n      address\n    }\n    timestamp\n  }\n}\n","variables":{"address":"tz1SbhKLqMa1h9eS6LdwL7dduz1qe41SraKQ"},"operationName":"myQuery"}
