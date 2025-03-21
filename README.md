# tez-tax

'tez-tax' is comprised of a series of R code and supplementary files which may provide potentially useful information for estimating gains or losses realized through trading XTZ and associated tokens on the Tezos blockchain. The code makes use of a first in, first out (FIFO) accounting methodology for gain or loss estimates and is geared toward U.S.-based users. 'tez-tax' and any code, outputs, or information derived thereof are provided as-is and at your own risk. Only a certified tax professional can accurately assess the tax implications of trading XTZ or associated tokens on the Tezos blockchain. 

## Disclaimer

The authors of 'tez-tax' are not tax professionals and 'tez-tax' is not a tax solution. 'tez-tax' and any code, outputs, or information derived thereof should not be considered a substitute for legal advice, tax advice, audit advice, accounting advice, or brokerage advice under the guidance of a licensed professional. 

## Issues

'tez-tax' is no longer actively maintained. As such, there are issues with the current version. Notable known issues:

* No explicit logic has been written to account for staking.
* Coinbase import no longer works due to changes in file structure provided by Coinbase.

## Instructions

'tez-tax' is a work in progress and a number of simplifying assumptions are made during the course of the code logic. Further, the identification and classification of smart contract operations is exceedingly complex by nature. It is important to thoroughly understand and debug each step in order to ensure any provided estimates are as accurate as possible. 'tez-tax' will never provide perfectly accurate outputs and only seeks to provide potentially useful information for estimating gain or loss estimates. It is worth noting that it is not possible to comprehensively categorize all smart contract operations and is important to validate all outputs at each step of the process. 

Users of 'tez-tax' should have a firm understanding of the R programming language and the tax implications of their Tezos blockchain activity. All 'tez-tax' outputs should be thoroughly reviewed for accuracy. Failure to critically validate the outputs can result in erroneous estimates.  

* Update the contents of configuration file, '00_Config.R', and run it. The configuration file should be run prior to each 'tez-tax' session.
	+ Currently, 'tez-tax' only provides support for Coinbase as an exchange, alternative exchange data will need to be manually imported and added to the initial income statement, please refer to 'functions/cb_import.R' for example. 
	+ To download Coinbase exchange data, navigate to [Coinbase.com/reports](https://www.coinbase.com/reports) and generate a transaction history CSV report. 
	+ Currently 'tez-tax' does not support any exchanges other than Coinbase. Other exchange transactions will have to be added or modified manually.

* Download operations data via the [TzKT API](https://api.tzkt.io/) by running '01_Operations_Data.R'.
	+ This step will likely take the longest as it downloads blockchain data for all wallets included in the configuration file.
	+ It is important to note that the [TzKT API](https://api.tzkt.io/) calls may not download transactions where both a wallet listed in the configuration file did not initiate a transaction and a wallet listed in the configuration file did not receive XTZ. For example, airdropped FA2 tokens will not be downloaded in this step due to this limitation. 
	+ Once the code has been run once, it does not need to be run again unless the configuration or code itself has been updated.

* Classify operation groups and generate an initial income statement by running '02_IS_Generation.R'.
	+ This step relies on 'functions/classify_tx.R' to classify transactions and calculate applicable income statement fields.  
	+ Once the code has finished running, users should verify that all transactions have been classified correctly. 
		+ Any misclassified or miscalculated rows or fields should be manually adjusted. 
		+ In the event that any operations are unclassified, nrow(filter(is, is.na(case))) > 0, the unclassified rows should be manually adjusted.

*  Generate necessary gain or loss data via a dynamic income statement/balance sheet relationship by running '03_BS_Generation.R'. 
	+ This step attempts to calculate gains or losses using the provided income statement and exchange data. 
	+ It is important to note that, due to the API limitations mentioned above, airdropped FA2 tokens from external accounts and similar transactions may not appear in the initial generated income statement. 
		+ A warning will appear if one of these tokens is otherwise interacted with, as such an interaction will result in a deficient token balance. 
		+ When a deficient token balance is encountered, the code will assume the token was acquired with a cost basis of 0 XTZ and no acquisition date will be provided for the token in the 'tax_8949' output. 
		+ Missing acquisition dates should be manually input by the user. 
	+ A number of additional, strong assumptions are made during this process that should be thoroughly reviewed in the code.
	+ Once the code has finished running, the balances provided in the balance sheet data should be recoonciled to those provided via the [TzKT API](https://api.tzkt.io/) at various points in time.
	+ Data included in the 'tax_8949' dataset are used to calculate capital gains income (i.e., Schedule D). Data included in 'xtzIncome_data' are used to calculate other income (i.e., Schedule 1). It is important to ensure that these classifications are correct.
	
* For U.S. users, it may be useful to generate tax documents using '04_Tax_Generation.R'. **The outputs of this step should be used for informational purposes only and are provided as-is. Only a certified tax professional can accurately assess the tax implications of trading XTZ or associated tokens on the Tezos blockchain.**
	+ This step relies on the ['staplr' R package](https://cran.r-project.org/web/packages/staplr/index.html).
	+ Please note that the current code assumes all transactions are short-term trades for the time being. 

## License

Copyright 2025 datcsv

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

```http://www.apache.org/licenses/LICENSE-2.0```

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. 
