# tez-tax

'tez-tax' is comprised of a series of R code and supplementary files which may provide potentially useful information for estimating gains or losses realized through trading XTZ and associated tokens on the Tezos blockchain. The code makes use of a first in, first out (FIFO) accounting methodology for gain or loss estimates and is geared toward U.S.-based users. 'tez-tax' and any code, outputs, or information derived thereof are provided as-is. Only a certified tax professional can accurately assess the tax implications of trading XTZ or associated tokens on the Tezos blockchain. 

## Disclaimer

The authors of 'tez-tax' are not certified tax professionals and 'tez-tax' is not a tax solution. 'tez-tax' and any code, outputs, or information derived thereof should not be considered a substitute for legal advice, tax advice, audit advice, accounting advice, or brokerage advice under the guidance of a licensed professional. 

## Instructions

'tez-tax' is a work in progress and a number of simplifying assumptions are made during the course of the code logic. As such, it is important to thoroughly understand and debug each step in order to ensure any provided estimates are as accurate as possible.

* Update the contents of configuration file, '00_Config.R'.
	+ Currently, 'tez-tax' only provides support for Coinbase as an exchange. 
	+ To download Coinbase exchange data, navigate to [Coinbase.com/reports](https://www.coinbase.com/reports) and generate a transaction history CSV report. 
