# CryptocurrencyData WL paclet

Cryptocurrency data retrieval with a WL paclet.

The data ingestion approach is described in [AA1]; 
related data explorations are given in [AA2].

-----

## Usage

`CryptocurencyData[name]`   
gives daily closing price data for the cryptocurrency name.

`CryptocurencyData[name, start]`   
gives daily closing price data for the cryptocurrency name from start to current date.

`CryptocurencyData[name, {start, end}]`   
gives daily closing price data for the cryptocurrency name from start to end.

`CryptocurencyData[name, prop, {start, end}]`   
gives value of the specified property for the cryptocurrency name from start to end.

-----

## Details

- Generally speaking, `CryptcurrencyData` adheres to the signatures design of 
[`FinancialData`](https://reference.wolfram.com/language/ref/FinancialData.html), 
but there are a number of differences.

- `CryptocurrencyData` utilizes two data sources: Yahoo Finance and data.bitcoinity.org.

- `CryptocurrencyData` caches the source-retrived-and-processed data in order to provide results faster.

-----

## References

[AA1] Anton Antonov, 
["Crypto-currencies data acquisition with visualization"](https://mathematicaforprediction.wordpress.com/2021/06/19/crypto-currencies-data-acquisition-with-visualization/), 
(2021), 
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[AA2] Anton Antonov, 
["Cryptocurrencies data explorations"](https://mathematicaforprediction.wordpress.com/2021/06/22/cryptocurrencies-data-explorations/), 
(2021),
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).
