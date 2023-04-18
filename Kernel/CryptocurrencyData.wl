(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`CryptocurrencyData`"];

CryptocurrencyData::usage = "Generally speaking, CryptcurrencyData adheres to the signatures design of FinancialData, but there are a number of differences.";

Begin["`Private`"];

(*Metadata*)

aCryptoCurrencySymbolToName = <|"BTC" -> "Bitcoin", "ETH" -> "Ethereum",
  "USDT" -> "Tether", "BNB" -> "BinanceCoin", "ADA" -> "Cardano",
  "XRP" -> "XRP", "USDC" -> "Coin", "DOGE" -> "Dogecoin",
  "DOT1" -> "Polkadot", "HEX" -> "HEX", "UNI3" -> "Uniswap",
  "BCH" -> "BitcoinCash", "LTC" -> "Litecoin", "LINK" -> "Chainlink",
  "SOL1" -> "Solana", "MATIC" -> "MaticNetwork", "THETA" -> "THETA",
  "XLM" -> "Stellar", "VET" -> "VeChain", "ICP1" -> "InternetComputer",
  "ETC" -> "EthereumClassic", "TRX" -> "TRON", "FIL" -> "FilecoinFutures",
  "XMR" -> "Monero", "EOS" -> "EOS"|>;

lsCryptoCurrencies = {"BTC", "ETH", "USDT", "BNB", "ADA", "DOGE", "XRP",
  "USDC", "DOT1", "HEX", "UNI3", "BCH", "LTC", "SOL1", "LINK", "THETA",
  "MATIC", "XLM", "ICP1", "VET", "ETC", "FIL", "TRX", "XMR", "EOS"};

lsCurrencies = {"all", "AED", "ARS", "AUD", "BRL", "CAD", "CHF", "CLP", "CNY",
  "COP", "CZK", "DKK", "EUR", "GBP", "HKD", "HRK", "HUF", "IDR", "ILS",
  "INR", "IRR", "JPY", "KES", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP",
  "PKR", "PLN", "RON", "RUB", "RUR", "SAR", "SEK", "SGD", "THB", "TRY",
  "UAH", "USD", "VEF", "XAU", "ZAR"};

lsExchanges = {"all", "bit-x", "bit2c", "bitbay", "bitcoin.co.id",
  "bitcoincentral", "bitcoinde", "bitcoinsnorway", "bitcurex", "bitfinex",
  "bitflyer", "bithumb", "bitmarketpl", "bitmex", "bitquick", "bitso",
  "bitstamp", "btcchina", "btce", "btcmarkets", "campbx", "cex.io",
  "clevercoin", "coinbase", "coinfloor", "exmo", "gemini", "hitbtc", "huobi",
  "itbit", "korbit", "kraken", "lakebtc", "localbitcoins", "mercadobitcoin",
  "okcoin", "paymium", "quadrigacx", "therocktrading", "vaultoro",
  "wallofcoins"};

lsTimeSpans = {"10m", "1h", "6h", "24h", "3d", "30d", "6m", "2y", "5y",
  "all"};

aTimeUnitToDBOSpec = <|"Second" -> "second", "Minute" -> "minute",
  "Hour" -> "hour", "Day" -> "day", "Week" -> "week", "Month" -> "month"|>;

aDataTypeToDBOSpec = <|"Price" -> "price", "Volume" -> "volume",
  "TradingVolume" -> "volume", "Rank" -> "rank", "BidAskSum" -> "bidask_sum",
  "BidAskSpread" -> "spread", "TradesPerMinute" -> "tradespm"|>;

aDBOPropertyCorrections = <|"min" -> "Low", "max" -> "High", "avg" -> "Mean",
  "volume" -> "Volume", "rank" -> "Rank", "price" -> "Price",
  "bidask_sum" -> "BidAskSum", "spread" -> "BidAskSpread",
  "tradespm" -> "TradesPerMinute"|>;

lsDataTypes = Values[aDataTypeToDBOSpec];

aDataTypeDescriptions =
    Association@{"price" -> "Price", "volume" -> "Trading Volume",
      "rank" -> "Rank", "bidask_sum" -> "Bid/Ask Sum",
      "spread" -> "Bid/Ask Spread", "tradespm" -> "Trades Per Minute"};

(*Stencil URLs*)

stYFURL =
    StringTemplate[
      "https://query1.finance.yahoo.com/v7/finance/download/`\
cryptoCurrencySymbol`-`currencySymbol`?period1=1410825600&period2=`endDate`&\
interval=`timeUnit`&events=history&includeAdjustedClose=true"];

stDBOURL =
    StringTemplate[
      "https://data.bitcoinity.org/export_data.csv?currency=`currencySymbol`&\
data_type=`dataType`&exchange=`exchange`&r=`timeUnit`&t=l&timespan=`timeSpan`\
"];

(*Defaults*)

aYFDefaultParameters = <|"cryptoCurrencySymobl" -> "BTC",
  "currencySymbol" -> "USD", "timeUnit" -> "1d",
  "endDate" ->
      Round[AbsoluteTime[Date[]] - AbsoluteTime[{1970, 1, 1, 0, 0, 0}]]|>;

aDBODefaultParameters = <|"currencySymbol" -> "USD", "dataType" -> "price",
  "exchange" -> "all", "timeUnit" -> "day", "timeSpan" -> "all"|>;

(*Data storage*)

aAllData = <||>;

(*Predicates*)

Clear[DateSpecQ];
DateSpecQ[x_ : (_String | {_?NumericQ ..} | _?NumericQ | _DateObject)] :=
    Quiet[DateObjectQ[DateObject[x]]];
DateSpecQ[___] := False;

Clear[PropertySpecQ];
PropertySpecQ[x_] := MatchQ[x, (_String | {_String ..} | All | Automatic)];

(*Conversion to time series*)

Clear[ToTimeSeriesAssociation];
ToTimeSeriesAssociation[dsCCData_Dataset] :=
    Block[{lsVars, aTSRes},
      lsVars = Complement[Normal[Keys[dsCCData[[1]]]], {"DateObject"}];
      aTSRes =
          Association@
              Map[# -> TimeSeries[
                Select[Normal@dsCCData[All, {"DateObject", #}][Values],
                  NumericQ[#[[2]]] &]] &, lsVars];
      aTSRes
    ];

Clear[ToQuantities];
ToQuantities[ts : (_TemporalData | _TimeSiries), unit_String] :=
    TimeSeries@Transpose[{ts["Times"], Quantity[ts["Values"], unit]}];
ToQuantities[aTS : Association[(_String -> (_TemporalData | _TimeSeries)) ..],
  currency_String, volumeUnit_String : "Items"] :=
    Association@
        KeyValueMap[
          #1 ->
              Which[
                MemberQ[ToLowerCase@{"Open", "High", "Low", "Close", "Adj Close"},
                  ToLowerCase[#1]],
                ToQuantities[#2, currency],

                MemberQ[ToLowerCase@{"Volume"}, ToLowerCase[#1]],
                ToQuantities[#2, volumeUnit],

                True,
                #2
              ] &,
          aTS
        ];

(*Main function*)

Clear[CryptocurrencyData];

CryptocurrencyData::nyfcc =
    "When of the option \"Source\" is set to \"YahooFinance\" the first \
argument is expected to be one of `1`.";

CryptocurrencyData::nsrc =
    "The value of the option \"Source\" is expected to be Automatic, \
\"YahooFinance\", or \"DataBitcoinityOrg\".";

CryptocurrencyData::nrt =
    "The value of the option \"ResultType\" is expected to be Automatic, \
Dataset, or TimeSeries.";

CryptocurrencyData::nc =
    "The value of the option \"Currency\" is expected to be one of `1`.";

CryptocurrencyData::args =
    "The first argument is expected to be a cryptocurrency specification. The \
second argument is expected to be a property or date range specification. If \
more than two arguments are given, then the third argument is expected to be \
a date range specification.";

CryptocurrencyData::dbobtc =
    "When the option \"Source\" is set to \"DataBitcoinityOrg\" the only \
allowed cryptocurrency is \"BTC\".";

Options[CryptocurrencyData] = {"Source" -> "YahooFinance",
  "Currency" -> "USD", "LedgerStart" -> DateObject[{2009, 1, 3}],
  "ResultType" -> TimeSeries, "Quantities" -> False};

CryptocurrencyData["ClearCachedData"] := (aAllData = <||>);

CryptocurrencyData["Classes"] := {"Cryptocurrencies", "Currencies",
  "Exchanges"};

CryptocurrencyData["CryptocurrencyNames"] := aCryptoCurrencySymbolToName;
CryptocurrencyData["Cryptocurrencies"] :=
    Complement[lsCryptoCurrencies, {"all"}];
CryptocurrencyData["Currencies"] := Complement[lsCurrencies, {"all"}];
CryptocurrencyData["Exchanges"] := Complement[lsExchanges, {"all"}];

CryptocurrencyData[ccSymbol_, opts : OptionsPattern[]] :=
    CryptocurrencyData[ccSymbol, Automatic, opts];

CryptocurrencyData[ccSymbol_, dateSpec_?DateSpecQ, opts : OptionsPattern[]] :=
    CryptocurrencyData[ccSymbol, Automatic, dateSpec, opts];

CryptocurrencyData[ccSymbol_, prop_?PropertySpecQ, opts : OptionsPattern[]] :=
    CryptocurrencyData[ccSymbol, prop, All, opts];

CryptocurrencyData[ccSymbol_, prop_?PropertySpecQ, All,
  opts : OptionsPattern[]] :=
    CryptocurrencyData[ccSymbol,
      prop, {OptionValue[CryptocurrencyData, "LedgerStart"], Now}, opts];

CryptocurrencyData[ccSymbol_, prop_?PropertySpecQ, dateSpec_?DateSpecQ,
  opts : OptionsPattern[]] :=
    CryptocurrencyData[ccSymbol, prop, {dateSpec, Now}, opts];

CryptocurrencyData[ccSymbol_, prop_?PropertySpecQ,
  dateSpec : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    Block[{source},

      source = OptionValue[CryptocurrencyData, "Source"];
      If[TrueQ[source === Automatic], source = "YahooFinance"];
      If[StringQ[source], source = ToLowerCase[source]];

      Which[
        MemberQ[ToLowerCase@{"YahooFinance", "YF"}, source],
        YahooFinanceCryptocurrencyData[ccSymbol, prop, dateSpec, opts],

        MemberQ[ToLowerCase@{"DataBitcoinityOrg", "DBO"}, source],
        DataBitcoinityOrgCryptocurrencyData[ccSymbol, prop, dateSpec, opts],

        True,
        ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::nsrc];
        $Failed
      ]
    ];

CryptocurrencyData[___] :=
    (ResourceFunction["ResourceFunctionMessage"][
      CryptocurrencyData::args]; $Failed);

(*YahooFinanceCryptocurrencyData*)

Clear[YahooFinanceCryptocurrencyData];

Options[YahooFinanceCryptocurrencyData] = Options[CryptocurrencyData];

YahooFinanceCryptocurrencyData[All, "Summary", _, opts : OptionsPattern[]] :=
    Block[{lsData, pos, dsCryptoCurrenciesColumnNames, dsCryptoCurrencies},
      If[KeyExistsQ[aAllData, {"YahooFinance", All, "Summary"}],
        (*Already obtained, return as a result*)
        Lookup[aAllData, Key[{"YahooFinance", All, "Summary"}]],
        (*ELSE*)

        (*Import web page*)
        lsData = Import["https://finance.yahoo.com/cryptocurrencies", "Data"];

        (*Find position of the column names row*)
        pos =
            First@Position[
              lsData, {"Symbol", "Name", "Price (Intraday)", "Change",
                "% Change", ___}];

        (*Get column names*)
        dsCryptoCurrenciesColumnNames = lsData[[Sequence @@ pos]];

        (*Make dataset*)
        dsCryptoCurrencies = lsData[[Sequence @@ Append[Most[pos], 2]]];
        dsCryptoCurrencies =
            Dataset[dsCryptoCurrencies][All,
              AssociationThread[dsCryptoCurrenciesColumnNames[[1 ;; -3]], #] &];

        (*Parse numbers*)
        dsCryptoCurrencies =
            dsCryptoCurrencies[All,
              Association@
                  KeyValueMap[
                    #1 ->
                        Which[
                          MemberQ[{"Symbol", "Name"}, #1], #2,
                          NumericQ[#2], #2,
                          True,
                          ToExpression[
                            StringReplace[#2, {"," -> "", "%" -> "*0.01", "T" -> "*10^12",
                              "B" -> "*10^9", "M" -> "*10^6"}]]
                        ] &, #] &
            ];

        (*Save dataset for reuse*)
        aAllData =
            Append[aAllData, {"YahooFinance", All, "Summary"} ->
                dsCryptoCurrencies];

        (*Result*)
        dsCryptoCurrencies
      ]
    ];

YahooFinanceCryptocurrencyData[All, prop_?PropertySpecQ,
  dateSpec : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    YahooFinanceCryptocurrencyData[lsCryptoCurrencies, prop, dateSpec, opts];

YahooFinanceCryptocurrencyData[ccSymbols : {_String ..}, prop_?PropertySpecQ,
  dateSpec : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    Association[# -> YahooFinanceCryptocurrencyData[#, prop, dateSpec, opts] & /@
        ccSymbols];

YahooFinanceCryptocurrencyData[ccSymbol_String, propArg_?PropertySpecQ,
  dateSpecArg : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    Block[{prop = propArg, dateSpec = dateSpecArg, currencySymbol, resultType,
      quantitiesQ, ccNow, aCryptoCurrenciesDataRaw, aCryptoCurrenciesData,
      dsRes},

      (*Process ccSymbol*)
      If[! MemberQ[lsCryptoCurrencies, ccSymbol],
        ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::nyfcc,
          ToString[lsCryptoCurrencies]];
        Return[$Failed]
      ];

      (*Process propeties*)
      Which[
        TrueQ[prop === Automatic],
        prop = {"DateObject", "Close"},

        VectorQ[prop, StringQ],
        prop = Flatten[{prop}];
        prop = DeleteDuplicates[Prepend[prop, "DateObject"]],

        StringQ[prop],
        prop = {"DateObject", prop},

        True,
        prop = All
      ];

      (*Process date spec*)
      dateSpec = Sort[AbsoluteTime@*DateObject /@ dateSpec];

      (*Get currency symbol*)
      currencySymbol = OptionValue[YahooFinanceCryptocurrencyData, "Currency"];
      If[! MemberQ[lsCurrencies, currencySymbol],
        ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::nc,
          ToString[lsCurrencies]];
        Return[$Failed]
      ];

      (*Process result type*)
      resultType = OptionValue[YahooFinanceCryptocurrencyData, "ResultType"];
      If[TrueQ[resultType === Automatic], resultType = TimeSeries];

      (*Proces quanties*)
      quantitiesQ =
          TrueQ[OptionValue[YahooFinanceCryptocurrencyData, "Quantities"]];

      (*Get data*)
      dsRes =
          If[KeyExistsQ[aAllData, {"YahooFinance", ccSymbol, currencySymbol}],
            Lookup[aAllData, Key[{"YahooFinance", ccSymbol, currencySymbol}]],
            (*ELSE*)
            ccNow =
                Round@AbsoluteTime[Date[]] - AbsoluteTime[{1970, 1, 1, 0, 0, 0}];

            (*Retrieve all time series data*)
            aCryptoCurrenciesDataRaw =
                Association@
                    Map[
                      {"YahooFinance", #, currencySymbol} ->

                          ResourceFunction["ImportCSVToDataset"][
                            stYFURL[<|"cryptoCurrencySymbol" -> #,
                              "currencySymbol" -> currencySymbol, "endDate" -> ccNow|>]] &,
                      lsCryptoCurrencies
                    ];

            (*Add ID, Symbol, and DateObject *)
            aCryptoCurrenciesData =
                Association@
                    KeyValueMap[
                      Function[{k, v},
                        k -> v[All,
                          Join[<|"ID" -> StringRiffle[k, "-"], "Symbol" -> k[[2]],
                            "Currency" -> currencySymbol,
                            "DateObject" -> DateObject[#Date]|>, #] &]],
                      aCryptoCurrenciesDataRaw];

            (*Add to data storage.*)
            aAllData = Join[aAllData, aCryptoCurrenciesData];

            (*Retrieve*)
            Lookup[aAllData, Key[{"YahooFinance", ccSymbol, currencySymbol}]]
          ];

      (*Filter to specs*)
      dsRes =
          dsRes[Select[
            dateSpec[[1]] <= AbsoluteTime[#DateObject] <= dateSpec[[2]] &], prop];

      (*Result*)
      Which[
        MemberQ[{Dataset, "Dataset"}, resultType],
        dsRes,

        MemberQ[{TimeSeries, EventSeries, "TimeSeries", "EvenSeries"}, resultType],
        dsRes = ToTimeSeriesAssociation[dsRes];
        dsRes =
            KeyMap[# /. {"Adj Close" -> "AdjustedClose"} &,
              KeyDrop[dsRes, {"ID", "Symbol", "Currency", "Date"}]];
        dsRes =
            If[quantitiesQ, ToQuantities[dsRes, currencySymbol, "Items"], dsRes];
        If[Length[dsRes] == 1, dsRes[[1]], dsRes],

        True,
        ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::nrt];
        dsRes
      ]
    ];

(*DataBitcoinityOrgCryptocurrencyData*)

Clear[DataBitcoinityOrgCryptocurrencyData];

Options[DataBitcoinityOrgCryptocurrencyData] = Options[CryptocurrencyData];

DataBitcoinityOrgCryptocurrencyData[ccSpec_, props_?ListQ,
  dateSpec : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    Block[{lsRes},
      lsRes =
          DataBitcoinityOrgCryptocurrencyData[ccSpec, #, dateSpec, opts] & /@
              props;
      If[VectorQ[lsRes, AssociationQ],
        Join @@ lsRes,
        (*ELSE*)
        lsRes
      ]
    ];

DataBitcoinityOrgCryptocurrencyData[Automatic | "BTC", prop_?PropertySpecQ,
  dateSpec : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    DataBitcoinityOrgCryptocurrencyData[{Automatic, "BTC"}, prop, dateSpec,
      opts];

DataBitcoinityOrgCryptocurrencyData[{exchangeArg : (_String | All |
    Automatic), "BTC"}, propArg : (_String | Automatic),
  dateSpecArg : {_?DateSpecQ, _?DateSpecQ}, opts : OptionsPattern[]] :=
    Block[{exchange = exchangeArg, prop = propArg, dateSpec = dateSpecArg,
      currencySymbol, resultType, quantitiesQ, dsBTCData, aColumnCorrections,
      dsRes},

      (*Process exchange*)
      If[TrueQ[exchange === All], exchange = "all"];
      If[TrueQ[exchange === Automatic], exchange = "coinbase"];

      (*Process propeties*)
      Which[
        StringQ[prop],
        prop = prop /. aDataTypeToDBOSpec,

        True,
        prop = "price"
      ];

      (*Process data spec*)
      dateSpec = Sort[AbsoluteTime@*DateObject /@ dateSpec];

      (*Get currency symobl*)
      currencySymbol =
          OptionValue[DataBitcoinityOrgCryptocurrencyData, "Currency"];

      (*Process result type*)
      resultType = OptionValue[DataBitcoinityOrgCryptocurrencyData, "ResultType"];
      If[TrueQ[resultType === Automatic], resultType = TimeSeries];

      (*Proces quanties*)
      quantitiesQ =
          TrueQ[OptionValue[DataBitcoinityOrgCryptocurrencyData, "Quantities"]];

      (*Get data*)
      dsRes =
          If[KeyExistsQ[
            aAllData, {"DataBitcoinityOrg", "BTC", exchange, prop,
              currencySymbol}],
            Lookup[aAllData,
              Key[{"DataBitcoinityOrg", "BTC", exchange, prop, currencySymbol}]],
            (*ELSE*)

            (*Import data*)
            dsBTCData =
                ResourceFunction["ImportCSVToDataset"][
                  stDBOURL[Join[
                    aDBODefaultParameters, <|"dataType" -> prop,
                      "currency" -> currencySymbol, "timeUnit" -> "hour",
                      "timeSpan" -> "all", "exchange" -> exchange|>]]];

            (*Correct the column names*)
            (*Replace empty column name with prop, if present*)
            aColumnCorrections =
                Append[aDBOPropertyCorrections, "" -> (prop /. aDBOPropertyCorrections)];
            dsBTCData =
                dsBTCData[All,
                  AssociationThread[Keys[#] /. aColumnCorrections, Values[#]] &];

            (*Add ID, Symbol, and DateObject *)
            dsBTCData =
                dsBTCData[All,
                  Join[<|"ID" ->
                      StringRiffle[{"DataBitcoinityOrg", "BTC", exchange}, "-"],
                    "Symbol" -> "BTC", "DateObject" -> DateObject[#Time]|>, #] &];

            (*Save for reuse*)
            aAllData =
                Append[aAllData, {"DataBitcoinityOrg", "BTC", exchange, prop,
                  currencySymbol} -> dsBTCData];

            (*Retrieve*)
            Lookup[aAllData,
              Key[{"DataBitcoinityOrg", "BTC", exchange, prop, currencySymbol}]]
          ];

      (*Filter to specs*)
      dsRes =
          dsRes[Select[
            dateSpec[[1]] <= AbsoluteTime[#DateObject] <= dateSpec[[2]] &], All];

      (*Result*)
      Which[
        MemberQ[{Dataset, "Dataset"}, resultType],
        dsRes,

        MemberQ[{TimeSeries, EventSeries, "TimeSeries", "EvenSeries"}, resultType],
        dsRes = ToTimeSeriesAssociation[dsRes];
        dsRes =
            KeyMap[# /. aDBOPropertyCorrections &,
              KeyDrop[dsRes, {"ID", "Symbol", "Time"}]];
        dsRes =
            If[quantitiesQ, ToQuantities[dsRes, currencySymbol, "Items"], dsRes];
        If[Length[dsRes] == 1, dsRes[[1]], dsRes],

        True,
        ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::nrt];
        dsRes
      ]
    ];

DataBitcoinityOrgCryptocurrencyData[___] :=
    (ResourceFunction["ResourceFunctionMessage"][CryptocurrencyData::dbobtc]; $Failed);

End[];
EndPackage[];