# Insider-trading
Datasets and codes about insider trading in the Brazilian market. For further information check our ECML PKDD 2020 paper 207: "Detecting and predicting evidences of insider trading in the Brazilian market".

# Instructions

To check the results presented in the paper, open the ``ECML_PKDD_2020`` folder. To run the R codes you need the packages ``data.table``, ``MLmetrics`` and ``xgboost``. To obtain the exact same results as reported, we suggest using the version 0.90.0.2 of the xgboost package available on R. You also need to set the directory of the file ``dataset.csv`` to run the codes.

The folder  ``Datasets`` contain all the dataset versions already pre-processed and with updates on the output. The files are named as ``dataset_DD_MM_YYYY.csv``, where ``DD_MM_YYYY`` is the updation date of the dataset.

The file ``originalFeatures.csv`` contains the dataset without any pre-processing.

# Dataset

|     N | Column name                 | Type     | Description                                         |
|-------|-----------------------------|----------|-----------------------------------------------------|
|     1 | stockName                   | string   | Stock code                                          |
|     2 | date                        | date     | Date in format YYYY-MM-DD                           |
|     3 | return                      | numeric  | Daily return                                        |
|     4 | sumSD                       | numeric  | Feature corresponding to Eq. (3) in the main paper  |
|     5 | maxNegDay                   | numeric  | Feature corresponding to Eq. (1) in the main paper  |
|     6 | maxNegTime                  | numeric  | Feature corresponding to Eq. (2) in the main paper  |
|     7 | totalVolume                 | numeric  | Total volume traded                                 |
|     8 | numberOfTrades              | numeric  | Number of trades in the day                         |
|     9 | callsSum                    | numeric  | Feature corresponding to Eq. (4) in the main paper  |
|    10 | PutsSum                     | numeric  | Feature corresponding to Eq. (5) in the main paper  |
|    11 | maxCall                     | numeric  | Feature corresponding to Eq. (6) in the main paper  |
|    12 | maxPut                      | numeric  | Feature corresponding to Eq. (7) in the main paper  |
|    13 | differenceSumOfCallsAndPuts | numeric  | Feature corresponding to Eq. (8) in the main paper  |
|    14 | differenceMaxCallAndPut     | numeric  | Feature corresponding to Eq. (9) in the main paper  |
|    15 | spread                      | numeric  | Relative spread between the highest                 |
|       |                             |          | and lowest prices                                   |
|    16 | return1                     | numeric  | Return on day $t-1$                                 |
|    17 | return2                     | numeric  | Return on day $t-2$                                 |
|    18 | return3                     | numeric  | Return on day $t-3$                                 |
|    19 | return4                     | numeric  | Return on day $t-4$                                 |
|    20 | return5                     | numeric  | Return on day $t-5$                                 |
|    21 | returnNewsEventDay          | numeric  | Return on news event day (0.5 if not applicable)    |
|    22 | daysBeforeNewsEvent         | category | Days before the next news event day (0 if           |
|       |                             |          | not applicable)                                     |
|    23 | daysAfterNewsEvent          | category | Days after next news event day (0 if not applicable)|
|    24 | scoreNewsEvent              | category | Succeeding news event category: -1 (bad event),     |
|       |                             |          | 0 (no event), 1 (good event)                        |
|    25 | insiderTraindgEvidence      | binary   | Possible evidence of insider trading: 0 (no         |
|       |                             |          | evidence), 1 (evidence)                             |
|    26 | spotMarket                  | binary   | Possible evidence of insider trading in the spot    |
|       |                             |          | market: 0 (no evidence), 1 (evidence)               |
|    27 | optionsMarket               | binary   | Possible evidence of insider trading on options     |
|       |                             |          | market: 0 (no evidence), 1 (evidence)               |
