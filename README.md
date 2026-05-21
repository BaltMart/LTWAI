# LTWAI
Bachelor thesis developing a weekly activity index

The repository features all of the code I did for the thesis project. I do not include data or most of the figures as they are stored in different directories and are largely trivial. Although, I will include the data sets used to estimate the index.

All used data is available publicly to download. For Lithuanian data, data is available on data.gov.lt, electricity data from ENTSO-E, NO2 emmision data is from European Environment Agency, Flights are from flightradar and others are self explanatory.

To follow along the code one should start with DATA PREPARATION folder, after which follows Seasonal Adjustments. Having adjusted the data, it is prepared in codes that are featured in Final Preparation folder, where data is mostly converted into 13-week (3-month) rolling growth rates over preceding 13-weeks (3-months) and collected into a single data set.

EM_PCA folder features codes estimating the index along codes testing different starting points of the index and different index specifications.

Calculation folder features codes that explore the relationship of the index to the GDP.

While the index visually seems to be tracking general Lithuanian economic activity relatively well, index quarter end value correlation with GDP growth (what is considered to be comparable due to previously mentioned data transformation) is around 0.727 while comparable indices created around the COVID pandemic report a correlation closer to 0.9. However, the variance explained by the first principal component is comparable to those of other papers.

Unfortunately, while initial analysis seems to suggest the index provides useful information to predict the quarterly GDP, that result is not robust to exclusion of COVID years of 2020 and 2021.
