## ----setup, include=FALSE-----------------------------------------------------
library(IndexNumR)
library(tidyr)

#devtools::load_all()


## ----head_CES-----------------------------------------------------------------
head(CES_sigma_2)

## ----head_CES_ordered---------------------------------------------------------
head(CES_sigma_2[order(CES_sigma_2$time),])

## ----carry--------------------------------------------------------------------

# create a dataset with some missing observations on product 1 and 2
df <- CES_sigma_2[-c(1,2,15,16),]
df <- df[df$prodID %in% 1:2 & df$time <= 6,]

dfMissing <- df[, c("time", "prices", "prodID")] %>%
  tidyr::pivot_wider(id_cols = time, names_from = prodID, values_from = prices)
dfMissing[order(dfMissing$time),]

# compute carry prices
carryPrices <- imputeCarryPrices(df, pvar = "prices", qvar = "quantities", 
                                 pervar = "time", prodID = "prodID")

# print the data with the product prices in columns to see the filled data
carryPrices[, c("time", "prices", "prodID")] %>%
  tidyr::pivot_wider(id_cols = time, names_from = prodID, values_from = prices)

# print the data with the product quantities in columns to see the corresponding zeros
carryPrices[, c("time", "quantities", "prodID")] %>%
  tidyr::pivot_wider(id_cols = time, names_from = prodID, values_from = quantities)


## ----bilateral_examples-------------------------------------------------------
priceIndex(CES_sigma_2,
           pvar = "prices",
           qvar = "quantities",
           pervar = "time",
           prodID = "prodID", 
           indexMethod = "laspeyres", 
           output = "chained")

## ----multiple_bilateral-------------------------------------------------------
methods <- c("laspeyres","paasche","fisher","tornqvist")
prices <- lapply(methods, 
                 function(x) {priceIndex(CES_sigma_2,
                                         pvar = "prices", 
                                         qvar = "quantities", 
                                         pervar = "time", 
                                         prodID = "prodID", 
                                         indexMethod = x, 
                                         output = "chained")})

as.data.frame(prices, col.names = methods)

## ----elasticity---------------------------------------------------------------
elasticity(CES_sigma_2, 
           pvar="prices",
           qvar="quantities",
           pervar="time",
           prodID="prodID",
           compIndex="ces")

## ----relative_dissimilarity---------------------------------------------------
lq <- relativeDissimilarity(CES_sigma_2, 
                            pvar="prices", 
                            qvar="quantities", 
                            pervar = "time", 
                            prodID = "prodID", 
                            indexMethod = "fisher", 
                            similarityMethod = "logquadratic")

head(lq)

## ----similarity_links---------------------------------------------------------
maximumSimilarityLinks(lq)

## ----index_similarity---------------------------------------------------------
priceIndex(CES_sigma_2,
           pvar = "prices",
           qvar = "quantities",
           pervar = "time",
           prodID = "prodID", 
           indexMethod = "laspeyres", 
           output = "chained", 
           chainMethod = "logquadratic")

## ----geks_splicing------------------------------------------------------------
# Assume that the data in CES_sigma_2 are quarterly data with time period
# 1 corresponding to the December quarter. 

splices <- c("window", "half", "movement", "mean", "fbew", "fbmw", "wisp", "hasp", "mean_pub")

# estimate a GEKS index using the different splicing methods. Under
# the above assumptions, the window must be 5 to ensure the base period is
# each December quarter.
result <- as.data.frame(lapply(splices, function(x){
  GEKSIndex(CES_sigma_2, 
          pvar = "prices", 
          qvar = "quantities", 
          pervar = "time", 
          prodID = "prodID", 
          indexMethod = "tornqvist", 
          window=5, 
          splice = x)
}))

colnames(result) <- splices
result


## ----indicators---------------------------------------------------------------

methods <- c("laspeyres", "paasche", "bennet", "montgomery")

p <- lapply(methods, function(x) {priceIndicator(CES_sigma_2, 
                                                 pvar = "prices", 
                                                 qvar = "quantities", 
                                                 pervar = "time", 
                                                 prodID = "prodID", 
                                                 method = x)})

as.data.frame(p, col.names = methods)


## ----value--------------------------------------------------------------------

valueDecomposition(CES_sigma_2, 
                   pvar = "prices",  
                   qvar = "quantities",  
                   pervar = "time",  
                   prodID = "prodID",  
                   priceMethod = "bennet")


## ----groups-------------------------------------------------------------------

# add a group variable to the CES_sigma_2 dataset
# products 1 and 2 will be in group 1, products 3 and 4 in group 2
df <- CES_sigma_2
df$group <- c(rep(1, 24), rep(2, 24))

# put the arguments to the priceIndex function into a named list
argsList <- list(x = df, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID",
             indexMethod = "fisher", output = "chained")

# estimate a bilateral chained fisher index on the groups
groupIndexes("group", "priceIndex", argsList)

# put the arguments for the GEKSIndex function in a named list
argsGEKS <- list(x = df, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID",
                 indexMethod = "fisher", window = 12)

# estimate a GEKS index on the groups
groupIndexes("group", "GEKSIndex", argsGEKS)



## ----yoy----------------------------------------------------------------------

# Assume the CES_sigma_2 data are quarterly observations over three years. 
# This results in 4 indexes (one for each quarter) of 3 periods each.
# Estimate year-over-year chained fisher indexes.
argsList <- list(x = CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", 
                 prodID = "prodID", indexMethod = "fisher", output = "chained")

yearOverYearIndexes("quarterly", "priceIndex", argsList)


