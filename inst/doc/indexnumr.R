## ----setup, include=FALSE-----------------------------------------------------
library(IndexNumR)

## ----head_CES-----------------------------------------------------------------
head(CES_sigma_2)

## ----head_CES_ordered---------------------------------------------------------
head(CES_sigma_2[order(CES_sigma_2$time),])

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

splices <- c("window", "half", "movement", "mean", "fbew", "fbmw")

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


