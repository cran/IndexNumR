# IndexNumR: a package for index number computation
# Copyright (C) 2018 Graham J. White (g.white@unswalumni.com)
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.



#' Geary-Khamis index for a window
#'
#' Calculate the GK index for a window of w periods.
#' This is not exposed to the user since it only computes
#' part of the index, and is called by GKIndex().
#' @keywords internal
#' @noRd
gk_w <- function(x,pvar,qvar,pervar,prodID, sample) {

  # total time periods
  obs <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # if matching requested, keep only products that occur through the whole window
  if(sample == "matched"){
    x <- windowMatch(x, pervar, prodID)
  }
  else {
    # fill out the gaps from missing/new products with zeros.
    # this makes sure that the dimensions of all vectors/matrices
    # are conformable for the calculations
    x <- fillMissing(x, pvar, qvar, pervar, prodID, priceReplace = 0, quantityReplace = 0)
  }

  # set up some variables
  # list of time periods
  pers <- sort(unique(x[[pervar]]))
  # list of products
  prods <- sort(unique(x[[prodID]]))
  # total number of products
  n <- length(prods)

  # share of expenditure on each product in each time period
  stn <- matrix(0, nrow = obs, ncol = n)
  # quantities matrix
  qtn <- matrix(0, nrow = obs, ncol = n)
  # expenditures matrix
  etn <- matrix(0, nrow = obs, ncol = n)

  pmat <- matrix(x[[pvar]], nrow = obs, byrow = TRUE)
  qtn <- matrix(x[[qvar]], nrow = obs, byrow = TRUE)
  etn <- Reduce(`*`, list(pmat, qtn))

  # replace NAs with zeros
  etn <- replace(etn, is.na(etn), 0)
  qtn <- replace(qtn, is.na(qtn), 0)

  # calculate expenditure shares
  et <- rowSums(etn)
  for(i in 1:obs){
    stn[i,] <- etn[i,]/et[i]
  }

  # sum quantities for each product across all time
  q <- colSums(qtn)

  # formulate the inverse diagonal matrix with q down the main diagonal
  D <- solve(diag(q, nrow = n, ncol = n))

  # calculate C
  sq <- matrix(0, nrow = n, ncol = n)
  for(i in 1:obs){
    sq <- sq + stn[i,]%*%t(qtn[i,])
  }
  C <- D%*%sq

  # Compute I
  I <- diag(1, nrow = n, ncol = n)

  # [I-C] is not invertible so need to normalise
  # Make z vector
  z <- c(1, rep(0, n-1))
  # make R matrix
  R <- matrix(0, nrow = n, ncol = n)
  R[1,] <- rep(1, n)

  # Now solve for b
  b <- solve(I - C + R)%*%z

  # With b we can calculate P
  pindex <- matrix(0, nrow = obs, ncol = 1)
  for(i in 1:obs){

    temp <- x[x[[pervar]] == pers[i],]

    pindex[i,1] <- (temp[[pvar]]%*%temp[[qvar]])/(t(b)%*%temp[[qvar]])

  }

  # normalise to first period
  pindex <- pindex/pindex[1,1]

  return(pindex)

}

#' Compute the Geary-Khamis index
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param prodID A character string for the name of the product identifier
#' @param sample set to "matched" to only use products that occur
#' across all periods in a given window. Default is not to match.
#' @param window An integer specifying the length of the window.
#' @param splice the splicing method to use to extend the index. Valid methods are
#' window, movement, half, mean, fbew or fbmw. The default is mean.
#' @details The splicing methods are used to update the price index when new data become
#' available without changing prior index values. The window, movement, half and mean splices
#' use the most recent index value as the base period, which is multiplied by a price movement
#' computed using new data. The fbew (Fixed Base Expanding Window) and fbmw (Fixed Base Moving
#' Window) use a fixed base onto which the price movement using new data is applied. The base
#' period is updated periodically. IndexNumR calculates which periods are the base periods using
#' \code{seq(from = 1, to = n, by = window - 1)}, so the data must be set up correctly and the
#' right window length chosen. For example, if you have monthly data and want December
#' of each year to be the base period, then the first period in the data must be December
#' and the window must be set to 13.
#' @examples
#' # compute a Geary-Khamis index with mean splicing
#' GKIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", window=11, splice = "mean")
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#'
#' Geary, R. G. 1958. “A Note on Comparisons of Exchange Rates and Purchasing Power Between
#' Countries.” Journal of the Royal Statistical Society Series A 121: 97–99.
#'
#' Khamis, S. H. 1970. “Properties and Conditions for the Existence of a New Type of Index Number.”
#' Sankhya: The Indian Journal of Statistics, Series B (1960-2002) 32: 81–98.
#' @export
GKIndex <- function(x, pvar, qvar, pervar, prodID, sample = "", window, splice = "mean"){

  # check that only valid splice methods are chosen
  if(!(tolower(splice) %in% c("mean", "window", "movement", "half", "fbew", "fbmw"))){
    stop("Not a valid splicing method.")
  }

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  # check that columns are the right class
  x <- checkTypes(x, pvar, qvar, pervar)

  # get the number of periods
  n <- max(x[[pervar]], na.rm = TRUE)
  if(n < window){
    stop("The window length exceeds the number of periods in the data")
  }

  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[prodID]]),]

  # initialise some matrices
  # final price index
  pGK <- matrix(0, nrow = n, ncol = 1)

  # set the sequence of base periods for fbew and fbmw splices
  bases <- seq(from = 1, to = n, by = window - 1)

  # first estimate a GK index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call gk_w on first window
  pGK[1:window,1] <- gk_w(xWindow, pvar, qvar, pervar, prodID, sample)

  # use a splicing method to compute the rest of the index
  if(n > window){
    for(i in 2:(n-window+1)){

      # find the base period for fbew and fbmw splices
      base <- max(bases[bases <= i + window - 2])

      # set the old window
      if(i==2){
        old <- pGK[(i-1):(i+window-2),1]
      }
      else {
        old <- new
      }

      # set the base value for fbew
      fbewBase <- pGK[base,1]

      # fetch the next window of data
      if(splice == "fbew"){
        xWindow <- x[x[[pervar]] >= base & x[[pervar]] < i + window,]
      }
      else {
        xWindow <- x[x[[pervar]] >= i & x[[pervar]] < i + window,]
      }

      # call gk_w on this window
      new <- gk_w(xWindow, pvar, qvar, pervar, prodID, sample)

      # splice the new datapoint on
      switch(splice,
             fbew = {pGK[i+window-1,1] <- fbewBase*new[length(new)]},
             fbmw = {pGK[i+window-1,1] <- fbewBase*new[length(new)]/new[length(new)-(i+window-1-base)]},
             pGK[i+window-1,1] <- splice_t(pGK[i+window-2,1], old, new, method=splice))

    }
  }

  return(pGK)

}
