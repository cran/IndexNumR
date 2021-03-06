% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CESData.R
\name{CESData}
\alias{CESData}
\title{Generate data assuming CES preferences}
\usage{
CESData(sigma)
}
\arguments{
\item{sigma}{the elasticity of substitution parameter}
}
\value{
a dataframe containing time period, prices, quantities and
product identifier.
}
\description{
This function is useful for generating datasets that can be used for
testing where the 'true' price index is known. The data are constructed
using assumed prices and total expenditure in each period. Expenditure
shares and quantities are then computed assuming CES preferences. For
further details, see the references.
}
\examples{
\dontrun{
# generate data assuming the elasticity of substitution is 2
CESData(2)
}
}
\references{
W.E. Diewert and K.J. Fox (2017),
"Substitution Bias in Multilateral Methods for CPI Construction Using
Scanner Data", Discussion Paper 17-02, Vancouver School of Economics,
The University of British Columbia.
}
