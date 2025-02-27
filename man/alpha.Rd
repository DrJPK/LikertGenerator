% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_alpha.R
\name{alpha}
\alias{alpha}
\title{Calculate Cronbach Alpha}
\usage{
alpha(data)
}
\arguments{
\item{data}{a dataframe or tibble}
}
\value{
a numeric value for alpha
}
\description{
Using a wide dataframe compute the value of Cronbach alpha across all numeric columns.
}
\examples{
d <- tibble::tibble(
  I1 = c(6,5,9,3,2,1,5), 
  I2 = c(6,5,8,2,3,1,4), 
  I3 = c(8,6,6,4,2,2,6)
)
alpha(d)

}
