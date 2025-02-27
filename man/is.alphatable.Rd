% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_alpha.R
\name{is.alphatable}
\alias{is.alphatable}
\title{Checking data classes of alphatable}
\usage{
is.alphatable(x)
}
\arguments{
\item{x}{any R object}
}
\value{
returns TRUE it its argument is an alphatable dataframe or FALSE otehrwise
}
\description{
Check if an object contains all the necessary information to be an alphatable model
}
\examples{
x <- tibble::tibble(sd = seq(0,1, by=0.1), alpha = (1 - sd))
is.alphatable(x)

x <- make_alpha_table()
is.alphatable(x)

}
