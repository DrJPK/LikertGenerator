#' GenerateDF
#'
#' Generates a dataframe of variable width that corresponds to **raw** Likert data
#'
#' @param n The number of rows to generate
#' @param itemlength How many scale points does the Likert scale have?
#' @param scalewidth How many items make up the full scale
#' @param mu The intended mean scale point of the returned data set. If not set, then the centre of the scale is used.
#' @param delta If uncertainty related to misunderstanding the scale items is desired, then this sets the magnitude of the standard deviation of that uncertainty
#' @param rho The standard deviation of the item responses **per** row. This corresponds to variability within the items on a per respondent level.
#' @param sigma how many standard deviations should the full range really represent
#' @param identifier An optional string that is prepended to id variables
#' @param seed optional seed for the random number generator
#' @param trim If TRUE then out of range values will be replaced by NA. If FALSE then out of range values will be set to the min and max values as appropriate
#'
#' @return A wide format dataframe of simulated response values (x) and values with additional uncertainty (e) if delta is set.
#' @export
#'
#' @import tibble
#' @import dplyr
#'
#' @examples
#' ##Create a simple dataframe for a 5-item Likert scale with each item on a scale from 1 to 5
#' generateDF(identifier = "C")
#' 
#' ##As above but replace NA with min and max scale values
#' generateDF(trim = FALSE, identifier = "C")
#' 
#' ##As example 1, but simulate 32% of participants misinterpreting scale points
#' generateDF(delta = 1, identifier = "C")
#'
#' ##Create a 6-item scale, of 7-point Likert items centred on a response of 5 and no uncertainty.
#' generateDF(n=20, itemlength = 7, scalewidth = 6, mu =5, identifier = "Student")
#' 
generateDF <- function(n = 10,
                       itemlength = 5,
                       scalewidth = 5,
                       mu = NULL,
                       delta = NULL,
                       rho = 1.46,
                       sigma = 2,
                       identifier = "",
                       seed = NULL,
                       trim = TRUE) {
  if (!is.null(seed)) {
    if (is.numeric(seed)) {
      set.seed(as.numeric(seed))
    } else{
      set.seed(42)
    }
  }
  
  s = (4 * pnorm(sigma)) / (itemlength + 1) #This sets the width of the whole scale in terms of the number of standard deviations that should fit in the full scale width
  if (is.null(mu)) {
    mu <- (itemlength + 1) / 2 #If mu not set, then set to middle of scale.
  }
  
  d <- tibble::tibble(id = as.factor(paste0(identifier, seq(1:n))),
                      x = rnorm(n = n, mean = mu, sd = s))
  for (i in 1:scalewidth) {
    d <- d %>%
      dplyr::mutate("x{i}" := round(x + rnorm(
        n = n, mean = 0, sd = rho
      ), 0))
  }
  
  if (!is.null(delta)) {
    d <- d %>%
      dplyr::mutate(e = rnorm(n = n, mean = 0, sd = delta))
    for (i in 1:scalewidth) {
      d <- d %>%
        dplyr::mutate("e{i}" := round(!!as.name(paste0("x", i)) + e, 0))
    }
  }
  
  if (trim) {
    d <- d %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("^(x|e)[1-9]+$"),
        ~ dplyr::case_when(. < 1 ~ NA, . > itemlength ~ NA, TRUE ~ .)
      ))
  } else{
    d <- d %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("^(x|e)[1-9]+$"),
        ~ dplyr::case_when(. < 1 ~ 1, . > itemlength ~ itemlength, TRUE ~ .)
      ))
  }
  d <- d %>% select(-x)
  if (!is.null(delta)) {
    d <- d %>% select(-e)
  }
  return(d)
}
