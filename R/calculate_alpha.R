#' Compute table of data spread and Cronbacj alpha
#' 
#' Alpha, a commonly accepted measure of internal consistency needs to be computed from actual data rather than theory. Data that have very little variability within a case have high internal consistency. This helper function creates a series of simulated data points with random noise added drawn from a normal distribution of varying standard distribution from 0 to 4. Alpha is then calculated for each set of results and a summary table is returned.  This is a slow function and it is very unlikely you actually want to be calling it.
#'
#' @param scalelength How many items are in the simulated scale? This should match the Likert Scale you are trying to replicate.
#' @param samples How many runs should be undertaken? More is generally better and will result in less noise at low consistency, but this is at the expense of speed.
#' @param cases How many cases should be generated for each simulation run.
#'
#' @return a 2 column tibble
#' @export
#'
#' @examples
#' make_alpha_table()
#' 
#' make_alpha_table(scalelength = 7, samples = 8)
make_alpha_table <- function(scalelength = 5, samples = 5, cases=5000){
  t <- tibble::tibble(sd = numeric(),
                      alpha = numeric()
                      )
  
  for (s in 1:samples){
    for(delta in seq(0,4, by = 0.02)){
      df <- tibble::tibble(x = rnorm(cases,0,1))
      for(n in 1:scalelength){
        df <- df %>%
          dplyr::mutate("y{n}" := x + rnorm(cases,0,delta))
      }
      df <- df %>%
        dplyr::mutate(Response = dplyr::select(., tidyr::matches("^y[1-9]+")) %>%
                        rowMeans(na.rm = TRUE),
                      Deviation = Response - x)
      tmp <- tibble::tibble(sd = delta,
                            alpha = suppressWarnings(
                              suppressMessages(
                                psych::alpha(
                                  dplyr::select(df, tidyr::matches("^y[1-9]+")))$total$raw_alpha,
                                classes = "message"),
                              classes = "warning"))
      t <- dplyr::bind_rows(t,tmp)
    }
  }
  return(t)
}