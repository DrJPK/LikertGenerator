#' Generate Likert Data
#' 
#' Generates a data set of simulated Likert data based on well-defined parameters. The centrality, and distribution of responses can be set for the generated data as well as the number of items in the scale and the number of response points for the Likert items. It is possible to also generate data sets for control and one or more intervention groups defined by an effect size, and to also generate data that includes a degree of additional uncertainty representing respondents misinterpretting the meaning of scale points.
#'
#' @param n The number of cases to generate for each group
#' @param itemlength How many choices should the Likert item have? Defaults to 5.
#' @param scalewidth How many items should be generated to represent the Likert scale? Defaults to 5
#' @param mu The intended mean response of the first group. If not set, then the centre of the item length is used.
#' @param delta If simulation of misunderstanding the scale choice points is desired, then this sets the standard deviation of the uncertainty in the data.
#' @param rho The standard deviation of the item responses **per** case. This corresponds to variability **within** each respondent's scores
#' @param sigma How many standard deviations should the full width of the Likert item choices really represent?
#' @param trim What should be done with out of range values. If TRUE then out of range values will be replaced with NA. If FALSE, then out of range values will be trimmed to the min of max item choices as appropriate.
#' @param identifiers A vector of strings corresponding to group identifiers. The first three letters will be manipulated to create case identifiers. The first value will always be used. subsequent values will only be used if effect is not NULL
#' @param effect A numeric vector corresponding to the intended effect sizes for each of the groups other than the first group. This should have length 1 less than identifiers.
#' @param seed optional seed for the random number generator
#' @param type Either "mean" or "sum" that describes the type of summary data returned. Note that if set to sum then trim will automatically be set to FALSE
#' @param return One of "all", "raw", "summary", "uncertainty" (first letter will work). This determines the nature of the returned dataframe.
#'
#' @return A data frame with structure controlled by the value of return
#' @export
#'
#' @examples
#' ##Create a simple dataframe of 5 cases
#' generateData(n=5)
#' 
#' # A tibble: 5 × 3
#'  id    Treatment Response
#'  <fct> <fct>        <dbl>
#'1 CON1  Control        2.8
#'2 CON2  Control        2.6
#'3 CON3  Control        3.4
#'4 CON4  Control        3.4
#'5 CON5  Control        2.6
#' 
#' ##Create a simple dataframe of 5 cases for each of boys and girls where the boys' responses are 1sd greater than the girls'.
#' generateData(n=5,effect=1,identifiers=c("girls","boys"))
#' 
#' # A tibble: 10 × 3
#'   id    Treatment Response
#'   <fct> <fct>        <dbl>
#' 1 GIR1  girls         2   
#' 2 GIR2  girls         2.75
#' 3 GIR3  girls         3.5 
#' 4 GIR4  girls         2.6 
#' 5 GIR5  girls         2.2 
#' 6 BOY1  boys          3.25
#' 7 BOY2  boys          3   
#' 8 BOY3  boys          4.4 
#' 9 BOY4  boys          3.6 
#'10 BOY5  boys          2.25
#'
#' ##Create a dataframe of 5 cases for each of humans and aliens where the aliens' responses are 1 sd lower than the humans and fill in any NAs with min and max values. Generate items corresponding to a 4 point Likert item and have 7 items in the scale
#' generateData(n=5,itemlength = 4, scalewidth = 7, effect = -1,identifiers=c("humans","aliens"), trim = FALSE, return = "raw")
#' 
#' # A tibble: 10 × 9
#'   id    Treatment    x1    x2    x3    x4    x5    x6    x7
#'   <fct> <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#' 1 HUM1  humans        1     1     1     3     3     2     1
#' 2 HUM2  humans        4     1     4     3     4     4     2
#' 3 HUM3  humans        1     3     2     2     3     2     3
#' 4 HUM4  humans        4     2     4     1     4     4     4
#' 5 HUM5  humans        4     2     2     2     4     4     3
#' 6 ALI1  aliens        4     3     3     4     2     2     3
#' 7 ALI2  aliens        4     1     1     2     1     2     1
#' 8 ALI3  aliens        1     2     1     1     1     3     3
#' 9 ALI4  aliens        1     2     1     2     3     1     2
#'10 ALI5  aliens        1     2     2     4     4     4     3
#'
#'
generateData <- function(
    n=10,
    itemlength = 5,
    scalewidth = 5,
    mu = NULL,
    delta = NULL,
    rho = 1.46,
    sigma = 2,
    trim = TRUE,
    identifiers = c("Control","Intervention"),
    effect = NULL,
    seed = NULL, 
    type = "mean", 
    return = "summary"){
  
  if (!is.null(seed)) {
    if (is.numeric(seed)) {
      set.seed(as.numeric(seed))
    } else{
      set.seed(42)
    }
  }
  
  if(type=="sum"){
    trim = FALSE
  }
  
  rtn <- stringr::str_to_lower(stringr::str_sub(return,1,1))
  
  if(is.null(delta) & rtn == "u"){
    warning("You cannot have a return value of uncertainty if delta is not set! ALL results will be returned")
    rtn <- "a"
  }
  
  s = (4 * pnorm(sigma)) / (itemlength + 1) #This sets the width of the whole scale in terms of the number of standard deviations that should fit in the full scale width
  if (is.null(mu)) {
    mu <- (itemlength + 1) / 2 #If mu not set, then set to middle of scale.
  }
  
  ##Make the Base Data Frame
  
  d <- generateDF(n = n, 
                   itemlength = itemlength, 
                   scalewidth = scalewidth, 
                   mu = mu,
                   delta = delta,
                   rho = rho,
                   sigma = s,
                   identifier = stringr::str_to_upper(stringr::str_pad(stringr::str_sub(identifiers[1],1,3), width = 3, pad = "_", side = "right")),
                   trim = trim)
  d <- d %>%
    dplyr::mutate(Treatment = as.factor(identifiers[1]))
  
  if(type == "sum"){
    d <- d %>%
      dplyr::mutate(Response = rowSums(dplyr::across(dplyr::matches("^x[1-9]+$")), na.rm = TRUE))
    if(!is.null(delta)){
      d<-d %>%
        dplyr::mutate(`Response + Uncertainty` = rowSums(dplyr::across(dplyr::matches("^e[1-9]+$")), na.rm = TRUE))
    }
  }else{
    d <- d %>%
      dplyr::mutate(Response = rowMeans(dplyr::across(dplyr::matches("^x[1-9]+$")), na.rm = TRUE))
    if(!is.null(delta)){
      d<-d %>%
        dplyr::mutate(`Response + Uncertainty` = rowMeans(dplyr::across(dplyr::matches("^e[1-9]+$")), na.rm = TRUE))
    }
  }

  ##Now repeat for each identifier
  
  if(!is.null(effect)){
    for(i in 1:length(effect)){
      tmp <- generateDF(n = n, 
                      itemlength = itemlength, 
                      scalewidth = scalewidth, 
                      mu = mu + effect[i],
                      delta = delta,
                      rho = rho,
                      sigma = s,
                      identifier = stringr::str_to_upper(stringr::str_pad(stringr::str_sub(identifiers[1+i],1,3), width = 3, pad = "_", side = "right")),
                      trim = trim)
      
      tmp <- tmp %>%
        dplyr::mutate(Treatment = as.factor(identifiers[1+i]))
      
      if(type == "sum"){
        tmp <- tmp %>%
          dplyr::mutate(Response = rowSums(dplyr::across(dplyr::matches("^x[1-9]+$")), na.rm = TRUE))
        if(!is.null(delta)){
          tmp<-tmp %>%
            dplyr::mutate(`Response + Uncertainty` = rowSums(dplyr::across(dplyr::matches("^e[1-9]+$")), na.rm = TRUE))
        }
      }else{
        tmp <- tmp %>%
          dplyr::mutate(Response = rowMeans(dplyr::across(dplyr::matches("^x[1-9]+$")), na.rm = TRUE))
        if(!is.null(delta)){
          tmp<-tmp %>%
            dplyr::mutate(`Response + Uncertainty` = rowMeans(dplyr::across(dplyr::matches("^e[1-9]+$")), na.rm = TRUE))
        }
      }
      
      d <- dplyr::bind_rows(d,tmp)
    }
    
  }
  
  ##FINALLY TIDY UP AND RETURN ONLY WHAT IS ASKED FOR
  
  if(rtn == "s"){
    if(is.null(delta)){
      tmp <- tmp %>%
        dplyr::select(id, Treatment, Response)
    }else{
      tmp <- tmp %>%
        dplyr::select(id, Treatment, Response, `Response + Uncertainty`)
    }
  }else if(rtn == "r"){
    d <- d %>%
      dplyr::select(id, Treatment, dplyr::matches("^x[1-9]+$"))
  }else if(rtn == "u"){
    d <- d %>%
      dplyr::select(id, Treatment, dplyr::matches("^e[1-9]+$"))
  }else{
    d <- d%>%
      dplyr::relocate(Treatment, .after = id)
  }
  
  return(d)
}