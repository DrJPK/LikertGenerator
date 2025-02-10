#' Label Variables with Human Readable Names
#'
#' @param .data A data frame containing columns named x## e## or Response created by generateData()
#' @param scale One of **AG**reement, **AP**proval, or **TE**mporal
#' @param labels An optional vector of labels to apply to the factor levels.
#'
#' @return A data frame of the same form as the input with numbers replaced by labels
#' @export
#'
#' @examples
#' generateData(return = "raw")%>%
#'   label_variables()
#'   
#' generateData(return = "all", delta = 0.2, scalewidth = 3)%>%
#'   label_variables(scale = "TE") 
#'  
#' generateData(return = "all", scalewidth = 3, itemlength = 7)%>%
#'   label_variables(scale = "AP")   
#'   
#' generateData(return = 'raw')%>%
#'   label_variables(labels = c("a","b","c","d","e"))

label_variables <- function(.data, 
                            scale = "agreement", 
                            labels = NULL){
  ## Quick variable sanity check
  if(is.null(scale)){
    if(is.null(labels)){
      stop("A scale is not set and labels is null. I cannot continue")
    }
  }
  
  ## Manual labelling of variables, assume that user knows what they are doing
  if(!is.null(labels)){
    if("Response" %in% names(.data)){
      if(is.factor(.data$Response)){
        .data <- .data %>%
          dplyr::mutate(Response = factor(Response,
                                          levels = seq(1:nlevels(Response)),
                                          labels = labels
          ))
      }
    }
    .data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::matches("^(x|e)[1-9]+$"),
        function(x){
          factor(x,
                 levels = seq(1:nlevels(x)),
                 labels = labels)
        }
      ))
  }else{
    ## Automatic labeling of levels based on known scales  
    stepsTable <- tibble::tibble(length = c(3,4,5,6,7,9),
                            steps = list(c(3,5,7),
                                         c(3,4,6,7),
                                         c(2,3,5,7,8),
                                         c(2,3,4,6,7,8),
                                         c(2,3,4,5,6,7,8),
                                         c(1,2,3,4,5,6,7,8,9)
                                        )
    )
    
    if(!is.null(scale)){
      if(stringr::str_to_upper(stringr::str_sub(scale,1,2)) == "AG"){
        labels = c("Very Strongly Disagree",
                   "Strongly Disagree",
                   "Disagree",
                   "Somewhat Disagree",
                   "Neither Agree nor Disagree",
                   "Somewhat Agree",
                   "Agree",
                   "Strongly Agree",
                   "Very Strongly Agree")
      }else if(stringr::str_to_upper(stringr::str_sub(scale,1,2)) == "AP"){
        labels = c("Very Strongly Disapprove",
                   "Strongly Disapprove",
                   "Disapprove",
                   "Somewhat Disapprove",
                   "Undecided",
                   "Somewhat Approve",
                   "Approve",
                   "Strongly Approve",
                   "Very Strongly Approve")
      }else if(stringr::str_to_upper(stringr::str_sub(scale,1,2)) == "TE"){
        labels = c("Never",
                   "Almost Never",
                   "Rarely",
                   "Very Rarely",
                   "Sometimes",
                   "Occasionally",
                   "Often",
                   "Almost Always",
                   "Always")
      }else{
        stop("A suitable scale cannot be selected!!")
      }
    }
    
    if("Response" %in% names(.data)){
      if(is.factor(.data$Response)){
        .data <- .data %>%
          dplyr::mutate(Response = factor(Response,
                     levels = seq(1:nlevels(Response)),
                     labels = labels[unlist(
                       stepsTable%>%
                         dplyr::filter(length == nlevels(Response))%>%
                         dplyr::pull(steps))])
            )
      }
    }
    .data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::matches("^(x|e)[1-9]+$"),
        function(x){
          factor(x,
                 levels = seq(1:nlevels(x)),
                 labels = labels[unlist(
                   stepsTable%>%
                     dplyr::filter(length == nlevels(x))%>%
                                     dplyr::pull(steps))])
        }))
  }
}