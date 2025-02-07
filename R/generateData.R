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
  
  s = (4 * pnorm(sigma)) / (itemlength + 1) #This sets the width of the whole scale in terms of the number of standard deviations that should fit in the full scale width
  if (is.null(mu)) {
    mu <- (itemlength + 1) / 2 #If mu not set, then set to middle of scale.
  }
  
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
    dplyr::mutate(Treatment = identifiers[1])
  
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
  if(return == "summary"){
    if(is.null(delta)){
      d <- d %>%
        dplyr::select(id, Response)%>%
        dplyr::rename(!!(identifiers[1]) := Response)%>% ##ISSUE ON THIS LINE HERE
        tidyr::pivot_longer(cols = -id, names_to = "Case", values_to = "Response")
    }else{
      d <- d %>%
        dplyr::select(id, Response,`Response + Uncertainty`)%>%
        dplyr::rename("{identifiers[1]}" = Response, "{identifiers[1]} + Uncertainty" = `Response + Uncertainty`)%>%
        tidyr::pivot_longer(cols = -id, names_to = "Case", values_to = "Response")
    }
  }
  
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
      
      d <- d %>%
        dplyr::mutate(Treatment = identifiers[1+i])
      
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
      if(return == "summary"){
        if(is.null(delta)){
          tmp <- tmp %>%
            dplyr::select(id, !!as.name(paste0(identifiers[1+i])))%>%
            tidyr::pivot_longer(cols = -id, names_to = "Case", values_to = "Response")
        }else{
          tmp <- tmp %>%
            dplyr::select(id, !!as.name(paste0(identifiers[1+i])),!!as.name(paste0(identifiers[1+i]," + Uncertainty")))%>%
            tidyr::pivot_longer(cols = -id, names_to = "Case", values_to = "Response")
        }
      }
      d <- dplyr::bind_rows(d,tmp)
    }
    
  }
  return(d)
}