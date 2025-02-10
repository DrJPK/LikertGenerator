## code to prepare `_boys_girls_attitudes` dataset goes here

girls_attitudes <- generateData(n=125, 
                                scalewidth = 7, 
                                mu = 2.9,  
                                rho = 1.8, 
                                effect = 0.8, 
                                trim = FALSE,
                                seed = 1234, 
                                return = "all")

boys_attitudes <- generateData(n=125, 
                               scalewidth = 7, 
                               mu = 3.4,  
                               rho = 1.4, 
                               effect = 0.5, 
                               trim = FALSE, 
                               seed = 9865, 
                               return = "all")

girls_attitudes <- girls_attitudes %>%
  dplyr::mutate(id = as.factor(paste0("G_",id)),
                dplyr::across(dplyr::starts_with("x"), as.factor),
                Gender = as.factor("girl"))%>%
  dplyr::relocate(Gender, .after = Treatment)
  

boys_attitudes <- boys_attitudes %>%
  dplyr::mutate(id = as.factor(paste0("B_",id)),
                dplyr::across(dplyr::starts_with("x"), as.factor),
                Gender = as.factor("boy"))%>%
  dplyr::relocate(Gender, .after = Treatment)

boys_girls_attitudes <- bind_rows(boys_attitudes,girls_attitudes)

rm("boys_attitudes")
rm("girls_attitudes")

usethis::use_data(boys_girls_attitudes, overwrite = TRUE)
