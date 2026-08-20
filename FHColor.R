FHColor <- function(x){
  dplyr::case_when(x == "Pass" ~ "success", x == "Yellow" ~ 
                     "#EBC106", x == "Warning" ~ "#d65d26", x == "Fail" ~ "danger", 
                   TRUE ~ NA_character_)
}