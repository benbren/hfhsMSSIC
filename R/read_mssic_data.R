#' Function to read in MSSIC data just by giving a date
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#' @param all logical value indicating whether you want an object with the full dataset, the abstraction form, ie form and proms form
#' or just the full dataset. defaults to TRUE
#' @param filt logical whether you want the abstraction form and inclusion/exclusion form filters. defaults to TRUE
#'
#' @export


read.mssic.data <- function(date, all = T, filt = T) {

  lapply(c("stringr", "lubridate", "dplyr", "readr"), require, character.only =  T)


  dt = lubridate::mdy(date)
  m = lubridate::month(dt)
  m.n = lubridate::month(dt, label = T, abbr = F)
  no.dash = paste0(stringr::str_split(date, "-")[[1]], collapse = "")
  dy = format(dt, "%d")
  mo = format(dt, "%m")
  y = format(dt, "%Y")

  fp = paste0(
    "S:/Main Campus/MSSIC PatientIQ/Data Archive DO NOT EDIT/Data Downloads ",
    y,
    "/",
    m.n,
    "/",
    no.dash
  )



  ab = read.csv(paste0(fp, "/mssic_abstraction_form_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv")) #|> filter(abstraction_form_status == "complete")
  proms = read.csv(paste0(fp, "/mssic_proms_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv"))
  ie = read.csv(paste0(fp, "/mssic_ie_form_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv"))


full_dat = proms |> inner_join(ab, by = "Intervention_id", suffix = c(".proms", ".ab")) |>
  inner_join(ie, by = "Intervention_id", suffix = c(".ie", ".pab"))

if(filt){
  full_dat = full_dat %>%
    filter(abstraction_form_status == "complete" & sampling_status == 1 & inclusion_exclusion_e == 1)
}

if(all){ret = list(full_dat = full_dat, ab = ab, proms = proms, ie = ie)} else {ret = full_dat}

return(ret)

}
