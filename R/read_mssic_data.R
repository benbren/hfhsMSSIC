#' Function to read in MSSIC data just by giving a dat
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#'
#' @export


read.mssic.data <- function(date, all = T) {

  lapply(c("stringr", "lubridate", "dplyr", "readr"), require, character.only =  T)


  dt = mdy(date)
  m = month(dt)
  m.n = month(dt, label = T, abbr = F)
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
    no.dash,
    "/"
  )



  ab = readr::read_csv(paste0(fp, "/mssic_abstraction_form_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv")) #|> filter(abstraction_form_status == "complete")
  proms = readr::read_csv(paste0(fp, "/mssic_proms_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv"))
  ie = readr::read_csv(paste0(fp, "/mssic_ie_form_",
                                 y,
                                 "-",
                                 mo,
                                 "-",
                                 dy,
                                 "_p000.csv"))


full_dat = proms |> inner_join(ab, by = "Intervention_id", suffix = c(".proms", ".ab")) |>
  inner_join(ie, by = "Intervention_id", suffix = c(".ie", ".pab"))

if(all){ret = list(full_dat = full_dat, ab = ab, proms = proms, ie = ie)} else {ret = full_dat}

return(ret)

}
