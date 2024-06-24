#' Function to read in MSSIC data just by giving a dat
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#'
#' @export


read.mssic.data <- function(date) {

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
                                 "_p000.csv"))
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

full_dat = proms |> left_join(ab, by = "Intervention_id", suffix = c("", ".y")) |> select(-ends_with(".y")) |>
  left_join(ie, by = "Intervention_id", suffix = c("", ".y")) |> select(-ends_with(".y"))

full_dat

}
