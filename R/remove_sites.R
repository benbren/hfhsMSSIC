#' Remove records from disabled sites
#'
#' @param additional.sites sites not listed as of 06/2024. Defaults to NULL

#'
#' @export
#'

remove.sites = function(dat,additional.sites = NULL) {
  require("dplyr")
  if(!require("dplyr")){
    install.packages("dplyr")
    library(dplyr)
    }
  remove = c(519, #  Covenant Medical Center
             523, # Ascension Genesys Hospital
             524, # Beaumont - Grosse Pointe
             521, # DMC Harper University Hospital
             525, # DMC Detroit
             additional.sites)
  dat <- dat |> filter(!(siteid %in% remove))
  dat

}


