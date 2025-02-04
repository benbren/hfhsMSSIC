#' Function to read in MSSIC data, recode and converto xlsx
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#' @param fp filepath to save the sas files
#' @param all whether or not it should return all 3 (uncoded) datasets or just the merged dataset, default just the merged
#'
#' @export


mssicToXLSX <- function(date, fp, all = F, levels = F) {

  dat = read.mssic.data(date)
  dat.r = recode.mssic(dat$full_dat)
  readxl::write_excel(dat.r, paste0(fp, ".xlsx"))

  if(all){
    readxl::write_excel(dat$ab, paste0(fp,"_ab.xlsx"))
    readxl::write_excel(dat$ie, paste0(fp,"_ie.xlsx"))
    readxl::write_excel(dat$proms, paste0(fp,"_proms.xlsx"))
  }

}
