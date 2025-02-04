#' Function to read in MSSIC data, recode and converto SAS7bdat
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#' @param fp filepath to save the sas files
#' @param all whether or not it should return all 3 (uncoded) datasets or just the merged dataset, default just the merged
#'
#' @export


mssicRtoSAS <- function(date, fp, all = F) {

  dat = read.mssic.data(date)
  dat.r = recode.mssic(dat$full_dat)
  haven::write_sas(dat.r, paste0(fp))

  if(all){
    haven::write_sas(dat$ab, paste0(fp,"_ab"))
    haven::write_sas(dat$ie, paste0(fp,"_ie"))
    haven::write_sas(dat$proms, paste0(fp,"_proms"))
  }

}
