#' Function to read in MSSIC data, recode and converto SAS7bdat
#'
#'
#' e
#' @param date date in %m-%d-%Y format
#' @param fp filepath to save the sas files
#' @param all whether or not it should return all 3 (uncoded) datasets or just the merged dataset, default just the merged
#'
#' @export


mssicRtoSAS <- function(date, fp, all = F, levels = F) {

  dat = read.mssic.data(date)
  dat.r = recode.mssic(dat$full_dat)
  haven::write_xpt(dat.r, paste0(fp, ".xpt"))

  if(all){
    haven::write_xpt(dat$ab, paste0(fp,"_ab.xpt"))
    haven::write_xpt(dat$ie, paste0(fp,"_ie.xpt"))
    haven::write_xpt(dat$proms, paste0(fp,"_proms.xpt"))
  }

}
