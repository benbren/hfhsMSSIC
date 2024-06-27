#' Code one binary variable yes / no
#'
#' @param dat the data you are using
#' @param varname the variable you want to recode
#' @param labs the label of the binary variable. defaults to yes/no
#'
#'
#' @export

code.binary = function(dat, varname, labs = c("No", "Yes")){
  if(length(labs) != 2){
    stop("labs argument needs to length two")
  }
  dat[[varname]] = as.factor(dat[[varname]], levels = c(0,1), labels = labs)
  dat
}
