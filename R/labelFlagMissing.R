#' @export


value.label.flag.missing = function(varname) factor(
  varname, levels = c(1,0,NA), labels = c("Yes","No","Missing"), exclude =""
)
