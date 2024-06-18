#' @export

value.label.flag = function(varname) factor(
  varname, levels = c(0,1), labels = c("No", "Yes")
)
