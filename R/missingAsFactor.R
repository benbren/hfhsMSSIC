add.missing.as.factor = function(dat,varname){
  u = unique(dat[[varname]])
  dat[[varname]] = factor(dat[[varname]], levels = c(u,NA), labels = c(as.character(u), "Missing"), exclude = "")
  dat
}
