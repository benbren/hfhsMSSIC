value.label.flag.missing = function(varname) factor(
  varname, levels = c(1,0,NA), labels = c("Yes","No","Missing"), exclude =""
)

value.label.flag = function(varname) factor(
  varname, levels = c(0,1), labels = c("No", "Yes")
)

add.missing.as.factor = function(dat,varname){
  u = unique(dat[[varname]])
  dat[[varname]] = factor(dat[[varname]], levels = c(u,NA), labels = c(as.character(u), "Missing"), exclude = "")
  dat
}

