notone = function(x){ # Makes the equivalent for ne in SAS code as R != returns NA if the value is NA but we want 2 == NA to be FALSE not NA
  isit = (x !=1 | is.na(x))
  return(isit)
}
