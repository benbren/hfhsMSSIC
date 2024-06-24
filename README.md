# A package to support HFHS MSSIC analytics team using R 

`
hfhsMSSIC::read.mssic.data("04-14-2024") |>
  hfhsMSSIC::remove.sites() |> 
  hfhsMSSIC::recode.mssic() |>
  hfhsMSSIC::add.labels.after.recode() -> dat
`
