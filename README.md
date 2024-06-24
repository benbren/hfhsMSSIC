# A package to support HFHS MSSIC analytics team using R 

Example workflow: 

`library(hfhsMMSIC)`

`read.mssic.data("04-14-2024") |> ` Reads in data from the data download on April 14th 2024. 

`remove.sites() |>` Removes disabled sites  

`recode.mssic() |>` Recodes according to current standards 

`add.labels.after.recode()` Adds labels for visualization and tabulation 
