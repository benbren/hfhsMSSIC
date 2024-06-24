# A package to support HFHS MSSIC analytics team using R 

How to download: 

`devtools::install_github("benbren/hfhsMSSIC")`

If the the package has already been downloaded, you should run `detach("package:hfhsMSSIC", unload = T)` and then re-download using the above so that you get the most recent version of the package!  

Example workflow: 

`library(hfhsMMSIC)`

`read.mssic.data("04-14-2024") |> ` Reads in data from the data download on April 14th 2024. 

`remove.sites() |>` Removes disabled sites  

`recode.mssic() |>` Recodes according to current standards 

`add.labels.after.recode()` Adds labels for visualization and tabulation 
