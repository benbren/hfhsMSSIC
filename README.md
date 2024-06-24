# A package to support HFHS MSSIC analytics team using R 

Example workflow: 

`hfhsMSSIC::read.mssic.data("04-14-2024") |> ` Reads in data from the data download on April 14th 2024. 

`hfhsMSSIC::remove.sites() |>` Removes disabled sites  

`hfhsMSSIC::recode.mssic() |>` Recodes according to current standards 

`hfhsMSSIC::add.labels.after.recode()` Adds labels for visualization and tabulation 
