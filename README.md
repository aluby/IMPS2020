# Luby -- IMPS2020

This repo contains the code for fitting the IRT/IRTree models and replicating the graphs in my IMPS 2020 Spotlight talk "Psychometrics for Forensic Decision-Making"

+ `fit-stan-models.R` contains the code used to format the data for Stan, fit the models, and save the output to RDS files
+ `analysis-and-graphs.R` contains the R code used to create the graphs, tables, etc. used in the presentation
+ Stan code for the Rasch and IRTree model can be found in the `stan-files` directory
+ .png images are located in the `figures` directory


*Note:* I used my R package [blackboxstudyR](https://www.github.com/aluby/blackboxstudyR) to access the FBI "Black Box" study data, but you can also download the .txt file directly from the [FBI's website](https://www.fbi.gov/services/laboratory/scientific-analysis/research-and-support/black-box-study-results). (`TestResponses` data table)
