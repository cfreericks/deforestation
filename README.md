# Global patterns of deforestation

| :warning: WARNING: This repository is deprecated and not maintained! |
| --- |

## Background
In my 2014 master's thesis I adressed the issue of global deforestation ("Ursachen globaler Entwaldung – eine empirische Untersuchung unter Nutzung multivariater Analysemethoden"). I performed a factor-cluster analysis to identify patterns that drive global deforestation. The data is based on various FAO statistics which are publicly available, but are also included in the repository `data\raw`.

## How to use it?
The code is written in `R`. As I produced it in my early `R` beginner times, it is certainly not the most appealing code and could use some major refactoring. Anyhow, it still works (last tested: February 2021). 

### Where to start?
You can simply perform the analysis by running the script `run_analysis.R` and it will call all relevant functions. 
```R
# This master script is responsible for launching and orchestrating the
# functions defined in the src folder

# Load libraries ----------------------------------------------------------

library(here)

# Load scripts ------------------------------------------------------------

# "01_data_engineering.R", 
# "02_factoranalysis.R", 
# "03_clusterBeschreiben.R", 
# "04_regressionanalysis.R"

path = here("src/")
pathnames <- list.files(pattern = "[.]R$", path = path, full.names = TRUE)

sapply(pathnames, FUN = source)
```
Of course, it is also possible to run the analysis step-by-step and execute the scripts manually. You then just have to do it in the right order: `01_data_engineering.R`, `02_factoranalysis.R`, `03_clusterBeschreiben.R` and last `04_regressionanalysis.R`. 

### Structure

```
├───config
├───data
│   ├───interim
│   ├───processed
│   └───raw
├───docs
├───output
│   ├───data
│   ├───plots
│   └───tex
├───rmd
└───src
    ├───dataeng
    └───plot
```
#### data
In `data` you will find the `raw`, `interim` and `processed` data. `raw` takes only the unaltered original statistics retrieved mainly from FAO. `interim` is used as a temporary storage for intermediate calculation. `processed` holds the cleaned and prepared dataset which is used for further analysis. 

#### src
`src` contains all `R` scripts with subfolders for those dedicated to data engineering (`dataeng`) and plotting (`plot`).

#### output
Results of the analysis are stored in `output`. This can be .csv files (`data`), graphics (`plots`) or LaTeX snippets like tables (`tex`).

#### rmd and docs
R notebooks with some additional tests and plots to explore the data can be found in `rmd`. The knit result is stored as html file in `docs`. 
