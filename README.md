# avmUncertainty

&nbsp; 

This repository holds the code and access to the data necessary to reproduce the results in the **Uncertainty in Automated Valuation Models** paper.

All code and data is in the R language.

&nbsp; 

### Getting started

&nbsp; 

Begin by installing the following two packages from github

* avmUncertainty

`devtools::install_github('anonymousreauthor/avmUncertainty')`

* kingCoData

`devtools::install_github('anonymousreauthor/kingCoData')`

&nbsp; 

#### Walkthough

&nbsp; 

Then, navigate to `scripts/sandbox.R`. This file provides a quick check if the necessary packages are present on your system.  You can execute all code here, it will run quickly.  This also gives an brief look at what general analysis is being done in this paper.

&nbsp; 

### Reproduction

&nbsp; 

Open this project in RStudio. Note that you can reproduce without the RStudio IDE, you'll just have to update your working directories to match.  

&nbsp; 

1. Reproducing this work begin by re-running the experiments. Use **1_run_experiments.R** to reproduce our experiments exactly.  You can change the location of the output directory on line 19.

**NOTE: This will take multiple hours to reproduce**.  You may want to do it in chunks.  

&nbsp; 

2. Next, summarize the results with the **2_summarize_results.R** script.  You may want to restart R and this project after #1 or fix your working directories to match.  

&nbsp; 

3. Then, run (or knit) **3_create_tables_figures.rmd** to generate the tables and figures in the paper. 

&nbsp; 

4. Finally, run **4_create_map.R** to generate the sale location map.

&nbsp; 

#### Docker Version

Additionally, there is a Dockerfile in the /docker directory that will allow you to perfectly reproduce this work with a Docker Image.  

&nbsp; 

#### Problems?

&nbsp; 

Log an issue at www.github.com/anonymousreauthor/avmUncertainty.



