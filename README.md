# R code for the project Eligibility PARAGON KaRen

The aim of this R code is to be transparant and document the data handling performed for the project Eligibility PARAGON in KaRen. 

## Language 

English (sprinkled with Swedish). 

## Data

The data consists of European individual patient data and is not public, and therefore no data is stored in this repository. 

## Instructions

The project uses the R package projectTemplate, http://projecttemplate.net/ and is structured (and run) accordingly. Renv, https://rstudio.github.io/renv/articles/renv.html, is used for management of package dependencies.

Since the data is not available the code can not be run as is. 

Workflow: 
1. Run src/load_munge_cache.R or set loading and munging options in ProjectTemplate::reload.project() to TRUE in Statistical_report_PARAGONKaRen.Rmd
2. Run Statistical_report_PARAGONKaRen.Rmd

## Publication

https://onlinelibrary.wiley.com/doi/10.1002/ehf2.13705