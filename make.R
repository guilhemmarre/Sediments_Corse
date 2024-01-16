#' Sediments_Corse: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Guilhem Marre \email{guilhem.marre@andromede-ocean.com}
#' 
#' @date 2024/01/08



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())

source("R/degmindec2degdec.R")

source("R/function_folk.R")

source("R/function_shepard.R")


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

source("analyses/1_Nettoyage_donnees.R")

rmarkdown::render("analyses/2_Analyses.Rmd", output_file = "2_Analyses.html")

