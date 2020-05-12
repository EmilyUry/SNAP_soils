

#' ---
#' title: "Tracer plots for anions/cations in soil cores"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'


#' Created this script to look at the balance of anions and cations
#' in the experimental plot soils, relative to the ratios being added
#' from the instant ocean salt mixture.
#'   
#' Data is from the *soils mastersheet* from **2019**
#' 
#' Info about instant ocean salt mixture is found in 
#' http://cmetton.free.fr/Divers/Aqua/35509243.pdf
#' 



## file location
setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium

data <- read.csv("2019_SNAP_master.csv", header = TRUE)








