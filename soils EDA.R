

#' ---
#' title: "Exploratory Data Analysis"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'


#' Exploratory data analysis (EDA) as suggested by the Stats Consulting Group at
#' Duke. The goal of these stats is to determine what controls soil respiration
#' including both site variation across the plots and treatment effects
#' of salt and nutrient additions. 
#'
#' The goal of EDA is to assess the data set without making any assumptions.
#' 
#' 1. PCA
#' 2. Heirarchical clustering
#' 

#' General notes:
#' 
#' Log transformations:
#' The variables Cl, SO4 and Na are non-negative, highly right-skewed and span
#' several orders of magnitude, so they may be log transformed (particularly if 
#' their relationship with the response appears non-linear and convex).
#' If the scatter plots with these variables and response looks conical, consider 
#' log transforming the response as well.
#' 
#' Use the top PCs in place of the individual variables. 
#'  
#' 


