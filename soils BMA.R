


#' ---
#' title: "Baysian Model Averaging"
#' author: "Emily Ury"
#' date: "May 13, 2020"
#' output: github_document
#' ---
#'

#' *Notes:* this is the recommended approach for model selection
#' to identify which variables to include in the final regression
#' model. This approach fits and summarizes all possible multiple
#' regression models and reports the importance of each variable. 
#' 
#' Use the `bas.lm` function in the BAS package. 
#' 
#' Prior inclusion of 1 is set to all design variables (Site, Treatment)
#' so they will be included in all models (*include.always*).
#' 
#' Summary output provides top five models. USe this to decided what
#' to include in the final model (high marginal posterior inclusion
#' probabilities). The `image()` function provides a graphical sumamry. 
#' 
#' See Ref for more info: https://cran.r-project.org/web/packages/BAS/vignettes/BAS-vignette.html 
#' 
#' 