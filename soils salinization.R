#' ---
#' title: "Three axes of salinization"
#' author: "Emily Ury"
#' date: "May 7, 2020"
#' output: github_document
#' ---
#'
#' Data is from the *soils mastersheet* from **2019**
#' 
#' Info about instant ocean salt mixture is found in 
#' http://cmetton.free.fr/Divers/Aqua/35509243.pdf
#' 

library(plyr)


setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

data <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(data) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
                 "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
                 "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
                 "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")

element <- c("Cl", "SO4", "Na", "K", "Mg", "Ca", "P:PO4", "N:NO3", "N:NH4")
IO <- c(521, 23, 462, 9.4, 52, 9.4, 0.05, 1, 10.2)

#' How much salt have we added to the plots?
#' We have done 19 salt additions to date (however only 16 of these were prior to 
#' the 2019 soil sampling effort.
#' 
#' Each addition delivers 30 pounds of salt to a treatment plot (200 m2)

16*30/200   # additions * pounds / area

#' So far we have added 2.4 pounds of salt per square meter
#' 

BD <- data$BD
mean(BD)
sd(BD)

table(data$Treatment, data$Site, mean(data$BD))
table(data$Site, data$BD)


ddply(data, .(Site, Treatment), summarize,  BD=mean(BD))


cdata <- ddply(data, c( "Depth"), summarise,
               N    = length(BD),
               mean = mean(BD),
               sd   = sd(BD),
               se   = sd / sqrt(N)
)
cdata







