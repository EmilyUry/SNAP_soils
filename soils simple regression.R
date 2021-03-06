
#' ---
#' title: "Simple Regression of Soil Core Measurments"
#' author: "Emily Ury"
#' date: "May 7, 2020"
#' output: github_document
#' ---
#'


#' Created this script to look at general correlation 
#' analyses between soil core data. May be used as a coarse
#' check for *autocorrelated data*. **Not a final product.**


## Plots all data from the soil core mastersheet


## file location
setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

## packages 
library("officer")
library("rvg")

## Colors

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium


data <- read.csv("2019_SNAP_master.csv", header = TRUE)



names(data) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")
data <- data[,-c(23, 24, 29)] ## remove ICNO3, ICPO4 and Br

data$PO4[is.na(data$PO4)] <- 1   ### set the below detection
data$Mg[is.na(data$Mg)] <- 0.001
# data[82, 23] <- 0.61375  
# data[82, 24] <- 5.11 
# data[91, 25] <- mean(data[92:95,25])
# data[91, 26] <- mean(data[92:95,26])

data$Roots[data$Roots == 0] <- 0.01 


ld<-log(data[,c(11,14:19, 22, 28, 29)])
colnames(ld)<-paste("log",colnames(ld),sep="")
data<-cbind(data,ld); rm(ld)

## set the below detection
data <- na.omit(data)   ## omits three rows with values missing for SIR

### correlation  ## with Na.omits (3 rows)

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  text(0.5, 0.5, txt, col = ifelse(r > 0.5, "red", ifelse(r < -0.5, "blue", "gray70")))
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, cex = 1, col =  col3[data$Site])
}
# Create the plots
pairs(data[,c(7:10, 12, 13, 27, 30:39, 23, 25)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

#################  ### with PO4 and NO3 log transformed


d0 <- data[which(data$Treatment == "Control"),]
upper.panel<-function(x, y){
  points(x,y, pch = 19, cex = 1, col =  col3[d0$Site])
}
pairs(d0[,c(7:10, 12, 13, 27, 30:39, 23, 25)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

#######

## control plots only
data <- read.csv("2019_SNAP_master.csv", header = TRUE)
data <- data[which(data$Treatment == "Control"),]
d.info <- data[,2:5]
data <- data[,6:32]  # just variables
data <- data[,-c(18, 19, 24)]  ## omit NO3, PO4 and BR from IC
names(data) <- c("Cond.", "B.D.", "S.M.", "LOI", "pH", "Roots", "DOC", "TDN", 
           "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4",
           "Cmin.s", "Cmin.c", "SIR.s", "SIR.c", "Phenol", "NO3", "PO4")
data$PO4[is.na(data$PO4)] <- 1   ### set the below detection
data$Mg[is.na(data$Mg)] <- 0.001  ## set the below detection
data <- na.omit(data)   ## omits three rows with values missing for SIR
data$logPO4 <- log10(data$PO4)
data$logNO3 <- log10(data$NO3)


# Create the corellation plots
pairs(data[,], 
      lower.panel = panel.cor,
      upper.panel = upper.panel, 
      cex.labels = 1.5)







