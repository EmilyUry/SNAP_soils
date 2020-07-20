
### SNAP Figures for ESA2020

## soil c 
## DOC and CMIN

## veg
### plant growth and root biomass



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
x <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(x) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")
x <- x[,-c(23, 24, 29)] ## remove ICNO3, ICPO4 and Br

lX<-log(x[,c(14:19, 22, 28, 29)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")

## for now, lets ignore the nutrient and sn treatments

x <- x[which(x$Treatment == "Salt" | x$Treatment == "Control"), ]
x$Treatment <- as.factor(x$Treatment)

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium

par(mfrow = c(1,1), mar = c(4,4,3,3))
boxplot(DOC ~ Treatment*Site, data = x)
plot(x$logSO4, x$DOC, pch = c(21,22)[x$Treatment], col = col3[x$Site], bg = ifelse(x$Depth == "(5-10)", "white", col3[x$Site]))

x <- x[which(x$Depth == "(0-5)"),]




col <- c("red", "red", "purple", "purple", "blue", "blue") 
col.fill <- c("white", "#d6604d80", "white", "#9970AB80", "white", "#4393C380") 


par(mfrow = c(1,2))
response <- x$DOC
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "DOC (mg/L)", xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)

response <- x$Cmin_s
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "Respiration (ug C-CO2 / g dry soil)", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)


###### PLANTS



data <- read.csv("TL_tree_DBH.csv", header = T)
names(data) <- c("Site", "Plot", "Treatment", "Species", "Tag", "D15", "D16", "D17", "D18", "D19")
data <- data[which(data$Treatment == "S" | data$Treatment == "C"),]

data$growth <- data$D19 - data$D15
boxplot(growth ~ Treatment*Site, data = data, border = col, col = col.fill, 
        ylab = "Tree growth (DBH cm)", 
        xlab = NULL)
abline(v=2.5)
abline(v=4.5)




response <- x$Roots
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "Root biomass (g)", 
        xlab = NULL)
abline(v=2.5)
abline(v=4.5)





