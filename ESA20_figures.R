
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



##### edphic factors pca


c <- x[which(x$Treatment == "Control"),]

col <- c("red", "black", "purple", "black", "blue") 

par(mfrow = c(1,2), mar = c(5,5,3,3))

plot(c$Cl, c$SO4, col = col[c$Site], pch = 16, cex = 1.5, xlab = "Chloride", ylab = "Sulfate")
plot(c$Ca, c$pH, col = col[c$Site], pch = 16, cex = 1.5, xlab = "Calcium", ylab = "pH")




plot(c$TCC, c$SO4, col = col[c$Site], cex = 2)
plot(c$Ca, c$SM, col = col[c$Site], cex = 2)
plot(c$LOI, c$DOC, col = col[c$Site], cex = 2)
plot(c$Cl, c$Phenol/c$DOC, pch = 16, col = col[c$Site], cex = 2)

head(c)


pc <- c[, c(7,8,9,10,12,13,14,15,16,17,18,19,21,28,29)]
pc[is.na(pc)] <- 2
site <- c[, 2]


par(mfrow = c(1,1), mar = c(4,8,4,5))

#' Create a PCA for chemical characterisitcs of all soil cores
#' 
pca <-prcomp(pc, center = TRUE)
print(pca)
plot(pca)


summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(site, pca.scores[,1:3])
df <- cbind(df, resp)

#' Plot the pca

plot(df[,2], df[,3], pch = 16, cex = 1.5, col = col[site], 
     main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*7, pca.loading[,2]*7, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*7.5, pca.loading[,2]*7.3, row.names(pca.loading), cex = 0.8, col = "red")








pc <- c[, c(10,12,13,14,15,19, 29)]
pc[is.na(pc)] <- 2
site <- c[, 2]


par(mfrow = c(1,1), mar = c(5,12,4,8))

#' Create a PCA for chemical characterisitcs of all soil cores
#' 
pca <-prcomp(pc, center = TRUE)
print(pca)
plot(pca)


summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(site, pca.scores[,1:3])


#' Plot the pca

plot(df[,2], df[,3], pch = 16, cex = 1.5, col = col[site], 
     main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*6, pca.loading[,2]*6, length = 0.1, lwd = 1.5, col = "gray50")
text(pca.loading[,1]*9, pca.loading[,2]*9, row.names(pca.loading), cex = 0.8, col = "gray50")


