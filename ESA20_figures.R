
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
x$Site_treat <- paste(x$Site, x$Treatment, sep = "-")

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

## here is some mumbo jumbo i tried that didn't work for pairwise comparisons
{
# library(car)
# fit <- lm(DOC ~ Treatment*Site, data=x)
# Anova(fit, type = "II")
# fit
# summary(fit)
# TukeyHSD(fit)
# 
# 
# library(lsmeans)
# 
# marginal = lsmeans(fit,
#                    pairwise ~ Treatment:Site,
#                    adjust="tukey")           ### Tukey-adjusted comparisons
# 
# marginal$contrasts
# 
# library(emmeans)
# marginal <- emmeans(fit, ~ Treatment*Site)  
# pairs(marginal)
# 
# library("ggplot2")
# 
# pwpp(marginal, method = "pairwise")
# 
# pwpp(marginal, by = "Treatment", type = "Site")
# 
# multcomp::cld(marginal,
#     alpha=0.05,
#     Letters=letters,      ### Use lower-case letters for .group
#     adjust="tukey") 
  }

library(multcomp)
x$Site_treat <- as.factor(x$Site_treat)
aov <- aov(DOC~Site_treat, x)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)
summary(tukey)


kruskal.test(DOC ~ Site_treat, data = x)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(x$DOC, x$Site_treat,
                     p.adjust.method = "BH")

### significance codes are:
### A  A  A A B B


response <- x$Cmin_s
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "Respiration (ug C-CO2 /g dry soil/hour)", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)


aov <- aov(Cmin_s~Site_treat, x)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)

kruskal.test(Cmin_s ~ Site_treat, data = x)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(x$Cmin_s, x$Site_treat,
                     p.adjust.method = "BH")



## significance codes are:
### A  A  A A B B





response <- x$Cmin_c
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "Respiration (ug C-CO2 /g C/hour)", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)


aov <- aov(Cmin_c~Site_treat, x)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)

kruskal.test(Cmin_c ~ Site_treat, data = x)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(x$Cmin_s, x$Site_treat,
                     p.adjust.method = "BH")



response <- x$LOI
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "LOI (%))", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)


aov <- aov(LOI~Site_treat, x)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)

kruskal.test(LOI ~ Site_treat, data = x)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(x$Cmin_s, x$Site_treat,
                     p.adjust.method = "BH")





###### PLANTS



data <- read.csv("TL_tree_DBH.csv", header = T)
names(data) <- c("Site", "Plot", "Treatment", "Species", "Tag", "D15", "D16", "D17", "D18", "D19")
data <- data[which(data$Treatment == "S" | data$Treatment == "C"),]
data$Site_treat <- paste(x$Site, x$Treatment, sep = "-")
data$Site_treat <- as.factor(data$Site_treat)

data$growth <- data$D19 - data$D15
boxplot(growth ~ Treatment*Site, data = data, border = col, col = col.fill, 
        ylab = "Tree growth (DBH cm)", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)

aov <- aov(growth~Site_treat, data)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)

kruskal.test(growth ~ Site_treat, data = data)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(data$growth, data$Site_treat,
                     p.adjust.method = "BH")




response <- x$Roots
boxplot(response ~ Treatment*Site, data = x, border = col, col = col.fill, 
        ylab = "Root biomass (g)", 
        xlab = NULL, xaxt = 'n')
abline(v=2.5)
abline(v=4.5)

aov <- aov(Roots~Site_treat, x)
tukey <- glht(aov, linfct=mcp(Site_treat="Tukey"))
cld(tukey)

kruskal.test(Roots ~ Site_treat, data = x)  ## p-value is greater than 0.05, no sig diff between groups
pairwise.wilcox.test(x$Roots, x$Site_treat,
                     p.adjust.method = "BH")



##### edphic factors pca


c <- x[which(x$Treatment == "Control"),]
s <- x[which(x$Treatment == "Salt"),]
col <- c("red", "black", "purple", "black", "blue") 

par(mfrow = c(2,2), mar = c(5,5,3,3))


hist(c$pH, col = "#9400D399", breaks = 10)

hist(c$pH, col = "purple")

d <- density(c$pH)
d1 <- density(c$pH[which(c$Site == 1)])
plot(d1, main="pH", ylim = c(0,1.7), xlim = c(3,6.5), xlab = "pH")
polygon(d1, col="#FF000099", border="red")
d3 <- density(c$pH[which(c$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(c$pH[which(c$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")



# d <- density(c$Cl)
# #plot(d1)
# d1 <- density(c$Cl[which(c$Site == 1)])
# plot(d1, main="Chloride", ylim = c(0,2), xlim = c(-2,8), xlab = "Chloride")
# polygon(d1, col="#FF000099", border="red")
# d3 <- density(c$Cl[which(c$Site == 3)])
# polygon(d3, col="#9400D399", border="purple")
# d5 <- density(c$Cl[which(c$Site == 5)])
# polygon(d5, col="#0000FF99", border="blue")
# 

# 
# d <- density(c$Na)
# #plot(d1)
# d1 <- density(c$Na[which(c$Site == 1)])
# plot(d1, main="Sodium", ylim = c(0,1.3), xlim = c(0,9), xlab = "Sodium")
# polygon(d1, col="#FF000099", border="red")
# d3 <- density(c$Na[which(c$Site == 3)])
# polygon(d3, col="#9400D399", border="purple")
# d5 <- density(c$Na[which(c$Site == 5)])
# polygon(d5, col="#0000FF99", border="blue")





# #
# d <- density(c$DOC)
# #plot(d)
# d1 <- density(c$DOC[which(c$Site == 1)])
# plot(d1, main="DOC", ylim = c(0,0.25), xlim = c(-1,45), xlab = "DOC")
# polygon(d1, col="#FF000099", border="red")
# d3 <- density(c$DOC[which(c$Site == 3)])
# polygon(d3, col="#9400D399", border="purple")
# d5 <- density(c$DOC[which(c$Site == 5)])
# polygon(d5, col="#0000FF99", border="blue")
# 


d <- density(c$LOI)
#plot(d)
d1 <- density(c$LOI[which(c$Site == 1)])
plot(d1, main="Soil Organic Matter", ylim = c(0,1), xlim = c(5,14), xlab = "Soil Orgnaic Matter (%)")
polygon(d1, col="#FF000099", border="red")
d3 <- density(c$LOI[which(c$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(c$LOI[which(c$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")

d <- density(c$SO4)
#plot(d1)
d1 <- density(c$SO4[which(c$Site == 1)])
plot(d1, main="Sulfate", ylim = c(0,1), xlim = c(-1,6), xlab = "Sulfate (mg/L)")
polygon(d1, col="#FF000099", border="red")
d3 <- density(c$SO4[which(c$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(c$SO4[which(c$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")

# d <- density(c$Ca)
# #plot(d)
# d1 <- density(c$Ca[which(c$Site == 1)])
# plot(d1, main="Calcium", ylim = c(0,1.7), xlim = c(0,6), xlab = "Calcium")
# polygon(d1, col="#FF000099", border="red")
# d3 <- density(c$Ca[which(c$Site == 3)])
# polygon(d3, col="#9400D399", border="purple")
# d5 <- density(c$Ca[which(c$Site == 5)])
# polygon(d5, col="#0000FF99", border="blue")



#plot(d)
d1 <- density(c$BD[which(c$Site == 1)])
plot(d1, main="Bulk Density", ylim = c(0,5), xlim = c(0.5,1.7), xlab = "Bulk Density (g/cm3)")
polygon(d1, col="#FF000099", border="red")
d3 <- density(c$BD[which(c$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(c$BD[which(c$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")







plot(c$Cl, c$SO4, col = col[c$Site], pch = 16, cex = 1.5, xlab = "Chloride", ylab = "Sulfate")
plot(c$Ca, c$pH, col = col[c$Site], pch = 16, cex = 1.5, xlab = "Calcium", ylab = "pH")

plot(c$LOI, c$Phenol,col = col[c$Site], pch = 16, cex = 1.5, xlab = "LOI", ylab = "Phenol")
plot(c$pH, c$DOC,col = col[c$Site], pch = 16, cex = 1.5, xlab = "pH", ylab = "DOC")
plot(c$pH, c$LOI,col = col[c$Site], pch = 16, cex = 1.5, xlab = "pH", ylab = "LOI")

plot(c$TCC, c$SO4, col = col[c$Site], cex = 2)
plot(c$Ca, c$SM, col = col[c$Site], cex = 2)

plot(c$LOI, c$DOC, col = col[c$Site], cex = 2)


plot(s$LOI, s$DOC, col = col[s$Site], cex = 2, pch = 16, xlim = c(6, 20), ylim = c(10, 31))
points(c$LOI, c$DOC, col = col[c$Site], cex = 2, add = TRUE)

plot(c$Cl, c$Phenol/c$DOC, pch = 16, col = col[c$Site], cex = 2)

head(c)



plot(x$LOI, x$Cmin_s, col = col[x$Site], pch = 21,  bg = ifelse(x$Treatment == "Control", "white", col[x$Site]))
plot(x$DOC, x$Cmin_s,  col = col[x$Site], pch = 21,  bg = ifelse(x$Treatment == "Control", "white", col[x$Site]))




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


