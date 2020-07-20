

#' ---
#' title: "SIR + salt talk figure"
#' author: "Emily Ury"
#' date: "July 20, 2020"
#' output: github_document
#' ---
#'



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

x <-  read.csv("2019_SIR_salt_w_eplainers.csv")
head(x)



Response <- x$µg_C_CO2_hr_gC
Response <- x$µg_C_CO2_hr_gds

x$Treatment <-replace(x$Treatment, c(1:30), "a_zero")
x$Treatment <-replace(x$Treatment, c(31:60), "b_two")
x$Treatment <- replace(x$Treatment, c(61:90), "c_six")


par(mar = c(7,5,2,2))
boxplot(Response ~ Treatment*Plot*Site, data = x, las = 2, 
        col = c("#f4a582","#f4a582","#f4a582", "#d6604d", "#d6604d","#d6604d",
                "#C2A5CF", "#C2A5CF", "#C2A5CF", "#9970AB","#9970AB","#9970AB",
                "#92C5DE","#92C5DE","#92C5DE",  "#4393C3",  "#4393C3",  "#4393C3" ))



x <-  read.csv("2019_SIR_salt_w_eplainers.csv")
x$Respiration <- x$µg_C_CO2_hr_gds

x$Treatment <-replace(x$Treatment, c(1:30), 0)
x$Treatment <-replace(x$Treatment, c(31:60), 2)
x$Treatment <- replace(x$Treatment, c(61:90), 6)




### summarize the data
library(dplyr)

## omit rows with missing response var
x <- x[-83,]
x <- x[-82,]
x <- x[-80,]
x <- x[-53,]
x <- x[-52,]
x <- x[-50,]
x <- x[-23,]
x <- x[-22,]
x <- x[-20,]

x %>%
  group_by(Site, Plot, Treatment) %>%
  summarize(n())

summary <- x %>%
  group_by(Site, Plot, Treatment) %>%
  summarize(mean_resp = mean(Respiration), sd_resp = sd(Respiration))


Salt <- summary[which(summary$Plot == "Salt"),]
Control <- summary[which(summary$Plot == "Control"),]


col <- c("#d6604d", "#000000", "#9970AB", "#000000",  "#4393c3")
treatment <- c(0, 2, 6, 0, 2, 6, 0, 2, 6)



### figure here

{
par(mfrow = c(1,2), mar = c(5,5,2,1))
plot(Control$Treatment, Control$mean_resp, 
     xlab = "Salinity Treatment (ppt)",
     ylab = "Respiration (ug C-CO2/g dry soil)", 
     main = "Control Plot", 
     pch = 15,
     col = col[Control$Site],
     cex = 1.5, 
     ylim = c(3, 8.5))


arrows(treatment, (Control$mean_resp+Control$sd_resp/1), treatment,
       (Control$mean_resp-Control$sd_resp/1), length = 0.05, angle = 90, code = 3, 
       col = "gray60")

treatment <- c(0,2,6)
C1 <- Control[which(Control$Site == 1),]
abline(lm(C1$mean_resp ~ treatment), lwd = 2, col = "red")

C3 <- Control[which(Control$Site == 3),]
abline(lm(C3$mean_resp ~ treatment), lwd = 2, col = "purple")

C5 <- Control[which(Control$Site == 5),]
abline(lm(C5$mean_resp ~ treatment), lwd = 2, col = "blue")



plot(Salt$Treatment, Salt$mean_resp, 
     xlab = "Salinity Treatment (ppt)",
     ylab = " ", 
     main = "Salt Plot", 
     pch = 15,
     col = col[Control$Site],
     cex = 1.5, 
     ylim = c(3, 8.5))

arrows(treatment, (Salt$mean_resp+Salt$sd_resp/1), treatment,
       (Salt$mean_resp-Salt$sd_resp/1), length = 0.05, angle = 90, code = 3, 
       col = "gray60")

S1 <- Salt[which(Salt$Site == 1),]
abline(lm(S1$mean_resp ~ treatment), lwd = 2, col = "red")

S3 <- Salt[which(Salt$Site == 3),]
abline(lm(S3$mean_resp ~ treatment), lwd = 2, col = "purple")

S5 <- Salt[which(Salt$Site == 5),]
abline(lm(S5$mean_resp ~ treatment), lwd = 2, col = "blue")
}


















C <- x[which(x$Plot == "Control"),]



par(mfrow = c(1,2), mar = c(4,5,2,1))
plot(C$Treatment, C$Respiration, 
     xlab = "Salinity Treatment",
     ylab = "Respiration (ug C-CO2/g dry soil)", 
     main = "Control Plot", 
     pch = 16,
     col = col[C$Site],
     cex = 0.7, 
     ylim = c(2,9))

C1 <- x[which(x$Plot == "Control" & x$Site == "1"),]
fit <- lm(Respiration~Treatment, data = C1)
abline(fit, lwd = 2, col = "red")

C3 <- x[which(x$Plot == "Control" & x$Site == "3"),]
fit <- lm(Respiration~Treatment, data = C3)
abline(fit, lwd = 2, col = "purple")

C5 <- x[which(x$Plot == "Control" & x$Site == "5"),]
fit <- lm(Respiration~Treatment, data = C5)
abline(fit, lwd = 2, col = "blue")

S <- x[which(x$Plot == "Salt"),]


plot(S$Treatment, S$Respiration, 
     xlab = "Salinity Treatment",
     ylab = " ", 
     main = "Salt Plot", 
     pch = 16,
     col = col[S$Site],
     cex = 0.7, 
     ylim = c(2,9))

S1 <- x[which(x$Plot == "Salt" & x$Site == "1"),]
fit <- lm(Respiration~Treatment, data = S1)
abline(fit, lwd = 2, col = "red")

S3 <- x[which(x$Plot == "Salt" & x$Site == "3"),]
fit <- lm(Respiration~Treatment, data = S3)
abline(fit, lwd = 2, col = "purple")

S5 <- x[which(x$Plot == "Salt" & x$Site == "5"),]
fit <- lm(Respiration~Treatment, data = S5)
abline(fit, lwd = 2, col = "blue")

