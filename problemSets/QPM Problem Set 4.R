# Question One
policedata = matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
policedata
policedatatExpected = matrix(c(((27/42)*21), ((27/42)*13), ((27/42)*8), ((15/42)*21), ((15/42)*13), ((15/42)*8)), nrow = 2, byrow = TRUE)
Residualpolicedata = policedata - policedatatExpected
Residualpolicedata
chisquareteststat = sum((Residualpolicedata^2)/policedatatExpected)
chisquareteststat
chisq.test(policedata)
StandResPolice = Residualpolicedata/sqrt(policedatatExpected)
StandResPolice

# Question Three
femalepols = read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
femalepols
reserved = femalepols$reserved
water = femalepols$water
regressionfemalepols = lm(water ~ reserved, data=femalepols)
summary(regressionfemalepols)
summary(femalepols)


# Question Four
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
View(Prestige)
Prestige$professional = NA
Prestige$professional = Prestige$type
Prestige$professional = as.character(Prestige$professional)
Prestige$professional[ Prestige$professional=="prof" ] <- "1"
Prestige$professional[ Prestige$professional=="bc" ] <- "0"
Prestige$professional[ Prestige$professional=="wc" ] <- "0"
dummyint1 = "Prestige$income*Prestige$professional"
lm4B = lm(Prestige$prestige ~ Prestige$income + Prestige$professional + (Prestige$income : Prestige$professional))
summary(lm4B)
scatterplot(Prestige$prestige ~ Prestige$income + Prestige$professional)



# Question Five
library("faraway")
data("newhamp")
colnames(newhamp)
help(lm)
votesysonly = lm(newhamp$pObama ~ newhamp$votesys)
summary(votesysonly)
votesyspovrate = lm(newhamp$pObama ~ newhamp$votesys + newhamp$povrate)
VotPovPci = lm(newhamp$pObama ~ newhamp$votesys + newhamp$povrate + newhamp$pci)
VPPD = lm(newhamp$pObama ~ newhamp$votesys + newhamp$povrate + newhamp$pci + newhamp$Dean)
VPPDW = lm(newhamp$pObama ~ newhamp$votesys + newhamp$povrate + newhamp$pci + newhamp$Dean + newhamp$white)
DeanOnly = lm(newhamp$pObama ~ newhamp$Dean)
summary(votesysonly)
summary(votesyspovrate)
summary(VotPovPci)
summary(VPPD)
summary(VPPDW)
summary(DeanOnly)


# Question 6

setwd("GitHub/QPMspring2019")
read.csv("problemSets/incumbents_subset.csv")
Incumbents <- read.csv("problemSets/incumbents_subset.csv")
colnames(Incumbents)


scatterplot(Incumbents$voteshare ~ Incumbents$difflog,
            main="Model One", xlab="Diff Log", ylab="Vote Share")
ModelOne <- (lm(voteshare ~ difflog, data=Incumbents))
abline(lm(ModelOne, col="Red", lwd=2))
summary(ModelOne)

ResidualOne = (residuals(ModelOne))

plot(residuals(ModelOne) ~ fitted(ModelOne), data=Incumbents)
abline(h=0)
summary(ResidualOne)

# Prediction Equation: ybar = 0.579031 + 0.041666 X-difflog

coef(ModelOne)
abline(a=coef(ModelOne)[1], b=coef(ModelOne)[2], 
       lwd=2, col="Blue")

#6 B

scatterplot(Incumbents$presvote ~ Incumbents$difflog,
            main="Model Two", xlab="Diff Log", ylab="Presidential Vote")
ModelTwo <- (lm(presvote ~ difflog, data=Incumbents))
abline(ModelTwo, col="Red", lwd=2)
summary(ModelTwo)

ResidualTwo = (residuals(ModelTwo))

residuals <- plot(residuals(ModelTwo) ~ fitted(ModelTwo), data=Incumbents)
abline(h=0)
summary(ResidualTwo)

# ybar = 0.507583 + 0.023837 X-difflog

coef(ModelTwo)
abline(a=coef(ModelTwo)[1], b=coef(ModelTwo)[2], 
       lwd=2, col="Blue")

#6 C

scatterplot(Incumbents$presvote ~ Incumbents$voteshare,
            main="Model Three", xlab="Presidential Vote", ylab="Vote Share")
ModelThree <- (lm(presvote ~ voteshare, data=Incumbents))
abline(ModelThree, col="Red", lwd=2)
summary(ModelThree)

ResidualThree = (residuals(ModelThree))

residuals <- plot(residuals(ModelThree) ~ fitted(ModelThree), data=Incumbents)
abline(h=0)
summary(ResidualThree)

# ybar = 0.20363 + 0.53042 X-voteshare

coef(ModelThree)
abline(a=coef(ModelThree)[1], b=coef(ModelThree)[2], 
       lwd=2, col="Blue")

#6 D

scatterplot(ResidualOne ~ ResidualTwo,
            main="Residual Comparison", xlab="Residual Two", ylab="Residual One")
ResidualComparison <- (lm(ResidualOne ~ ResidualTwo, data=Incumbents))
abline(ResidualComparison, col="Red", lwd=2)
summary(ResidualComparison)

# ybar = 4.860e18 + 2.569e-01 X-Residual Two

#6 E

Incumbents$difflogpresvote = Incumbents$difflog + Incumbents$presvote

scatterplot(Incumbents$voteshare ~ Incumbents$difflogpresvote,
            main="Model Five", xlab="Diff Log and Presidential Vote", ylab="Vote Share")
ModelFive <- (lm(voteshare ~difflogpresvote, data=Incumbents))
abline(ModelFive, col="Red", lwd=2)
summary(ModelFive)

# ybar = 0.5554993 + 0.0419046 X-difflogpresvote
