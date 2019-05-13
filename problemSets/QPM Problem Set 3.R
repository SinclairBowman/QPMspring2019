install.packages("faraway")
library("faraway")
data("newhamp")
help("newhamp")
plot(x=newhamp$pObama, y=newhamp$Dean, xlab="Obama", ylab="Dean", main="Obama vs. Dean")
install.packages("Zelig")
library("Zelig")
data("voteincome")
?voteincome
OnlyVoters = voteincome[voteincome$vote==1,]
n1 = length(OnlyVoters$age)
n2 = sum(OnlyVoters$vote)
n3 = sum(voteincome$vote)
mean(OnlyVoters$age)
sd(OnlyVoters$age)
ObamaPerHand = newhamp[newhamp$pObama & newhamp$votesys=="H",]
ObamaPerMach = newhamp[newhamp$pObama & newhamp$votesys=="D",]
DeanPerHand = newhamp[newhamp$Dean & newhamp$votesys=="H",]
DeanPerMach = newhamp[newhamp$Dean & newhamp$votesys=="D",]

plot(ObamaPerHand$pObama, DeanPerHand$Dean, pch = 6, xlab = "Obama Voters", ylab = "Dean Voters",
     main = "Dean vs Obama Proportions", col="blue")
points(ObamaPerMach$pObama, DeanPerMach$Dean, pch = 1, col="orange") 
legend("topleft", legend = c("Hand-Counted Votes", "Machine-Counted Votes"), pch = c(6, 1), col = c("blue", "orange"))

