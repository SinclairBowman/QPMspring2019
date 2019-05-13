# Problem Four
help("Normal")

#Problem Five
set.seed(12345)
salaries <- rnorm(n=10000, mean = 40000, sd = 15000)
plot(salaries, main = "Average Salaries", col = 4)


#Problem Six
QA <- rnorm(n=1000, mean = 0, sd=.63)
plot(density(QA), main = "Density Plot Part A", xlab = "Sample Means")
QB <- rnorm(n=1000, mean = 0, sd=sqrt(3))
QC <- rnorm(n=1000, mean = 3, sd=sqrt(3))
QD <- rnorm(n=1000, mean = 3, sd=sqrt(.4))
QE <- rnorm(n=1000, mean = -2, sd=sqrt(5))
QF <- rnorm(n=1000, mean = -2, sd=sqrt(.25))
plot(density(QB), main = "Density Plot Part B", xlab = "Sample Means")
plot(density(QC), main = "Density Plot Part C", xlab = "Sample Means")
plot(density(QD), main = "Density Plot Part D", xlab = "Sample Means")
plot(density(QE), main = "Density Plot Part E", xlab = "Sample Means")
plot(density(QF), main = "Density Plot Part F", xlab = "Sample Means")

#Problem Eight
read.csv(wnominatehouse.csv)
