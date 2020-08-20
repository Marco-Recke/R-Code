nSubj <- 100
myData <- rnorm(nSubj, 5, 20)
muH0<- 0
t.test(myData, alternative="two.sided", mu=muH0)


