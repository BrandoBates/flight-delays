FlightDelays<-read.csv("C:/Users/Brandon/Desktop/Statistics/Data/FlightDelays.csv")
boxplot(data=FlightDelays, FlightLength~Carrier)
AALength<-subset(FlightDelays, select = FlightLength, subset = Carrier=="AA", drop=TRUE)
UALength<-subset(FlightDelays, select = FlightLength, subset = Carrier=="UA", drop=TRUE)
observed<-mean(AALength)-mean(UALength)
B<- 10^4 -1
result<-numeric(B)
for(i in 1:B)
{
  index <- sample(4029, size=2906, replace = FALSE)
  result[i] <- mean(FlightDelays$FlightLength[index]) - mean(FlightDelays$FlightLength[-index])
}
hist(result)
abline(v = observed, col = "blue", lty=5)
1-(sum(result <= observed)+1)/(B + 1)
qqplot(FlightDelays$FlightLength,result)
