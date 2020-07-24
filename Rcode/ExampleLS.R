S <- 1; K=1.1; r=0.06; T=3; NSteps=3; dt=T/NSteps;
discountVet = exp(-r*dt*(1:NSteps))

NRepl=8;
SPaths = rbind(
  c(1.09,1.08,1.34),
  c(1.16,1.26,1.54),
  c(1.22,1.07,1.03),
  c(0.93,0.97,0.92),
  c(1.11,1.56,1.52),
  c(0.76,0.77,0.90),
  c(0.92,0.84,1.01),
  c(0.88,1.22,1.34)
)

alpha <- rep(0,3)
CashFlows = max(0, K-SPaths[,NSteps])
ExerciseTime <- NSteps*matrix(1,NRepl,1)
for(step in (NSteps-1):1){
  InMoney <- grep(TRUE,SPaths[,step] < K)
  XData <- SPaths[InMoney, step]
  RegrMat <- cbind(matrix(1,length(XData),1),XData, XData^2)
  YData <- CashFlows[InMoney]*discountVet[ExerciseTime[InMoney] - step]
}