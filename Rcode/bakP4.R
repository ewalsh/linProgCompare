require(lpSolve)
require(linprog)

data <- data.frame(Cost=c(4.32,2.46,1.86),Availability=c(30,20,17),
                   row.names=c("Pork","Wheat","Starch"))

obj <- c(data$Cost,data$Cost)

constMat <- as.matrix(rbind(
  c(1,1,1,0,0,0),
  c(0,0,0,1,1,1),
  c(-0.6,0.4,0.4,0,0,0),
  c(0,0,0,-0.4,0.6,0.6),
  c(0.25,0.25,-0.75,0,0,0),
  c(0,0,0,0.25,0.25,-0.75),
  c(1,0,0,1,0,0),
  c(1,0,0,1,0,0),
  c(0,1,0,0,1,0),
  c(0,0,1,0,0,1)
))

constRHS <- c(350*0.05,500*0.05,0,0,0,0,23,30,20,17)
constDir <- c("==","==","<=","<=",">=",">=",">=",rep("<=",3))

# out <- lp(direction="max",obj,const.mat = constMat, const.dir = constDir, const.rhs = constRHS, all.int=TRUE,
#           int.vec=c(1,1), compute.sens=1)
# 
# out
# out$solution
# out$solution%*%obj

outC <- solveLP(obj,constRHS, constMat, maximum=FALSE,const.dir=constDir, lpSolve=TRUE)
outC$solution
outC$solution%*%obj
data.frame(constMat%*%outC$solution,constDir,constRHS)