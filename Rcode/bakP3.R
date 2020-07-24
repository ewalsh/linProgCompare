require(lpSolve)
require(linprog)

obj <- c(30000,45000)

constMat <- as.matrix(rbind(
  c(1,0),
  c(0,1),
  c(3,4),
  c(5,6),
  c(1.5,3)
))

constRHS <- c(0,0,30,60,21)
constDir <- c(">=",">=","<=","<=","<=")

out <- lp(direction="max",obj,const.mat = constMat, const.dir = constDir, const.rhs = constRHS, all.int=TRUE,
          int.vec=c(1,1), compute.sens=1)

out
out$solution
out$solution%*%obj

# outC <- solveLP(obj,constRHS, constMat, maximum=TRUE,const.dir=constDir)
# outC$solution