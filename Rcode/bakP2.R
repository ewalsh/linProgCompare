require(lpSolve)
require(linprog)

obj <- c(4,3)

constMat <- as.matrix(rbind(
  c(1,0),
  c(0,1),
  c(-1,-2),
  c(2,-4),
  c(2,-1)
))

constRHS <- c(0,2,-25,8,5)
constDir <- c(">=",">=",">=","<=",">=")

out <- lp(direction="max",obj,const.mat = constMat, const.dir = constDir, const.rhs = constRHS, all.int=TRUE,
          int.vec=c(1,1), compute.sens=1)

out
out$solution
out$solution%*%obj

outC <- solveLP(obj,constRHS, constMat, maximum=TRUE,const.dir=constDir)
outC$solution