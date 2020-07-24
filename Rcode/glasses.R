require(lpSolve)

obj <- c(5,4.5)

constMat <- as.matrix(rbind(
  c(6,5),
  c(10,20),
  c(1,0)
))

constDir <- rep("<=",3)
constRHS <- c(60,150,6)

out <- lp(direction="max",obj,const.mat = constMat, const.dir = constDir, const.rhs = constRHS, all.int=TRUE,
          int.vec=c(1,1))

out$solution
