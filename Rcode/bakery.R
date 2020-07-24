require(lpSolve)

obj <- c(20,40)

constMat <- as.matrix(rbind(
  c(0.5,1),
  c(1,2.5),
  c(1,2),
  c(1,1)
))

constDir <- c(rep("<=",3),">=")
constRHS <- c(30,60,22,0)

out <- lp(direction="max",obj,const.mat = constMat, const.dir = constDir, const.rhs = constRHS, all.int=TRUE,
          int.vec=c(1,1))

