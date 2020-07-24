# load libraries
require(Rsymphony)

# get data
fac_vars <- read.csv("~/Projects/py_supply/data/fac_vars.csv")
demand <- read.csv("~/Projects/py_supply/data/demand.csv")

# preallocate
inputList <- list()
inputList[[1]] <- list()
# create objective function
objMat <- c(fac_vars$Variable_Costs[1],fac_vars$Fixed_Costs[1])
typVec <- typMat  <- c("I","B")
# UBmat <- c(NA,fac_vars$Max_Capacity[1])
# LBmat <- c(NA,fac_vars$Min_Capacity[1])
for(i in 2:nrow(fac_vars)){
  objMat <- rbind(objMat,
                  c(fac_vars$Variable_Costs[i],fac_vars$Fixed_Costs[i]))
  typMat <- rbind(typMat,
                  typVec)
  # UBmat <- rbind(UBmat,c(NA,fac_vars$Max_Capacity[i]))
  # LBmat <- rbind(LBmat,c(NA,fac_vars$Max_Capacity[i]))
}

obj.df <- data.frame(objMat,row.names=apply(fac_vars,1,function(X){paste("Month:",X[1],"_","Factory:",X[2])}))
obj <- as.numeric(objMat)
typs <- c(typMat)

# # create constraint matrix, rhs, and dir vectors -- demand constraint
Amat <- c(1,1,rep(0,length(obj)-2))
constDir <- c("==")
bvec <- as.numeric(demand[1,"Demand"])
for(i in seq(4,nrow(obj.df),by=2)){
  tmp <- rep(0,length(obj))
  tmp[(i-1):(i)] <- 1
  Amat <- rbind(Amat,tmp)
  constDir <- c(constDir,"==")
  bvec <- c(bvec,as.numeric(demand[i/2,"Demand"]))
}
# production is greater than min capacity
for(i in 1:nrow(obj.df)){
  tmp <- rep(0,length(obj))
  tmp[c(i,i+nrow(obj.df))] <- c(1,-fac_vars[i,"Min_Capacity"])
  Amat <- rbind(Amat, tmp)
  constDir <- c(constDir, ">=")
  bvec <- c(bvec,0)
}
# production is less than max capacity
for(i in 1:nrow(obj.df)){
  tmp <- rep(0,length(obj))
  tmp[c(i,i+nrow(obj.df))] <- c(1,-fac_vars[i,"Max_Capacity"])
  Amat <- rbind(Amat, tmp)
  constDir <- c(constDir, "<=")
  bvec <- c(bvec,0)
}
# add constraint to be off in may in factory B
ID <- grep(TRUE,obj == 0)
tmp <- rep(0,length(obj))
tmp[ID[1]] <- 1
Amat <- rbind(Amat,tmp)
constDir <- c(constDir, "==")
bvec <- c(bvec,0)

ID <- grep(TRUE,obj == 0)
tmp <- rep(0,length(obj))
tmp[ID[2]] <- 1
Amat <- rbind(Amat,tmp)
constDir <- c(constDir, "==")
bvec <- c(bvec,0)

sol <- Rsymphony_solve_LP(obj, Amat, constDir, bvec, types=typs, max=FALSE, verbosity = 0, write_lp=TRUE, write_mps=TRUE)

sol$solution
sol$objval
sol$status

cbind(matrix(sol$solution,ncol=2,byrow=TRUE)[1:12,],
      data.frame(x=apply(matrix(sol$solution,ncol=2,byrow=TRUE)[1:12,],1,sum)),
      demand,matrix(sol$solution,ncol=2,byrow=TRUE)[13:24,])