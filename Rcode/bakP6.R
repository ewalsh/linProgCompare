# load libraries
require(Rsymphony)

# get data
fac_vars <- read.csv("~/Projects/py_supply/data/fac_vars.csv")
demand <- read.csv("~/Projects/py_supply/data/demand.csv")

# preallocate
inputList <- list()
inputList[[1]] <- list()
# add startup cost to each factory
fac_vars <- cbind(fac_vars,data.frame(startup=rep(c(20000,400000),nrow(fac_vars)/2)))
# create objective function
objMat <- c(fac_vars$Variable_Costs[1],
            fac_vars$Fixed_Costs[1],
            fac_vars$startup[1])
typVec <- typMat  <- c("I","B","B")
nmVec <- nmMat <- c("Prod","Status","Startup")
# UBmat <- c(NA,fac_vars$Max_Capacity[1])
# LBmat <- c(NA,fac_vars$Min_Capacity[1])
for(i in 2:nrow(fac_vars)){
  objMat <- rbind(objMat,
                  c(fac_vars$Variable_Costs[i],
                    fac_vars$Fixed_Costs[i],
                    fac_vars$startup[i]))
  typMat <- rbind(typMat,
                  typVec)
  nmMat <- rbind(nmMat,
                  nmVec)
  # UBmat <- rbind(UBmat,c(NA,fac_vars$Max_Capacity[i]))
  # LBmat <- rbind(LBmat,c(NA,fac_vars$Max_Capacity[i]))
}

obj.df <- data.frame(objMat,row.names=apply(fac_vars,1,function(X){paste("Month:",X[1],"_","Factory:",X[2])}))
obj <- as.numeric(objMat)
typs <- c(typMat)
nms <- c(nmMat)

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

## add startup const constraint
# first for month 1
ID <- grep(TRUE,nms == "Startup")
statusId <- grep(TRUE, nms == "Status")
tmp <- rep(0,length(obj))
tmp[c(statusId[1],ID[1])] <- c(1,-1)
Amat <- rbind(Amat, tmp)
constDir <- c(constDir, "==")
bvec <- c(bvec,0)

tmp <- rep(0,length(obj))
tmp[c(statusId[2],ID[2])] <- c(1,-1)
Amat <- rbind(Amat, tmp)
constDir <- c(constDir, "==")
bvec <- c(bvec,0)
# following months 
for(i in 3:length(ID)){
  tmp <- rep(0,length(obj))
  tmp[c(statusId[i],statusId[i-2],ID[i])] <- c(1,-1,-1)
  Amat <- rbind(Amat, tmp)
  constDir <- c(constDir, "<=")
  bvec <- c(bvec,0)
}

for(i in 3:length(ID)){
  tmp <- rep(0,length(obj))
  tmp[c(statusId[i-2],ID[i])] <- c(-1,-1)
  Amat <- rbind(Amat, tmp)
  constDir <- c(constDir, ">=")
  bvec <- c(bvec,-1)
}

for(i in 3:length(ID)){
  tmp <- rep(0,length(obj))
  tmp[c(statusId[i],ID[i])] <- c(1,-1)
  Amat <- rbind(Amat, tmp)
  constDir <- c(constDir, ">=")
  bvec <- c(bvec,0)
}

sol <- Rsymphony_solve_LP(obj, Amat, constDir, bvec, types=typs, max=FALSE, verbosity = 0)

sol$solution
sol$objval
sol$status

cbind(matrix(sol$solution,ncol=2,byrow=TRUE)[1:12,],
      data.frame(x=apply(matrix(sol$solution,ncol=2,byrow=TRUE)[1:12,],1,sum)),
      demand,matrix(sol$solution,ncol=2,byrow=TRUE)[13:24,])