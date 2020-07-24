# load libraries
require(ompr)
require(ompr.roi)
require(ROI.plugin.glpk)
require(magrittr)

# get data
fac_vars <- read.csv("~/Projects/py_supply/data/fac_vars.csv")
demand <- read.csv("~/Projects/py_supply/data/demand.csv")

# preallocate
inputList <- list()
inputList[[1]] <- list()
# create objective function
objMat <- c(fac_vars$Fixed_Costs[1],fac_vars$Variable_Costs[1])
typVec <- typMat  <- c("B","I")
UBmat <- c(NA,fac_vars$Max_Capacity[1])
LBmat <- c(NA,fac_vars$Min_Capacity[1])
for(i in 2:nrow(fac_vars)){
  objMat <- rbind(objMat,
                  c(fac_vars$Fixed_Costs[i],fac_vars$Variable_Costs[i]))
  typMat <- rbind(typMat,
                  typVec)
  UBmat <- rbind(UBmat,c(NA,fac_vars$Max_Capacity[i]))
  LBmat <- rbind(LBmat,c(NA,fac_vars$Max_Capacity[i]))
}

obj.df <- data.frame(objMat,row.names=apply(fac_vars,1,function(X){paste("Month:",X[1],"_","Factory:",X[2])}))
obj <- as.numeric(objMat)
typs <- c(typMat)