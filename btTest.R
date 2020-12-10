# load libraries
require(Rsymphony)

# get data
dat <- read.csv("~/Projects/py_supply/dat.csv")
tgt <- read.csv("~/Projects/py_supply/tgt.csv")
# get unique weeks and stores 
uWks = unique(dat$week)
uStores = unique(dat$storeid)
# need to drop stores where each week is not available
storeWeeks = data.frame(storeid=uStores, numWks=rep(NA,length(uStores)))
for(i in 1:length(uStores)){
  storeWeeks[i,"numWks"] <- length(grep(TRUE, dat$storeid == uStores[i]))
}
uStores = storeWeeks$storeid[grep(TRUE, storeWeeks$numWks == max(storeWeeks$numWks))]
# remake dat
dat2 = dat[grep(TRUE, dat$storeid == uStores[1]),]
for(i in 2:length(uStores)){
  dat2 <- rbind(dat2,
                dat[grep(TRUE, dat$storeid == uStores[i]),])
}

## In a new way
# first an upper limit for number of control stores (106 campaign stores * 110%)
uLimit <- bvec <- ceiling(106*1.1) #length(uStores)#
constDir <- "<="
# now add a ceiling and floor of target levels to bvec with an error band
errBand <- 0.1
bvec <- c(bvec,ceiling(tgt$sales*(1+errBand)), floor(tgt$sales*(1-errBand)))
constDir <- c(constDir,rep("<=",nrow(tgt)),rep(">=",nrow(tgt)))
## Now our Amat
# first a set of 1s to use as our upper limit on stores 
Amat <- data.frame(t(rep(1,length(uStores))))
colnames(Amat) <- as.character(uStores)
# now our upper limit on sales
# for each week
for(wk in uWks){
  # get sales for that week
  wklySales <- dat2[grep(TRUE, dat2$week == wk),]
  # ensure the stores line up
  tmp <- data.frame(t(round(wklySales$sales)))
  colnames(tmp) <- as.character(wklySales$storeid)
  Amat <- rbind(Amat,tmp)
}
# now our lower limit on sales
# for each week
for(wk in uWks){
  # get sales for that week
  wklySales <- dat2[grep(TRUE, dat2$week == wk),]
  # ensure the stores line up
  tmp <- data.frame(t(round(wklySales$sales)))
  colnames(tmp) <- as.character(wklySales$storeid)
  Amat <- rbind(Amat,tmp)
}

# our objective is to maximize number of stores so simple boolean
obj <- rep(1,length(uStores))
typs <- rep("B",length(uStores))
Amat <- as.matrix(Amat)

sol <- Rsymphony_solve_LP(obj, Amat, constDir, bvec, types=typs, max=TRUE, 
                          verbosity = 1, write_lp=TRUE, write_mps=TRUE,
                          first_feasible = FALSE)

sol$solution
sum(sol$solution)
sol$objval

# let's see how we did 
require(reshape2)
storeDat <- acast(dat2,week~storeid, value.var="sales")
cstoreDat <- apply(storeDat,1,function(X,sol){sol$solution%*%X},sol)

plot(cstoreDat,type="l")
lines(tgt$sales,col=2)
# # create objective function
# tmpId = grep(TRUE,dat2$week == uWks[1])
# objMat <- c(dat2$sales[tmpId]*100)
# typVec <- typMat  <- rep("B",length(tmpId))
# # UBmat <- c(NA,fac_vars$Max_Capacity[1])
# # LBmat <- c(NA,fac_vars$Min_Capacity[1])
# for(i in 2:length(uWks)){
#   tmpId = grep(TRUE,dat2$week == uWks[i])
#   #print(length(tmpId))
#   typVec <- rep("B",length(tmpId))
#   
#   objMat <- rbind(objMat,
#                   c(dat2$sales[tmpId]*100))
#   typMat <- rbind(typMat,
#                   typVec)
#   # UBmat <- rbind(UBmat,c(NA,fac_vars$Max_Capacity[i]))
#   # LBmat <- rbind(LBmat,c(NA,fac_vars$Max_Capacity[i]))
# }
# 
# #obj.df <- data.frame(objMat,row.names=apply(dat2,1,function(X){paste("week:",X[1],"_","store:",X[3])}))
# obj.df <- data.frame(objMat,row.names=uWks)
# colnames(obj.df) <- uStores
# obj <- as.numeric((objMat))
# typs <- c(typMat)
# 
# ## lets explore the matrix
# bvec <- round(tgt$sales*100)
# Amat <- matrix(0, length(uWks), length(uStores))
# i = 1
# Amat[i,] <- 1
# for(i in 2:length(uWks)){
#   tmp <- matrix(0, length(uWks), length(uStores))
#   tmp[i,] <- 1
#   Amat <- cbind(Amat,tmp)
# }
# test = rep(0, length(obj))
# test[c(1,length(bvec)+1)] = 1
# constDir = rep("==", length(bvec))
# 
# c(Amat%*%test)*obj
# 
# # create constraint matrix, rhs, and dir vectors -- demand constraint
# numStores = length(uStores)
# Amat <- c(rep(1,numStores),rep(0,length(obj)-numStores))
# #Amat <- c(1,1,rep(0,length(obj)-2))
# constDir <- c("==")
# bvec <- as.numeric(tgt$sales[1]*100)
# iters = seq(numStores+1,length(obj),by=numStores)
# for(i in 2:length(iters)){
#   tmp <- rep(0,length(obj))
#   tmp[iters[i-1]:iters[i]] <- 1
#   Amat <- rbind(Amat,tmp)
#   constDir <- c(constDir,"==")
#   bvec <- c(bvec,as.numeric(tgt$sales[i]*100))
# }
# 
# sol <- Rsymphony_solve_LP(obj, Amat, constDir, bvec, types=typs, max=FALSE, verbosity = 1, write_lp=TRUE, write_mps=TRUE)
# 
# sol$solution
# sol$objval

