require(Rsymphony)
require(ggplot2)

nCities <- 10
# boundries
max_x <- 500
max_y <- 500

# set random cities
set.seed(123456)
cities <- data.frame(id=1:nCities, x=runif(nCities, max=max_x), y=runif(nCities, max=max_y))

ggplot(cities, aes(x, y)) + 
  geom_point()

distance <- as.matrix(dist(cities[,c("x","y")], diag=TRUE, upper=TRUE))

obj <- c(as.numeric(distance),rep(1,nrow(distance)))
typVec <- c(rep("I",nrow(distance)*ncol(distance)),rep("C",nrow(distance)))
bounds <- list(upper=list(ind=grep(TRUE,obj == 0), val=rep(0,length(grep(TRUE,obj == 0)))))

# make constraint to leave each city
tmp <- matrix(0,10,10)
tmp[,1] <- 1
tmp <- cbind(tmp,matrix(0,10,1))
Amat <- as.numeric(tmp)
bvec <- 1
constDir <- "=="
for(i in 2:10){
  tmp <- matrix(0,10,10)
  tmp[,i] <- 1
  tmp <- cbind(tmp,matrix(0,10,1))
  Amat <- rbind(Amat,as.numeric(tmp))
  bvec <- c(bvec,1)
  constDir <- c(constDir, "==")
}
# must visit each
tmp <- matrix(0,10,10)
tmp[1,] <- 1
tmp <- cbind(tmp,matrix(0,10,1))
Amat <- rbind(Amat,as.numeric(tmp))
bvec <- c(bvec,1)
constDir <- c(constDir, "==")
for(i in 2:10){
  tmp <- matrix(0,10,10)
  tmp[i,] <- 1
  tmp <- cbind(tmp,matrix(0,10,1))
  Amat <- rbind(Amat,as.numeric(tmp))
  bvec <- c(bvec,1)
  constDir <- c(constDir, "==")
}
# add subtour constraints
tmp <- matrix(0,10,10)
tmp <- cbind(tmp,matrix(1,10,1))
Amat <- rbind(Amat,as.numeric(tmp))
bvec <- c(bvec,2)
constDir <- c(constDir, ">=")

for(i in 2:nCities){
  for(j in 2:nCities){
    tmp <- matrix(0,10,10)
    tmp[i,j] <- (nCities-1)
    tmp2 <-  matrix(0,10,1)
    tmp2[i,] <- 1
    tmp2[j,] <- -1
    Amat <- rbind(Amat,as.numeric(cbind(tmp,tmp2)))
    bvec <- c(bvec,nCities-2)
    constDir = c(constDir,"<=")
  }
}

sol <- Rsymphony_solve_LP(obj, Amat, constDir, bvec,bounds=bounds, types=typVec, max=FALSE, verbosity = 0)

sol$solution
sol$objval
sol$status

matrix(sol$solution[1:100],10,10)
# matrix(res$solution[11:110],10,10)