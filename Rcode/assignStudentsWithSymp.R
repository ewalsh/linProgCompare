require(Rsymphony)
# define parameters
n <- 40
m <- 4
capacity <- rep(11,4)
# create preference data
pref.df <- t(sapply(1:n, function(x, m){sample(1:m,3)},m))
# create objective vector -- step 1 in matrix
objMat <- matrix(NA,n,m)
for(i in 1:nrow(objMat)){
  objMat[i,] <- sapply(1:m,function(x, pref.df){
    out <- grep(TRUE,x == pref.df[i,])
    if(length(out) == 0){
      out <- -10000
    }
    return(out)
    }, pref.df)
}
obj <- as.numeric(t(objMat))
# create constraint matrix
oneClassMat <- t(sapply(1:n,function(nstudents,n,m){
  out <- rep(0,n*m)
  out[c((nstudents*m-3):(nstudents*m))] <- 1
  return(out)
},n,m))

Amat <- as.matrix(rbind(
  rep(c(1,0,0,0),n),
  rep(c(0,1,0,0),n),
  rep(c(0,0,1,0),n),
  rep(c(0,0,0,1),n),
  oneClassMat
))
# create constraint direction
dirVec <- c(rep("<=",4),rep("==",n))
# rhs bound
constRHS <- c(rep(11,4),rep(1,n))

ans <- Rsymphony_solve_LP(obj, Amat, dirVec, constRHS, max=TRUE,
                   write_lp = TRUE, write_mps = TRUE, verbosity = -2)
ans

ans2 <- Rglpk_solve_LP(obj, Amat, dirVec, constRHS, max=TRUE)
ans2