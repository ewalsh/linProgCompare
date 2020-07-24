# load libraries
require(ompr)
require(ompr.roi)
require(ROI.plugin.glpk)
require(magrittr)

# get data
fac_vars <- read.csv("~/Projects/py_supply/data/fac_vars.csv")
# replace character A and B with numeric
fac_vars$Factory <- as.character(fac_vars$Factory)
fac_vars$Factory <- as.numeric(fac_vars$Factory == "B")
demand <- read.csv("~/Projects/py_supply/data/demand.csv")

# create function for objective
varCostFunc <- function(month, factory, data){
  tmp <- data[grep(TRUE, as.numeric(data$Month) == month),]
  tmp <- tmp[grep(TRUE, as.character(tmp$Factory) == factory),]
  return(tmp$Variable_Costs)
}

fixedCostFunc <- function(month, factory, data){
  tmp <- data[grep(TRUE, as.numeric(data$Month) == month),]
  tmp <- tmp[grep(TRUE, as.character(tmp$Factory) == factory),]
  return(tmp$Fixed_Costs)
}

minCapFunction <- function(month, factory, data){
  tmp <- data[grep(TRUE, as.numeric(data$Month) == month),]
  tmp <- tmp[grep(TRUE, as.character(tmp$Factory) == factory),]
  return(tmp$Min_Capacity)
}

maxCapFunction <- function(month, factory, data){
  tmp <- data[grep(TRUE, as.numeric(data$Month) == month),]
  tmp <- tmp[grep(TRUE, as.character(tmp$Factory) == factory),]
  return(tmp$Max_Capacity)
}

months <- unique(fac_vars$Month)
factories <- unique(fac_vars$Factory) #== "A"


# create model
mod <- MIPModel() %>%
  add_variable(status[i, j], i=months, 
               j=factories, type="binary") %>%
  add_variable(prod[i, j], i=months, 
               j=factories, type="integer",
               lb = 0, ub = Inf) %>%
  set_objective(sum_expr(varCostFunc(i, j, data=fac_vars) *
                           prod[i, j], i=months, 
                         j=factories) +
                sum_expr(fixedCostFunc(i, j, data=fac_vars) *
                           status[i, j], i=months, 
                         j=factories), sense="min") %>%
  add_constraint(sum_expr(prod[i, j], j=factories) == demand[i,'Demand'],
                 i = months) %>%
  add_constraint(prod[i, j] >= 
                   minCapFunction(i, j, data=fac_vars) * status[i, j], i=months, 
                 j=factories) %>%
  add_constraint(prod[i, j]<= 
                   maxCapFunction(i, j, data=fac_vars) * status[i, j], i=months, 
                 j=factories) %>%
  add_constraint(sum_expr(prod[i, j], i=5) == 0, j=1) %>%
  add_constraint(sum_expr(status[i, j], i=5) == 0, j=1)


sol <- solve_model(mod, with_ROI(solver="glpk", verbose = TRUE))
sol$solution
sol$objective_value
# get_solution(sol, prod[i, j])