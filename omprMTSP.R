require(knitr)
require(dplyr)
require(ggplot2)
require(ompr)
require(ompr.roi)
require(ROI.plugin.glpk)
require(tidyr)

nCities <- 7
nSP <- 2
# boundries
max_x <- 500
max_y <- 500

# set random cities
set.seed(1)
cities <- data.frame(id=1:nCities, x=c(max_x/2, runif(nCities - 1, max=max_x)), 
                     y=c(max_y/2, runif(nCities - 1, max=max_y))) %>%
  mutate(is_depot = ifelse(id == 1, TRUE, FALSE))

ggplot(cities,aes(x,y)) +
  geom_point(aes(size = is_depot)) +
  scale_y_continuous(limits=c(0, max_y)) +
  scale_x_continuous(limits=c(0, max_x))

# distance matrix
distance <- as.matrix(dist(cities[,c("x","y")]), diag=TRUE, upper=TRUE)

# make model
mod <- MIPModel() %>%
  add_variable(x[i, j, k], i=1:nCities, j=1:nCities, k=1:nSP, type="binary") %>%
  add_variable(u[i, k], i=1:nCities, k=1:nSP, lb = 1, ub = nCities) %>%
  set_objective(sum_expr(distance[i, j]*x[i, j, k], i=1:nCities, j=1:nCities, k=1:nSP), "min") %>%
  add_constraint(x[i, i, k] == 0, i=1:nCities, k=1:nSP) %>%
  add_constraint(sum_expr(x[1, j, k], j=2:nCities) == 1, k=1:nSP) %>%
  add_constraint(sum_expr(x[i, 1, k], i=2:nCities) == 1, k=1:nSP) %>%
  add_constraint(sum_expr(x[j, i, k], j=1:nCities) == sum_expr(x[i, j, k], j=1:nCities), i=2:nCities, k=1:nSP) %>%
  add_constraint(sum_expr(x[i, j, k], j=1:nCities, k=1:nSP) == 1, i=2:nCities) %>%
  add_constraint(sum_expr(x[i, j, k], i=1:nCities, k=1:nSP) == 1, j=2:nCities) %>%
  add_constraint(u[i, k] >=2, i=2:nCities, k=1:nSP) %>%
  add_constraint(u[i, k] - u[j, k] + 1 <= (nCities-1)*(1-x[i,j,k]), i=2:nCities, j=2:nCities, k=1:nSP)

mod

res <- solve_model(mod, with_ROI(solver="glpk"))

sol <- get_solution(res, x[i, j, k]) %>%
  filter(value > 0)
kable(head(sol, 3))


paths <- select(sol, i, j, k) %>% 
  rename(from = i, to = j, salesman = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(cities, by = c("idx_val" = "id"))
kable(head(arrange(paths, trip_id), 4))

ggplot(cities, aes(x, y)) + 
  geom_point(aes(size = is_depot)) + 
  geom_line(data = paths, aes(group = trip_id, color = factor(salesman))) + 
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(res), 2))) +
  scale_y_continuous(limits = c(0, max_y)) + 
  scale_x_continuous(limits = c(0, max_x))