require(knitr)
require(dplyr)
require(ggplot2)
require(ompr)
require(ompr.roi)
require(ROI.plugin.glpk)
require(tidyr)

nCities <- 10
# boundries
max_x <- 500
max_y <- 500

# set random cities
set.seed(123456)
cities <- data.frame(id=1:nCities, x=runif(nCities, max=max_x), y=runif(nCities, max=max_y))

ggplot(cities,aes(x,y)) +
  geom_point()

# distance matrix
distance <- as.matrix(dist(cities[,c("x","y")], diag=TRUE, upper=TRUE))

mod <- MIPModel() %>%
  add_variable(x[i, j], i=1:nCities, j=1:nCities,
               type="integer", lb=0, ub=1) %>%
  add_variable(u[i], i=1:nCities, lb=1, ub=nCities) %>%
  set_objective(sum_expr(distance[i, j] * x[i,j], i=1:nCities, j=1:nCities) ,"min") %>%
  set_bounds(x[i, i], ub=0, i=1:nCities) %>%
  add_constraint(sum_expr(x[i, j], j=1:nCities) == 1, i=1:nCities) %>%
  add_constraint(sum_expr(x[i, j], i=1:nCities) == 1, j=1:nCities) %>%
  add_constraint(u[i] >= 2, i=2:nCities) %>%
  add_constraint(u[i] - u[j] + 1 <= (nCities - 1)*(1-x[i, j]), i=2:nCities, j=2:nCities)

mod

res <- solve_model(mod, with_ROI(solver="glpk", verbose=TRUE))

solution <- get_solution(res, x[i, j]) %>% 
  filter(value > 0) 
kable(head(solution, 3))

paths <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(cities, by = c("idx_val" = "id"))
kable(head(arrange(paths, trip_id), 4))

ggplot(cities, aes(x, y)) + 
  geom_point(size=4) + 
  geom_line(data = paths, aes(group = trip_id)) + 
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(res), 2)))