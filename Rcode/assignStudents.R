require(ompr)
require(ggplot2)
require(purrr)
require(dplyr)
require(ompr.roi)
require(ROI.plugin.glpk)

# set up data
n <- 40
m <- 4
capacity <- rep.int(11,m)
# create preference data
set.seed(1234)
preference_data <- lapply(seq_len(n), function(x){ 
  sample(seq_len(m), 3)
})
# preference function
preferences <- function(student){
  preference_data[[student]]
}
# find preference weight 
weight <- function(student, course){
  pref <- grep(TRUE,preferences(as.numeric(student)) == as.numeric(course))
  if(length(pref) == 0){
    out <- as.integer(-10000)
  } else {
    out <- as.integer(pref)
  }
  return(out)
}

plotData <- expand.grid(course=seq_len(m),weight=1:3) %>%
  rowwise() %>%
  mutate(count = sum(map_int(seq_len(n), 
                             ~weight(.x, course) == weight))) %>%
  mutate(course = factor(course), weight = factor(weight))

ggplot(plotData, aes(x=course, y = count, fill = weight)) +
  geom_bar(stat="identity") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept = 11)

mod <- MIPModel() %>%
  add_variable(x[i, j], i=1:n, j=1:m,type="binary") %>%
  set_objective(sum_expr(weight(i,j)*x[i,j], i=1:n, j=1:m)) %>%
  add_constraint(sum_expr(x[i,j], i=1:n) <= capacity[j], j=1:m) %>%
  add_constraint(sum_expr(x[i, j], j=1:m) == 1, i=1:n)

mod
res <- solve_model(mod, with_ROI(solver="glpk", verbose=TRUE))

matching <- res %>%
  get_solution(x[i, j]) %>%
  filter(value > 0.9) %>%
  select(i, j) %>%
  rowwise() %>%
  mutate(weight = weight(as.numeric(i), as.numeric(j)),
         preferences = paste0(preferences(as.numeric(i)), collapse=",")) %>%
  ungroup

matching %>%
  group_by(weight) %>%
  summarise(count=n())