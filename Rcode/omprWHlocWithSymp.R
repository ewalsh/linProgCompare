# warehouse locatoin model
require(knitr)
require(dplyr)
require(ggplot2)
require(Rsymphony)
require(tidyr)

set.seed(1234)
grid_size <- 1000
n <- 100
customer_locations <- data.frame(
  id = 1:n,
  x = round(runif(n) * grid_size),
  y = round(runif(n) * grid_size)
)

m <- 20
warehouse_locations <- data.frame(
  id = 1:m,
  x = round(runif(m) * grid_size),
  y = round(runif(m) * grid_size)
)

fixedcost <- round(rnorm(m, mean = grid_size * 10, sd = grid_size * 5))

transportcost <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  round(sqrt((customer$x - warehouse$x)^2 + (customer$y - warehouse$y)^2))
}
transportcost(1, 3)

p <- ggplot(customer_locations, aes(x, y)) + 
  geom_point() + 
  geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(0, grid_size)) +
  scale_y_continuous(limits = c(0, grid_size)) +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), panel.grid = element_blank())
p + ggtitle("Warehouse location problem", 
            "Black dots are customers. Light red triangles show potential warehouse locations.")

# create objective
tcMat <- matrix(NA, nrow(customer_locations), nrow(warehouse_locations))
for(i in 1:nrow(tcMat)){
  for(j in 1:ncol(tcMat)){
    tcMat[i, j] <- transportcost(i, j)
  }
}

obj <- c(as.numeric(tcMat),fixedcost)
typs <- rep("B",length(obj))
# add constraint each customer needs a warehouse
tmp <- matrix(0, nrow(customer_locations), nrow(warehouse_locations))
tmp[1,] <- 1
Amat <- c(as.numeric(tmp),rep(0,length(fixedcost)))
constDir <- "=="
bvec <- 1
for(i in 2:nrow(customer_locations)){
  tmp <- matrix(0, nrow(customer_locations), nrow(warehouse_locations))
  tmp[i,] <- 1
  Amat <- rbind(Amat,c(as.numeric(tmp),rep(0,length(fixedcost))))
  constDir <- c(constDir, "==")
  bvec <- c(bvec,1)
}

# add a constraint so if a customer is assigned a warehouse, that warehouse is built
for(i in 1:nrow(customer_locations)){
  for(j in 1:nrow(warehouse_locations)){
    tmp <- matrix(0, nrow(customer_locations), nrow(warehouse_locations))
    tmp[i,j] <- 1
    tmp2 <- rep(0,nrow(warehouse_locations))
    tmp2[j] <- -1
    Amat <- rbind(Amat,c(tmp,tmp2))
    constDir <- c(constDir,"<=")
    bvec <- c(bvec,0) #(obj[i] - obj[j+(n*2)])
  }
}

data.frame(c=Amat%*%(res$solution),d=constDir,bvec=bvec)

rmod <- Rsymphony_solve_LP(obj, Amat, constDir, bvec,types=typs,verbosity=0,max=FALSE)

suppressPackageStartupMessages(library(dplyr))
matching <- res %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j)

plot_assignment <- matching %>% 
  inner_join(customer_locations, by = c("i" = "id")) %>% 
  inner_join(warehouse_locations, by = c("j" = "id"))
customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)
plot_warehouses <- warehouse_locations %>% 
  mutate(costs = fixedcost) %>% 
  inner_join(customer_count, by = "id") %>% 
  filter(id %in% unique(matching$j))
p + 
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) + 
  geom_point(data  = plot_warehouses, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_warehouses, 
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n)), 
                            size = 2, nudge_y = 20) + 
  ggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
          "Big red triangles show warehouses that will be built, light red are unused warehouse locations. 
Dots represent customers served by the respective warehouses.")

sum(fixedcost[unique(matching$j)])