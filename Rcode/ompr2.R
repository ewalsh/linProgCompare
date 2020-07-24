## ompr 2
require(rgeos)
require(rgdal)
require(maptools)
require(dplyr)
require(purrr)
require(spdep)
require(ompr)
require(ompr.roi)
require(ROI.plugin.glpk)
require(ROI)

map_data <- rgdal::readOGR("https://raw.githubusercontent.com/datasets/geo-boundaries-us-110m/84e946f6b1de01e2642bcdb17d5b697acb6b48c4/json/ne_110m_admin_1_states_provinces_shp_scale_rank.geojson")

# this gives as an adjancy list
neighbors <- spdep::poly2nb(map_data)

# a helper function that determines if two nodes are adjacent
is_adjacent <- function(i, j) {
  purrr::map2_lgl(i, j, ~ .y %in% neighbors[[.x]])
}

# create input variables
n <- nrow(map_data@data)
max_colors <- 16

mod <- MILPModel() %>%
  add_variable(x[i, k], type="binary", i=1:n, k=1:max_colors) %>%
  add_variable(y[k], type="binary", k=1:max_colors) %>%
  set_objective(sum_expr(k*y[k], k=1:max_colors), sense="min") %>%
  add_constraint(sum_expr(x[i,k], k=1:max_colors) == 1, i=1:n) %>%
  add_constraint(x[i,k] <= y[k], i=1:n, k=1:max_colors) %>%
  add_constraint(x[i, k] + x[j,k] <= 1, i=1:n, j=1:n, k=1:max_colors, is_adjacent(i, j))

mod

res <- solve_model(mod, with_ROI("glpk",presolve=TRUE,verbose=TRUE))

