
library(data.table)
library(tidyverse)

# Daily total flows at grid level per person ====
pop <- grid_pop %>% # The number of China Unicom users per grid
  group_by(grid) %>% summarise(pop = sum(pop)) 

flow_data <- raw_mobility_data %>% # raw mobility data
  filter(o_grid != d_grid) %>% # filter grids
  group_by(date) %>% 
  summarise(ufre = sum(ufre)) %>% # calculate daily aggregated flows
  left_join(pop) %>% 
  mutate(trip_per_person = ufre/pop)

# Pre-outbreak population flows per grid (inflows+outflows) ====
# Pre-outbreak phase: 2022/02/15-2022/02/28
data <- raw_mobility_data %>%
  filter(date >= as.Date('2022-02-15'), date <= as.Date('2022-02-28')) %>%
  filter(o_grid != d_grid) 
# outflows
feb_out <- dat_feb %>%
  group_by(date_dt, o_grid) %>%
  summarise(ufre = sum(ufre, na.rm = T)) %>% mutate(type = 'out') %>% rename(grid = o_grid)

feb_flows <- dat_feb %>%
  group_by(date_dt, d_grid) %>% 
  summarise(ufre = sum(ufre, na.rm = T)) %>% mutate(type = 'in') %>%  # inflows 
  rename(grid = d_grid) %>%
  bind_rows(feb_out) %>% 
  group_by(date_dt, grid) %>%
  summarise(ufre = sum(ufre, na.rm = T)) %>%
  group_by(grid) %>% 
  summarise(bs = mean(ufre, na.rm = T)) # flows (inflows+outflows) at grid level during pre-outbreak phase

# Changes in travel distance and median travel distance ====
grid.dis <- g.dis %>% # distance between pairs of grids (7355*7355)
  filter(o_grid != d_grid) %>%
  rename(d_grid = o_grid, o_grid = d_grid) %>% bind_rows(g.dis) %>%
  mutate(dis = ifelse(dis == 0, 250, dis))  %>%
  mutate(dis = round(dis/1000, 2)) %>%
  mutate(distance = cut(dis, c(-Inf, 1, 3, 5, 10, 30, 50, Inf), right = F,
                        labels = c("d < 1km", "1km <= d < 3km", "3km <= d < 5km",
                                   "5km <= d < 10km", "10km <= d < 30km",  "30km <= d < 50km",
                                   "d >= 50km"))) %>%
  mutate(distance = str_replace(distance,'d', "D"))  

dist_change <- raw_mobility_data %>%
  left_join(grid.dis) %>%
  filter(o_grid != d_grid) %>%
  group_by(date, distance) %>%
  summarise(ufre = sum(ufre, na.rm = T)) %>%
  group_by(date) %>% 
  mutate(N = sum(ufre, na.rm = T)) %>%
  mutate(perc = ufre/N)

dist_median <- raw_mobility_data %>%
  left_join(grid.dis) %>%
  filter(o_grid != d_grid) %>%
  group_by(date) %>%
  summarise(median_dist = weighted.median(dis, w = ufre)) # weighted by the number of trips


# Two-sample t-test ====
# Comparison of number daily trips per person among sex during pre-outbreak phase is used as an example
pop_sex <- daily_pop %>% # The number of China Unicom users per day
  group_by(date, sex) %>%
  summarise(pop = sum(pop))
data <- raw_mobility_data %>% 
  select(date, o_grid, d_grid, sex, ufre) %>%
  filter(o_grid != d_grid) %>%
  filter(phase == 'Pre-outbreak') %>%
  group_by(date, sex) %>%
  summarise(ufre = sum(ufre, na.rm = T)) %>% 
  left_join(pop_sex) %>% 
  mutate(trip_per_person = ufre/pop)

t.test(trip_per_person ~ sex, data = data) 

# Kolmogorov-Smirnov test ====
# Comparison of degree of mobility network among sex during pre-outbreak phase is used as an example
data <- degree_data %>% 
  filter(type == 'sex') %>%
  filter(phase == 'Pre-outbreak')
ks.test(degree ~ sex, data = data)


#Pearson correlation ====
# test the association between epidemiologic metrics and mobility
data <- march_case %>%
  left_join(march_flow) %>% # changes of mobility compared with Pre-outbreak phase at grid level in March
  mutate(case = cut(case, breaks = c(0,1,25, 50,75,100,Inf), right = F)) 

cor.test(data$case, -data$n, method = 'pearson')

