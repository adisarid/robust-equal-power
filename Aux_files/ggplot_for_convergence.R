# ==== Count number of variables ====
tmp <- read_csv("c:/temp/grid_cascade_output/26-12-2017 12-05-46-1803.923 - temp_sol.csv") %>%
  mutate(name.short = stringr::str_sub(name, end = 2))

tmp %>% select(name.short) %>% group_by(name.short) %>% tally()

tmp %>% filter(name.short == "c_") %>% View()


# ==== Convergence study ====
tmp.batch <- read_csv("c:/Users/Adi Sarid/Documents/GitHub/grid_robust_opt/results/case30_solution_statistics.csv",
                      col_names = c("capacity_decision", "DVar priorities control", 
                                    "time_spent_total", "time_spent_cascade_sim", "best_incumbent")) %>%
  mutate(opt.gap = (94.6-tmp.batch$best_incumbent)/94.6) %>%
  mutate(time.min = time_spent_total/60) %>%
  mutate(prop.time.sim = time_spent_cascade_sim/time_spent_total)

# plot the optimality gap as a function of time

opt.gap.plot <- ggplot(tmp.batch %>%
         filter(capacity_decision == 5), 
       aes(x = time.min, y = opt.gap, color = `DVar priorities control`, linetype = `DVar priorities control`)) + 
  geom_line(size = 1.5) + theme_gray() + scale_y_continuous(labels = scales::percent) + 
  xlab("Run time in minutes (timelimit 180 min)") + ylab("Optimality gap [%]") + 
  ggtitle("Optimality gap as a function of runtime") + 
  coord_cartesian(ylim = c(0,0.4))

ggsave(filename = "../../robustness-for-seminar-day-Jan17/Aux_files/figure_6_opt_gap.pdf",
                plot = opt.gap.plot, width = 20, height = 6, units = "cm")

ggplot(tmp.batch %>%
         filter(`DVar priorities control`), aes(x = time.min, y = opt.gap, color = factor(capacity_decision))) + 
  geom_point(size = 5) + theme_gray() + scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(xlim = c(0,50)) + 
  xlab("Run time in minutes (timelimit 180 min)") + ylab("Optimality gap [%]")

tmp.batch %>% 
  group_by(capacity_decision, `DVar priorities control`) %>%
  summarise(opt.gap = min(opt.gap)*100,
            max.time = max(time_spent_total))

simulation.time.plot <- ggplot(tmp.batch %>%
                                 filter(capacity_decision == 5 & `DVar priorities control`), 
                               aes(x = time.min, y = prop.time.sim)) + 
  geom_line(size = 1.5) + theme_gray() + scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
  xlab("Run time in minutes (timelimit 180 min)") + ylab("Proportion of cascade simulation time [%]") + 
  ggtitle("Time spent on cascade simulation")

ggsave(filename = "../../robustness-for-seminar-day-Jan17/Aux_files/figure_7_time_simulating.pdf",
       plot = simulation.time.plot, width = 20, height = 6, units = "cm")






# Objective value as a function of runtime
setwd("C:\\Users\\Adi Sarid\\Documents\\GitHub\\robustness-for-seminar-day-Jan17\\run_results\\")

library(tidyverse)
all.data <- dir(pattern = "_solution_statistics.csv") %>%
  map_df(~ read_csv(., skip = 1, 
                    col_names = 
                      c("line_cost_coef_scale",	
                        "line_capacity_coef_scale",	
                        "set_decision_var_priorities",	
                        "runtime",	
                        "net_runtime_simulations",
                        "best_incumbent")))

ggplot(all.data, aes(x = runtime/60, y = best_incumbent, color = factor(line_cost_coef_scale))) + geom_line(size = 2) + 
  xlab("Run time in [minutes]") + ylab("Objective value reached") + guides(color=guide_legend(title="Line upgrade\ncost parameter")) -> objective.fun.time
ggsave(filename = "../../robustness-for-seminar-day-Jan17/Aux_files/figure_8_obj.time.pdf",
       plot = objective.fun.time, width = 20, height = 6, units = "cm")
  

# Don't show this one
all.data %>%
  group_by(line_cost_coef_scale) %>%
  summarize(obj.reached = max(best_incumbent)) %>%
  mutate(opt.gap = (94.6-obj.reached)/obj.reached) %>%
  ggplot(aes(x = line_cost_coef_scale, y = opt.gap)) + 
  geom_line() + geom_point() + coord_cartesian(ylim = c(0,0.35), xlim = c(0,1)) + 
  scale_y_continuous(labels = scales::percent)

