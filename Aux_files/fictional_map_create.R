# Assuma that create_synthetic_substations.R from the grid_robust_opt/israel_transmission_synthetic/ has been run
map4 <- ggplot() + ylim(c(34,36)) + xlim(c(29.5,33.3)) +
  geom_point(data = generator.substation.data %>% 
               rename(`Total population` = total_population,
                      `Node type` = node.type), 
             aes(y = clust.lon, x = clust.lat, shape = `Node type`, color = `Node type`, size = `Total population`), alpha = 0.8)

map5 <- map4 + 
  geom_segment(data = power.edge.adj, aes(y = x1, yend = x2, x = y1, xend = y2), size = 0.7, alpha = 0.7)


map6 <- map5 + 
  geom_segment(data = sample_n(power.delaunay, size = 50), aes(y = x1, yend = x2, x = y1, xend = y2), size = 0.7, alpha = 0.6, linetype = 2) +
  xlab("Longitude") + ylab("Latitude") + ylim(c(34.75,35.25)) + xlim(c(31.5,32.5))

ggsave(filename = "c:/Users/Adi Sarid/Documents/GitHub/robustness-for-seminar-day-Jan17/Aux_files/figure_2_example_grid.png",
       plot = map6, width = 30, height = 10, units = "cm")
