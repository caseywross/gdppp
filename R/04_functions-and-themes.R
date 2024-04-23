revisions_summary_calc <- function(data, .obs_start, .obs_last){
  
  data %>% 
    
    filter_by_time(date_obs_dt, 
                   .start_date = .obs_start, 
                   .end_date = .obs_last) %>%
    
    group_by(release_no) %>% 
    
    summarise(mean_rev = mean(rev_growth),
              sd_rev   = sd(rev_growth)) %>% 
    ungroup() 
  
}

#tests
output_growth_revised$rgdi %>% revisions_summary_calc("2004-12-31", "2013-03-31")

output_growth_revised$rgdp %>% revisions_summary_calc("2004-12-31", "2013-03-31")

output_growth_revised$gdpplus %>% revisions_summary_calc("2004-12-31", "2013-03-31")


#theme

measure_col = c(gdpplus = "#011E4D", rgdp = "#008EFF", rgdi = "#FF7F32") 

theme_phila_fed <- theme_minimal() +
  theme(text = element_text(family = "HelveticaNeueforSAS"),
        plot.title = element_text(size = 16, color = "#011e4d", face = "bold"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        axis.line.x.bottom = element_line(),
        axis.ticks.x = element_line(),
        axis.text = element_text(size = 12, color = "#515c70"),
        axis.title = element_blank(),
        legend.text = element_text(size = 12, color = "#515c70"),
        legend.background = element_rect(fill = "#ebf0f4", color = NA),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(rep(10, 4)))

# plot tests
output_growth_revised$rgdp %>% 
  filter_by_time(date_obs_dt, 
                 .start_date = "2004-12-31", 
                 .end_date = today()) %>% 
  filter(release_no %in% c(2, 3, 4)) %>% 
  ggplot(aes(date_obs_dt, rev_growth)) +
  geom_line(color = "#008EFF", size = 1.5) +
  facet_wrap(~release_no) +
  theme_phila_fed

output_growth_revised %>% list_rbind(names_to = "measure") %>% 
  group_by(measure) %>% 
  filter(vintage_dt == max(vintage_dt)) %>% ungroup() %>% 
  filter(date_obs_dt >= "2019-04-15") %>%
  
  ggplot(aes(date_obs_dt, growth_rt)) +
  geom_line(aes(color = measure), size = 1.5, key_glyph = "rect") +
  geom_hline(yintercept = 0) +
  labs(title = "PERCENTAGE (%)") +
  scale_color_manual(values = measure_col, breaks = names(measure_col),
                     labels = c("GDPplus", "Real GDP", "Real GDI") %>% str_pad(width = 12, side = "right")) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 12), ) +
  theme_phila_fed

