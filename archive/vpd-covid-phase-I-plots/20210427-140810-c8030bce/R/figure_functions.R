
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

figure_maker_burden_per_mod_dis <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                            year_end = 2030, dis_name = "Yellow fever",
                                            file_name = "deaths_by_year_YF", 
                                            figheight = 10, figwidth = 12){
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country_name, year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE))
  
  d_burden <- d_burden %>%
    bind_rows(d_pop %>%
                filter(burden_outcome == burden_t) %>%
                filter(year <= year_end) %>%
                filter(disease ==  dis_name) %>%
                filter(simple_scenario == "Postpone 2020 SIAs until 2021") %>%
                group_by(country_name, year, modelling_group_tidy) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "Business as usual"))
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("Business as usual",
                                                                                       "Postpone 2020 SIAs until 2021",
                                                                                       "50% reduction in RI",
                                                                                       
                                                                                       "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  d_burden <-  d_burden %>% 
    mutate(thickline = ifelse(simple_scenario == "Business as usual", 1, 0))  %>% 
    mutate(alpha  = ifelse(simple_scenario == "Business as usual", TRUE , FALSE))
  
  p <- d_burden %>% 
    ggplot(aes(x = year, color = simple_scenario, y = value, size = thickline, alpha = alpha)) +
    geom_line() +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
    scale_color_manual("", values = scenario_pal) +
    scale_size(range = c(1.3, 5), guide = "none") +
    scale_alpha_manual(values = c(1, 0.5), guide = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm')) +
    scale_y_log10() +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
  p2 <- p + facet_grid(modelling_group_tidy ~ country_name,
                       scales = "free_y")
  
  ggsave(plot = p2, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  
  return(p2)
}
#--------------------------------------------------------------------------------------------------#
figure_maker_burden_per_mod_dis_pop <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                                year_end = 2030, dis_name = "Yellow fever",
                                                file_name = "deaths_by_year_pop_YF", 
                                                figheight = 10, figwidth = 12){
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country_name, year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE))
  
  d_burden <- d_burden %>%
    bind_rows(d_pop %>%
                filter(burden_outcome == burden_t) %>%
                filter(year <= year_end) %>%
                filter(disease ==  dis_name) %>%
                filter(simple_scenario == "Postpone 2020 SIAs until 2021") %>%
                group_by(country_name, year, modelling_group_tidy) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          pop = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "Business as usual"))
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("Business as usual",
                                                                                       "Postpone 2020 SIAs until 2021",
                                                                                       "50% reduction in RI",
                                                                                       
                                                                                       "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  
  d_burden <-  d_burden %>% 
    mutate(thickline  = ifelse(simple_scenario == "Business as usual", 0.5 , 0)) %>% 
    mutate(alpha  = ifelse(simple_scenario == "Business as usual", TRUE , FALSE))
  
  ## calculate burden per capita
  d_burden <- d_burden %>% mutate(value = value/pop*100000)
  
  p <- d_burden %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(aes(color = simple_scenario, size = thickline, alpha = alpha)) +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = paste0(R.utils::capitalize(burden_t), " per 100,000")) +
    scale_color_manual("", values = scenario_pal) +
    scale_size(range = c(1.3, 5), guide = "none") +
    scale_alpha_manual(values = c(1,0.5), guide = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))  +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
  p2 <- p + facet_grid(modelling_group_tidy ~ country_name,
                       scales = "free_y")
  
  ggsave(plot = p2, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  
  return(p2)  
}
#-------------------------------------------------------------------------------------------------------------
figure_maker_excess_timeline_per_disease_mod <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                                         year_end = 2030, dis_name = "Yellow fever",
                                                         file_name = "excess_deaths_by_year_YF", 
                                                         figheight = 10, figwidth = 12){
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country_name, year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE),
              value_excess = -sum(impact, na.rm = TRUE), 
              pop = sum(population, na.rm = TRUE))
  
  d_burden <- d_burden %>%
    bind_rows(d_pop %>%
                filter(burden_outcome == burden_t) %>%
                filter(year <= year_end) %>%
                filter(disease ==  dis_name) %>%
                filter(simple_scenario == "Postpone 2020 SIAs until 2021") %>%
                group_by(country_name, year, modelling_group_tidy) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          value_excess = 0,
                          pop = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "Business as usual"))
  
  d_burden <- d_burden %>%
    mutate(value_proportional = ifelse(abs(value)>=1, value_excess/value*100, 0))
  
  d_burden <- d_burden %>% 
    mutate(simple_scenario = factor(simple_scenario, 
                                    levels = c("Business as usual",
                                               "Postpone 2020 SIAs until 2021",
                                               "50% reduction in RI",
                                               
                                               "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  d_burden <-  d_burden %>% 
    mutate(thickline = ifelse(simple_scenario == "Business as usual", 1, 0))  %>% 
    mutate(alpha  = ifelse(simple_scenario == "Business as usual", TRUE , FALSE))
  
  p <- d_burden %>% 
    ggplot(aes(x = year, 
               color = simple_scenario, 
               y = value_proportional, 
               size = thickline, 
               alpha = alpha)) +
    geom_line() +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = paste0("% increase in ", R.utils::capitalize(burden_t))) +
    scale_color_manual("", values = scenario_pal) +
    scale_size(range = c(1.3, 0.1), guide = "none") +
    scale_alpha_manual(values = c(1, 0.5), guide = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))+
    guides(color = guide_legend(override.aes = list(size = 2)))
  
  p2 <- p + facet_grid(modelling_group_tidy ~ country_name,
                       scales = "free_y")
  ggsave(plot = p2, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  return(p2) 
}
#-------------------------------------------------------------------------------------------------------------
figure_maker_survival_timeline_per_disease_mod <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                                           year_end = 2030, dis_name = "Yellow fever",
                                                           file_name = "survival_change_by_year_YF", 
                                                           figheight = 10, figwidth = 12){
  
  stopifnot(burden_t == "deaths")
  
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country_name, year, modelling_group_tidy, simple_scenario) %>%
    summarise(focal_burden = sum(focal_burden, na.rm = TRUE),
              baseline_burden = sum(baseline_burden, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE))
  
  
  d_burden <- d_burden %>%
    mutate(value_survival = (((pop-focal_burden)/pop)-((pop-baseline_burden)/pop))*100)
  
  d_burden <- d_burden %>% 
    mutate(simple_scenario = factor(simple_scenario, 
                                    levels = c("Postpone 2020 SIAs until 2021",
                                               "50% reduction in RI",
                                               
                                               "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  d_burden <-  d_burden %>% 
    mutate(thickline = ifelse(simple_scenario == "Business as usual", 1, 0))  %>% 
    mutate(alpha  = ifelse(simple_scenario == "Business as usual", TRUE , FALSE))
  
  p <- d_burden %>% 
    ggplot(aes(x = year, 
               color = simple_scenario, 
               y = value_survival, 
               alpha = alpha)) +
    geom_line(size = 1) +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = "% change in survival") +
    scale_color_manual("", values = scenario_pal[-1]) +
    scale_alpha_manual(values = c(1, 0.5), guide = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))+
    guides(color = guide_legend(override.aes = list(size = 2)))
  
  p2 <- p + facet_grid(modelling_group_tidy ~ country_name,
                       scales = "free_y")
  
  ggsave(plot = p2, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  
  return(p2) 
}

#-------------------------------------------------------------------------------------------------------------
figure_maker_burden_ribbon <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030,
                                       file_name = "deaths_by_year_ribbon", 
                                       figheight = 10, figwidth = 16){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country_name, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE)) %>%
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs until 2021", 
                       year<=year_end) %>%
                group_by(country_name, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "Business as usual"))
  
  ## 
  d_burden <- 
    d_burden %>% ungroup() %>%
    group_by(year, country_name, disease) %>%
    mutate(value_min = min(value, na.rm = TRUE),
           value_max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, country_name, disease, simple_scenario) %>% 
    summarise(value_mean = mean(value, na.rm = TRUE),
              value_min = value_min[1],
              value_max = value_max[1])
  
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("Business as usual",
                                                                                       "Postpone 2020 SIAs until 2021",
                                                                                       "50% reduction in RI",
                                                                                       
                                                                                       "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  d_burden <- d_burden %>% mutate(thickline = ifelse(simple_scenario == "Business as usual", 1, 0))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      group_by(year, country_name, simple_scenario) %>%
      ggplot(aes(x = year, y = value_mean, ymin = value_min, ymax = value_max)) +
      
      geom_ribbon(fill = "grey90")+
      geom_line( aes(colour = simple_scenario, group = simple_scenario, size = thickline)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country_name, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
      scale_color_manual("", values = scenario_pal) +
      scale_size(range = c(1.3, 5), guide = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            text = element_text(size = 14), 
            legend.text = element_text(size=rel(1)), 
            legend.key.width = unit(1.5, "line"), 
            legend.spacing.x = unit(0.6, 'cm')) +
      ylim(0, NA)
    
    p
  }
  
  p1 <- burd_per_dis(d_burden, "Measles")+theme(legend.position = "none")
  p2 <- burd_per_dis(d_burden, "Meningitis A")+theme(legend.position = "none")
  p3 <- burd_per_dis(d_burden, "Yellow fever")+theme(legend.position = "none")
  leg <- get_legend(burd_per_dis(d_burden, "Measles"))
  
  p4 <- cowplot::plot_grid(NULL,p1,p2,p3,
                           leg,
                           ncol = 1,
                           rel_heights = c(0.1,1,1,1,0.1),
                           labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                           hjust = -0.1,
                           vjust = 0.1)
  
  ggsave(plot = p4, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
}
#------------------------------------------------------------------------------------------------------------------
figure_maker_burden_per_pop_ribbon <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030,
                                               file_name, figheight, figwidth){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country_name, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE)) %>%
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs until 2021", 
                       year<=year_end) %>%
                group_by(country_name, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          pop = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "Business as usual"))
  
  ## calculate burden per capita
  d_burden <- d_burden %>% mutate(value = value/pop*100000)
  
  ## 
  d_burden <- 
    d_burden %>% ungroup() %>%
    group_by(year, country_name, disease) %>%
    mutate(value_min = min(value, na.rm = TRUE),
           value_max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, country_name, disease, simple_scenario) %>% 
    summarise(value_mean = mean(value, na.rm = TRUE),
              value_min = value_min[1],
              value_max = value_max[1])
  
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("Business as usual",
                                                                                       "Postpone 2020 SIAs until 2021",
                                                                                       "50% reduction in RI",
                                                                                       
                                                                                       "50% reduction in RI, postpone 2020 SIAs until 2021")))
  
  d_burden <- d_burden %>% mutate(thickline = ifelse(simple_scenario == "Business as usual", 1, 0))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      group_by(year, country_name, simple_scenario) %>%
      ggplot(aes(x = year, y = value_mean, ymin = value_min, ymax = value_max)) +
      
      geom_ribbon(fill = "grey90")+
      geom_line( aes(colour = simple_scenario, group = simple_scenario, size = thickline)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country_name, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
      scale_color_manual("", values = scenario_pal) +
      scale_size(range = c(1.3, 5), guide = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            text = element_text(size = 14), 
            legend.text = element_text(size=rel(1)), 
            legend.key.width = unit(1.5, "line"), 
            legend.spacing.x = unit(0.6, 'cm')) 
    
    p
  }
  
  p1 <- burd_per_dis(d_burden, "Measles")+theme(legend.position = "none")
  p2 <- burd_per_dis(d_burden, "Meningitis A")+theme(legend.position = "none")
  p3 <- burd_per_dis(d_burden, "Yellow fever")+theme(legend.position = "none")
  leg <- get_legend(burd_per_dis(d_burden, "Measles"))
  
  p4 <- cowplot::plot_grid(NULL,p1,p2,p3,
                           leg,
                           ncol = 1,
                           rel_heights = c(0.1,1,1,1,0.1),
                           labels= c("","Measles", "Meningitis A", "Yellow Fever"),
                           hjust = -0.1,
                           vjust = 0.1)
  
  ggsave(plot = p4, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  
}

#-----------------------------------------------------------------------------------------------------------
figure_maker_excess_country_pop <- function(d_excess, burden_t = "deaths", scenario_pal, year_end = 2030,
                                            file_name, figheight, figwidth) {
  
  p <- d_excess %>%
    filter(burden_outcome == burden_t, year <= year_end)%>%
    group_by(disease) %>% 
    mutate(model_no = paste0("model_", as.integer(as.factor(modelling_group)))) %>% 
    group_by(simple_scenario, country_name, disease, model_no) %>%
    summarise(value_excess = -sum(impact, na.rm = TRUE), pop = sum(population, na.rm = TRUE)) %>% 
    mutate(value_excess_pop = value_excess/pop*1e5) %>%
    group_by(simple_scenario, country_name, disease) %>%
    mutate(value_excess_mean = mean(value_excess_pop, na.rm = TRUE),
           value_excess_min = min(value_excess_pop, na.rm = TRUE),
           value_excess_max = max(value_excess_pop, na.rm = TRUE)) %>% 
    
    
    ggplot(aes(y= value_excess_mean, x = country_name, fill = simple_scenario)) +
    geom_hline(yintercept = 0, color = "grey80")+
    geom_errorbar(aes(x=country_name, ymin=value_excess_min, ymax=value_excess_max, colour= simple_scenario), 
                  position = position_dodge(0.9), alpha=0.9, size=1.3)+
    facet_wrap(.~disease, scales = "free_y", ncol = 1)+
    theme_bw()+ 
    labs(x = "", y = paste0("Excess ", ifelse(burden_t == "dalys", "DALYs", burden_t), " per 100,000"), colour = "Scenario")+
    geom_vline(xintercept = 0, size = 0.5, colour = "black") +
    scale_colour_manual(values = scenario_pal[-1])+
    scale_fill_manual(values = scenario_pal[-1],  guide = FALSE )+
    scale_alpha_manual(values = c(0.5, 1), guide = FALSE)+
    #coord_flip() +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 20, hjust =1),
          legend.position = "bottom", 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))
  
  ggsave(plot = p, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
  
}

#------------------------------------------------------------------------------------------------------

figure_maker_norm_per_country_dis <- function(d, burden_t = "deaths",dis_name, scenario_pal, year_end = 2030,
                                              file_name, figheight, figwidth, shape_leg = FALSE) {
  
  shapes <- data.frame(group = unique(d_pop$modelling_group_tidy),
                       shape = c(0, 15, 1, 16, 2, 17, 23))
  
  if(!shape_leg){
    
    shapes <- shapes %>% filter(group %in% (d_pop %>% filter(disease==dis_name) %>% pull(modelling_group_tidy) %>% unique()))
    
    p <- d %>%
      
      bind_rows(d %>% 
                  filter(burden_outcome == burden_t, 
                         simple_scenario == "Postpone 2020 SIAs until 2021") %>%
                  group_by(country_name, year, disease, modelling_group) %>%
                  summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                  mutate(simple_scenario = "Business as usual", impact = 0, burden_outcome = burden_t)) %>%
      
      filter(burden_outcome == burden_t, year <= year_end, disease == dis_name)%>%
      group_by(simple_scenario, disease, country_name, modelling_group_tidy) %>%
      summarise(impact = -sum(impact, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(disease, country_name, modelling_group_tidy) %>% 
      mutate(max_impact = max(impact), min_impact = min(impact)) %>%
      ungroup() %>%
      group_by(simple_scenario, disease, country_name, modelling_group_tidy) %>%
      mutate(norm_impact = ifelse(abs(max_impact)> 1, (impact-0)/(max_impact-0), 0) ) %>% 
      
      mutate(simple_scenario = factor(simple_scenario, 
                                      levels = c("Business as usual",
                                                 "Postpone 2020 SIAs until 2021",
                                                 "50% reduction in RI",
                                                 "50% reduction in RI, postpone 2020 SIAs until 2021"))) %>%
      filter(!is.na(modelling_group_tidy)) %>%
      mutate(modelling_group_tidy = as.factor(modelling_group_tidy)) %>%
      
      ggplot()+
      geom_hline(yintercept = 0, colour = "grey50")+
      geom_jitter(size = 5,
                  height = 0,
                  width = 0.2,
                  stroke = 2)+
      aes(x = reorder(country_name, -norm_impact),
          y = norm_impact,
          colour = simple_scenario,
          shape = modelling_group_tidy) +
      theme_bw() +
      scale_colour_manual(values = scenario_pal[-1])+
      scale_shape_manual(values = shapes$shape, guide = guide_none())+
      labs(x = "Country",
           y = paste0("Normalised excess ", ifelse(burden_t == "dalys", "DALYs", burden_t)),
           colour = "Scenario", shape = "Modelling group") +
      theme(text = element_text(size = 14),
            legend.position = "bottom",
            legend.text = element_text(size=rel(1)), 
            strip.text.x = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggsave(plot = p, filename = paste0(file_name, "-1.png"), height = figheight, width = figwidth)
    
    
  } else {
    p <- d %>%
      ggplot()+
      aes(shape = modelling_group_tidy,
          x = country, y = impact)+
      geom_point(size = 5)+
      scale_shape_manual(values = shapes%>% arrange(group) %>% pull(shape))+
      labs(shape = "Modelling group")+
      theme_minimal()+
      theme(text = element_text(size = 14),
            legend.position = "bottom",
            legend.text = element_text(size=rel(1)), 
            strip.text.x = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = 20, hjust = 1))+ 
      guides(shape = guide_legend(nrow = 1))
      
  }
  
  return(p)
}

#--------------------------------------------------------------------------------------------------
figure_maker_under5 <- function(d_pop){
  p <- d_pop %>%
    filter(year<=2030)  %>%
    filter(grepl("deaths", burden_outcome)) %>%
    mutate(under5 = ifelse(grepl("under5", burden_outcome),
                           "<5",
                           "All ages")) %>%
    group_by(modelling_group_tidy, simple_scenario, disease, under5) %>%
    summarise(Excess = -sum(impact, na.rm = TRUE)) %>%
    mutate(Excess = round(Excess)) %>%
    mutate(disease = factor(disease, levels = rev(c("Measles", "Meningitis A", "Yellow fever")))) %>%
    mutate(mod_grp_dis = paste0(disease, ", ",modelling_group_tidy)) %>%
    
    ggplot()+
    geom_col(position = "dodge", color = "black")+
    aes(x = mod_grp_dis, y = Excess, fill = under5)+
    coord_flip()+
    facet_wrap(simple_scenario~., nrow = 2, scales = "free_y")+
    theme_minimal()+
    scale_fill_manual(values = c("black", "white"))+
    labs(y = "Excess deaths", x = "", fill = "Age" )
  
  ggsave(plot = p, filename = paste0("under_5s_deaths", "-1.png"), height = 10, width = 12)
}

