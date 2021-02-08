
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

figure_maker_burden_per_mod_dis <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                            year_end = 2030, dis_name = "Yellow fever"){
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country, year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE))
  
  d_burden <- d_burden %>%
    bind_rows(d_pop %>%
                filter(burden_outcome == burden_t) %>%
                filter(year <= year_end) %>%
                filter(disease ==  dis_name) %>%
                filter(simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, modelling_group_tidy) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       "50% RI",
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  p <- d_burden %>% 
    ggplot(aes(x = year, color = simple_scenario, y = value)) +
    geom_line(size = 1.3) +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
    scale_color_manual("", values = scenario_pal) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm')) +
    scale_y_log10()
  
  p + facet_grid(modelling_group_tidy ~ country)
  
  
}
#--------------------------------------------------------------------------------------------------#
figure_maker_burden_per_mod_dis_pop <- function(d_pop, burden_t = "deaths", scenario_pal, 
                                            year_end = 2030, dis_name = "Yellow fever"){
  d_burden <- 
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    filter(disease ==  dis_name) %>%
    group_by(country, year, modelling_group_tidy, simple_scenario) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE))
  
  d_burden <- d_burden %>%
    bind_rows(d_pop %>%
                filter(burden_outcome == burden_t) %>%
                filter(year <= year_end) %>%
                filter(disease ==  dis_name) %>%
                filter(simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, modelling_group_tidy) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE),
                          pop = sum(population, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       "50% RI",
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  
  ## calculate burden per capita
  d_burden <- d_burden %>% mutate(value = value/pop*100000)
  
  p <- d_burden %>% 
    ggplot(aes(x = year, color = simple_scenario, y = value)) +
    geom_line(size = 1.3) +
    
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    theme_bw()+
    
    labs(x = "Year", y = paste0(R.utils::capitalize(burden_t), " per 100,000")) +
    scale_color_manual("", values = scenario_pal) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          text = element_text(size = 14), 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm')) 
  
  p + facet_grid(modelling_group_tidy ~ country)
  
  
}


figure_maker_burden <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE)) %>%
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021", 
                       year<=year_end) %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  ## label modelling groups 
  d_burden <- 
    d_burden %>%
    group_by(disease) %>% 
    mutate(model_no = paste0("model_", as.integer(as.factor(modelling_group)))) %>% 
    group_by(year, country, disease, simple_scenario) %>% 
    mutate(value_mean = mean(value, na.rm = TRUE)) %>% 
    select(-modelling_group) %>% 
    pivot_wider(names_from = model_no, values_from = value)
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       "50% RI",
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      ggplot(aes(x = year, colour = simple_scenario)) +
      geom_line(aes(y = model_1),
                linetype = "dashed") +
      geom_line(aes(y = model_2),
                linetype = "dashed") +
      
      geom_line(size = 1.3, aes(y = value_mean)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
      scale_color_manual("", values = scenario_pal) +
      scale_fill_manual("", values = scenario_pal) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            text = element_text(size = 14), 
            legend.text = element_text(size=rel(1)), 
            legend.key.width = unit(1.5, "line"), 
            legend.spacing.x = unit(0.6, 'cm')) 
    
    if("model_3" %in% names(d_burd) & "model_4" %in% names(d_burd)){
      return(p+geom_line(aes(y = model_3),
                         linetype = "dashed")+
               geom_line(aes(y = model_4),
                         linetype = "dashed"))
    } else {
      return(p)
    }
  }
  
  p1 <- burd_per_dis(d_burden, "Measles")+theme(legend.position = "none")
  p2 <- burd_per_dis(d_burden, "Meningitis A")+theme(legend.position = "none")
  p3 <- burd_per_dis(d_burden, "Yellow fever")+theme(legend.position = "none")
  leg <- get_legend(burd_per_dis(d_burden, "Measles"))
  
  cowplot::plot_grid(p1,p2,p3,
                     leg,
                     ncol = 1,
                     rel_heights = c(1,1,1,0.1),
                     labels= c("A", "B", "C"),
                     hjust = 0)
  
}


figure_maker_burden_pop <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE), 
              pop = sum(population, na.rm = TRUE)) %>%
    
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021", 
                       year<=year_end) %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE), 
                          pop = sum(population, na.rm=TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  ## calculate burden per capita
  d_burden <- d_burden %>% mutate(burden_per_pop = value/pop*100000)
  
  ## label modelling groups 
  d_burden <- 
    d_burden %>%
    group_by(disease) %>% 
    mutate(model_no = paste0("model_", as.integer(as.factor(modelling_group)))) %>% 
    group_by(year, country, disease, simple_scenario) %>% 
    mutate(value_mean = mean(burden_per_pop, na.rm = TRUE)) %>% 
    ungroup() %>%
    select(-c(modelling_group, value)) %>% 
    pivot_wider(names_from = model_no, values_from = burden_per_pop)
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       
                                                                                       "50% RI", 
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      ggplot(aes(x = year, colour = simple_scenario)) +
      geom_line(aes(y = model_1),
                linetype = "dashed") +
      geom_line(aes(y = model_2),
                linetype = "dashed") +
      
      geom_line(size = 1.3, aes(y = value_mean)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t), " per 100,000")) +
      scale_color_manual("", values = scenario_pal) +
      scale_fill_manual("", values = scenario_pal) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            text = element_text(size = 14), 
            legend.text = element_text(size=rel(1)), 
            legend.key.width = unit(1.5, "line"), 
            legend.spacing.x = unit(0.6, 'cm')) 
    
    if("model_3" %in% names(d_burd) & "model_4" %in% names(d_burd)){
      return(p+geom_line(aes(y = model_3),
                         linetype = "dashed")+
               geom_line(aes(y = model_4),
                         linetype = "dashed"))
    } else {
      return(p)
    }
  }
  
  p1 <- burd_per_dis(d_burden, "Measles")+theme(legend.position = "none")
  p2 <- burd_per_dis(d_burden, "Meningitis A")+theme(legend.position = "none")
  p3 <- burd_per_dis(d_burden, "Yellow fever")+theme(legend.position = "none")
  leg <- get_legend(burd_per_dis(d_burden, "Measles"))
  
  cowplot::plot_grid(p1,p2,p3,
                     leg,
                     ncol = 1,
                     rel_heights = c(1,1,1,0.1),
                     labels= c("A", "B", "C"),
                     hjust = 0)
  
}

figure_maker_burden_ribbon <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE)) %>%
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021", 
                       year<=year_end) %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  ## 
  d_burden <- 
    d_burden %>% ungroup() %>%
    group_by(year, country, disease) %>%
    mutate(value_min = min(value, na.rm = TRUE),
           value_max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, country, disease, simple_scenario) %>% 
    summarise(value_mean = mean(value, na.rm = TRUE),
              value_min = value_min[1],
              value_max = value_max[1])
    
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       "50% RI",
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      group_by(year, country, simple_scenario) %>%
      ggplot(aes(x = year, y = value_mean, ymin = value_min, ymax = value_max)) +
      
      geom_ribbon(fill = "grey90")+
      geom_line(size = 1.3, aes(colour = simple_scenario, group = simple_scenario)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t))) +
      scale_color_manual("", values = scenario_pal) +
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
  
  cowplot::plot_grid(p1,p2,p3,
                     leg,
                     ncol = 1,
                     rel_heights = c(1,1,1,0.1),
                     labels= c("A", "B", "C"),
                     hjust = 0)
  
}

figure_maker_burden_per_pop_ribbon <- function(d_pop, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d_burden <-
    d_pop %>%
    filter(burden_outcome == burden_t) %>%
    filter(year <= year_end) %>%
    group_by(country, year, modelling_group, simple_scenario, disease) %>%
    summarise(value = sum(focal_burden, na.rm = TRUE),
              pop = sum(population, na.rm = TRUE)) %>%
    bind_rows(d_pop %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021", 
                       year<=year_end) %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU"))
  
  ## calculate burden per capita
  d_burden <- d_burden %>% mutate(value = value/pop*100000)
  
  ## 
  d_burden <- 
    d_burden %>% ungroup() %>%
    group_by(year, country, disease) %>%
    mutate(value_min = min(value, na.rm = TRUE),
           value_max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, country, disease, simple_scenario) %>% 
    summarise(value_mean = mean(value, na.rm = TRUE),
              value_min = value_min[1],
              value_max = value_max[1])
  
  
  d_burden <- d_burden %>% mutate(simple_scenario = factor(simple_scenario, levels = c("BAU", 
                                                                                       "50% RI",
                                                                                       "Postpone 2020 SIAs -> 2021",
                                                                                       
                                                                                       "50% RI, postpone 2020 SIAs -> 2021")))
  
  burd_per_dis <- function(d_burd, dis_name){                                                                                                                                                                         
    p <- d_burd %>% 
      filter(disease == dis_name) %>%
      group_by(year, country, simple_scenario) %>%
      ggplot(aes(x = year, y = value_mean, ymin = value_min, ymax = value_max)) +
      
      geom_ribbon(fill = "grey90")+
      geom_line(size = 1.3, aes(colour = simple_scenario, group = simple_scenario)) +
      
      scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
      theme_bw()+
      facet_wrap(.~country, scales = "free_y", ncol = 6) +
      labs(x = "Year", y = paste0(R.utils::capitalize(burden_t), " per 100,000")) +
      scale_color_manual("", values = scenario_pal) +
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
  
  cowplot::plot_grid(p1,p2,p3,
                     leg,
                     ncol = 1,
                     rel_heights = c(1,1,1,0.1),
                     labels= c("A", "B", "C"),
                     hjust = 0)
  
}

figure_maker_excess <- function(d, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d %>%
    filter(burden_outcome == burden_t, year <= year_end) %>%
    group_by(year, country,  simple_scenario, disease) %>%
    summarise(impact = sum(impact, na.rm = TRUE)) %>%
    
    ggplot() +
    geom_line(size = 2) +
    aes(x = year,
        y = -impact, # as we want excess deaths/dalys
        colour = simple_scenario)+
    theme_bw()+
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    facet_wrap(disease~country, scales = "free_y", ncol = 5) +
    labs(x = "Year", y = paste0("Excess ", ifelse(burden_t == "dalys", "DALYs", burden_t)), colour = "Scenario") +
    scale_color_manual(values = scenario_pal[-1])+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size = 14),
          legend.position = "bottom", 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))
  
  
}


figure_maker_excess_pop <- function(d, burden_t = "deaths", scenario_pal, year_end = 2030){
  
  d %>%
    filter(burden_outcome == burden_t, year <= year_end) %>%
    group_by(year, country,  simple_scenario, disease) %>%
    summarise(impact = mean(impact, na.rm = TRUE), pop = mean(population,na.rm = TRUE)) %>%
    mutate(impact_per_pop=impact/pop*100000) %>%           
    
    ggplot() +
    geom_line(size = 2) +
    aes(x = year,
        y = -impact_per_pop, # as we want excess deaths/dalys
        colour = simple_scenario)+
    theme_bw()+
    scale_x_continuous( breaks = seq(min(d$year), year_end, by = 5), limits = c(min(d$year), year_end))+
    facet_wrap(disease~country, scales = "free_y", ncol = 5) +
    labs(x = "Year", y = paste0("Excess ", ifelse(burden_t == "dalys", "DALYs", burden_t), " per 100,000"), colour = "Scenario") +
    scale_color_manual(values = scenario_pal[-1])+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14),
          legend.position = "bottom", 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))
  
  
}


figure_maker_excess_country <- function(d_excess, burden_t = "deaths", scenario_pal, year_end = 2030) {
  
  d_excess %>%
    filter(burden_outcome == burden_t, year <= year_end)%>%
    group_by(disease) %>% 
    mutate(model_no = paste0("model_", as.integer(as.factor(modelling_group)))) %>% 
    group_by(simple_scenario, country, disease, model_no) %>%
    summarise(value_excess = -sum(impact, na.rm = TRUE)) %>% 
    group_by(simple_scenario, country, disease) %>%
    mutate(value_excess_mean = mean(value_excess, na.rm = TRUE),
           value_excess_min = min(value_excess, na.rm = TRUE),
           value_excess_max = max(value_excess, na.rm = TRUE)) %>% 
    
    
    ggplot(aes(y= value_excess_mean, x = country, fill = simple_scenario)) +
    geom_hline(yintercept = 0, color = "grey80")+
    geom_bar(stat = "identity", position = "dodge", alpha = 0.3) +
    geom_errorbar(aes(x=country, ymin=value_excess_min, ymax=value_excess_max, colour= simple_scenario), 
                  position = position_dodge(0.9), alpha=0.9, size=1.3)+
    facet_wrap(.~disease, scales = "free_y", ncol = 1)+
    theme_bw()+ 
    labs(x = "", y = paste0("Excess ", ifelse(burden_t == "dalys", "DALYs", burden_t)), colour = "Scenario")+
    geom_vline(xintercept = 0, size = 0.5, colour = "black") +
    scale_colour_manual(values = scenario_pal[-1])+
    scale_fill_manual(values = scenario_pal[-1],  guide = FALSE )+
    scale_alpha_manual(values = c(0.5, 1), guide = FALSE) +
    theme(text = element_text(size = 14),
          legend.position = "bottom", 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))
  
  
  
}

figure_maker_excess_country_pop <- function(d_excess, burden_t = "deaths", scenario_pal, year_end = 2030) {
  
  d_excess %>%
    filter(burden_outcome == burden_t, year <= year_end)%>%
    group_by(disease) %>% 
    mutate(model_no = paste0("model_", as.integer(as.factor(modelling_group)))) %>% 
    group_by(simple_scenario, country, disease, model_no) %>%
    summarise(value_excess = -sum(impact, na.rm = TRUE), pop = sum(population, na.rm = TRUE)) %>% 
    mutate(value_excess_pop = value_excess/pop*1e5) %>%
    group_by(simple_scenario, country, disease) %>%
    mutate(value_excess_mean = mean(value_excess_pop, na.rm = TRUE),
           value_excess_min = min(value_excess_pop, na.rm = TRUE),
           value_excess_max = max(value_excess_pop, na.rm = TRUE)) %>% 
    
    
    ggplot(aes(y= value_excess_mean, x = country, fill = simple_scenario)) +
    geom_hline(yintercept = 0, color = "grey80")+
    geom_bar(stat = "identity", position = "dodge", alpha = 0.3) +
    geom_errorbar(aes(x=country, ymin=value_excess_min, ymax=value_excess_max, colour= simple_scenario), 
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
          legend.position = "bottom", 
          legend.text = element_text(size=rel(1)), 
          legend.key.width = unit(1.5, "line"), 
          legend.spacing.x = unit(0.6, 'cm'))
  
  
  
}

figure_maker_norm <- function(d, burden_t = "deaths", scenario_pal, year_end = 2030) {
  
  d %>%
    
    bind_rows(d %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU", impact = 0, burden_outcome = burden_t)) %>%
    
    
    filter(burden_outcome == burden_t, year <= year_end)%>%
    group_by(simple_scenario, disease) %>%
    summarise(impact = -sum(impact, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(disease) %>% 
    mutate(max_impact = max(impact), min_impact = min(impact)) %>%
    ungroup() %>%
    group_by(simple_scenario, disease) %>%
    mutate(norm_impact = (impact-0)/(max_impact-0) ) %>%
    
    mutate(simple_scenario = factor(simple_scenario, 
                                    levels = c("BAU", "50% RI", "Postpone 2020 SIAs -> 2021", "50% RI, postpone 2020 SIAs -> 2021"))) %>%
    
    ggplot()+
    geom_point(size = 5)+
    geom_line(size = 1)+
    aes(x = disease,
        y = norm_impact,
        colour = simple_scenario,
        group = simple_scenario) +
    theme_bw() +
    scale_colour_manual(values = scenario_pal)+
    labs(x = "Disease",
         y = paste0("Normalised excess ", ifelse(burden_t == "dalys", "DALYs", burden_t)),
         colour = "Scenario") +
    theme(text = element_text(size = 14),
          legend.position = "bottom")
  
}

figure_maker_norm_per_country <- function(d, burden_t = "deaths", scenario_pal, year_end = 2030) {
  
  d %>%
    
    bind_rows(d %>% 
                filter(burden_outcome == burden_t, 
                       simple_scenario == "Postpone 2020 SIAs -> 2021") %>%
                group_by(country, year, disease, modelling_group) %>%
                summarise(value = sum(baseline_burden, na.rm = TRUE)) %>%
                mutate(simple_scenario = "BAU", impact = 0, burden_outcome = burden_t)) %>%
    
    filter(burden_outcome == burden_t, year <= year_end)%>%
    group_by(simple_scenario, disease, country) %>%
    summarise(impact = -sum(impact, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(disease, country) %>% 
    mutate(max_impact = max(impact), min_impact = min(impact)) %>%
    ungroup() %>%
    group_by(simple_scenario, disease, country) %>%
    mutate(norm_impact = (impact-0)/(max_impact-0) ) %>%
    
    mutate(simple_scenario = factor(simple_scenario, 
                                    levels = c("BAU", "50% RI", "Postpone 2020 SIAs -> 2021", "50% RI, postpone 2020 SIAs -> 2021"))) %>%
    
    ggplot()+
    geom_point(size = 5)+
    geom_line(size = 1)+
    aes(x = reorder(country, -norm_impact),
        y = norm_impact,
        colour = simple_scenario,
        group = simple_scenario) +
    theme_bw() +
    facet_wrap(disease~., scales = "free_x")+
    scale_colour_manual(values = scenario_pal)+
    labs(x = "Country",
         y = paste0("Normalised excess ", ifelse(burden_t == "dalys", "DALYs", burden_t)),
         colour = "Scenario") +
    theme(text = element_text(size = 14),
          legend.position = "bottom",
          legend.text = element_text(size=rel(1)), 
          strip.text.x = element_text(face = "bold", size = 12))
  
}
