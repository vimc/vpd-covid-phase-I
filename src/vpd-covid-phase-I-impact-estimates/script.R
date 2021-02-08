# prepare impact recipes - method0
meta_0 <- vimpact:::get_meta_from_recipe(default_recipe = FALSE, con = con, recipe = "recipe/method0/impact_recipe.csv")

# calculate cross-view impact estimates
meta <- split(meta_0, meta_0$index)
## deaths
deaths <- lapply(meta, function(meta1) vimpact:::get_raw_impact_details(con = con, meta1, burden_outcome = "deaths"))

deaths <- do.call(rbind, deaths)

# under_5
deaths_under5 <- lapply(meta, function(meta1) vimpact:::get_raw_impact_details(con = con, meta1, burden_outcome = "deaths", 
                                                                               is_under5 = TRUE))

deaths_under5 <- do.call(rbind, deaths_under5) 

deaths_under5 <- deaths_under5 %>% mutate(burden_outcome = "deaths_under5")

## dalys
dalys <- lapply(meta, function(meta1) vimpact:::get_raw_impact_details(con = con, meta1, burden_outcome = "dalys"))

dalys <- do.call(rbind, dalys)

#under 5
dalys_under5 <- lapply(meta, function(meta1) vimpact:::get_raw_impact_details(con = con, meta1, burden_outcome = "dalys",
                                                                              is_under5 = TRUE))

dalys_under5 <- do.call(rbind, dalys_under5)

dalys_under5 <- dalys_under5 %>% mutate(burden_outcome = "dalys_under5")

## binding
dat <- rbind(deaths, deaths_under5, dalys, dalys_under5)

## standardise output
scenario_description <- DBI::dbGetQuery(con, "SELECT scenario.id, scenario.scenario_description, scenario_description.description
                                        FROM scenario
                                        JOIN scenario_description
                                        ON scenario_description.id = scenario.scenario_description
                                        WHERE touchstone = $1", meta_0$touchstone[1L])

meta_0$scenario_description <- scenario_description$description[match(meta_0$scenario, scenario_description$id)]
                                                                
meta_tmp <- meta_0[meta_0$meta_type == "focal", ][c("touchstone", "disease", "modelling_group", "index", "scenario_type","scenario_description")]
dat <- vimpact:::merge_by_common_cols(dat, meta_tmp) 

country_code <- DBI::dbReadTable(con, "country")
dat$country <- country_code$id[match(dat$country, country_code$nid)]

names(dat) <- c("index", "country", "year", "baseline_burden", "focal_burden", 
                "impact", "burden_outcome", "touchstone", "disease", "modelling_group", "scenario_type",
                "scenario_description")

saveRDS(meta_0, "meta.rds")
saveRDS(dat, "impact.rds")

