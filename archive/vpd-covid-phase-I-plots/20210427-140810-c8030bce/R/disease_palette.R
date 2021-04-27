disease_palette <- function(disease){
  vimc_colours <- c("#466EE7",
                    "#E69942",
                    "#EF4050",
                    "#E7BF44",
                    "#3EEFE1",
                    "#9573B5",
                    "#F3DFA2",
                    "#645CD1",
                    "#A1D15C",
                    "#D15B8F",
                    "#6F3B3A",
                    "#EFE13E"
  )
  
  col_df <- data.frame(disease = c("Cholera",
                                   "JE",
                                   "Measles",
                                   "HepB",
                                   "Hib",
                                   "HPV",
                                   "MenA",
                                   "PCV",
                                   "Rota",
                                   "Rubella",
                                   "Typhoid",
                                   "YF"
  ), colour = vimc_colours
  )
  
  #arrange to diseases
  vimc_colours <- vimc_colours[col_df$disease%in% disease]
  col_df <- col_df[col_df$disease%in% disease,]
  
  #arrange alphabetically
  vimc_colours <- vimc_colours[order(col_df$disease)]
  col_df <- col_df[order(col_df$disease),]
  
  return(list(pal = vimc_colours, col_df=col_df))
}
