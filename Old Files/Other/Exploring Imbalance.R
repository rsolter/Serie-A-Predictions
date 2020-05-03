# Exploring Im/balance of results

source(file = "~/Personal_Git/Soccer_Prediction/SerieA/Processing_and_Engineering/engineering_seriea_design_matrix trailing.R") # runs in under 2 seconds

glimpse(DF_trailing)

results <- DF_trailing %>% select(Team,season,round,result)

results$result_new <- ifelse(results$result=="Team","Win",
                             ifelse(results$result=="Opp","Loss","Draw"))
results$result_new <- factor(results$result_new, levels=c("Win","Draw","Loss"))


ggplot(results, 
       aes(forcats::fct_reorder(Team, as.numeric(result_new)), 
           fill = result_new)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values=c("#7fc97f","#cbd5e8","#d53e4f")) +
  xlab(" ") + ylab(" ") + theme_minimal()


