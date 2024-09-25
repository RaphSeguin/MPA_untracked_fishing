describe_results <- function(){
 
   load("output/SAR_eez_stats.Rdata")
   load("output/SAR_stats.Rdata")
   
   SAR_eez_stats_unique <- SAR_eez_stats %>% distinct(unique_id, .keep_all = T)
   SAR_stats_unique <- SAR_stats %>% distinct(unique_id, .keep_all = T)
   
   #Number of total detections
   nrow(SAR_eez_stats_unique) + nrow(SAR_stats_unique)
  
   #Number of detections in MPAs
   nrow(SAR_stats_unique)
   
   #Percentage
   nrow(SAR_stats_unique)/(nrow(SAR_eez_stats_unique) + nrow(SAR_stats_unique))
   
   #Number of MPAs with fishing vessels
   nrow(MPA_final_vars %>% filter(SAR_presence == "SAR"))
   
   #Total of MPAs
   nrow(MPA_final_vars)
   
   #Percentage of MPAs with fishing vessels inside
   nrow(MPA_final_vars %>% filter(SAR_presence == "SAR"))/nrow(MPA_final_vars)
   
   #Number of MPAs with or without a management plan
   nrow(MPA_final_vars %>% filter(management_plan == "No management plan"))/nrow(MPA_final_vars)
   
   #Number of MPAs without an IUCN category
   nrow(MPA_final_vars %>% filter(!iucn_cat %in% c("I","II","III","IV","V","VI")))/nrow(MPA_final_vars)
   
   #Number of MPAs with fishing and without a management plan
   nrow(MPA_final_vars %>% filter(SAR_presence == "SAR" & management_plan == "No management plan"))/nrow(MPA_final_vars %>% filter(SAR_presence == "SAR"))
   
   #Number of MPAs with fishing without an IUCN category
   nrow(MPA_final_vars %>% filter(SAR_presence == "SAR" & !iucn_cat %in% c("I","II","III","IV","V","VI")))/nrow(MPA_final_vars %>% filter(SAR_presence == "SAR"))
   
   #Stat test EEZ- MPA
   EEZ_MPA_stat_test <- MPA_final_vars %>%
     dplyr::filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
     bind_rows(EEZ_final_vars %>% dplyr::rename(area_correct = "eez_area"))
   
   kruskal_test <- EEZ_MPA_stat_test %>%
     kruskal.test(relative_sum_all ~ iucn_cat, data = .) %>%
     tidy()
   
   pairwise_comparisons_density <- EEZ_MPA_stat_test %>%
     dunn_test(relative_sum_all ~ iucn_cat, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     ) %>%
     mutate_if(is.numeric, round, digits = 2)
   
   write.csv(pairwise_comparisons_density, file = "figures/supp/pairwise_comparisons_density.csv")
   
   pairwise_comparisons_unmatched <- EEZ_MPA_stat_test %>%
     dunn_test(unmatched_relative ~ iucn_cat, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     )%>%
     mutate_if(is.numeric, round, digits = 2)
   
   write.csv(pairwise_comparisons_unmatched, file = "figures/supp/pairwise_comparisons_unmatched.csv")
   
   pairwise_comparisons_fraction<- EEZ_MPA_stat_test %>%
     filter(unmatched_ratio > 0) %>%
     dunn_test(unmatched_ratio ~ iucn_cat, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     )%>%
     mutate_if(is.numeric, round, digits = 2)
   
   write.csv(pairwise_comparisons_fraction, file = "figures/supp/pairwise_comparisons_fraction.csv")
   
   #Size
   MPA_Size <- SAR_stats_unique %>%
     dplyr::filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
     group_by(iucn_cat, id_iucn) %>%
     reframe(average_length = mean(length_m, na.rm = T)) %>%
     ungroup() %>%
     bind_rows(EEZ_size)

   EEZ_MPA_size <- SAR_eez_stats %>%
     group_by(SOVEREIGN1) %>%
     reframe(average_length = mean(length_m, na.rm = T)) %>%
     ungroup() %>%
     mutate(iucn_cat = "Outside") %>%
     bind_rows(EEZ_MPA_Size)
   
   #Kruskal
   kruskal_test <- EEZ_MPA_Size %>%
     kruskal.test(average_length ~ iucn_cat, data = .) %>%
     tidy()

   #Dunn
   pairwise_comparison_size <- EEZ_MPA_Size %>%
     dunn_test(average_length ~ iucn_cat, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     )%>%
     mutate_if(is.numeric, round, digits = 2)
   
   write.csv(pairwise_comparison_size, file = "figures/supp/pairwise_comparison_size.csv")
   
   #Stat MPA managment plan
   MPA_management_stat_test <- MPA_final_vars %>%
     dplyr::filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
     mutate(relative_sum_all = log(relative_sum_all))
   
   pairwise_comparisons <- MPA_management_stat_test %>%
     dunn_test(relative_sum_all ~ management_plan, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     )
   
   pairwise_comparisons_unmatched <- MPA_management_stat_test %>%
     dunn_test(unmatched_relative ~ management_plan, p.adjust.method = "bonferroni") %>%
     mutate(
       comparison = paste0(group1, " vs ", group2),
       higher_group = ifelse(statistic > 0, group1, group2)
     )
   
   ggplot(MPA_management_stat_test, aes(management_plan, log(relative_sum_all))) + geom_boxplot()
   
   #Number of vessel detections
   range((MPA_final_vars %>% filter(SAR_presence == "SAR"))$sum_all)
   mean((MPA_final_vars %>% filter(SAR_presence == "SAR"))$sum_all)
   sd((MPA_final_vars %>% filter(SAR_presence == "SAR"))$sum_all)
   
   #Per IUCN cat
   MPA_final_vars %>%
     group_by(iucn_cat) %>%
     reframe(average_number = mean(sum_all),
             sd_number = sd(sum_all)) %>%
     ungroup() %>%
     arrange(desc(average_number))
   
   #Number of untracked fishing vessels
   (nrow(SAR_eez_stats_unique %>% filter(matched_category == "unmatched")) + nrow(SAR_stats_unique %>% filter(matched_category == "unmatched")))
   
   #Propotion of untracked fishing vessels
   ((nrow(SAR_eez_stats_unique %>% filter(matched_category == "unmatched")) + nrow(SAR_stats_unique %>% filter(matched_category == "unmatched"))))/(nrow(SAR_eez_stats_unique) + nrow(SAR_stats_unique))
   
   #Within MPAS
   nrow(SAR_stats_unique %>% filter(matched_category == "unmatched"))/nrow(SAR_stats_unique)
   
   #Within MPAs with fishing
   nrow(MPA_final_vars %>% filter(SAR_presence == "SAR" & unmatched_fishing > 0))
   
   #In proportion
   (nrow(MPA_final_vars %>% filter(SAR_presence == "SAR" & unmatched_fishing > 0)))/(nrow(MPA_final_vars %>% filter(SAR_presence == "SAR")))
   
   #Which MPAs have SAR but not unmatched fishing ? 
   MPA_SAR_no_unmatched <- MPA_final_vars %>% filter(SAR_presence == "SAR" & unmatched_fishing == 0)
   nrow(MPA_SAR_no_unmatched)
   
   #Cor
   cor(log(MPA_final_vars$fishing+1),log(MPA_final_vars$unmatched_fishing + 1))
   
   #Ratio of unmatched in Mpas
   range(MPA_final_vars$unmatched_ratio,na.rm=T)
   mean(MPA_final_vars$unmatched_ratio,na.rm=T)
   sd(MPA_final_vars$unmatched_ratio,na.rm=T)
   
   #Ratio of unmatched in EEZs
   range(EEZ_final_vars$unmatched_ratio,na.rm=T)
   mean(EEZ_final_vars$unmatched_ratio,na.rm=T)
   sd(EEZ_final_vars$unmatched_ratio,na.rm=T)
   
   #Per IUCN cat
   ratio_iucn <- MPA_final_vars %>%
     group_by(iucn_cat) %>%
     reframe(count = n(),
             average_ratio = mean(unmatched_ratio,na.rm=T),
             sd_ratio = sd(unmatched_ratio,na.rm=T)) %>%
     ungroup()
   
   #Number of MPAs with fishing and SAR
   fishing_AIS <- MPA_final_vars %>% 
     filter(sum_all > 0) %>%
     left_join(MPA_covariates %>% dplyr::select(id_iucn, AIS_fishing_2022, AIS_fishing_2023), by = "id_iucn") %>%
     mutate(fishing_AIS = AIS_fishing_2022 + AIS_fishing_2023,
            fishing_AIS_presence = as.factor(ifelse(fishing_AIS == 0, "No fishing","Fishing")))
   
   nrow(fishing_AIS %>% filter(fishing_AIS_presence == "Fishing"))
   nrow(fishing_AIS %>% filter(fishing_AIS_presence == "Fishing"))/nrow(fishing_AIS)
   
   nrow(fishing_AIS %>% filter(fishing_AIS_presence == "No fishing"))/nrow(fishing_AIS)
   
   #Correlation between fishing and number of vessels
   temp_cor <- fishing_AIS %>%
     filter(fishing > 0) %>%
     filter(fishing_AIS > 0)
   
   cor(log(temp_cor$fishing), log(temp_cor$fishing_AIS))
   
   #Number of MPAs with fishing in 2022 and 2023
   fishing_AIS_all <- mpa_model %>% 
     mutate(fishing_AIS = AIS_fishing_2022 + AIS_fishing_2023,
            fishing_AIS_presence = as.factor(ifelse(fishing_AIS == 0, "No fishing","Fishing")))
   
   nrow(fishing_AIS_all %>% filter(AIS_fishing_2022 > 0))/nrow(fishing_AIS_all)
   nrow(fishing_AIS_all %>% filter(AIS_fishing_2023 > 0))/nrow(fishing_AIS_all)
   
   sum(fishing_AIS_all$AIS_fishing_2022,na.rm=T)
   sum(fishing_AIS_all$AIS_fishing_2023,na.rm=T)
   sum(fishing_AIS_all$AIS_fishing_2022,na.rm=T) + sum(fishing_AIS_all$AIS_fishing_2023,na.rm=T)
   
   #Results on size
   range(SAR_stats_unique$length_m)
   mean(SAR_stats_unique$length_m)
   sd(SAR_stats_unique$length_m)
   
   iucn_size <- SAR_stats_unique %>%
     group_by(iucn_cat) %>%
      reframe(mean_size = mean(length_m),
             sd_size = sd(length_m)) %>%
     ungroup()
   
   range(SAR_eez_stats$length_m)
   mean(SAR_eez_stats$length_m)
   sd(SAR_eez_stats$length_m)
   
  
}

