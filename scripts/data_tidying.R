# view column names
colnames(ch4)

# cleaning column names
ch4 <- janitor::clean_names(ch4)

# removing sample and age_category columns from data frame
ch4 = subset(ch4, select = -c(sample, age_category))

# missing daat
ch4 <- ch4 |> 
  drop_na(age, cp_g_1_tet2, cp_g_2_tet2, cp_g_3_tet2, cp_g_4_tet2, cp_g_gria2_1, cp_g_gria2_2, aspa_1)
# What we should see is that it was the same two observations missing all three variables - this makes sense logically - two penguins failed to have their measurements taken or their notes were misplaced.    