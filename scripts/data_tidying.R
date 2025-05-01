# view column names
colnames(ch4)

# cleaning column names
ch4 <- janitor::clean_names(ch4)

# removing sample and age_category columns from data frame
ch4 = subset(ch4, select = -c(sample, age_category))
