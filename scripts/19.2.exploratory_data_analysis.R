## --- exploratory data analysis --- ##

# read the data set into R
ch4 <- read_xlsx(here("data", "DNA methylation data.xlsm"), sheet = 1)

# explore the first few rows
head(ch4)
