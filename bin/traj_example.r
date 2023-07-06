setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("pacman")
pacman::p_load("lcmm", "mixAK", "traj", "kml3d", "dplyr", "reshape2")


paquid %>% head()

paquid_ID_MMSE <- paquid %>% select("ID", "MMSE")

paquid_ID_MMSE$assessment_number <- ave(paquid$"age", paquid$"ID", FUN=seq_along) 

paquid_table_by_patient_original <- reshape(paquid_ID_MMSE, idvar = "ID", timevar = "assessment_number", direction = "wide")

paquid_table_by_patient <- paquid_table_by_patient_original %>% na.omit()

colnames(paquid_table_by_patient)[2:ncol(paquid_table_by_patient)] <- paste0("t",1:(ncol(paquid_table_by_patient)-1))

PATIENT_LIMIT <- 40
paquid_table_by_patient <- paquid_table_by_patient[c(1:PATIENT_LIMIT),]

paquid_table_by_time <- paquid_table_by_patient
for(i in c(1:ncol(paquid_table_by_time))) if(i >= 2) { paquid_table_by_time[,i] <- i-1 }

this_datatable <- c()
this_datatable$"data" <- paquid_table_by_patient
this_datatable$"time" <- paquid_table_by_time

# s1 <- step1measures(this_datatable$"data", this_datatable$"time", ID = TRUE)
s1 <- step1measures(this_datatable$"data",  ID = TRUE, verbose = TRUE)
s2 <- step2factors(s1)
s3 <- step3clusters(s2, nclusters = 2)


plot(s3)
plotMeanTraj(s3)
plotMedTraj(s3)
plotBoxplotTraj(s3)

