setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("pacman")
pacman::p_load("lcmm", "mixAK", "traj", "kml3d", "dplyr", "reshape2", "longitudinalData")


paquid_CEP_zero <- paquid[which(paquid$"CEP"==0),]
paquid_CEP_one <- paquid[which(paquid$"CEP"==1),]

cat("paquid_CEP_one\n")
paquid_ID_MMSE <- paquid_CEP_one %>% select("ID", "MMSE")
paquid_ID_MMSE$assessment_number <- ave(paquid_CEP_one$"age", paquid_CEP_one$"ID", FUN=seq_along) 
paquid_table_by_patient_original <- reshape(paquid_ID_MMSE, idvar = "ID", timevar = "assessment_number", direction = "wide")
paquid_table_by_patient <- paquid_table_by_patient_original %>% na.omit()

PATIENT_LIMIT <- 36
paquid_table_by_patient <- paquid_table_by_patient[c(1:PATIENT_LIMIT),]
paquid_table_by_patient <- paquid_table_by_patient %>% na.omit()

cat("example_kml:\n")
example_kml <- NULL
init <- (paquid_table_by_patient %>% nrow()) / 4
example_kml <- generateArtificialLongData(init) 

cat("rownames:\n")
rownames(example_kml@traj) <- paquid_table_by_patient$"ID"  %>% as.character()

NUM_ASSESSMENTS <- ncol(paquid_table_by_patient) - 1

cat("assignments:\n")
example_kml@idAll <- paquid_table_by_patient$"ID" %>% as.character()
example_kml@idFewNA <- paquid_table_by_patient$"ID" %>% as.character()
example_kml@time <- c(1:NUM_ASSESSMENTS)

example_kml@dimTraj[1] <- paquid_table_by_patient %>% nrow()
example_kml@dimTraj[2] <- NUM_ASSESSMENTS

example_kml@traj <- paquid_table_by_patient[, c(1:NUM_ASSESSMENTS+1)] %>% as.matrix()

cat("rownames:\n")
rownames(example_kml@traj) <- paste0("i", paquid_table_by_patient$"ID")

example_kml@idAll <- paste0("i", paquid_table_by_patient$"ID")
example_kml@idFewNA <-  paste0("i", paquid_table_by_patient$"ID")
example_kml@maxNA <-  0

time_list <- paste0("t", c(1:NUM_ASSESSMENTS))
colnames(example_kml@traj) <- time_list

cat("kml:\n")
kml(example_kml, nbClusters=2, nbRedrawing=2, toPlot="traj")

command <- "rm ./example_kml.Rdata"
system(command)
cat("Executed: ", command, "\n")
