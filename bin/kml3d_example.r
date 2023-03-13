setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

library("easypackages")
libraries("lcmm", "mixAK", "traj", "kml3d", "dplyr", "reshape2")

paquid %>% head()

paquid_ID_MMSE <- paquid %>% select("ID", "MMSE")

paquid_ID_MMSE$assessment_number <- ave(paquid$"age", paquid$"ID", FUN=seq_along) 

paquid_table_by_patient_original <- reshape(paquid_ID_MMSE, idvar = "ID", timevar = "assessment_number", direction = "wide")

paquid_table_by_patient <- paquid_table_by_patient_original %>% na.omit()

PATIENT_LIMIT <- 40
paquid_table_by_patient <- paquid_table_by_patient[c(1:PATIENT_LIMIT),]

example_kml <- NULL
init <- (paquid_table_by_patient %>% nrow()) / 4
example_kml <- generateArtificialLongData(init) 
rownames(example_kml@traj) <- paquid_table_by_patient$"ID"  %>% as.character()

NUM_ASSESSMENTS <- ncol(paquid_table_by_patient) - 1

example_kml@idAll <- paquid_table_by_patient$"ID" %>% as.character()
example_kml@idFewNA <- paquid_table_by_patient$"ID" %>% as.character()
example_kml@time <- c(1:NUM_ASSESSMENTS)

example_kml@dimTraj[1] <- paquid_table_by_patient %>% nrow()
example_kml@dimTraj[2] <- NUM_ASSESSMENTS

example_kml@traj <- paquid_table_by_patient[, c(1:NUM_ASSESSMENTS+1)] %>% as.matrix()

rownames(example_kml@traj) <- paste0("i", paquid_table_by_patient$"ID")

example_kml@idAll <- paste0("i", paquid_table_by_patient$"ID")
example_kml@idFewNA <-  paste0("i", paquid_table_by_patient$"ID")
example_kml@maxNA <-  0

time_list <- paste0("t", c(1:NUM_ASSESSMENTS))
colnames(example_kml@traj) <- time_list

kml(example_kml, nbClusters=3, nbRedrawing=2, toPlot="traj")
