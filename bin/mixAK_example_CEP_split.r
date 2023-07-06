setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("pacman")
pacman::p_load("lcmm", "mixAK", "traj", "kml3d", "dplyr")

# PBC910 dataset #
# 
# data("PBC910", package = "mixAK")
# tail(PBC910)[, c(1, 2, 3, 6:9)]
# 
# ip <- getProfiles(t = "month", y = c("lbili", "platelet", "spiders",  "jspiders"), id = "id", data = PBC910)
# # plotProfiles(ip = ip, data = PBC910, var = "lbili", tvar = "month", main = "Log(bilirubin)", highlight = c(1, length(ip)), xlab = "Time (months)", ylab = "Log(bilirubin)")
# 
# mod <- GLMM_MCMC(y = PBC910[, c("lbili", "platelet", "spiders")], 
#                                             dist = c("gaussian", "poisson(log)", "binomial(logit)"), 
#                                             id = PBC910[, "id"], 
#                                             x = list(lbili = "empty", platelet = "empty",  spiders = PBC910[, "month"]), 
#                                             z = list(lbili = PBC910[, "month"], platelet = PBC910[, "month"], spiders = "empty"), 
#                                             random.intercept = rep(TRUE, 3), 
#                                             prior.b = list(Kmax = 3), 
#                                             nMCMC = c(burn = 100, 
#                                             keep = 1000, 
#                                             thin = 10, 
#                                             info = 100), 
#                                             parallel = FALSE)
# 
# mod <- NMixRelabel(mod, 
#                                          type = "stephens", 
#                                          keep.comp.prob = TRUE)
# 
# delta <- 0.3
# tpred <- seq(0, 30, by = delta)
# fit <- fitted(mod[[1]], x = list("empty", "empty", tpred), z = list(tpred, tpred, "empty"), glmer = TRUE) 
# names(fit) <- c("lbili", "platelet", "spiders")
# 
# K <- mod[[1]]$prior.b$Kmax
# clCOL <- c("darkgreen", "red3")
# plotProfiles(ip = ip, 
#                          data = PBC910, 
#                          var = "lbili", 
#                          tvar = "month", 
#                          col = "azure3", 
#                          main = "Log(bilirubin)", 
#                          xlab = "Time (months)", 
#                          ylab = "Log(bilirubin)")
# for (k in 1:K) lines(tpred, fit[["lbili"]][, k], col = clCOL[k], lwd = 2)
# 
# # paquid dataset #
# 
ip_paquid <- getProfiles(t = "age", y = c("MMSE", "BVRT", "IST",  "CEP"), id = "ID", data = paquid)
# 
# # dataset load
# # https://cran.r-project.org/web/packages/lcmm/vignettes/pre_normalizing.html
# # paquid$"normMMSE" <- normMMSE(paquid$MMSE)

# resultsGenerator
resultsGenerator <- function(thisPaquidDataset, thisFlag) {

    mod_paquid <- GLMM_MCMC(y = thisPaquidDataset[, c("MMSE", "BVRT", "male")], 
                                                dist = c("gaussian", "poisson(log)", "binomial(logit)"), 
                                                id = thisPaquidDataset[, "ID"], 
                                                x = list(MMSE = "empty", BVRT = "empty",  male = thisPaquidDataset[, "age"]), 
                                                z = list(MMSE = thisPaquidDataset[, "age"], BVRT = thisPaquidDataset[, "age"], male = "empty"), 
                                                random.intercept = rep(TRUE, 3), 
                                                prior.b = list(Kmax = 2), 
                                                nMCMC = c(burn = 100, 
                                                keep = 1000, 
                                                thin = 10, 
                                                info = 100), 
                                                parallel = FALSE)

    mod_paquid <- NMixRelabel(mod_paquid, 
                                            type = "stephens", 
                                            keep.comp.prob = TRUE)
                                            
    delta <- 0.3
    tpred <- seq(0, 100, by = delta)
    fit_paquid <- fitted(mod_paquid[[1]], x = list("empty", "empty", tpred), z = list(tpred, tpred, "empty"), glmer = TRUE) 
    names(fit_paquid) <- c("MMSE", "BVRT", "male")

    K <- mod_paquid[[1]]$prior.b$Kmax
    clCOL <- c("darkgreen", "red3", "blue")
    plotProfiles(ip = ip_paquid, 
                            data = thisPaquidDataset, 
                            var = "MMSE", 
                            tvar = "age", 
                            col = "azure3", 
                            main = "Log(MMSE)", 
                            xlab = "Time (age)", 
                            ylab = "Log(MMSE)")
                            
    if(flag == 0)     for (k in 1:K) lines(tpred, fit_paquid[["MMSE"]][, k], col = clCOL[k], lwd = 2)
    if(flag == 1)     for (k in 1:K) lines(tpred, fit_paquid[["MMSE"]][, k], col = clCOL[k], lwd = 2, type = "p")
    
    return(fit_paquid)
}


paquid_CEP_zero <- paquid[which(paquid$"CEP"==0),]
paquid_CEP_one <- paquid[which(paquid$"CEP"==1),]

cat("\n\nresultsGenerator(paquid_CEP_zero):\n")
flag <- 0
fit_paquid_CEP_zero <- resultsGenerator(paquid_CEP_zero, flag)
cat("\n\nresultsGenerator(paquid_CEP_one):\n")
flag <- 1
fit_paquid_CEP_one <-resultsGenerator(paquid_CEP_one, flag)

delta <- 0.3
tpred <- seq(0, 100, by = delta)
K <- 2
clCOL <- c("darkgreen", "red3")
plotProfiles(ip = ip_paquid, 
                            data = paquid_CEP_zero, 
                            var = "MMSE", 
                            tvar = "age", 
                            col = "azure3", 
                            main = "Log(MMSE)", 
                            xlab = "Time (age)", 
                            ylab = "Log(MMSE)")                            
for (k in 1:K) lines(tpred, fit_paquid_CEP_zero[["MMSE"]][, k], col = clCOL[k], lwd = 2)
for (k in 1:K) lines(tpred, fit_paquid_CEP_one[["MMSE"]][, k], col = clCOL[k], lwd = 2, type = "p")


