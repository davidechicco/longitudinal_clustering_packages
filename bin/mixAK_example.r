setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

library("easypackages")
libraries("lcmm", "mixAK", "traj", "kml3d", "dplyr")

# PBC910 dataset #

data("PBC910", package = "mixAK")
tail(PBC910)[, c(1, 2, 3, 6:9)]

ip <- getProfiles(t = "month", y = c("lbili", "platelet", "spiders",  "jspiders"), id = "id", data = PBC910)
# plotProfiles(ip = ip, data = PBC910, var = "lbili", tvar = "month", main = "Log(bilirubin)", highlight = c(1, length(ip)), xlab = "Time (months)", ylab = "Log(bilirubin)")

mod <- GLMM_MCMC(y = PBC910[, c("lbili", "platelet", "spiders")], 
                                            dist = c("gaussian", "poisson(log)", "binomial(logit)"), 
                                            id = PBC910[, "id"], 
                                            x = list(lbili = "empty", platelet = "empty",  spiders = PBC910[, "month"]), 
                                            z = list(lbili = PBC910[, "month"], platelet = PBC910[, "month"], spiders = "empty"), 
                                            random.intercept = rep(TRUE, 3), 
                                            prior.b = list(Kmax = 2), 
                                            nMCMC = c(burn = 100, 
                                            keep = 1000, 
                                            thin = 10, 
                                            info = 100), 
                                            parallel = FALSE)

mod <- NMixRelabel(mod, 
                                         type = "stephens", 
                                         keep.comp.prob = TRUE)

delta <- 0.3
tpred <- seq(0, 30, by = delta)
fit <- fitted(mod[[1]], x = list("empty", "empty", tpred), z = list(tpred, tpred, "empty"), glmer = TRUE) 
names(fit) <- c("lbili", "platelet", "spiders")

K <- mod[[1]]$prior.b$Kmax
clCOL <- c("darkgreen", "red3")
plotProfiles(ip = ip, 
                         data = PBC910, 
                         var = "lbili", 
                         tvar = "month", 
                         col = "azure3", 
                         main = "Log(bilirubin)", 
                         xlab = "Time (months)", 
                         ylab = "Log(bilirubin)")
for (k in 1:K) lines(tpred, fit[["lbili"]][, k], col = clCOL[k], lwd = 2)

# paquid dataset #

ip_paquid <- getProfiles(t = "age", y = c("MMSE", "BVRT", "IST",  "CEP"), id = "ID", data = paquid)

# dataset load
# https://cran.r-project.org/web/packages/lcmm/vignettes/pre_normalizing.html
paquid$"normMMSE" <- normMMSE(paquid$MMSE)


mod_paquid <- GLMM_MCMC(y = paquid[, c("MMSE", "BVRT", "CEP")], 
                                            dist = c("gaussian", "poisson(log)", "binomial(logit)"), 
                                            id = paquid[, "ID"], 
                                            x = list(MMSE = "empty", BVRT = "empty",  CEP = paquid[, "age"]), 
                                            z = list(MMSE = paquid[, "age"], BVRT = paquid[, "age"], CEP = "empty"), 
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
names(fit_paquid) <- c("MMSE", "BVRT", "CEP")

K <- mod_paquid[[1]]$prior.b$Kmax
clCOL <- c("darkgreen", "red3")
plotProfiles(ip = ip_paquid, 
                         data = paquid, 
                         var = "MMSE", 
                         tvar = "age", 
                         col = "azure3", 
                         main = "Log(MMSE)", 
                         xlab = "Time (age)", 
                         ylab = "Log(MMSE)")
for (k in 1:K) lines(tpred, fit[["MMSE"]][, k], col = clCOL[k], lwd = 2)
