setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("pacman")
pacman::p_load("NormPsy", "lcmm", "dplyr")

# dataset load
# https://cran.r-project.org/web/packages/lcmm/vignettes/pre_normalizing.html
paquid$"normMMSE" <- normMMSE(paquid$MMSE)

paquid$"age65" <- (paquid$"age" - 65)/10 # "#We recenter and scale the time variable "age" in order to avoid numerical problems"
# I think it's a way to avoid having all the age values as integer values greater than 65. All the ages in the original dataset are greater than 65.

# A data frame with 2250 observations over 500 subjects and 12 variables
# CEP: binary indicator of educational level (CEP=1 for subjects who graduated from primary school; CEP=0 otherwise)

paquid %>% head()

# Explanation of the parameters of the hlme() function
# https://cran.r-project.org/web/packages/lcmm/lcmm.pdf

# estimation of the model for 2 latent classes
model2classes <- hlme(
    fixed = normMMSE ~ age65 + I(age65^2) + CEP,  # two-sided linear formula object for the fixed-effects in the linear mixed model.
    mixture =~ age65 + I(age65^2), # one-sided formula object for the fixed effects in the linear mixed model (only for a number of latent classes greater than 1). 
    random =~ age65 + I(age65^2), # optional one-sided formula for the random-effects in the linear mixed model.
    subject = 'ID', # name of the covariate representing the grouping structure specified with ”
    data = paquid, # optional data frame containing the variables named in fixed, mixture, random, classmb and subject
    ng = 2, # optional number of latent classes considered. If ng=1 (by default) no mixture nor classmb should be specified. If ng>1, mixture is required.
    B = c(0, 60, 40, 0, -4, 0, -10, 10, 212.869397, -216.421323,456.229910, 55.713775, -145.715516, 59.351000, 10.072221) # optional specification for the initial values for the parameters.
)

# estimation of the model for 1 latent classes
model1class <- hlme(
    fixed = normMMSE ~ age65 + I(age65^2) + CEP,  # two-sided linear formula object for the fixed-effects in the linear mixed model.
    random =~ age65 + I(age65^2), # optional one-sided formula for the random-effects in the linear mixed model.
    subject = 'ID', # name of the covariate representing the grouping structure specified with ”
    data = paquid, # optional data frame containing the variables named in fixed, mixture, random, classmb and subject
    ng = 1 # optional number of latent classes considered. If ng=1 (by default) no mixture nor classmb should be specified. If ng>1, mixture is required.
)

# fixed formula: y = x + x^2 + k
# where x = age65 and k = CEP are covariates


# # The function computes the predicted values of the random effects given observed data provided in input. With multiple latent classes, these predictions are averaged over classes using the posterior class-membership probabilities.
# predictionsMatrix <-  predictRE(model1class,newdata=paquid[1:6,])
# predictionsMatrix %>% print()
# # a matrix containing the grouping structure and the predicted random-effects.

data_pred0 <- data.frame(age=seq(65,95,length.out=50),CEP=0)
data_pred1 <- data.frame(age=seq(65,95,length.out=50),CEP=1)
data_pred0$"age65" <- (data_pred0$age - 65)/10
data_pred1$"age65" <- (data_pred1$age - 65)/10

# > data_pred0 %>% head()
#        age CEP      age65
# 1 65.00000   0 0.00000000
# 2 65.61224   0 0.06122449
# 3 66.22449   0 0.12244898
# 4 66.83673   0 0.18367347
# 5 67.44898   0 0.24489796
# 6 68.06122   0 0.30612245
# > data_pred1 %>% head()
#        age CEP      age65
# 1 65.00000   1 0.00000000
# 2 65.61224   1 0.06122449
# 3 66.22449   1 0.12244898
# 4 66.83673   1 0.18367347
# 5 67.44898   1 0.24489796
# 6 68.06122   1 0.30612245


pred0 <- predictY(model2classes, data_pred0, var.time = "age")
pred1 <- predictY(model2classes, data_pred1, var.time = "age")

# predictY: 
# Predictions (marginal and possibly subject-specific in some cases) of a hlme, lcmm, multlcmm or Jointlcmm object in the natural scale of the longitudinal outcome(s) computed from a profile of covariates (marginal) or individual data (subject specific in case of hlme).
# - pred : a matrix with the same rows (number and order) as in newdata.

# Example with one class
# https://cran.r-project.org/web/packages/lcmm/vignettes/latent_class_model_with_hlme.html

plot(pred0, col=c("red","navy"), lty=1,lwd=5,ylab="normMMSE",legend=NULL,  main="Predicted trajectories for normMMSE ",ylim=c(0,100))
plot(pred1, col=c("red","navy"), lty=2,lwd=3,legend=NULL,add=TRUE)
# legend(x="topright",legend=c("class1 :","CEP-","CEP+","class2:","CEP-","CEP+"), col=c(rep("red",3),rep("navy",3)), lwd=2, lty=c(0,1,2,0,1,2), ncol=2, bty="n", cex = 0.7)
legend(x="topright",legend=c("class1:","CEP-","CEP+"), col=rep("red",3), lwd=2, lty=c(0,1,2,0,1,2), ncol=2, bty="n", cex = 0.7)
grid()

# predIC0 <- predictY(model2classes, data_pred0, var.time = "age",draws=TRUE)
# predIC1 <- predictY(model2classes, data_pred1, var.time = "age",draws=TRUE)
# plot(predIC0, col=c("deeppink","deepskyblue"), lty=1, lwd=2, ylab="normMMSE", main="Predicted trajectories for normMMSE", ylim=c(0,100), shades=TRUE)
# plot(predIC1, col=c("deeppink","deepskyblue"), lty=2, lwd=2, ylab="normMMSE", main="Predicted trajectories for normMMSE", legend=NULL, ylim=c(0,100), shades=TRUE, add=TRUE)
# grid()

