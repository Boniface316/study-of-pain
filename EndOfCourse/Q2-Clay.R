library(readxl)
library(data.table)
library(stringr)


biomarkerFile <- "biomarkers.xlsx"
covariateFile <- "covariates.xlsx"

biomarkerFilePath <- paste0(getwd(),"/data/",biomarkerFile)
covariateFilePath <- paste0(getwd(),"/data/",covariateFile)

x1 <- read_xlsx(biomarkerFilePath)
x2 <- read_xlsx(covariateFilePath)

#x1 <- read_xlsx(path = "~/Downloads/mscassignment/biomarkers.xlsx", sheet = "NPX (Page 2)")
x1 <- as.data.table(x1)
#x2 <- read_xlsx(path = "~/Downloads/mscassignment/covariates.xlsx", sheet = "Ark1")
x2 <- as.data.table(x2)

# more informative categorical variables:
setnames(x2, c("Sex (1=male, 2=female)", "Smoker (1=yes, 2=no)"), c("Sex", "Smoker"))
x2[, Sex := factor(ifelse(Sex == 1, "male", "female"))]
x2[, Smoker := factor(ifelse(Smoker == 1, "yes", "no"))]


#clean up the data
x1[, c("PatientID", "time") := tstrsplit(Biomarker, "-")]
x1[, PatientID := as.numeric(PatientID)]
x1 <- x1[!is.na(Biomarker), ]
x1[, Biomarker := NULL]


# want biomarkets at 0weeks, and the Vas-12months values
x1 <- x1[time == "0weeks"]
# merge:
x3 <- x2[x1, on = .(PatientID)]
# Could sort for completeness
x3 <- x3[order(PatientID, time)]



target <- "Vas-12months"

cols <- names(x1)
# get biomarket columns
biocols <- cols[!cols %in% c("PatientID", "time")]

predictors <- c("Age", "Sex", "Smoker", biocols)

# Could look at plots of variables/features, see if anything may want rescaling/transforming.
library(ggplot2)
x3_m <- melt(x3, id.vars = "Vas-12months", measure.vars = c(biocols, "Age"))


#only use the observations at inclusion so need to subset

y <- x3[, .SD, .SDcols = c(predictors, target)]

# there are some NA values, remove from analysis

y[, summary(`Vas-12months`)]

y <- y[!is.na(`Vas-12months`),]
y[, summary(`Vas-12months`)]

set.seed(2) #reproducible
train_i <- sample(1:NROW(y), size = round(0.8 * NROW(y)), replace = FALSE)
y_trn <- y[train_i, ]
y_test <- y[-train_i, ]

# covariables are 2 categorical variables, Sex and smoker status, and one numeric variable age
#https://stackoverflow.com/questions/5774813/short-formula-call-for-many-variables-when-building-a-model
mod1 <- lm(`Vas-12months` ~ ., y_trn)
summary(mod1)


library(yardstick)

# how well fits training set?

rmse_vec(mod1$fitted.values, y_trn[, `Vas-12months`])
#2.722152
# multiple R2 30%, adjusted R2 is 19.66% - approx20% of variability explained (look up what R2 means and word appropriately)

# appears that IL-8, Il-7 OPG and TGF are statistically significant (H0: beta = 0 is rejected for these predictors)

#look at the assumptions
# want residials to be iid and approx normal
#interative plots:
plot(mod1)
# normal QQ plot shows tails have some outliers. not exactly normal

# check normality of residuals:
hist(mod1$residuals)
# residual distn seems a bit skewed to the right, not exactly normal

# check independence
plot(mod1$residuals)
# OK, residuals look independent, no clear patterns, no funnel shapes of residuals

# check your notes, probably other things you can check



# consider refitting a model using only predictors that seem to matter (are statistically significant, or close to being significant at the 95% confidence level).

mod2 <- lm(`Vas-12months` ~ `IL-8` + `IL-6` + `OPG`, y_trn)
summary(mod2)
# how well does it fit training set?
#now the R2 is lower, but adjusted R2 is slightly hgiher.  this is OK, as we may get better performance out of sample (less likely to overfit) by not including all the other predictors/features which were not statistically significant for prediction

rmse_vec(mod2$fitted.values, y_trn[, `Vas-12months`])
# 2.8944
# training set RMSE will always be lower if you include more variables (overfitting the data more) so this si expected that RMSE is higher with fewer variables

plot(mod2)
# review the assumptions again
hist(mod2$residuals)
#assumptions seem violated, but this is real world data so do not expect assumptions to always hold true "perfectly"

# c)
# Now do out of sample prediction

yhat_test <- predict(mod2, y_test)

res <- data.table(yhat = yhat_test, y = y_test[,`Vas-12months`])

rmse_vec(res[,y], res[, yhat])
#3.445222 # bad!
# you need a metric to check performance.  RMSE?  what do you use in your course notes???

# check against rmse of the full model
yhat_test1 <- predict(mod1, y_test)
res1 <- data.table(yhat = yhat_test1, y = y_test[,`Vas-12months`])
rmse_vec(res1[,y], res1[, yhat])
# or use 
rmse(res1, "y", "yhat")
#2.466824



# what is std dev of the actual values? if model predicts any good, it should get an rmse less than standard error of targets alone
sd(res[, y])
# 2.511696

hist(res[,y])
summary(res[,y])

# Ok so predicted RSME from the full model is a bit better than sd(y)
# https://datascience.stackexchange.com/questions/9167/what-does-rmse-points-about-performance-of-a-model-in-machine-learning
# https://stats.stackexchange.com/questions/56302/what-are-good-rmse-values
#seems that full model predicts much better for this particular test set at least!
# seems better to use more than just the stat signfiicant predictors selected!!

#d) The model does not seem very accurate.  training set R2 is reasonable and several statistically signicant features (although assumptions for underlying regression do not seem to hold strongly, (i.e residuals are not iid) so intepreting the p values and standard errors must be done with caution)
# What optimsiation metrics are discussed in your course???/