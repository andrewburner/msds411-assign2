#install.packages('psych')

library(psych)
library(tidyverse)
library(corrplot)
library(formattable)


bfi_data <- psych::bfi

head(bfi_data)
dim(bfi_data)

bfi_data <- bfi_data %>% 
  drop_na() %>% 
  select(-gender, -education, -age)

length(names(bfi_data)) *20

corr_bfi <- cor(bfi_data)
corrplot(corr_bfi)
eigen(corr_bfi)$values
sum(eigen(corr_bfi)$values >= 1)

factor_analysis <- psych::fa(corr_bfi)
eigen <- factor_analysis$e.values

PVE <- eigen / sum(eigen) %>% sort()

qplot(c(1:25), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 0.25)

qplot(c(1:25), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1) +
  geom_hline(yintercept = 0.9)


factor_data <- fa(r = corr_bfi, nfactors = 6, rotate = 'varimax', fm = 'ml')
#names(factor_data)
#factor_data$loadings

factor_df <- as.data.table(unclass(factor_data$loadings), keep.rownames = TRUE)
factor_df <- factor_df %>% mutate_if(is.numeric, round, digits = 3)

# write.csv exports this file so I could open it in excel
# and copy it into my assignment doc
write.csv(factor_df, file = 'loadings.csv', row.names = TRUE)


factanal(covmat=corr_bfi,  n.obs=2236, factors=1, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=5, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=8, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=9, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=10, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=11, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=12, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=13, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=14, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=15, rotation="varimax", fm = "ml")
factanal(covmat=corr_bfi,  n.obs=2236, factors=16, rotation="varimax", fm = "ml")

bfi_regression <- psych::bfi %>% drop_na()
five_fact <- fa(r = corr_bfi, nfactors = 5, n.obs = 2236, rotate = 'varimax', fm = 'ml')
fs <- factor.scores(bfi_data, five_fact)
factor_scores <- fs$scores
bfi_regression <- cbind(bfi_regression, factor_scores)
bfi_regression$gender <- bfi_regression$gender - 1
bfi_regression$education <- bfi_regression$education - 1

gender_model <- lm(gender ~ ML1 + ML2 + ML3 + ML4 + ML5, data = bfi_regression)
summary(gender_model)

gender_predictions <- bfi_regression %>%
  add_predictions(model = gender_model, var = "gender_pred") %>% 
  select(ML1, ML2, ML3, ML4, ML5, gender, gender_pred) %>% 
  mutate(rounded_pred = round(gender_pred, 0))

gender_confusion <- gender_predictions %>% 
  select(gender, rounded_pred)

gender_confusion$gender <- as.factor(gender_confusion$gender)
gender_confusion$rounded_pred <- as.factor(gender_confusion$rounded_pred)
confusionMatrix(reference = gender_confusion$gender, gender_confusion$rounded_pred)

ggplot(gender_predictions, aes(x=gender_pred)) +
  geom_freqpoly(bins = 100)
ggplot(gender_predictions, aes(x=gender)) +
  geom_bar()
ggplot(gender_predictions, aes(y = ML3)) +
  geom_boxplot(aes(group = gender))



education_model <- lm(education ~ ML1 + ML2 + ML3 + ML4 + ML5, data = bfi_regression)
summary(education_model)

ed_predictions <- bfi_regression %>% 
  add_predictions(model = education_model, var = "education_pred") %>% 
  select(education, education_pred) %>% 
  mutate(rounded_pred = round(education_pred, 0))

education_confusion <- ed_predictions %>% 
  select(education, rounded_pred)

education_confusion$education <- as.factor(education_confusion$education)
education_confusion$rounded_pred <- as.factor(education_confusion$rounded_pred)
confusionMatrix(reference = education_confusion$education, education_confusion$rounded_pred)


ggplot(ed_predictions, aes(x = education, y = education_pred)) +
  geom_jitter()

ggplot(ed_predictions, aes(y = education_pred)) +
  geom_boxplot()
ggplot(ed_predictions) +
  geom_bar(aes(x = education))



age_model <- lm(age ~ ML1 + ML2 + ML3 + ML4 + ML5, data = bfi_regression)
summary(age_model)

age_predictions <- bfi_regression %>%
  add_predictions(model = age_model, var = "age_pred") %>% 
  select(ML1, ML2, ML3, ML4, ML5, age, age_pred)

ggplot(age_predictions, aes(x = age, y = age_pred)) +
  geom_point() +
  geom_smooth()

age_predictions <- augment(age_model)
head(age_predictions)

ggplot(age_predictions, aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()


thirteen_fact <- factanal(covmat=corr_bfi,  n.obs=2236, factors=13, rotation="varimax", fm = "ml")
fs13 <- factor.scores(bfi_data, thirteen_fact)
factor_scores_13 <- fs13$scores
bfi_regression_13 <- cbind(bfi_regression, factor_scores_13)
bfi_regression_13$gender <- bfi_regression_13$gender - 1
bfi_regression_13$education <- bfi_regression_13$education - 1

gender_model_13 <- lm(gender ~ Factor1 + Factor2 + Factor3 + Factor4 + Factor5 + Factor6 + Factor7 + Factor8 + Factor9 + Factor10 + Factor11 + Factor12 + Factor13, data = bfi_regression_13)
summary(gender_model_13)




