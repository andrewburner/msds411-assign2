#install.packages('psych')

library(psych)
library(tidyverse)
library(corrplot)
library(formattable)


bfi_data <- psych::bfi

head(bfi_data)
dim(bfi_data)

bfi_data <- bfi_data %>% 
  drop_na()

length(names(bfi_data)) *20

corr_bfi <- cor(bfi_data)
corrplot(corr_bfi)

factor_analysis <- psych::fa(corr_bfi)
eigen <- factor_analysis$e.values

PVE <- eigen / sum(eigen) %>% sort()

qplot(c(1:28), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 0.25)

qplot(c(1:28), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(1,0.5)


factor_data <- fa(r = corr_bfi, nfactors = 7, rotate = 'varimax', fm = 'ml')
factor_data <- factanal(covmat = corr_bfi, n.obs = 2236, factors = 7, rotation = 'varimax')
names(factor_data)
write.csv(factor_data$loadings, file = 'loadings2.csv')

summary(factor_data)







