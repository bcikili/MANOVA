library("mvnormtest")
library("car")




heartAttacks <- read.csv('heartAttacks.csv')

str(heartAttacks$trestbps)
str(heartAttacks$chol)

#both DVs are numeric. 

keeps <- c('trestbps', 'chol')
heart1 <- heartAttacks[keeps]
# number of variables more enough to run tests. 

heart2 <- as.matrix(heart1)
mshapiro.test(t(heart2))
# pv > .05 so it violates multivariate normality 


str(heartAttacks$sex)
heartAttacks$sex <- as.character(heartAttacks$sex)
leveneTest(trestbps ~ sex, data = heartAttacks)
#p value is significant it violates Homogeneity of Variance
leveneTest(chol ~ sex, data = heartAttacks)
# it does not violate Homogeneity of Variance

cor.test(heartAttacks$trestbps, heartAttacks$chol, method="pearson", use="complete.obs")
# have an absence of multicollinearity


MANOVA <- manova(cbind(trestbps, chol) ~ sex, data = heartAttacks)
summary(MANOVA)
# There is significant difference between blood pressure (trestbps) and cholesterol (chol)

#posthocs 
summary.aov(MANOVA, test = 'wilks')
# gender has no sign. relations with resting blood pressure, but cholesterol has sign. relations with gender.

