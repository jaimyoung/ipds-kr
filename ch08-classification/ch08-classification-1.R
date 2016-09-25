# 8. 빅데이터 분류분석 I: 기본개념과 로지스틱모형

install.packages(c("dplyr", "ggplot2", "ISLR", "MASS", "glmnet",
                   "randomForest", "gbm", "rpart", "boot"))

library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)




binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
}




# curl https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data > adult.data
# curl  https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names > adult.names

adult <- read.csv("adult.data", header = FALSE, strip.white = TRUE)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                  'education-num', 'marital-status', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital-gain', 'capital-loss',
                  'hours-per-week', 'native-country',
                  'wage')


glimpse(adult)

summary(adult)

levels(adult$wage)

# 8.3.3. 범주형 설명변수에서 문제의 복잡도

levels(adult$race)
adult$race[1:5]
levels(adult$sex)
adult$sex[1:5]

x <- model.matrix( ~ race + sex + age, adult)
glimpse(x)
colnames(x)


x_orig <- adult %>% dplyr::select(sex, race, age)
View(x_orig)

x_mod <- model.matrix( ~ sex + race + age, adult)
View(x_mod)


x <- model.matrix( ~ . - wage, adult)
dim(x)

# 8.4. 훈련, 검증, 테스트셋의 구분

set.seed(1601)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx = sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx,]
validation <- adult[validate_idx,]
test <- adult[test_idx,]


# 8.5. 시각화

training %>%
  ggplot(aes(age, fill=wage)) +
  geom_density(alpha=.5)



training %>%
  filter(race %in% c('Black', 'White')) %>%
  ggplot(aes(age, fill=wage)) +
  geom_density(alpha=.5) +
  ylim(0, 0.1) +
  facet_grid(race ~ sex, scales = 'free_y')



training %>%
  ggplot(aes(`education-num`, fill=wage)) +
  geom_bar()


# 8.6. 로지스틱 회귀분석
ad_glm_full <- glm(wage ~ ., data=training, family=binomial)

summary(ad_glm_full)


alias(ad_glm_full)


predict(ad_glm_full, newdata = adult[1:5,], type="response")


# 8.6.4. 예측 정확도 지표
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full, newdata=validation, type='response')

library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(y_obs, yhat_lm, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()
p2 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)



binomial_deviance(y_obs, yhat_lm)

library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]

