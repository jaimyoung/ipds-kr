# 8. 빅데이터 분류분석 I: 기본개념과 로지스틱모형

install.packages(c("dplyr", "ggplot2", "ISLR", "MASS", "glmnet",
                   "randomForest", "gbm", "rpart", "boot"))

library(tidyverse)
library(gridExtra)
library(ROCR)

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
                  'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hours_per_week', 'native_country',
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
ggsave("../../plots/8-3.png", width=5.5, height=4, units='in', dpi=600)



training %>%
  filter(race %in% c('Black', 'White')) %>%
  ggplot(aes(age, fill=wage)) +
  geom_density(alpha=.5) +
  ylim(0, 0.1) +
  facet_grid(race ~ sex, scales = 'free_y')
ggsave("../../plots/8-4.png", width=5.5, height=4, units='in', dpi=600)



training %>%
  ggplot(aes(`education_num`, fill=wage)) +
  geom_bar()
ggsave("../../plots/8-5.png", width=5.5, height=4, units='in', dpi=600)


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

g <- arrangeGrob(p1, p2, ncol=2)
ggsave("../../plots/8-6.png", g, width=5.5*1.5, height=4, units='in', dpi=600)



binomial_deviance(y_obs, yhat_lm)

library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]


png("../../plots/8-7.png", 5.5, 4, units='in', pointsize=9, res=600)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
dev.off()


# 9. 빅데이터 분류분석 II: 라쏘와 랜덤포레스트

# 9.1. glmnet 함수를 통한 라쏘 모형, 능형회귀, 변수선택
xx <- model.matrix(wage ~ .-1, adult)
x <- xx[training_idx, ]
y <- ifelse(training$wage == ">50K", 1, 0)
dim(x)

ad_glmnet_fit <- glmnet(x, y)

plot(ad_glmnet_fit)

png("../../plots/9-1.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(ad_glmnet_fit)
dev.off()

ad_glmnet_fit

coef(ad_glmnet_fit, s = c(.1713, .1295))



ad_cvfit <- cv.glmnet(x, y, family = "binomial")

plot(ad_cvfit)

png("../../plots/9-2.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(ad_cvfit)
dev.off()

log(ad_cvfit$lambda.min)
log(ad_cvfit$lambda.1se)

coef(ad_cvfit, s=ad_cvfit$lambda.1se)
coef(ad_cvfit, s="lambda.1se")

length(which(coef(ad_cvfit, s="lambda.min")>0))
length(which(coef(ad_cvfit, s="lambda.1se")>0))

# 9.1.4.  값의 선택

set.seed(1607)
foldid <- sample(1:10, size=length(y), replace=TRUE)
cv1 <- cv.glmnet(x, y, foldid=foldid, alpha=1, family='binomial')
cv.5 <- cv.glmnet(x, y, foldid=foldid, alpha=.5, family='binomial')
cv0 <- cv.glmnet(x, y, foldid=foldid, alpha=0, family='binomial')

png("../../plots/9-3.png", 5.5, 4, units='in', pointsize=7, res=600)
par(mfrow=c(2,2))
plot(cv1, main="Alpha=1.0")
plot(cv.5, main="Alpha=0.5")
plot(cv0, main="Alpha=0.0")
plot(log(cv1$lambda), cv1$cvm, pch=19, col="red",
     xlab="log(Lambda)", ylab=cv1$name, main="alpha=1.0")
points(log(cv.5$lambda), cv.5$cvm, pch=19, col="grey")
points(log(cv0$lambda), cv0$cvm, pch=19, col="blue")
legend("topleft", legend=c("alpha= 1", "alpha= .5", "alpha 0"),
       pch=19, col=c("red","grey","blue"))
dev.off()


predict(ad_cvfit, s="lambda.1se", newx = x[1:5,], type='response')

y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_glmnet <- predict(ad_cvfit, s="lambda.1se", newx=xx[validate_idx,], type='response')
yhat_glmnet <- yhat_glmnet[,1] # change to a vectro from [n*1] matrix
binomial_deviance(y_obs, yhat_glmnet)
# [1] 4257.118
pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet, measure="tpr", x.measure="fpr")

performance(pred_glmnet, "auc")@y.values[[1]]

png("../../plots/9-4.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, col='blue', add=TRUE)
abline(0,1, col='gray')
legend('bottomright', inset=.1,
       legend=c("GLM", "glmnet"),
       col=c('black', 'blue'), lty=1, lwd=2)
dev.off()


# 9.2. 나무모형
library(rpart)
cvr_tr <- rpart(wage ~ ., data = training)
cvr_tr


printcp(cvr_tr)
summary(cvr_tr)



png("../../plots/9-6.png", 5.5, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow = c(1,1), xpd = NA)
plot(cvr_tr)
text(cvr_tr, use.n = TRUE)
par(opar)
dev.off()


yhat_tr <- predict(cvr_tr, validation)
yhat_tr <- yhat_tr[,">50K"]
binomial_deviance(y_obs, yhat_tr)
pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = "tpr", x.measure = "fpr")
performance(pred_tr, "auc")@y.values[[1]]

png("../../plots/9-7.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_tr, col='blue', add=TRUE)
abline(0,1, col='gray')
legend('bottomright', inset=.1,
    legend = c("GLM", "Tree"),
    col=c('black', 'blue'), lty=1, lwd=2)
dev.off()


# 9.3. 랜덤 포레스트 -----------

set.seed(1607)
ad_rf <- randomForest(wage ~ ., training)
ad_rf

png("../../plots/9-8.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(ad_rf)
dev.off()

tmp <- importance(ad_rf)
head(round(tmp[order(-tmp[,1]), 1, drop=FALSE], 2), n=10)

png("../../plots/9-9.png", 5.5, 4, units='in', pointsize=9, res=600)
varImpPlot(ad_rf)
dev.off()

predict(ad_rf, newdata = adult[1:5,])

predict(ad_rf, newdata = adult[1:5,], type="prob")


yhat_rf <- predict(ad_rf, newdata=validation, type='prob')[,'>50K']
binomial_deviance(y_obs, yhat_rf)
pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure="tpr", x.measure="fpr")
performance(pred_tr, "auc")@y.values[[1]]

png("../../plots/9-10.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
abline(0,1, col='gray')
legend('bottomright', inset=.1,
       legend = c("GLM", "glmnet", "RF"),
       col=c('black', 'blue', 'red'), lty=1, lwd=2)
dev.off()


# 9.3.5. 예측확률값 자체의 비교
p1 <- data.frame(yhat_glmnet, yhat_rf) %>%
  ggplot(aes(yhat_glmnet, yhat_rf)) +
  geom_point(alpha=.5) +
  geom_abline() +
  geom_smooth()
p2 <- reshape2::melt(data.frame(yhat_glmnet, yhat_rf)) %>%
  ggplot(aes(value, fill=variable)) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)
g <- arrangeGrob(p1, p2, ncol=2)
ggsave("../../plots/9-11.png", g, width=5.5*1.2, height=4*.8, units='in', dpi=600)


# 9.4. 부스팅 ----------

set.seed(1607)
adult_gbm <- training %>% mutate(wage=ifelse(wage == ">50K", 1, 0))
ad_gbm <- gbm(wage ~ ., data=adult_gbm,
             distribution="bernoulli",
             n.trees=50000, cv.folds=3, verbose=TRUE)
(best_iter <- gbm.perf(ad_gbm, method="cv"))

ad_gbm2 <- gbm.more(ad_gbm, n.new.trees=10000)
(best_iter <- gbm.perf(ad_gbm2, method="cv"))


png("../../plots/9-12.png", 5.5, 4, units='in', pointsize=9, res=600)
(best_iter <- gbm.perf(ad_gbm2, method="cv"))
dev.off()


predict(ad_gbm, n.trees=best_iter, newdata=adult_gbm[1:5,], type='response')

yhat_gbm <- predict(ad_gbm, n.trees=best_iter, newdata=validation, type='response')
binomial_deviance(y_obs, yhat_gbm)
pred_gbm <- prediction(yhat_gbm, y_obs)
perf_gbm <- performance(pred_gbm, measure="tpr", x.measure="fpr")
performance(pred_gbm, "auc")@y.values[[1]]


png("../../plots/9-13.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
plot(perf_gbm, add=TRUE, col='cyan')
abline(0,1, col='gray')
legend('bottomright', inset=.1,
    legend=c("GLM", "glmnet", "RF", "GBM"),
    col=c('black', 'blue', 'red', 'cyan'), lty=1, lwd=2)
dev.off()



# 9.5. 모형 비교, 최종 모형 선택, 일반화 성능 평가 ----


# 9.5.2. 모형의 예측확률값의 분포 비교
# exmaple(pairs) 에서 따옴
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

png("../../plots/9-14.png", 5.5, 4, units='in', pointsize=9, res=600)
pairs(data.frame(y_obs=y_obs,
                yhat_lm=yhat_lm,
                yhat_glmnet=c(yhat_glmnet),
              yhat_rf=yhat_rf,
                yhat_gbm=yhat_gbm),
    lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
    upper.panel = panel.cor)
dev.off()


# 9.5.3. 테스트셋을 이용한 일반화능력 계산
y_obs_test <- ifelse(test$wage == ">50K", 1, 0)
yhat_gbm_test <- predict(ad_gbm, n.trees=best_iter, newdata=test, type='response')
binomial_deviance(y_obs_test, yhat_gbm_test)
pred_gbm_test <- prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test, "auc")@y.values[[1]]

# 9.6.5. 캐럿 (caret) 패키지
install.packages("caret", dependencies = c("Depends", "Suggests"))



# This is for the earlier ROC curve example. ---
{
  png("../../plots/8-1.png", 5.5*1.2, 4*.8, units='in', pointsize=9, res=600)
  opar <- par(mfrow=c(1,2))
  plot(perf_lm, col='black', main="ROC Curve")
  plot(perf_tr, col='blue', add=TRUE)
  abline(0,1, col='gray')
  legend('bottomright', inset=.1,
      legend = c("GLM", "Tree"),
      col=c('black', 'blue'), lty=1, lwd=2)
  plot(perf_lm, col='black', main="ROC Curve")
  plot(perf_glmnet, add=TRUE, col='blue')
  plot(perf_rf, add=TRUE, col='red')
  plot(perf_gbm, add=TRUE, col='cyan')
  abline(0,1, col='gray')
  legend('bottomright', inset=.1,
      legend=c("GLM", "glmnet", "RF", "GBM"),
      col=c('black', 'blue', 'red', 'cyan'), lty=1, lwd=2)
  par(opar)
  dev.off()
}
