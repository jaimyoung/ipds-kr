library(tidyverse)
library(gridExtra)

mpg <- tbl_df(mpg)
mpg

# 7.2. 모든 자료에 행해야 할 분석
library(dplyr)
library(ggplot2)
glimpse(mpg)

head(mpg)

summary(mpg)

# 7.3. 수량형 변수의 분석
summary(mpg$hwy)
mean(mpg$hwy)
median(mpg$hwy)
range(mpg$hwy)
quantile(mpg$hwy)


png("../plots/7-1.png", 5.5*.8, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)
dev.off()


# 7.3.1. 일변량 t-검정
hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu=mu0, alternative = "greater")


t.test(hwy)

# 7.3.2. 이상점과 로버스트 통계방법
c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))


# 7.4. 성공-실패값 범주형 변수의 분석
set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels = c(0,1), labels = c("no", "yes"))
x

table(x)

prop.table(table(x))

barplot(table(x))

binom.test(x=length(x[x=='yes']), n = length(x), p = 0.5, alternative = "two.sided")



binom.test(x=5400, n = 10000)


n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96 * sqrt(1/(4 * n)),4))
curve(1.96 * sqrt(1/(4 * x)), 10, 10000, log='x')
grid()

png("../plots/7-2.png", 5.5, 4, units='in', pointsize=9, res=600)
n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96 * sqrt(1/(4 * n)),4))
curve(1.96 * sqrt(1/(4 * x)), 10, 10000, log='x')
grid()
dev.off()


# 7.6. 수량형 X, 수량형 Y의 분석

ggplot(mpg, aes(cty, hwy)) + geom_jitter() + geom_smooth(method="lm")
ggsave("../plots/7-4.png", width=5.5, height=4, units='in', dpi=600)

cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method = "kendall"))
with(mpg, cor(cty, hwy, method = "spearman"))


# 7.6.3. 선형회귀모형 적합

(hwy_lm <- lm(hwy ~ cty, data=mpg))
summary(hwy_lm)

predict(hwy_lm)
resid(hwy_lm)
predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)))



opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm, las = 1)   # Residuals, Fitted, ...
par(opar)

png("../plots/7-6.png", 5.5, 4*1.2, units='in', pointsize=9, res=600)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm, las = 1)   # Residuals, Fitted, ...
par(opar)
dev.off()

# 7.6.6. 로버스트 선형 회귀분석

library(MASS)
set.seed(123) # make reproducible
lqs(stack.loss ~ ., data = stackloss) # 로버스트
lm(stack.loss ~ ., data = stackloss) # 보통 선형모형


# 7.6.7. 비선형/비모수적 방법, 평활법과 LOESS

plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~ displ, data=mpg)
mpg_lo
summary(mpg_lo)
xs <- seq(2,7,length.out = 100)
mpg_pre <- predict(mpg_lo, newdata=data.frame(displ=xs), se=TRUE)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96*mpg_pre$se.fit, lty=2)
lines(xs, mpg_pre$fit + 1.96*mpg_pre$se.fit, lty=2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()


png("../plots/7-8-left.png", 5.5*.8, 4, units='in', pointsize=9, res=600)
plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~ displ, data=mpg)
xs <- seq(2,7,length.out = 100)
mpg_pre <- predict(mpg_lo, newdata=data.frame(displ=xs), se=TRUE)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96*mpg_pre$se.fit, lty=2)
lines(xs, mpg_pre$fit + 1.96*mpg_pre$se.fit, lty=2)
dev.off()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()
ggsave("../plots/7-8-right.png", width=5.5*.8, height=4, units='in', dpi=600)


# 7.7. 범주형 x, 수량형 y

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
ggsave("../plots/7-9.png", width=5.5, height=4, units='in', dpi=600)


(hwy_lm2 <- lm(hwy ~ class, data=mpg))
summary(hwy_lm2)


predict(hwy_lm2, newdata=data.frame(class="pickup"))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)    # Residuals, Fitted, ...
par(opar)

png("../plots/7-10.png", 5.5*.8, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)    # Residuals, Fitted, ...
par(opar)
dev.off()


# 7.8. 수량형 x, 범주형 y (성공-실패)

library(gridExtra)
p1 <- ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=function(x) log(x/(1-x))) + ylab('logit(x)') +
  ggtitle("Logit function")
p2 <- ggplot(data.frame(y=c(-6, 6)), aes(y)) +
  stat_function(fun=function(y) 1/(1+exp(-y))) + ylab('logistic(y)') +
  ggtitle("Logistic function")
g <- arrangeGrob(p1, p2, ncol=2)
ggsave("../plots/7-11.png", g, width=5.5*1.5, height=4, units='in', dpi=600)


chall <- read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv')
chall <- tbl_df(chall)
glimpse(chall)


chall %>% ggplot(aes(temperature, distress_ct)) +
  geom_point()

chall %>% ggplot(aes(factor(distress_ct), temperature)) +
  geom_boxplot()


p1 <- chall %>% ggplot(aes(temperature, distress_ct)) +
  geom_point()
p2 <- chall %>% ggplot(aes(factor(distress_ct), temperature)) +
  geom_boxplot()
g <- arrangeGrob(p1, p2, ncol=2)
ggsave("../plots/7-12.png", g, width=5.5*1.5, height=4, units='in', dpi=600)


(chall_glm <-
    glm(cbind(distress_ct, o_ring_ct - distress_ct) ~
        temperature, data=chall, family='binomial'))

summary(chall_glm)

predict(chall_glm, data.frame(temperature=30))

exp(3.45) / (exp(3.45) +1)
predict(chall_glm, data.frame(temperature=30), type='response')


logistic <- function(x){exp(x)/(exp(x)+1)}

plot(c(20,85), c(0,1), type = "n", xlab = "temperature",
     ylab = "prob")
tp <- seq(20, 85, 1)
chall_glm_pred <-
  predict(chall_glm,
          data.frame(temperature = tp),
          se.fit = TRUE)
lines(tp, logistic(chall_glm_pred$fit))
lines(tp, logistic(chall_glm_pred$fit - 1.96 * chall_glm_pred$se.fit), lty=2)
lines(tp, logistic(chall_glm_pred$fit + 1.96 * chall_glm_pred$se.fit), lty=2)
abline(v=30, lty=2, col='blue')


logistic <- function(x){exp(x)/(exp(x)+1)}

png("../plots/7-13.png", 5.5*.8, 4, units='in', pointsize=9, res=600)
plot(c(20,85), c(0,1), type = "n", xlab = "temperature", ylab = "prob")
tp <- seq(20, 85, 1)
chall_glm_pred <- predict(chall_glm, data.frame(temperature = tp), se.fit = TRUE)
lines(tp, logistic(chall_glm_pred$fit))
lines(tp, logistic(chall_glm_pred$fit - 1.96 * chall_glm_pred$se.fit), lty=2)
lines(tp, logistic(chall_glm_pred$fit + 1.96 * chall_glm_pred$se.fit), lty=2)
abline(v=30, lty=2, col='blue')
dev.off()

