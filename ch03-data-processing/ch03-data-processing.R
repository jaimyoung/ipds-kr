

# datasets 패키지에서 제공하는 다양한 자료들과 도움말
help(package='datasets')
# ggplot2 패키지에서 제공되는 데이터
data(package='ggplot2')
# 현재 실행환경에서 로드되어서 사용가능한 모든 데이터를 살펴보려면 옵션 없이
data()


# 자료 다운로드:
# curl https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data > housing.data
# curl https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names > housing.names

boston <- read.table("housing.data")
library(dplyr)
glimpse(boston)

names(boston) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 'rad', 'tax', 'ptratio', 'black', 'lstat', 'medv')
glimpse(boston)

plot(boston)
summary(boston)

# 큰 자료 읽어 들이기
library(data.table)
DT <- fread("very_big.csv")
DT <- fread("very_big.csv", data.table=FALSE)


# R에서 SQL연습
# install.packages("sqldf")
library(sqldf)
sqldf("select * from iris")
sqldf("select count(*) from iris")
sqldf("select Species, count(*), avg(`Sepal.Length`)
      from iris
      group by `Species`")
sqldf("select Species, `Sepal.Length`, `Sepal.Width`
      from iris
      where `Sepal.Length` < 4.5
      order by `Sepal.Width`")



library(dplyr)
(df1 <- data_frame(x = c(1, 2), y = 2:1))
(df2 <- data_frame(x = c(1, 3), a = 10, b = "a"))
sqldf("select *
    from df1 inner join df2
      on df1.x = df2.x")
sqldf("select *
    from df1 left join df2
      on df1.x = df2.x")


# install.packages("foreign")
library(foreign)
x <- read.dbf(system.file("files/sids.dbf", package="foreign")[1])
dplyr::glimpse(x)
summary(x)


#-------------------------------
# 기본적인 gapminder 자료 처리

# 자료를 로드한다
library(gapminder)

# 행과 열 선택
gapminder[gapminder$country=='Korea, Rep.', c('pop', 'gdpPercap')]

# 행 선택
gapminder[gapminder$country=='Korea, Rep.', ]
gapminder[gapminder$year==2007, ]
gapminder[gapminder$country=='Korea, Rep.' & gapminder$year==2007, ]
gapminder[1:10,]
head(gapminder, 10)

# 정렬
gapminder[order(gapminder$year, gapminder$country),]

# 변수 선택:
gapminder[, c('pop', 'gdpPercap')]
gapminder[, 1:3]

# 변수 이름 바꾸기: gdpPercap 를 gdp_per_cap 으로 변경
f2 = gapminder
names(f2)
names(f2)[6] = 'gdp_per_cap'

# 변수변환과 변수 생성
f2 = gapminder
f2$total_gdp = f2$pop * f2$gdpPercap

# 요약통계량 계산
median(gapminder$gdpPercap)
apply(gapminder[,4:6], 2, mean)
summary(gapminder)


#----------------------------
library(dplyr)

# tbl_df() 와 glimpse()
i2 <- tbl_df(iris)
class(i2)
i2
glimpse(i2)

iris %>% head
iris %>% head(10)




filter(gapminder, country=='Korea, Rep.')
filter(gapminder, year==2007)
filter(gapminder, country=='Korea, Rep.' & year==2007)

gapminder %>% filter(country=='Korea, Rep.')
gapminder %>% filter(year==2007)
gapminder %>% filter(country=='Korea, Rep.' & year==2007)


arrange(gapminder, year, country)
gapminder %>% arrange(year, country)



select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)



gapminder %>%
  mutate(total_gdp = pop * gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap,
         lgrk = le_gdp_ratio * 100)


gapminder %>%
  summarize(n_obs = n(),
            n_countries = n_distinct(country),
            n_years = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))


sample_n(gapminder, 10)
sample_frac(gapminder, 0.01)


distinct(select(gapminder, country))
distinct(select(gapminder, year))


gapminder %>% select(country) %>% distinct()
gapminder %>% select(year) %>% distinct()


gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(median(lifeExp))



# 함수형 프로그래밍의 장점 예시
d1 = filter(gapminder, year == 2007)
d2 = group_by(d1, continent)
d3 = summarize(d2, lifeExp = median(lifeExp))
arrange(d3, -lifeExp)

arrange(
  summarize(
    group_by(
      filter(gapminder, year==2007), continent
    ), lifeExp=median(lifeExp)
  ), -lifeExp
)


gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(lifeExp = median(lifeExp)) %>%
  arrange(-lifeExp)


# 조인 연산자;  inner, left, right, full(outer) join
(df1 <- data_frame(x = c(1, 2), y = 2:1))
(df2 <- data_frame(x = c(1, 3), a = 10, b = "a"))
df1 %>% inner_join(df2)
df1 %>% left_join(df2)
df1 %>% right_join(df2)
df1 %>% full_join(df2)

