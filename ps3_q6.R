# ps3_q6
# Author: Katerina Gan

# (a) 420 observations
library(readxl)
caschool <- read_excel("Documents/Documents - MacBook Pro/2021 spring/Econometrics/caschool.xlsx")
tibble::glimpse(caschool)


# (b_i) income measures average district income in dollar terms
income <- caschool$avginc * 1000

# (b_ii)
mean(caschool$avginc)
sd(caschool$avginc)

# (b_iii)
mean(income)
sd(income)

# (c_i)
mean(caschool$math_scr)

# (c_ii) 243 observations out of 420 observations
str_under20 <- subset(caschool,caschool$str<=20)
mean(str_under20$math_scr)

# (c_iii) 177 observations out of 420 observations
str_over20 <- subset(caschool,caschool$str>20)
mean(str_over20$math_scr)

# (c_v)
x <- caschool$math_scr[caschool$str <= 20]
y <- caschool$math_scr[caschool$str > 20]

t.test(x = x, y = y, mu = 0, alternative = "two.sided", conf.level = 0.90)

# (c_vi)
cov(caschool$avginc,caschool$math_scr)
cov(income,caschool$math_scr)

# (c_vii)
cor(caschool$avginc,caschool$math_scr)
cor(income,caschool$math_scr)

