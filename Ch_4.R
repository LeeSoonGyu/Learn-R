# Ch_4

num <- 100
num1 <- 20
result <- num + num1
result
result <- num - num1
result
result <- num * num1
result
result <- num / num1
result

num <- 9
num1 <-2
result <- num %% num1
result

result <- num^2
result

result <- num^num1
result

## 100 1e+2(1*100) 120 1.2e+2(1.2*100) 125 1.25e+2(1.25*100)

boolean <- num == num1
boolean
boolean <- num != num1
boolean

boolean <- num > num1
boolean
boolean <- num >= num1
boolean

logical <- num >= 50 & num1 <= 10
logical
logical <- num >= 50 | num1 <= 10
logical

# &(and) (둘다 T일때 T, 둘중 하나만 T일때 F) |(or) (둘다 T일때 T, 둘중 하나만 T일때 T)

x <- T; y <- F
xor(x,y)
# xor 함수 알아둘것!!

# 단순 if문, ifelse문
x <- 4;y <- 4;z <- x*y
x;y;z
if(x * y >= 40) {
  cat("x * y의 결과는 40이상입니다.\n")
  cat("x * y = ", z)
} else{
  cat("x * y의 결과는 40미만입니다. x * y = ", z, "\n")
}


score <- scan()
score
result <- "노력"
result

if(score >= 80){
  result <- "우수"
}

cat("당신의 학점은", result, score)

score <- scan()
if(score >= 90){
  result <-"A학점"
}else if(score >= 80){
  result <- "B학점"
}else if(score >= 70){
  result <- "C학점"
}else if(score >= 60){
  result <- "D학점"
}else {
  result <- "F학점"
}
cat("당신의 학점은", result)

score <- scan()
ifelse(score >= 80, "우수", "노력")
ifelse(score <= 80, "우수", "노력")

excel <- read.csv(file = "excel.csv", header = T)
excel
q1 <- excel$q1
q1
ifelse(q1 >= 3, sqrt(q1), q1)
ifelse(q1 >= 2 & q1 <= 4, q1 ^ 2, q1)

# switch함수
switch("name", id="hong", pwd="1234", age="105", name="홍길동")

# scan(what = "") <- 문자열로 받겠다.
empname <- scan(what = "")
empname
switch(empname,
       hong = 250,
       lee = 350,
       kim = 200,
       kang = 400)

# which함수
name <- c("kim", "lee", "choi", "park")
which(name == "choi")

no <- c(1:5)
name <- c("홍길동", "이순신", "강감찬", "유관순", "김유신")
score <- c(85, 78, 89, 90, 74)
exam <- data.frame(학번 = no, 이름 = name, 성적 = score)
exam

which(exam$이름 == "유관순")
exam[4,]

# 반복문 for함수 (시작과 종료 조건을 넣어 참인 경우에 해당 문장을 반복)
# while함수 (수식이 거짓이 될 때까지 해당 문장을 반복)

i <- c(1:10)
i
for(n in i){
  print(n*10)
  print(n)
}

# 짝수만 추출
for (n in i){
  if(n %% 2 == 0) print(n)
}
# 홀수만 추출
for (n in i){
  if(n %% 2 == 1) print(n)
}
# 짝수는 넘기고 홀수만 추출
for (n in i){
  if(n %% 2 == 0){
    next
  } else {
    print(n)
  }
}

name <- c(names(exam))
name
for (n in name){
  print(n)
}

score <- c(85, 95, 98)
name <- c("홍길동", "이순신", "강감찬")
i <- 1 # 몇번 돌아갔는지 확인해보기 위한 변수데이터 입력
for(s in score){
  cat(name[i], "->", s, "\n")
  i <- i +1
}

# while 함수
i <- 0
while(i < 10){
  i <- i+1
  print(i)
}

# 사용자 정의 함수

f1 <- function(){
  cat("매개변수가 없는 함수")
}
f1
f1()

f3 <- function(x,y){
  add <- x+y
  return(add)
}

add <- f3(10,20)
add

# 통계 및 사용자 정의 함수

excel <- read.csv("excel.csv", header = T)
excel
head(excel)

summary(test)

table(test$A, test$B)

data_pro <- function(x){
  for(idx in 1: length(x)){
    cat (idx, "번째 칼럼의 빈도 분석 결과")
    print(table(x[idx]))
    cat("\n")
  }
  for(idx in 1:length(x)){
    f <- table(x[idx])
    cat(idx, "번째 칼럼의 최대값/최소값\n")
    cat("max = ", max(f), "min = ", min(f), "\n")
  }
}

data_pro(excel)

x <- c(7, 5, 12, 9, 15, 6)

var_sd <- function(x){
  var <- sum((x - mean(x))^2)/(length(x)-1)
  sd <- sqrt(var)
  cat("표본분산: ", var, "\n")
  cat("표본표준편차: ",sd)
}

var_sd(x)
summary(x)

pytha <- function(s, t){
  a <- s^2 - t^2
  b <- 2*s*t
  c <- s^2 + t^2
  cat("피타고라스 정리: 3개의 변수:", a, b, c)
}

pytha(2,1)

gugu <- function(i, j){
  for(x in i){
    cat("**", x, "단**\n")
    for(y in j){
      cat(x, "*", y, " = ", x*y, "\n")
    }
    cat("\n")
  }
}

i <- c(1:9)
j <- (1:9)
gugu(i,j)

data <- c(10, 20, 5, 4, 40, 7, NA, 6, 3, NA, 2, NA)
# 1차 option 이용
lee <- function(x){
  print(x)
  print(mean(x, na.rm = T))
  # 2차 NA -> 0으로 대체
  data = ifelse(!is.na(x), x, 0)
  print(mean(data))
  # 3차 NA -> 평균으로 대체
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm = T), 2))
  print(data2)
  print(mean(data2))
}

lee(data)

r <- runif(10, min = 0, max = 1)
r
coin <- function(n){
  r <- runif(10, min = 0, max = 1)
  result <- numeric()
  for(i in 1:n){
    if(r[i] <= 0.5)
      result[i] <- 0
    else
      result[i] <- 1
  }
  return(result)
}

coin(10)

montaCoin <- function(n){
  cnt <- 0
  for(i in 1:n){
    cnt <- cnt + coin(1)
  }
  result <- cnt / n
  return(result)
}

montaCoin(10)
montaCoin(30)
montaCoin(100)
montaCoin(1000)
montaCoin(10000)
montaCoin(100000)
montaCoin(1000000)
montaCoin(10000000)

# 통계 관련 내장함수
# sd(표준편차), range(범위), sort(정렬), rnorm(정규분포)

library(RSADBE)
data("Bug_Metrics_Software")
Bug_Metrics_Software[,,1]

rowSums(Bug_Metrics_Software[,,1])
rowMeans(Bug_Metrics_Software[,,1])
colSums(Bug_Metrics_Software[,,1])
colMeans(Bug_Metrics_Software[,,1])

seq(-2, 2, by = .2)
vec <- 1:10
min(vec)
max(vec)
range(vec)
mean(vec)
median(vec)
sum(vec)
sd(rnorm(10))
table(vec)

n <- 1000
rnorm(n, mean = 0, sd = 1)
hist(rnorm(n, mean = 0, sd = 1))

runif(n, min = 0, max = 10)
hist(runif(n, min = 0, max = 10))

n <- 20
rbinom(n, 1, prob = 1/2)
rbinom(n, 2, prob = 0.5)
rbinom(n, 10, prob = 0.5)
rbinom(n, 5, prob = 1/6)

rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)
set.seed(345)
rnorm(5, mean = 0, sd = 1)

# 수학 관련 내장함수
# abs -> 절대값, sqrt -> 루트, ceiling -> 올림, floor -> (소수점)버림 
# round -> 반올림, factorial -> 내림순곱하기
# cumsun -> 누적합산, exp -> 지수함수

vec1 <- seq(-2, 2, by = .2)
ceiling(vec1)
vec <- 1:10
prod(vec)
factorial(5)
abs(-5)
sqrt(16)
vec
cumsum(vec)
cumprod(vec)
log(10)
log10(10)

# 행렬연산 내장함수
# t -> 행열바꾸는 것, diag -> 행열의 대각선

x <- matrix(1:9, nrow = 3, ncol = 3, byrow = T)
y <- matrix(1:3, nrow = 3)
x
y
ncol(x)
nrow(x)
t(x)
cbind(x, 1:3)
rbind(x, 10:12)
diag(x)
det(x)
apply(x, 1, sum)
apply(x, 2, sum)
apply(x, 2, mean)
apply(x, 1, mean)
svd(x)
eigen(x)
x
y
x %*% y

x <- c(1,2,3,4,5)
y <- c(5)
x %% y

x <- matrix(c(1,4,2,3), nrow = 2)
y <- matrix(c(1,3,2,4,5,6), nrow = 2)
x
y

x %*% y

z <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
z

x %*% z
y %*% z

c(1,2,3) %*% c(4,5,6)

x <- matrix(c(1,4,2,3), nrow = 2)
x
y <- matrix(c(1,3,2,4), nrow = 2)
y

x %/% y

z <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
z
x %/% z

x %in% y
sum(x %in% y)

x <- c(1,3,5,7,9)
y <- c(3,7)
union(x, y)
setequal(x,y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)

5 %in% y
5 %in% x
