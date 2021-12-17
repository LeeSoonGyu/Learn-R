# Ch_1

a=5
a <- 5
a <- 7
b <- 6
print(a)

dim(available.packages())
a <- available.packages()
head(a)

sessionInfo()

installed.packages("stringr")
installed.packages()
library(stringr)
search()

hist(Nile)
hist(Nile, freq = F)
lines(density(Nile))

var1 <- 0
var2 <- 2
var1 <- 1
var1
var3 <- 3

goods.code <-'a001'
goods.code
goods.name <-'냉장고'
goods.price <-85000
goods.des <- '최고 사양, 동급 최고 품질'

age <- 35
age
names <- '홍길동'
names
names <- c('홍길동', '이순신', '유관순')
names

int <- 20
int

string <- "홍길동"
string

boolean <- TRUE
boolean

# comments 쓰기

sum(10, 20, 20)
sum(10, 20, 20, NA)
sum(10, 20, 20, NA, na.rm = TRUE)
max(1, 2, 3, 4, NA, na.rm = T)
min(1, 2, 3, 4, NA, na.rm = T)
ls()

x <- is.numeric(int)
x

is.character(string)
is.logical(boolean)
is.logical(x)

x <- c(1, 2, "3")
x2 <- c(1, 2, 3)
x
x2


result <- x*3
result
result <- as.numeric(x)*3
result


z <- 5.3 - 3i
Re(z)
Im(z)
is.complex(z)
as.complex(5.3)


mode(int)
mode(string)
mode(boolean)

class(int)
class(string)
class(boolean)


gender <- c('man', 'woman', 'woman', 'man', 'man')
plot(gender)

Ngender <- as.factor(gender)
table(Ngender)

plot(Ngender)
mode(Ngender)
class(Ngender)
class(gender)
is.factor(Ngender)

Ngender

args(factor)
Ogender <- factor(gender, levels = c("woman", "man"), ordered = T)
Ogender

par(mfrow = c(1,4))
plot(Ngender)
plot(Ogender)
plot(Ngender)
plot(Ogender)

as.Date("21/09/27", "%y/%m,%d")


Sys.getlocale(category = "LC_ALL")
Sys.time()

sdate <- "2019-11-11 12:47:05"
sdate

class(sdate)

today <- strptime(sdate, format = "%Y-%m-%d %H:%M:%S")
class(today)

strptime("30-11-2019", format = ("%d-%m-%Y"))
strptime("2-1-19", format = ("%d-%m-%y"))

Sys.setlocale(category = 'LC_ALL', locale = "Korean_Korea")
Sys.setlocale(category = "LC_ALL", locale = "English_US")
Sys.setlocale(category = "LC_ALL", locale = "Japanese_Japan")

Sys.getlocale()

strptime("01-nov-19", format = "%d-%b-%y")

day <- strptime("tuesday, 19 nov 2019", format = "%A,%d %b %Y")
day <- strptime("Tue, 19 nov 2019", format = "%a,%d %b %y")
weekdays(day)
strptime("19 Nov 19", format = "%d %b %y")
day <- c("1may99", "2jun01", "28jul15")
strptime(day, format = "%d%b%y")


help("print")
?print
help(rlm, package = "MASS")

args(max)
example(print)
example(seq)
example("sequence")
max(10, 20, NA, 30)

# mean()
# 1 3 5 7 9 11
# 모든 수의 합 / 모든 수의 갯수

example(mean)

# c(0:10, 50) -> c(0, 1, 2, ...., 10, 50)
x <- c(0:10, 50)
xm <- mean(x)
xm
sum(0:10)
105/12


x <- c(0:10, 50:60)
x
xm <- mean(x)
xm
sum(50:60)
670/22


x <- c(0:10, NA, 50)
x
xm <- mean(x)
xm


getwd()
setwd("C:/Temp/Rwork/dataset1")
data <- read.csv("test.csv", header = T)
data

head(data, 10)
tail(data, 3)

getwd()
setwd("C:/Temp/Rwork")
getwd()