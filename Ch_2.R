# Ch_2

a <- c(1:100)
a
1:20
c(1, 2, 3, 4, 5)

seq(1, 10, 2)

rep(1:3, 3)
rep(1:3, each=3)

x <- c(1, 3, 5, 7)
y <- c(3, 5)

union(x,y)
setdiff(x,y)
intersect(x,y)


v1 <- c(33, -5, 20:23, 12, -2:3)
v2 <- c("홍길동", "이순신", "유관순")
v3 <- c(T, TRUE, FALSE, T, TRUE, F, T)
v1
v2
v3

v1; v2; v3

v4 <- c(33, 05, 20:23, 12, "4")
v4

age <- c(30, 35, 40)
age
names(age) <- c("홍길동", "이순신", "유관순")
age
age <- NULL
age


a <- c(1:50)
a[10:45]
a[19:(length(a)-5)]
a[19:45]

a[1,2]


v1 <- c(13, -5, 20:23, 12, -2:3)
v1[1]
v1[c(2,4)]
v1[c(3:5)] #v1[c(3, 4, 5)]
v1[c(4, 5:8, 7)] #v1[c(4, 5, 6, 7, 8, 7)]

v1
v1[-1]
v1
v1[-c(2,4)]
v1[c(-2,-4)]
v1[-c(2, 5:10, 1)] #v1 [3, 4, 11, 12, 13] 20, 21, 1, 2, 3

install.packages("RSADBE")
library(RSADBE)
data("Severity_Counts")
str(Severity_Counts)
head(Severity_Counts)


m <- matrix(c(1:5))
m

m <- matrix(c(1:10), nrow = 2)
m

m <- matrix(c(1:11), nrow = 2)
m

m <- matrix(c(1:10), nrow = 2, byrow = T)
m

m <- NULL
x1 <- c(10, 40, 50:52)
x2 <- c(20, 30, 6:8)
mr <- rbind(x1, x2)
mr

mc <- cbind(x1, x2)
mc

mc2 <- cbind(x2, x1)
mc2

m3 <- matrix(10:19,2) #matrix(data, nrow, ncol, byrow, dimnames)
m4 <- matrix(10:20,2)
m3
m4
mode(m3)
class(m3)

m3[1,]
m3[,5]
m3[2,3]
m3[1,c(2:5)]

x <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = T)
x

x
apply(x,1,max)
apply(x,2,max)
apply(x,1,min)
apply(x,2,min)
apply(x,1,mean)
apply(x,2,mean)

f <- function(x){
  x*c(1,2,3)
}
f
result <- apply(x,1,f)
result
x
result <- apply(x,2,f)
result

x
colnames(x) <- c("one", "two", "three")
x
rownames(x) <- c("row one", "row two", "row three")
x


vec <- c(1:12)
vec
arr <- array(vec, c(3, 2, 2))
arr
arr[,,1]
arr[,,2]
arr[,2,]
mode(arr);class(arr)


library(RSADBE)
data("Bug_Metrics_Software")
str(Bug_Metrics_Software)

head(Bug_Metrics_Software, 3)


no <- c(1,2,3)
name <- c("hong", "lee", "kim")
pay <- c(150,250,300)
vemp <- data.frame(No = no, Name = name, Pay = pay)
vemp

m <- matrix(
  c(1, "hong", 150, 2, "lee", 250, 3, "kim", 300), 3, by = T)

m
memp <- data.frame(m)
memp

getwd()
setwd("C:/Temp/Rwork/dataset1/dataset1")
txtemp <- read.table('emp.txt', header = 1, sep = "")
txtemp <- read.table('emp.txt', header = T, sep = "")
txtemp

csvtemp <- read.csv('emp.csv', header = T)
csvtemp
help(read.csv)
help(read.table)

read.csv('emp2.csv', header = F)
name <- c("사번", "이름", "급여")
read.csv('emp2.csv', header = F, col.names = name)

df <- data.frame(x = c(1:5), y = seq(2,10,2), z = c('a', 'b', 'c', 'd', 'e')) 
df
df$x
df$z

str(df)
ncol(df)
nrow(df)
names(df)
df[c(2:3), 1]

summary(df)

df
apply(df[,c(1,2)], 2, sum)

x1 <-subset(df, x>=3)
x1

y1 <- subset(df, y<=8)
y1

xyand <- subset(df, x<=2 & y<=6)
xyand
xyor <- subset(df, x<=2  | y<=0)
xyor


sid <- c("A", "B", "C", "D")
score <- c(90, 80, 70, 60)
subject <- c("컴퓨터", "국어국문", "소프트웨어", "유아교육")
student <- data.frame(sid, score, subject)
student

mode(student); class(student)
str(sid)
str(score)
str(subject)
str(student)


height <- data.frame(id = c(1,2), h = c(180,175))
height
weight <- data.frame(id = c(1,2), w = c(80,75))
weight
user <- merge(height, weight, by.x = 'id', by.y = 'id')
user

install.packages("UsingR")
library(UsingR)
data(galton)

str(galton)
dim(galton)
head(galton, 15)

list <- list("lee", "이순신", 95)
list

unlist <- unlist(list)
unlist

unm <- list(c(1:5), c(6,10))
unm


member <- list(name = c("홍길동", "유관순"), age = c(35, 25),
               address = c("한양", "충남"), gender = c("남자", "여자"),
               htype = c("아파트", "오피스텔"))
member

member$name
member$name[1]
member$name[3]

member <- NULL
member$age[1] <- 35
member$age
member$age[2] <- 25
member$age

member$id <- "hong"
member

member$age <- NULL
member

member$pwd <- "1234"
member

length(member)

mode(member)
class(member)

a <- list(c(1:5))
b <- list(c(6:10))
lapply(c(a,b), max)
sapply(c(a,b), max)
vapply(c(a,b), rbind)


multi_list <- list(c1 = list(1,2,3), c2 = list(10,20,30),
                  c3 = list(100,200,300))
multi_list
multi_list$c1
multi_list$c2

do.call(cbind, multi_list)
do.call(rbind, multi_list)


install.packages("stringr")
library(stringr)

str_extract("홍길동35이순신45유관순25", "[1-9]{2}")
str_extract("홍길동35이순신45유관순25", "[1-9]{3}")
str_extract("홍길동35이순신45유관순25", "[1-9]{1}")
str_extract_all("홍길동35이순신45유관순25", "[1-9]{2}")
str_extract_all("홍길동35이순신45유관순25", "[1-9]{1}")

string <- "hongkd105leess1002you25강감찬2005"
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[a-z]{3,5}")

str_extract(string, "hong")
str_extract(string, "25")
str_extract(string, "[가-힣]{3}")
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[0-9]{4}")

# "[^제외문자열]" : 해당 문자열을 제외 후 나머지 추출
# "[^제외문자열]{n}" : 해당 문자열을 제외 후 연속 된 n글자 추출

string
str_extract_all(string, "[^a-z]")
str_extract_all(string, "[^a-z]{4}")
str_extract_all(string, "[^가-힣]{5}")
str_extract_all(string, "[^0-9]{3}")

jumin <- '123456-1234567'
str_extract(jumin, '[0-9]{6}-[1234][0-9]{6}')
str_extract(jumin, '[0-9]{6}-[56][0-9]{6}')

str_extract_all(jumin, "\\d{6}-[1234]\\d{6}")
str_extract(jumin, "\\d{6}-[1234]\\d{6}")

name <- '홍길동1234,이순신5678,강감찬1012'
str_extract(name, '\\w{7,}')
str_extract_all(name, '\\w{7,}')

string <- 'hongkd105leess1002you25강감찬2005'
len <- str_length(string)
len

str_locate(string, '강감찬')

string_sub <- str_sub(string, 1, len-7)
string_sub
string_sub <- str_sub(string, 1, 23)
string_sub
string_sub <- str_sub(string, 31, 33)
string_sub

ustr <- str_to_upper(string_sub)
ustr
ustr2 <- str_to_lower(ustr)
ustr2

string_sub
string_rep <- str_replace(string_sub, "hongkd105", "홍길동35,")
string_rep
string_rep <- str_replace(string_rep, "leess1002", "이순신45,")
string_rep <- str_replace(string_rep, "you25", "유관순25,")
string_rep

string_c <- str_c(string_rep, "강감찬55")
string_c

string_sp <- str_split(string_c, ",")
string_sp

string_vec <- c("홍길동35", "이순신45", "유관순25", "강감찬55")
string_vec
string_join <- paste(string_vec, collapse = "/")
string_join
