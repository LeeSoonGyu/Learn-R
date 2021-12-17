# Ch_3

num <- scan()

num

sum(num)

name <- scan(what = character())

name

df = data.frame()
df = edit(df)

getwd()
setwd("C:/Temp/Rwork/dataset1/dataset1")
getwd()

student <- read.table(file = "student.txt")
student
names(student) <- c("번호","이름","키","몸무게")
student

student <- read.table(file = 'student.txt', header = F)
student

student1 <- read.table(file.choose(), header = T)

student2 <- read.table(file = 'student2.txt', sep = ';',header = T)
student2

student3 <- read.table(file = 'student3.txt',header = T, na.strings = '-')
student3

student4 <- read.csv(file = 'student4.txt',sep = ',',na.strings = '-')
student4

titanic <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv')
titanic

dim(titanic)
str(titanic)

table(titanic$age)
table(titanic$sex)
table(titanic$survived)

head(titanic)
tail(titanic)

tab <- table(titanic$survived, titanic$sex)
tab

barplot(tab, col = rainbow(2), main = '성별에 따른 생존 여부')

x <- 10
y <- 20
z <- x*y
cat("x*y의 결과는 ", x, "입니다\n")
cat("x*y = ", z)

print("x*y의 결과는 ", x, "입니다\n")
print(x)
print('문자열')

library(RSADBE)
data("Severity_Counts")
sink('severity.txt')
severity <- Severity_Counts
severity
sink()

write.table(titanic, 'titanic.txt', row.names = T)
write.table(titanic, 'titanic.txt', row.names = F)

titanic_df <- read.table(file = 'titanic.txt', sep = "", header = T)
titanic_df

getwd()
st.df <- studentx

getwd()
setwd("C:/Temp/Rwork")
getwd()
