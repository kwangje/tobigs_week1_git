### R_assignment_백광제

library(stringr)
library(dplyr)
library(ggplot2)

setwd("~/Project/ToBigs/Week1/R")


## 1. is_edible column 

EdibleOrDiscard <- function(X)
{
  cow_data = read.csv("cow_data.csv", header=T, stringsAsFactors = T, 
                      fileEncoding = "EUC-KR", encoding = "UTF-8")
  cow_data$grade <- as.character(cow_data$grade)
  
  # Q. grade 2개 조건("3", "등외") 설정하는 방법 모르겠음.
  cow_data2 <- cow_data %>%
                mutate(is_edible = ifelse((age >= 50 & grade ==("3")), 
                                          "폐기용", "식용"))
  cow_data3 <- cow_data2 %>%
                mutate(is_edible = ifelse((age >= 50 & grade ==("등외")), 
                                          "폐기용", "식용"))
  
  return(cow_data3)
}

EdibleOrDiscard(cow_data)


## 2. grade "1++" 가장 많은 Address 구하기

cow_data = read.csv("cow_data.csv", header=T, stringsAsFactors = T, 
                    fileEncoding = "EUC-KR", encoding = "UTF-8")
str(cow_data)

# text split
cow_data$grade <- as.character(cow_data$grade)
cow_data$address <- as.character(cow_data$address)
cow_data$address <- str_split(cow_data$address, pattern =  " ", n = 3, simplify = TRUE)
cow_data$address <- cow_data$address[, 2]
head(cow_data)

# sub-sampling
cow_data2 <- subset(cow_data, grade == "1++")
cow_data3 <- select(cow_data2, grade, address)
head(cow_data3)

# group_by
sort(table(cow_data3$address))
highrank_grade_address <- head(sort(table(cow_data3$address), decreasing = T), n=3)
highrank_grade_address


## 3. Mean Price per Address

# handling
cow_data4 <- subset(cow_data, select = c(address, grade, price), 
                    address == c("정읍시", "고흥군", "안성시"))
head(cow_data4, n=20)
str(cow_data4)

cow_data4$price <- as.integer(cow_data4$price)
str(cow_data4)

# group by & mean(price)
cow_data4 %>%
  group_by(address, grade) %>% 
  summarise(mean_price = mean(price)) %>%
  arrange(desc(address), desc(mean_price))

  # Q. 결과가 이상하다 ㅠㅠㅠ 1등급보다 3등급이 비싸게 나왔다 !!!


# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 

# sub-sampling
cow_data5 <- subset(cow_data, select = c(address, slaughter_date), 
                    address == c("정읍시", "고흥군", "안성시"))
str(cow_data5)

cow_data5 <- transform(cow_data5, 
				slaughter_date = as.Date(as.character(slaughter_date), "%Y%m%d"))
head(cow_data5, n=20)

cow_data6 <- cow_data5 %>%
              mutate(month = format(slaughter_date, "%m"), 
                     year = format(slaughter_date, "%Y")) %>%
              group_by(address, month, year) %>%
              summarise(numofcows = n()) %>%
              arrange(address, month)
head(cow_data6, n=15)

# visualization
bar <- ggplot(cow_data6, aes(x=month, y=numofcows, fill=address))
bar+geom_bar(stat="identity", position="dodge")

# 한글 폰트 설정 : 이해는 못했음 ㅠㅠ
install.packages('extrafont')
library(extrafont)

theme.ti <- element_text(family="NanumGothic", face="bold", size=12)
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5)
theme.leti<-element_text(family="NanumGothic", face="bold") 
theme.lete<-element_text(family="NanumGothic")

bar <- ggplot(cow_data6, aes(x=month, y=numofcows, fill=address)) + 
  theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)

bar+geom_bar(stat="identity", position="dodge")

