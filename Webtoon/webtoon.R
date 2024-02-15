rm(list=ls())
setwd("C:/Users/NJH/Desktop/webtoon")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

raw <- read.csv("naver.csv", header = T)
str(raw)

webtoon <- raw %>% select(-c(id,link))
webtoon$completed <- as.logical(webtoon$completed)
webtoon$free <- as.logical(webtoon$free)

webtoon$title %>% head(10)

webtoon[7,]<-c("가우스전자 시즌1~4","곽백수","에피소드, 개그","다국적 문어발 기업 가우스 전자에서 벌어지는 웃픈 현실 직장인 이야기", 9.97, "2019.10.27 23:20", T, "전체연령가",F)
webtoon[8,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[308,]<-c("닥터 프로스트 시즌1~4","이종범","스토리, 드라마","당신의 마음을 읽는 천재 심리학자 닥터 프로스트. 다양한 인간 군상의 마음의 질병을 파헤치는 그의 활약이 시작된다.", 9.96, "2021.09.30 22:58", T, "15세 이용가",T)
webtoon[309,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[375,]<-c("동네변호사 조들호 시즌1~2","해츨링","스토리, 드라마","본격 생활법정만화 동네변호사 조들호", 8.98, "2017.12.27 23:19", T, "12세 이용가",F)
webtoon[376,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[421,]<-c("레사 시즌1~3","POGO","스토리, 판타지","어느날부턴가, 사람들은 디맨으로 변한다. 매일밤 계속되는 습격, 그리고 동생을 찾는 한 남자. 그 혼돈 속에서 신이 눈을 뜬다!", 9.96, "2020.11.24 23:26", T, "12세 이용가",T)
webtoon[422,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[870,]<-c("스퍼맨 시즌1~2","하일권","스토리, 드라마","사랑하는 여자친구와 드디어 첫날밤을 맞게된 김기두 갑자기 정체모를 사람들에게 납치되고 자신이 슈퍼정자를 가진 능력자라는 얘기를 듣게 되는데...", 9.95, "2020.04.07 20:45", T, "18세 이용가",T)
webtoon[871,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[914,]<-c("심연의 하늘 시즌1~5","윤인완/김선희","스토리, 스릴러","한치 앞도 보이지 않는 어둠 속 서울. 도대체 무슨 일이 벌어진 걸까?", 8.79, "2018.06.24 23:26", T, "18세 이용가",T)
webtoon[915,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
webtoon[916,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[1197,]<-c("웃지 않는 개그반 시즌1~3","현용민","스토리, 개그","미친 예술고등학교 개그반에 입학한 왕진지와 그의 친구들. 그들이 겪는 웃지 못 할 이야기가 펼쳐진다.", 9.24, "2017.09.26 23:24", T, "12세 이용가",T)
webtoon[1198,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[1400,]<-c("질풍기획 시즌1~2","이현민","스토리, 개그","광고대행사 '질풍기획'의 제3기획팀. 평범한 그들의 질풍같은 하루는 오늘도 계속된다.", 9.97, "2015.06.08 23:15", T, "12세 이용가",F)
webtoon[1401,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

which(webtoon$title=="놓지마 정신줄")
which(webtoon$title=="첩보의 별")
which(webtoon$title=="키드갱")
which(webtoon$title=="슈퍼트리오")
which(webtoon$title=="연")

webtoon[290,]<-c("놓지마 정신줄 시즌1~2","신태훈/나승훈","에피소드, 개그","정신, 정주리 남매와 독특한 주변 인물들의 정신줄 놓게 만드는 다양한 에피소드들! 본격 유체이탈 예방 프로젝트 '놓지마 정신줄!'", 9.93, "2019.06.14 23:19", T, "전체연령가",F)
webtoon[291,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[1430,]<-c("첩보의 별 시즌1~2","이상신/국중록","스토리, 개그","배신이 난무하는 그들의 뜨거운 우정본격 스파이 모험 만화!", 9.90, "2018.05.14 23:15", T, "전체연령가",T)
webtoon[1431,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[1478,]<-c("키드갱 시즌1~2","신영우","스토리, 개그","삼류 건달들의 좌충우돌 우왕좌왕 예측불허 육아일기. 트렌드를 이끈 정통 코믹만화의 부활! 2012년, 신영우 작가의 히트 만화 <키드갱>이 네이버 웹툰으로 다시 찾아옵니다.", 9.93, "2014.01.12 23:15", T, "12세 이용가",F)
webtoon[1479,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[846,]<-c("슈퍼트리오 시즌1~2","황미나","스토리, 개그","살아 있는 전설 황미나 작가님의 명작 '슈퍼트리오' 웹툰 리메이크로 슈퍼트리오를 다시 만나다.", 8.31, "2012.06.04 23:10", T, "12세 이용가",F)
webtoon[847,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon[1065,]<-c("연 시즌1~2","구아진","옴니버스, 스릴러","'연'오늘, 지금, 당신에게도 일너어날 수 있는 일들.", 9.78, "2012.08.26 16:40", T, "12세 이용가",F)
webtoon[1066,]<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

webtoon <- na.omit(webtoon)

webtoon$rating <- as.numeric(webtoon$rating)
webtoon$completed <- as.logical(webtoon$completed)
webtoon$free <- as.logical(webtoon$free)

useNIADic()
nouns <- extractNoun(webtoon$title)
wordcount <- table(unlist(nouns))

title_word <- as.data.frame(wordcount, stringsAsFactors = F)
title_word %>% head(10)

title_word2 <- title_word %>% 
  rename(word = Var1, freq = Freq) %>%
  filter(nchar(word) >= 2)           

title_word2 %>% arrange(-freq) %>% head(10)

webtoon$title[str_count(webtoon$title, "드라마원작")==T]

title_word2[title_word2$word=="원작",2] <- 1
title_word2 <- title_word2 %>% filter(word!="시즌1~" & word!="드라마")

top_word_15 <- title_word2 %>% 
  arrange(-freq) %>% head(15)
top_word_15

ggplot(top_word_15, aes(x = reorder(word,-freq), y = freq)) + 
  geom_col() + coord_cartesian(ylim = c(5, 15)) + xlab("단어") + ylab("빈도 수")

pal <- brewer.pal(8, "Blues")
set.seed(700)
wordcloud(words = title_word2$word,
          freq = title_word2$freq,
          min.freq = 1,
          max.words = 200,
          random.order = F,
          scale = c(4, 0.2),
          colors = pal)

webtoon$author %>% head(10)

webtoon$author <- str_replace_all(webtoon$author, ",", "/")
webtoon$author <- str_replace_all(webtoon$author, " ", "")

max(str_count(webtoon$author, "/"))
webtoon$author[str_count(webtoon$author,"/")==3]

author1<-c()
author2<-c()
author3<-c()
author4<-c()

for(i in 1:length(webtoon$author)){
  author1 <- c(author1, str_split(webtoon$author, "/")[[i]][1])
}

for(i in 1:length(webtoon$author)){
  author2 <- c(author2, str_split(webtoon$author, "/")[[i]][2])
}

for(i in 1:length(webtoon$author)){
  author3 <- c(author3, str_split(webtoon$author, "/")[[i]][3])
}

for(i in 1:length(webtoon$author)){
  author4 <- c(author4, str_split(webtoon$author, "/")[[i]][4])
}

author1 %>% head(10)
author4 %>% head(10)
table(author4)

webtoon <- webtoon %>% select(-author) %>% cbind(author1,author2,author3,author4)

webtoon %>% group_by(author1) %>% summarise(mean_rate = mean(rating)) %>% arrange(-mean_rate)

webtoon %>% filter(is.na(author2)==F) %>% group_by(author2) %>% summarise(mean_rate=mean(rating)) %>% arrange(-mean_rate) %>% head(5)
webtoon %>% filter(is.na(author3)==F) %>% group_by(author3) %>% summarise(mean_rate=mean(rating)) %>% arrange(-mean_rate) %>% head(5)

webtoon %>% filter(author1!="웹툰작가") %>% group_by(author1) %>% summarise(n=n()) %>% arrange(-n)

webtoon %>% filter(is.na(author2)==F) %>% group_by(author2) %>% summarise(n=n()) %>% arrange(-n) %>% head(5)
webtoon %>% filter(is.na(author3)==F) %>% group_by(author3) %>% summarise(n=n()) %>% arrange(-n) %>% head(5)

genre1 <- c()
for(i in 1:length(webtoon$genre)){
  genre1 <- c(genre1, str_split(webtoon$genre, ", ")[[i]][1])
}

genre2 <- c()
for(i in 1:length(webtoon$genre)){
  genre2 <- c(genre2, str_split(webtoon$genre, ", ")[[i]][2])
}

table(genre1)
table(genre2)

webtoon <- webtoon %>% select(-genre) %>% cbind(genre1, genre2)

webtoon_genre1 <- webtoon %>%
  group_by(genre1) %>%
  summarise(mean_rate = mean(rating), n=n()) %>%
  arrange(-n)

webtoon_genre1

3088/(3088+382+214)

n_genr1 <- ggplot(data = webtoon_genre1, 
                  aes(x = genre1, y = n)) + geom_col() +
  xlab("전개방식") + ylab("작품 수")

rate_genre1 <- ggplot(data = webtoon_genre1, 
                      aes(x = genre1, y = mean_rate)) + geom_col() + 
  coord_cartesian(ylim = c(9.5, 10.0)) + xlab("전개방식") + ylab("평점")
grid.arrange(n_genr1, rate_genre1, ncol=2)

webtoon_genre2 <- webtoon %>%
  group_by(genre2) %>% 
  summarise(mean_rate = mean(rating), n=n()) %>%
  arrange(-n)

webtoon_genre2

n_genre2 <- ggplot(data = webtoon_genre2, 
                   aes(x = reorder(genre2,-n), y = n)) + geom_col() +
  xlab("장르") + ylab("작품 수")

rate_genre2 <- ggplot(data = webtoon_genre2, 
                      aes(x = reorder(genre2,-n), y = mean_rate)) + geom_col() + 
  coord_cartesian(ylim = c(9.5, 10.0)) + xlab("장르") + ylab("평점")
grid.arrange(n_genre2, rate_genre2, ncol=2)

webtoon$date %>% head(10)
webtoon$year <- str_sub(webtoon$date, 1, 4)
webtoon$year <- as.numeric(webtoon$year)

webtoon_year <- webtoon %>% group_by(year) %>% summarise(n=n())
webtoon_year

ggplot(data = webtoon_year, aes(x = year, y = n)) + geom_col() +
  ylab("작품 수")

summary(webtoon$rating)

boxplot(webtoon$rating, ylim = c(9,10))

ggplot(data = webtoon, aes(x = year, y = rating)) + geom_point() + xlab("연도") + ylab("평점")

ggplot(data = webtoon, aes(x = as.character(year), y = rating)) + geom_boxplot() + coord_cartesian(ylim=c(9,10)) + xlab("연도") + ylab("평점")

rating_year <- webtoon %>% group_by(year) %>% summarise(mean_rate = mean(rating))

ggplot(data = rating_year, aes(x = year, y = mean_rate)) + geom_line() + geom_point() + xlab("연도") + ylab("평균 평점")

pie(table(webtoon$completed), labels = c("X", "O"), main = "완결여부")

webtoon_com_year <- webtoon %>% filter(completed == T) %>% 
  group_by(year) %>% summarise(n=n())
webtoon_com_year

ggplot(data = webtoon_com_year, aes(x = year, y = n)) + geom_col() + xlab("연도") + ylab("당해 완결 작품 수")

pie(table(webtoon$age), main = "나이제한")

pie(table(webtoon$free), labels = c("Yes","No"), main = "기다리면 무료")



