# 유준

# KoNLP패키지 설치
install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip",
                 repos = NULL)

# 실습: 한글 사전과 텍스트 마이닝 관련 패키지 설치
install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")

# 실습: 패키지 로딩
install.packages("hash")
install.packages("tau")
install.packages("devtools")
install.packages("RSQLite")
library(KoNLP)  # 한글 형태소 분석 패키지
library(tm)  # 텍스트 마이닝 분석 패키지
library(wordcloud)  # 워드 클라우드 시각화 패키지\
library(stringr)  # 문자편집 패키지지
useNIADic()  # 한국정보화진흥원 제공 사전 불러오기


# 1. 제공된 데이터를 이용하여 토픽 분석을 실시하여 
# 단어구름으로 시각화 하고 단어 출현 빈도수를 기반하여 
# 어떤 단어들이 주요 단어인지 설명하시오.

# 1단계 : 데이터 가져오기 및 전처리
# 1-1단계 : 데이터 가져오기
setwd('C:/Users/You/Desktop/빅데이터/빅데이터 수업자료/R/사례연구/사례연구 5')
Lincoln <- file('Lincoln.txt', encoding = 'UTF-8')
Lincoln_data <- readLines(Lincoln)
head(Lincoln_data)

# 1-2단계 : 데이터 전처리
a <- extractNoun(Lincoln_data)  # 문자만 추출
a <- gsub('[a-z]+', '', a)  # 소문자 제거
a <- str_replace_all(a, '[하,들]', '')

# 2단계 : 추출된 단어를 대상으로 전처리하기
# 2-1단계 : 추출된 단어를 이용하여 말뭉치(Corpus) 생성
myCorpus <- Corpus(VectorSource(a))
# 2-2단계 : 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 2-3단계 : 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 2-4단계 : 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 2-5단계 : 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))
# 2-6단계 : 전처리 결과 확인
inspect(myCorpusPrepro[1:13])

# 3단계 : 단어 선별(2 ~ 8 음절 사이 단어 선택)하기
# 3-1단계 : 전처리된 단어집에서 단어 대상 선정
myCorpusPrepro_term <- 
  TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4,12)))  # 4글자를 표현하기 위해 수치 설정
myCorpusPrepro_term

# 3-2단계 : matrix 자료구조를 data.frame 자료구조로 변경
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)


# 4단계 : 단어 출변 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult


# 5단계 : 단어구름 만들기
# 5-1단계 : 단어 이름과 빈도수로 data.frame 셍성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df)

# 5-2단계 : 단어 구름 시각화
# library(devtools)
# devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data=word.df, size=2.5, color='random-light', backgroundColor = "black")


# 결과 : 주요단어는 13번 사용된 우리, 5번 사용된 나라. 3번 사용된 자리 국민 신념 이다.


# 2. 제공된 데이터를 이용하여 연관어 분석을 실시하여 
# 연관어를 시각화 하고 시각화 결과에 대해 설명하시오.

# 1단계 : 데이터 가져오기
setwd('C:/Users/You/Desktop/빅데이터/빅데이터 수업자료/R/사례연구/사례연구 5')
Lincoln <- file('Lincoln.txt', encoding = 'UTF-8')
Lincoln_data <- readLines(Lincoln)
head(Lincoln_data)

# 2단계 : 줄 단위 단어 추출
lword <- Map(extractNoun, Lincoln_data)
length(lword)
lword <- unique(lword)  # 중복제거
length(lword)

# 3단계 : 연관어 분석을 위한 전처리
# 3-1단계 : 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x){Filter(filter1, x)}
# 3-2단계 : 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword

# 4단계 : 트랜잭션 생성하기
# 4-1단계 : 연관분석을 위한 패키지 설치와 로딩
# install.packages('arules')
library(arules)
# 4-2단계 : 트랜잭션 생성
wordtran <- as(lword, 'transactions')
wordtran

# 5단계 : 단어 간 연관 규칙 발견하기
# 5-1단계 : 연관규칙 발견
tranrules <- apriori(wordtran, parameter = list(supp = 0.15, conf = 0.5))   # 적정수의 개수를 보여 주기에 설정. 다른값은 없거나, 너무 많음
# 5-2단계 : 연관 규칙 생성 결과보기
detach(package:tm, unload = T)
inspect(tranrules)

# 6단계 : 연관어 시각화하기
# 6-1 : 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules
# 6-2단계 : 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules
# 6-3단계 : 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)
# 6-4단계 : 연관어 시각화를 위한 igraph 패키지 설치와 로딩
# install.packages("igraph")
library(igraph)
# 6-5단계 : edgelist 보기
ruleg <- graph.edgelist(rulemat[c(2:33), ], directed = F)  # 
ruleg
# 단계 6: edgelist 시각화
plot.igraph(ruleg, 
            vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2,
            vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')


# 결과 : '우리'라는 단어가 중심이다. '우리' 라는 단어를 통해서 여러가지 단어들이 연결됨을 볼 수 있다.



# 3. 다음 포털사이트의 실시간 뉴스(https://news.daum.net/)를 
# 수집하고 실시간 토픽분석을 실행하여 단어구름으로 시각화 하고, 
# 분석 시점에서 주요 이슈가 무엇인지 설명하시오.

# 1단계 : 웹 문서 요청과 파싱 관련 패키지 설치 및 로딩
# install.packages("httr")
# install.packages("XML")
library(httr)  # httr 표준 요청을 다루는 패키지
library(XML)  # XML및 문서(DTD 포함)를 읽고 생성하기 위한 여러 접근 방식, 로컬 및 HTTP 또는 FTP를 통해 액세스 가능.
library(tm)

# 2단계 : url요청
url <- "http://news.daum.net"
web <- GET(url)
web

# 3단계 : HTML 파싱
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)

# 4단계 :  태그(tag) 자료 수집
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news

# 5단계 : 수집한 자료 전처리
# 5-1단계: 자료 전처리 - 수집한 문서를 대상으로 불용어 제거
news_pre <- gsub("[\r\n\t]", ' ', news)  # r태그, t태그 제거
news_pre <- gsub('[[:punct:]]', ' ', news_pre) # 구두점 문자, 특수문자 제거
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre) # \n, \r같은 문자 제거
news_pre <- gsub('\\d+', ' ', news_pre) # corona19(covid19) 때문에 숫자 제거 생략
news_pre <- gsub('[a-z]+', ' ', news_pre) # 소문자 영어 제거
news_pre <- gsub('[A-Z]+', ' ', news_pre) # 대문자 영어 제거
news_pre <- gsub('\\s+', ' ', news_pre)  # 간격 제거
news_pre
# 5-2단계 : 기사와 관계없는 'TODAY', '검색어 순위' 등의 내용은 제거
news_data <- news_pre[1:59]
news_data


# 6단계 : 단어 추출 사용자 함수 정의
# 6-1단계 : 사용자 정의 함수 작성
exNouns <- function(x){paste(extractNoun(x), collapse = ' ')}
# 6-2단계 : exNouns() 함수를 이용하여 단어 추출
news_nouns <- sapply(news_data, exNouns)
news_nouns
# 6-3단계 : 추출 결과 확인
str(news_nouns)

# 7단계 : 말뭉치 생성과 집계 행렬 만들기
# 7-1단계 : 추출된 단어를 이용한 말뭉치 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])
# 7-2단계 : 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 18)))  # 4글자의 단어가 있기에 8음절
TDM
# 7-3단계 : matrix 자료구조 data.frame으로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
# 7-4단계 : 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = T)
wordResult[1:10]


# 8단계 : 단어 구름 생성
# 8-1단계 : 패키지 로딩과 단어 이름 추출
myNames <- names(wordResult)
myNames
# 8-2단계 : 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df)
# 8-3단계 : 단어 구름 생성
library(wordcloud2)
wc2data <- data.frame(df$word, df$freq)
wordcloud2(data=wc2data, size=0.8, color='random-light', backgroundColor = "black")  # 큰


# 결과 : 21년 11월 24일 이재명 대선후보자가 제일 큰 이슈고, 삼성 투자가 두번째 이슈, 무단횡단 치료 사회가 세번재 이슈이다.

