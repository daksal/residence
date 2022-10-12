#패키지 불러오기
c(
  "rio", "psych", "psychTools", "skimr", "janitor", 
  "tidyverse", "tidytable", "tidymodels", "lm.beta",
  "GGally", "ggforce", "gt", "mdthemes", "patchwork",
  "r2symbols", "equatiomatic", "purrr", "palmerpenguins",
  "textdata", "tidytext", "epubr", "stm", "quarto",
  "RcppMeCab", "KoNLP", "tidyr", "dplyr", "tidylo", "lubridate"
) -> pkg 

lapply(pkg, require, ch = T)

sapply(pkg, function(x){
  if(!require(x, ch = T)) install.packages(x)
  library(x, ch = T)
})


#파일경로 설정
setwd("C:/Users/YH JUNG/Documents/R/GIT/residence/data")
getwd()

#감정어 빈도
##1감정어 사전 설치
url_v <- "https://github.com/park1200656/KnuSentiLex/archive/refs/heads/master.zip"
dest_v <- "data/knusenti.zip"

download.file(url = url_v, 
              destfile = dest_v,
              mode = "wb")

list.files("data/.")
list.files("data/KnuSentiLex-master/")

##2사전내용 선택
senti_name_v <- list.files("data/KnuSentiLex-master/.")[9]
senti_dic_df <- read_tsv("data/KnuSentiLex-master/SentiWord_Dict.txt", col_names = F)

##3 감정사전 할당
senti_dic_df <- senti_dic_df %>% rename(word = X1, sScore = X2) 
senti_dic_df %>% 
  mutate(emotion = ifelse(sScore >= 1, "긍정",
                          ifelse(sScore <= -1, "부정", "중립"))) %>% 
  count(emotion)

knu_dic_df <- senti_dic_df %>% 
  filter(!is.na(sScore))

##4 데이터 불러오기
yanolja_df <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/yanolja_after_covid.xlsx") %>% 
  select.(제목, 언론사, 본문, URL)

hah_df <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/hah_after_covid.xlsx") %>% 
  select.(제목, 언론사, 본문, URL)

yanolja <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/yanolja_after_covid.xlsx")
hah <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/hah_after_covid.xlsx")
### 정제
yanolja_tk <- 
  yanolja_df %>% 
  drop_na() %>% 
  unnest_tokens(word, 제목, token = pos) %>% 
  separate(word, c("word", "pos"), sep = "/") %>% 
  filter(pos == 'nng')

hah_tk <- 
  hah_df %>% 
  drop_na() %>% 
  unnest_tokens(word, 제목, token = pos) %>% 
  separate(word, c("word", "pos"), sep = "/") %>% 
  filter(pos == 'nng')

##5 데이터 감정어 분석 야놀자
yanolja_senti_df <- yanolja_tk %>% 
  unnest_tokens(word, 본문, token = pos) %>% 
  separate(col = word, 
           into = c("word", "morph"), 
           sep = "/" ) %>% 
  inner_join(knu_dic_df) %>% 
  count(word, sScore, sort = T) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_head(n = 15) 

yanolja_senti_df %>% 
  ggplot()+ 
  geom_col(aes(n, word, fill = sScore), show.legend = F) 

#감정분석 점수
yanolja_tk %>% 
  unnest_tokens(word, 본문) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
  arrange(sScore)

#감정 긍정 부정 수
yanolja_tk %>% 
  unnest_tokens(word, 본문) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = ifelse(sScore >= 1, "긍정",
                         ifelse(sScore <= -1, "부정", "중립"))) %>% 
  count(sScore)
#감정 점수
yanolja_tk %>% 
  unnest_tokens(word, 본문) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
  summarise(emotion = sum(sScore))

#감정 표 상위 50개
yanolja_tk %>% 
  unnest_tokens(word, 본문) %>% 
  inner_join(knu_dic_df) %>% 
  mutate(emotion = ifelse(sScore > 0, "긍정", ifelse(sScore < 0, "부정", "중립"))) %>% 
  filter(emotion != "중립") %>% 
  count(word, emotion, sort = T) %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(str_length(word) > 1) %>% 
  top_n(20) %>% 
  ggplot(aes(word, n, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~emotion, scales = "free_y") +
  labs(y = "단어 빈도",
       x = "감정 단어",
       title = "야놀자 긍정어 부정어") +
  coord_flip()


### 워드크라우드 야놀자
library(wordcloud)
yanolja_tk %>% 
  unnest_tokens(word, 본문) %>% 
  inner_join(knu_dic_df) %>% 
  mutate(emotion = ifelse(sScore > 0, "긍정", ifelse(sScore < 0, "부정", "중립"))) %>% 
  filter(emotion != "중립") %>% 
  count(word, emotion, sort = T) %>% 
  filter(str_length(word) > 1) %>% 
  reshape2::acast(word ~ emotion, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("blue", "red"), max.words = 50)


##6 데이터 감정어 분석 여기어때
hah_senti_df <- hah_tk %>% 
  unnest_tokens(word, 본문, token = pos) %>% 
  separate(col = word, 
           into = c("word", "morph"), 
           sep = "/" ) %>% 
  inner_join(knu_dic_df) %>% 
  count(word, sScore, sort = T) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_head(n = 15)

 hah_senti_df %>% 
  ggplot()+ 
  geom_col(aes(n, word, fill = sScore), show.legend = F) 

 hah_tk %>% 
   unnest_tokens(word, 본문) %>% 
   left_join(knu_dic_df) %>% 
   mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
   arrange(sScore)
 
 hah_tk %>% 
   unnest_tokens(word, 본문) %>% 
   left_join(knu_dic_df) %>% 
   mutate(sScore = ifelse(sScore >= 1, "긍정",
                          ifelse(sScore <= -1, "부정", "중립"))) %>% 
   count(sScore)
 
 hah_tk %>% 
   unnest_tokens(word, 본문) %>% 
   left_join(knu_dic_df) %>% 
   mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
   summarise(emotion = sum(sScore))
 
 #여기어때 감정분석 긍정부정표
hah_tk %>% 
   unnest_tokens(word, 본문) %>% 
   inner_join(knu_dic_df) %>% 
   mutate(emotion = ifelse(sScore > 0, "긍정", ifelse(sScore < 0, "부정", "중립"))) %>% 
   filter(emotion != "중립") %>% 
   count(word, emotion, sort = T) %>% 
   mutate(word = reorder(word, n)) %>% 
   filter(str_length(word) > 1) %>% 
   top_n(20) %>% 
   ggplot(aes(word, n, fill = emotion)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~emotion, scales = "free_y") +
  labs(y = "단어 빈도",
       x = "감정 단어",
       title = "여기어때 긍정어 부정어") +
  coord_flip()

### 워드크라우드 - 여기어때
hah_tk %>% 
  unnest_tokens(word, 본문) %>% 
  inner_join(knu_dic_df) %>% 
  mutate(emotion = ifelse(sScore > 0, "긍정", ifelse(sScore < 0, "부정", "중립"))) %>% 
  filter(emotion != "중립") %>% 
  count(word, emotion, sort = T) %>% 
  filter(str_length(word) > 1) %>% 
  reshape2::acast(word ~ emotion, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("blue", "red"), max.words = 50)

#2 총빈도
##1 야놀자 총빈도
yanolja_total <- yanolja_tk %>% 
  count(word, sort = T) %>%
  slice_max(n, n = 15) %>% 
  ggplot(aes(x = n,
             y = reorder(word, n))) +
  geom_col(show.legend = F)

##2여기어때 총빈도
hah_total <- hah_tk %>% 
  count(word, sort = T) %>%
  slice_max(n, n = 15) %>% 
  ggplot(aes(x = n,
             y = reorder(word, n))) +
  geom_col(show.legend = F)

#3 상대빈도
##1 야놀자 상대빈도


yanolja_tk %>% 
  count(언론사, word) %>% 
  bind_log_odds(set = 언론사,
                feature = word,
                n = n) %>% 
  group_by(언론사) %>% 
  slice_max(abs(log_odds_weighted), n = 7) %>% 
  
  ggplot(aes(x = log_odds_weighted,
             y = reorder(word, log_odds_weighted),
             fill = 언론사)
  ) +
  geom_col(show.legend = F) +
  facet_wrap( ~ 언론사, scales = "free")

##2 여기어때 상대빈도
hah_tk %>% 
  count(언론사, word) %>% 
  bind_log_odds(set = 언론사,
                feature = word,
                n = n) %>% 
  group_by(언론사) %>% 
  slice_max(abs(log_odds_weighted), n = 2) %>% 
  ggplot(aes(x = log_odds_weighted,
             y = reorder(word, log_odds_weighted))) +
  geom_col(show.legend = F)

#주제모형
yanolja_tk %>% 
  unnest_tokens(sentence, 본문, token = "sentences") %>% 
  mutate(lineID = row_number()) %>% 
  mutate(lineID = as.factor(lineID)) %>% 
  unnest_tokens(word, sentence) %>% 
  count(lineID, word) %>% 
  anti_join(stop_words) %>% 
  rename(tfd = n)

#주제모형(공변인)
yanolja_sub <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/yanolja_after_covid.xlsx") %>% 
  select(일자, 제목, 본문, 언론사, cat = `통합 분류1`, 키워드)  

hah_sub <- readxl::read_excel("C:/Users/YH JUNG/Documents/R/GIT/residence/data/hah_after_covid.xlsx") %>% 
  select(일자, 제목, 본문, 언론사, cat = `통합 분류1`, 키워드) 

fullchar_v <- "ㆍ|ㅣ|‘|’|“|”|○|●|◎|◇|◆|□|■|△|▲|▽|▼|〓|◁|◀|▷|▶|♤|♠|♡|♥|♧|♣|⊙|◈|▣"

##야놀자 정제
yanolja_sub2 <- yanolja_sub %>% 
  # 중복기사 제거
  distinct(제목, .keep_all = T) %>% 
  # 기사별 ID부여
  mutate(ID = factor(row_number())) %>% 
  # 월별로 구분한 열 추가(lubridate 패키지)
  mutate(week = week(ymd(일자))) %>%       
  # 기사 제목과 본문 결합
  unite(제목, 본문, col = "text", sep = " ") %>% 
  # 중복 공백 제거
  mutate(text = str_squish(text)) %>% 
  # 언론사 구분: 야당지, 여당지 %>% 
  mutate(press = case_when(
    언론사 == "조선일보" ~ "야당지",
    언론사 == "중앙일보" ~ "야당지",
    언론사 == "동앙일보" ~ "야당지",
    언론사 == "국민일보" ~ "야당지",
    언론사 == "문화일보" ~ "야당지",
    언론사 == "한국경제" ~ "야당지",
    언론사 == "경향신문" ~ "여당지",
    TRUE ~ "여당지") ) %>% 
  separate(cat, sep = ">", into = c("cat", "cat2")) %>% 
  select(-cat2) %>% 
  mutate(catSoc = case_when(
    cat == "경제" ~ "경제면",
    cat == "문화" ~ "경제면",
    TRUE ~ "비경제면") )

#야놀자 주제별 갯수
yanolja_sub2 %>% count(cat, sort = T)

#야놀자 데이터 정제 후 토큰화
yanolja_S <- 
  yanolja_sub2 %>% 
  mutate(키워드 = str_remove_all(키워드, "[^(\\w+|\\d+|,)]")) %>% 
  mutate(키워드 = str_remove_all(키워드, fullchar_v)) %>% 
  unnest_tokens(word, 키워드, token = "regex", pattern = ",") 

#야놀자 데이터 결합
yanolja_C <-
  yanolja_S %>%
  group_by(ID) %>%
  summarise(text2 = str_flatten(word, " ")) %>%
  ungroup() %>% 
  inner_join(yanolja_sub2, by = "ID")

#야놀자 토픽 모델링
processed_yanol <-
  yanolja_C %>% textProcessor(
    documents = yanolja_C$text2,
    metadata = .,
    wordLengths = c(2, Inf)
  )

out_yanol <-
  prepDocuments(processed_yanol$documents,
                processed_yanol$vocab,
                processed_yanol$meta,
                lower.thresh = 0)
summary(out_yanol)

docs_yanol <- out_yanol$documents
vocab_yanol <- out_yanol$vocab
meta_yanol <- out_yanol$meta

#주제 갯수
topicN <- c(3, 9, 100)

storage_yanol <- searchK(docs_yanol, vocab_yanol, K = topicN)
storage_yanol
plot(storage_yanol)

t1 <- Sys.time()
meta_fit_yanol <-
  stm(
    documents = docs_yanol,
    vocab = vocab_yanol,
    data = meta_yanol,
    K = 9,         
    prevalence =~ press + s(week, 6), # 투입하는 공변인
    max.em.its = 75,                # 최대 반복계산 회수 
    verbose = F,                    # 반복계산결과 화면출력 여부
    init.type = "Spectral",
    seed = 37 
  )
t2 <- Sys.time()
t2-t1
#주제 이름
labelTopics(meta_fit_yanol)
topic_name_yanol <- tibble(topic = 1:9,
                     name = c("1. 투자",
                              "2. 여행",
                              "3. 인터파크 인수",
                              "4. 마케팅",
                              "5. 매출",
                              "6. 주식",
                              "7. 기업",
                              "8. 거래",
                              "9. 야놀자 X 현대카드") )

td_beta_yanol <- meta_fit_yanol %>% tidy(matrix = 'beta') 

term_topic_name_yanol <- 
  td_beta_yanol %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  left_join(topic_name_yanol, by = "topic")

term_topic_name_yanol %>% 
  ggplot(aes(x = beta, 
             y = reorder_within(term, beta, name),  # 각 주제별로 재정렬
             fill = name)) +
  geom_col(show.legend = F) +
  facet_wrap(~name, scales = "free") +
  scale_y_reordered() +                             # 재정렬한 y축의 값 설정
  labs(x = expression("단어 확률분포: "~beta), y = NULL,
       title = "주제별 단어 확률 분포",
       subtitle = "주제별로 다른 단어들로 군집") +
  theme(plot.title = element_text(size = 20))

#감마분석
td_gamma_yanol <- meta_fit_yanol %>% tidy(matrix = 'gamma') 

doc_topic_name_yanol <- 
  td_gamma_yanol %>% 
  group_by(topic) %>% 
  left_join(topic_name_yanol, by = "topic")
#주제별 7개 단어 선정
top_terms_yanol <- 
  td_beta_yanol %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  select(topic, term) %>% 
  summarise(terms = str_flatten(term, collapse = ", ")) 

# 주제별 감마 평균 계산  
gamma_terms_yanol <- 
  td_gamma_yanol %>% 
  group_by(topic) %>% 
  summarise(gamma = mean(gamma)) %>% 
  left_join(top_terms, by = 'topic') %>%  # 주제별 단어 데이터프레임과 결합
  left_join(topic_name, by = 'topic')     # 주제 이름 데이터프레임과 결합

#상위 주제어 그래프
gamma_terms_yanol %>% 
  
  ggplot(aes(x = gamma, y = reorder(name, gamma), fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(gamma, 2)), # 소수점 2자리 
            hjust = 1.15) +                # 라벨을 막대도표 안쪽으로 이동
  geom_text(aes(label = terms), 
            hjust = -0.05) +              # 단어를 막대도표 바깥으로 이동
  scale_x_continuous(expand = c(0, 0),    # x축 막대 위치를 Y축쪽으로 조정
                     limit = c(0, .8)) +   # x축 범위 설정
  labs(x = expression("문서 확률분포"~(gamma)), y = NULL,
       title = "야놀자 관련 상위 주제어",
       subtitle = "주제별로 기여도가 높은 단어 중심") +
  theme(plot.title = element_text(size = 20))

##여기어때 정제
hah_sub2 <- hah_sub %>% 
  # 중복기사 제거
  distinct(제목, .keep_all = T) %>% 
  # 기사별 ID부여
  mutate(ID = factor(row_number())) %>% 
  # 월별로 구분한 열 추가(lubridate 패키지)
  mutate(week = week(ymd(일자))) %>%       
  # 기사 제목과 본문 결합
  unite(제목, 본문, col = "text", sep = " ") %>% 
  # 중복 공백 제거
  mutate(text = str_squish(text)) %>% 
  # 언론사 구분: 야당지, 여당지 %>% 
  mutate(press = case_when(
    언론사 == "조선일보" ~ "야당지",
    언론사 == "중앙일보" ~ "야당지",
    언론사 == "동앙일보" ~ "야당지",
    언론사 == "국민일보" ~ "야당지",
    언론사 == "문화일보" ~ "야당지",
    언론사 == "한국경제" ~ "야당지",
    언론사 == "경향신문" ~ "여당지",
    TRUE ~ "여당지") ) %>% 
  separate(cat, sep = ">", into = c("cat", "cat2")) %>% 
  select(-cat2) %>% 
  mutate(catSoc = case_when(
    cat == "경제" ~ "경제면",
    cat == "문화" ~ "경제면",
    TRUE ~ "비경제면") )

#여기어때 주제별 갯수
hah_sub2 %>% count(cat, sort = T)

#여기어때 데이터 정제 후 토큰화
hah_S <- 
  hah_sub2 %>% 
  mutate(키워드 = str_remove_all(키워드, "[^(\\w+|\\d+|,)]")) %>% 
  mutate(키워드 = str_remove_all(키워드, fullchar_v)) %>% 
  unnest_tokens(word, 키워드, token = "regex", pattern = ",") 

#여기어때 데이터 결합
hah_C <-
  hah_S %>%
  group_by(ID) %>%
  summarise(text2 = str_flatten(word, " ")) %>%
  ungroup() %>% 
  inner_join(hah_sub2, by = "ID")

#여기어때 토픽 모델링
processed_hah <-
  hah_C %>% textProcessor(
    documents = hah_C$text2,
    metadata = .,
    wordLengths = c(2, Inf)
  )

out_hah <-
  prepDocuments(processed_hah$documents,
                processed_hah$vocab,
                processed_hah$meta,
                lower.thresh = 0)
summary(out_hah)

docs_hah <- out_hah$documents
vocab_hah <- out_hah$vocab
meta_hah <- out_hah$meta

#주제 갯수
topicN <- c(3, 9, 100)

storage_hah <- searchK(docs_hah, vocab_hah, K = topicN)
storage_hah
plot(storage_hah)

t1 <- Sys.time()
meta_fit_hah <-
  stm(
    documents = docs_hah,
    vocab = vocab_hah,
    data = meta_hah,
    K = 9,         
    prevalence =~ press + s(week, 6), # 투입하는 공변인
    max.em.its = 75,                # 최대 반복계산 회수 
    verbose = F,                    # 반복계산결과 화면출력 여부
    init.type = "Spectral",
    seed = 37 
  )
t2 <- Sys.time()
t2-t1
#주제 이름
labelTopics(meta_fit_hah)
topic_name_hah <- tibble(topic = 1:9,
                         name = c("1. 여행",
                                  "2. 계약",
                                  "3. 기업",
                                  "4. 할인 해택",
                                  "5. 성수기 예약",
                                  "6. 여행테마",
                                  "7. 투자",
                                  "8. 할인해택",
                                  "9. 범죄") )

td_beta_hah <- meta_fit_hah %>% tidy(matrix = 'beta') 

term_topic_name_hah <- 
  td_beta_hah %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  left_join(topic_name_hah, by = "topic")

term_topic_name_hah %>% 
  ggplot(aes(x = beta, 
             y = reorder_within(term, beta, name),  # 각 주제별로 재정렬
             fill = name)) +
  geom_col(show.legend = F) +
  facet_wrap(~name, scales = "free") +
  scale_y_reordered() +                             # 재정렬한 y축의 값 설정
  labs(x = expression("단어 확률분포: "~beta), y = NULL,
       title = "주제별 단어 확률 분포",
       subtitle = "주제별로 다른 단어들로 군집") +
  theme(plot.title = element_text(size = 20))

#감마분석
td_gamma_hah <- meta_fit_hah %>% tidy(matrix = 'gamma') 

doc_topic_name_hah <- 
  td_gamma_hah %>% 
  group_by(topic) %>% 
  left_join(topic_name_hah, by = "topic")
#주제별 7개 단어 선정
top_terms_hah <- 
  td_beta_hah %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  select(topic, term) %>% 
  summarise(terms = str_flatten(term, collapse = ", ")) 

# 주제별 감마 평균 계산  
gamma_terms_hah <- 
  td_gamma_hah %>% 
  group_by(topic) %>% 
  summarise(gamma = mean(gamma)) %>% 
  left_join(top_terms, by = 'topic') %>%  # 주제별 단어 데이터프레임과 결합
  left_join(topic_name, by = 'topic')     # 주제 이름 데이터프레임과 결합

#상위 주제어 그래프
gamma_terms_hah %>% 
  
  ggplot(aes(x = gamma, y = reorder(name, gamma), fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(gamma, 2)), # 소수점 2자리 
            hjust = 1.15) +                # 라벨을 막대도표 안쪽으로 이동
  geom_text(aes(label = terms), 
            hjust = -0.05) +              # 단어를 막대도표 바깥으로 이동
  scale_x_continuous(expand = c(0, 0),    # x축 막대 위치를 Y축쪽으로 조정
                     limit = c(0, .8)) +   # x축 범위 설정
  labs(x = expression("문서 확률분포"~(gamma)), y = NULL,
       title = "야놀자 관련 상위 주제어",
       subtitle = "주제별로 기여도가 높은 단어 중심") +
  theme(plot.title = element_text(size = 20))
