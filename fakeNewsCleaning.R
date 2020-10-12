## Libraries
library(tidyverse)
library(tidytext)
library(sentimentr)
library(ngram)

## Read in the Data
fakeNews.train <- read_csv("train.csv")
fakeNews.test <- read_csv("test.csv")
fakeNews <- bind_rows(train=fakeNews.train, test=fakeNews.test,
                      .id="Set")

fakeNews$text <- tolower(fakeNews$text)
fakeNews$author <- tolower(fakeNews$author)

## cleaning authors
# before storing label as a factor
# counts number of fake news authors
(fake <- fakeNews %>%
    filter(label == 1) %>%
    group_by(author) %>%
    summarise(n = sum(label)) %>%
    arrange(-n)) %>% head

# counts number of real news authors
(legit <- fakeNews %>%
    filter(label == 0) %>%
    group_by(author) %>%
    summarise(n = sum(label + 1)) %>%
    arrange(-n)) %>% head

sum( fake$author %in% legit$author) #calculates number of overlap between the two lists

#labels safe authors as "reliable" and fake news authors with "unreliable"
fakeNews$author <- ifelse(fakeNews$author %in% fake$author, "unreliable", "reliable")

### langauage detection
fakeNews$missing_text <- ifelse(is.na(fakeNews$text), 1, 0)
fakeNews$lan_text <- cld2::detect_language(text = fakeNews$text, plain_text = FALSE)

#fakeNews$lan_title <- cld2::detect_language(text = fakeNews$title, plain_text = FALSE)

fakeNews <- fakeNews %>% 
  mutate(lan_text=fct_explicit_na(lan_text, na_level="Missing")) %>%
  mutate(lan_text=fct_lump(lan_text, n=6))


### word count
#strip punctuation, return number of characters, word count, more to add
strip <- function(x){
  stripped <- str_replace_all(x, "[[:punct:]]", "")
  mywords <- wordcount(stripped)
  return(mywords)
}
fakeNews <- fakeNews %>%
  mutate(wcount = map_int(text,strip))

###Lexical Div
token_obj <- quanteda::tokens(fakeNews$text, remove_numbers = FALSE, remove_punct = FALSE, remove_symbols = TRUE, remove_separators = TRUE)

lex_div <- quanteda::textstat_lexdiv(token_obj, measure = c("TTR", "C", "R", "CTTR"))

fakeNews$lex_div <- lex_div$TTR


## Tokenization
sw <- bind_rows(get_stopwords(language="en"), #English
                get_stopwords(language="ru"), #Russian
                get_stopwords(language="es"), #Spanish
                get_stopwords(language="de"), #German
                get_stopwords(language="fr")) #French
sw <- sw %>%
  bind_rows(., data.frame(word="это", lexicon="snowball"))

tidyNews <- fakeNews %>%
  unnest_tokens(tbl=., output=word, input=text)

news.wc <-  tidyNews %>%
  anti_join(sw) %>% 
  count(id, word, sort=TRUE)

all.wc <- news.wc %>% 
  group_by(id) %>% 
  summarize(total = sum(n))

news.wc <- left_join(news.wc, all.wc) %>%
  left_join(x=., y=fakeNews %>% select(id, title))

news.wc <- news.wc %>% mutate(tf=n/total)


a.doc <- sample(news.wc$title,1)
ggplot(data=(news.wc %>% filter(title==a.doc)), aes(tf)) +
  geom_histogram() + ggtitle(label=a.doc)

word.count <- news.wc %>%
  count(word, sort=TRUE) %>%
  mutate(cumpct=cumsum(n)/sum(n))
ggplot(data=word.count, aes(x=1:nrow(word.count), y=cumpct)) + 
  geom_line()
top.words <- word.count %>%
  filter(cumpct<0.30) #takes first 75% of words

news.wc.top <- news.wc %>% filter(word%in%top.words$word) %>%
  bind_tf_idf(word, id, n)


a.doc <- sample(news.wc$title,1)
news.wc.top %>% filter(title==a.doc) %>%
  slice_max(order_by=tf_idf, n=20) %>%
  ggplot(data=., aes(x=reorder(word, tf_idf), y=tf_idf)) + 
  geom_bar(stat="identity") +
  coord_flip() + ggtitle(label=a.doc)

names(news.wc.top)[1] <- "Id"
news.tfidf <- news.wc.top %>%
  pivot_wider(id_cols=Id,
              names_from=word,
              values_from=tf_idf)

## Fix NA's to zero
news.tfidf <- news.tfidf %>%
  replace(is.na(.), 0)

## Merge back with fakeNews data
names(fakeNews)[c(2,6)] <- c("Id", "isFake")
fakeNews.tfidf <- left_join(fakeNews, news.tfidf, by="Id")

## Remaining articles with NAs all have missing text so should get 0 tfidf
fakeNews.clean <- fakeNews.tfidf %>%
  select(-isFake, -title, -author, -text) %>% 
  replace(is.na(.), 0) %>% 
  left_join(fakeNews.tfidf %>% select(Id, isFake, title, author, text),., by="Id")


## Sentiment Sore
sentiment_df <- sentiment_by(get_sentences(fakeNews$text))
fakeNews.clean$tone <- sentiment_df$ave_sentiment

#save
write_csv(x=fakeNews.clean %>% select(-author, -title, -text),
          path="./CleanFakeNews.csv")