---
---
title: "BAHCS-10 Prelim Results"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(prettyR)
library(lme4)
#library(lmerTest)
library(MuMIn)
library(HLMdiag)
library(MASS)
library(descr)
library(brms)
library(future)
library(caret)
#install_github("iqss-research/VA-package")
#install_github("iqss-research/ReadMeV1")
library(ReadMe)
library(tidyr)
library(tm)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(topicmodels)
library(modeltools)
library(methods)
library(stringr)
```
Get the data and get rid of missing values

Need to grab demographics for each of them first

Don't get rid of NAs, because those might have valuable information
```{r}
head(adult)
adult = data.frame(Like = adult$X5..Like.Best, Help = adult$X6..How.Help, Improve = adult$X7..Like.Least.Improve)

sum(is.na(adult))
dim(adult)

youth = data.frame(Like = youth$X5..Like.Best, Help = youth$X6..How.Help, Improve = youth$X7..Life.Least)

both = rbind(adult, youth)
dim(both)
both$id = 1:dim(both)[1]
head(both)
```
Try topic modeling
Change int dtm, which is one row for the response, one column for word, and the cell is the number of times someone used that statement

I think you need to so some cleaning.  Need to stem the data and figure out what else you need to do

Document = response id
Term = word
Count = count of word for that response

Clean data
Get rid of NAs
Make all lower case
get rid of stopwords
remove Punctuation
```{r}
dim(both)
both = na.omit(both)
both_words = data.frame(Like = both$Like, Help = both$Help, Improve = both$Improve)
both_words = apply(both_words, 2, function(x){tolower(x)})
both_words = apply(both_words, 2, function(x){removePunctuation(x)})
both_words = apply(both_words, 2, function(x){stemDocument(x)})
both_words = data.frame(both_words)

both_like = data.frame(id = both$id,Like = both_words$Like)
both_like$Like = as.character(both_like$Like)
```
Try step by step
Need each row to be a person, each column to be a word, and each cell to be a count.  The count is the number of times 

Ok so unnest get the responses out by word (so mulitple rows per person)
Then group_by although not sure if that is necessary
Then you count which gives you the number of times that person used that word in the document per cell.
```{r}

bothCount = both_like %>%
  unnest_tokens(word, Like)  %>%
  anti_join(stop_words) %>%
  group_by(id)
  
bothCount = count(bothCount)
dim(bothCount)


  
colnames(bothCount) =c("document", "term", "count")
bothCount$document = as.integer(bothCount$document)
bothCountDTM = bothCount %>%
  cast_dtm(document, term, count)
bothCountDTM
inspect(bothCountDTM)
```
Now try running the model
```{r}
lda_3 = LDA(bothCountDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3
topics_lda_3 = tidy(lda_3, matrix = "beta")
topics_lda_3

lda_4 = LDA(bothCountDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4
topics_lda_4 = tidy(lda_4, matrix = "beta")
topics_lda_4
```
Now plot the top ten terms
```{r}
terms_lda_3 <- topics_lda_3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3


terms_lda_4 <- topics_lda_4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4
```
Now get a plot with them
Need complete dictionary
```{r}
library(ggplot2)
terms_lda_3 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

```
Now you can also see which words had the greatest difference in the probabilty of being in topic one versus topic two and so.  This can help us understand what the differences are bewteen the topics. 
```{r}
beta_spread = topics_lda_4 %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001) %>%
  mutate(log_ratio_top2_1 = log2(topic2/topic1)) %>%
  mutate(log_ratio_top3_1 = log2(topic3/topic1)) %>%
  mutate(log_ratio_top4_1 = log2(topic4/topic1)) %>%
  mutate(log_ratio_top3_2 = log2(topic3/topic2)) %>%
  mutate(log_ratio_top4_2 = log2(topic2/topic1)) %>%
  mutate(log_ratio_top4_3 = log2(topic4/topic1))

beta_spread

### Get top ten items 

ap_top_terms <- beta_spread %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

```
























