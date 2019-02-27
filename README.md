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
library(prettyR)
library(psych)
library(dplyr)
library(plyr)
library(descr)
library(tidyr)
library(tm)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(topicmodels)
library(modeltools)
library(methods)
library(stringr)
library(ggplot2)
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

Create three data sets: Like, Help, Improve
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

both_help = data.frame(id = both$id,help = both_words$Help)
both_help$help = as.character(both_help$help)

both_improve = data.frame(id = both$id,improve = both_words$Improve)
both_improve$improve = as.character(both_improve$improve)

```
Try step by step
Need each row to be a person, each column to be a word, and each cell to be a count.  The count is the number of times 

Ok so unnest get the responses out by word (so mulitple rows per person)
Then group_by although not sure if that is necessary
Then you count which gives you the number of times that person used that word in the document per cell.

Doing this for Like, Help, Improve
```{r}

##Like
bothCountLike = both_like %>%
  unnest_tokens(word, Like)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountLike = count(bothCountLike)
dim(bothCountLike)



colnames(bothCountLike) =c("document", "term", "count")
bothCountLike$document = as.integer(bothCountLike$document)
bothCountLikeDTM = bothCountLike %>%
  cast_dtm(document, term, count)
inspect(bothCountLikeDTM)

### Help

bothCountHelp = both_help %>%
  unnest_tokens(word, help)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountHelp = count(bothCountHelp)
dim(bothCountHelp)



colnames(bothCountHelp) =c("document", "term", "count")
bothCountHelp$document = as.integer(bothCountHelp$document)
bothCountHelpDTM = bothCountHelp %>%
  cast_dtm(document, term, count)
inspect(bothCountHelpDTM)


###Improve
bothCountImprove = both_improve %>%
  unnest_tokens(word, improve)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountImprove = count(bothCountImprove)
dim(bothCountImprove)


colnames(bothCountImprove) =c("document", "term", "count")
bothCountImprove$document = as.integer(bothCountImprove$document)
bothCountImproveDTM = bothCountImprove %>%
  cast_dtm(document, term, count)
inspect(bothCountImproveDTM)


```
Now try running the models 
3:5 for Like, Help, Improve
```{r}
lda_3_like = LDA(bothCountLikeDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_like
topics_lda_3_like = tidy(lda_3_like, matrix = "beta")
topics_lda_3_like

lda_4_like = LDA(bothCountLikeDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_like
topics_lda_4_like = tidy(lda_4_like, matrix = "beta")
topics_lda_4_like

lda_5_like = LDA(bothCountLikeDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_like
topics_lda_5_like = tidy(lda_5_like, matrix = "beta")
topics_lda_5_like

lda_3_help = LDA(bothCountHelpDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_help
topics_lda_3_help = tidy(lda_3_help, matrix = "beta")
topics_lda_3_help

lda_4_help = LDA(bothCountHelpDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_help
topics_lda_4_help = tidy(lda_4_help, matrix = "beta")
topics_lda_4_help

lda_5_help = LDA(bothCountHelpDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_help
topics_lda_5_help = tidy(lda_5_help, matrix = "beta")
topics_lda_5_help


lda_3_improve = LDA(bothCountImproveDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_improve
topics_lda_3_improve = tidy(lda_3_improve, matrix = "beta")
topics_lda_3_improve

lda_4_improve = LDA(bothCountImproveDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_improve
topics_lda_4_improve = tidy(lda_4_improve, matrix = "beta")
topics_lda_4_improve

lda_5_improve = LDA(bothCountImproveDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_improve
topics_lda_5_improve = tidy(lda_5_improve, matrix = "beta")
topics_lda_5_improve

```
Now plot the top ten terms
```{r}
terms_lda_3_like <- topics_lda_3_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_like


terms_lda_4_like <- topics_lda_4_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_like

terms_lda_5_like <- topics_lda_5_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_like

terms_lda_3_help <- topics_lda_3_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_help


terms_lda_4_help <- topics_lda_4_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_help

terms_lda_5_help <- topics_lda_5_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_help

terms_lda_3_improve <- topics_lda_3_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_improve


terms_lda_4_improve <- topics_lda_4_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_improve

terms_lda_5_improve <- topics_lda_5_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_improve

```
Plots for Like 3,4,5
```{r}

terms_lda_3_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

```
Plots for Help 3,4,5
```{r}
terms_lda_3_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

```
Plots for Improve 3,4,5
```{r}
terms_lda_3_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```
#################################
Without stemming
################################
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

Create three data sets: Like, Help, Improve
```{r}

dim(both)
both = na.omit(both)
both_words = data.frame(Like = both$Like, Help = both$Help, Improve = both$Improve)
both_words = apply(both_words, 2, function(x){tolower(x)})
both_words = apply(both_words, 2, function(x){removePunctuation(x)})

both_words = data.frame(both_words)


both_like = data.frame(id = both$id,Like = both_words$Like)
both_like$Like = as.character(both_like$Like)

both_help = data.frame(id = both$id,help = both_words$Help)
both_help$help = as.character(both_help$help)

both_improve = data.frame(id = both$id,improve = both_words$Improve)
both_improve$improve = as.character(both_improve$improve)

```
Try step by step
Need each row to be a person, each column to be a word, and each cell to be a count.  The count is the number of times 

Ok so unnest get the responses out by word (so mulitple rows per person)
Then group_by although not sure if that is necessary
Then you count which gives you the number of times that person used that word in the document per cell.

Doing this for Like, Help, Improve
```{r}

##Like
bothCountLike = both_like %>%
  unnest_tokens(word, Like)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountLike = count(bothCountLike)
dim(bothCountLike)



colnames(bothCountLike) =c("document", "term", "count")
bothCountLike$document = as.integer(bothCountLike$document)
bothCountLikeDTM = bothCountLike %>%
  cast_dtm(document, term, count)
inspect(bothCountLikeDTM)

### Help

bothCountHelp = both_help %>%
  unnest_tokens(word, help)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountHelp = count(bothCountHelp)
dim(bothCountHelp)



colnames(bothCountHelp) =c("document", "term", "count")
bothCountHelp$document = as.integer(bothCountHelp$document)
bothCountHelpDTM = bothCountHelp %>%
  cast_dtm(document, term, count)
inspect(bothCountHelpDTM)


###Improve
bothCountImprove = both_improve %>%
  unnest_tokens(word, improve)  %>%
  anti_join(stop_words) %>%
  group_by(id)

bothCountImprove = count(bothCountImprove)
dim(bothCountImprove)


colnames(bothCountImprove) =c("document", "term", "count")
bothCountImprove$document = as.integer(bothCountImprove$document)
bothCountImproveDTM = bothCountImprove %>%
  cast_dtm(document, term, count)
inspect(bothCountImproveDTM)


```
Now try running the models 
3:5 for Like, Help, Improve
```{r}
lda_3_like = LDA(bothCountLikeDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_like
topics_lda_3_like = tidy(lda_3_like, matrix = "beta")
topics_lda_3_like

lda_4_like = LDA(bothCountLikeDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_like
topics_lda_4_like = tidy(lda_4_like, matrix = "beta")
topics_lda_4_like

lda_5_like = LDA(bothCountLikeDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_like
topics_lda_5_like = tidy(lda_5_like, matrix = "beta")
topics_lda_5_like

lda_3_help = LDA(bothCountHelpDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_help
topics_lda_3_help = tidy(lda_3_help, matrix = "beta")
topics_lda_3_help

lda_4_help = LDA(bothCountHelpDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_help
topics_lda_4_help = tidy(lda_4_help, matrix = "beta")
topics_lda_4_help

lda_5_help = LDA(bothCountHelpDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_help
topics_lda_5_help = tidy(lda_5_help, matrix = "beta")
topics_lda_5_help


lda_3_improve = LDA(bothCountImproveDTM, k = 3, control = list(seed = 1234), methdod = "Gibbs")
lda_3_improve
topics_lda_3_improve = tidy(lda_3_improve, matrix = "beta")
topics_lda_3_improve

lda_4_improve = LDA(bothCountImproveDTM, k = 4, control = list(seed = 1234), methdod = "Gibbs")
lda_4_improve
topics_lda_4_improve = tidy(lda_4_improve, matrix = "beta")
topics_lda_4_improve

lda_5_improve = LDA(bothCountImproveDTM, k = 5, control = list(seed = 1235), methdod = "Gibbs")
lda_5_improve
topics_lda_5_improve = tidy(lda_5_improve, matrix = "beta")
topics_lda_5_improve

```
Now plot the top ten terms
```{r}
terms_lda_3_like <- topics_lda_3_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_like


terms_lda_4_like <- topics_lda_4_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_like

terms_lda_5_like <- topics_lda_5_like %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_like

terms_lda_3_help <- topics_lda_3_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_help


terms_lda_4_help <- topics_lda_4_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_help

terms_lda_5_help <- topics_lda_5_help %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_help

terms_lda_3_improve <- topics_lda_3_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3_improve


terms_lda_4_improve <- topics_lda_4_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_4_improve

terms_lda_5_improve <- topics_lda_5_improve %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_5_improve

```

Plots for Like 3,4,5
```{r}

terms_lda_3_like

terms_lda_3_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_like %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

```
Plots for Help 3,4,5
```{r}
terms_lda_3_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_help %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

```
Plots for Improve 3,4,5
```{r}
terms_lda_3_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

terms_lda_4_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


terms_lda_5_improve %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```




####################################################################
This is too confusing


Now you can also see which words had the greatest difference in the probabilty of being in topic one versus topic two and so.  This can help us understand what the differences are bewteen the topics. 
```{r}
beta_spread = topics_lda_4 %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001) %>%
  mutate(ratio_top2_1 = abs(log2(topic2/topic1))) %>%
  mutate(ratio_top3_1 = abs(log2(topic3/topic1))) %>%
  mutate(ratio_top4_1 = abs(log2(topic4/topic1))) %>%
  mutate(ratio_top3_2 = abs(log2(topic3/topic2))) %>%
  mutate(ratio_top4_2 = abs(log2(topic2/topic1))) %>%
  mutate(ratio_top4_3 = abs(log2(topic4/topic1)))

beta_spread_data = data.frame(ratio_top2_1 = beta_spread$ratio_top2_1, ratio_top3_1 = beta_spread$ratio_top3_1, ratio_top4_1 = beta_spread$ratio_top4_1, ratio_top3_2 = beta_spread$ratio_top3_2, ratio_top4_2 = beta_spread$ratio_top4_2, ratio_top4_3 = beta_spread$ratio_top4_3)



topic_ratio_top2_1 =  beta_spread %>%
  top_n(10, ratio_top2_1) %>%
  ungroup() %>%
  arrange(-ratio_top2_1)

topic_ratio_top2_1 = data.frame(term = topic_ratio_top2_1$term, ratio_top2_1 =  topic_ratio_top2_1$ratio_top2_1)

topic_ratio_top3_1 =  beta_spread %>%
  top_n(10, ratio_top3_1) %>%
  ungroup() %>%
  arrange(-ratio_top3_1)

topic_ratio_top4_1 =  beta_spread %>%
  top_n(10, ratio_top4_1) %>%
  ungroup() %>%
  arrange(-ratio_top4_1)

topic_ratio_top3_2 =  beta_spread %>%
  top_n(10, ratio_top3_2) %>%
  ungroup() %>%
  arrange(-ratio_top3_2)

topic_ratio_top4_2 =  beta_spread %>%
  top_n(10, ratio_top4_2) %>%
  ungroup() %>%
  arrange(-ratio_top4_2)

topic_ratio_top4_3=  beta_spread %>%
  top_n(10, ratio_top4_3) %>%
  ungroup() %>%
  arrange(-ratio_top4_3)



```
























