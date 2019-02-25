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
```{r}
library(tidyr)
library(tm)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(topicmodels)
library(modeltools)
library(methods)
library(stringr)
dim(both)
both = na.omit(both)
both$Like = stemDocument(both$Like)

bothCount = both %>%
  unnest_tokens(word, Like)  %>%
  anti_join(stop_words)  %>%
  group_by(id)  %>%
  count(word) 

colnames(bothCount) =c("document", "term", "count")
bothCount$document = as.integer(bothCount$document)
bothCount
bothCountDTM = bothCount %>%
  cast_dtm(document, term, count)
bothCountDTM
inspect(bothCountDTM)
```
Now try running the model
```{r}
lda_3 = LDA(bothCountDTM, k = 3, control = list(seed = 1234))
lda_3
topics_lda_3 = tidy(lda_3, matrix = "beta")
topics_lda_3
```
Now plot the top ten terms
```{r}
terms_lda_3 <- topics_lda_3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terms_lda_3
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
```
























