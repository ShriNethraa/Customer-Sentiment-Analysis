library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(readr)

amz <- read_csv("~/Downloads/Reviews.csv")

na.omit(amz)
amaz <- distinct(amz)

words <- amaz %>%
  select(c("ProductId", "Score", "Summary", "Text")) %>%
  unnest_tokens(word, Summary) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

datatable(head(words))

afinn <- get_sentiments("afinn")

reviews.afinn <- words %>%
  inner_join(afinn, by = "word")

head(reviews.afinn)

word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Score), score = max(Score), count_word = n()) %>%
  arrange(desc(count_word))

datatable(head(word_summary))

filtered_word_summary <- filter(word_summary, count_word < 50000)

custom_colors <- c("#FF5733", "#33FF57", "#5733FF", "#FF5733", "#33FF57")

ggplot(filtered_word_summary, aes(mean_rating, score)) + 
  geom_text(aes(label = word, color = count_word),
            size = 6,                   
            position = position_jitter(), 
            show.legend = FALSE,
            vjust = -1.5) + 
  scale_color_gradientn(colors = custom_colors) +
  coord_cartesian(xlim = c(3, 4.5), ylim = c(3.5, 5)) + 
  guides(size = FALSE, color = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f5f5f5"),
    panel.grid.major = element_line(color = "white"),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )


library(RColorBrewer)
library(wordcloud)

wordcloud(words = word_summary$word, freq = word_summary$count_word, 
          scale=c(5,0.5),max.words=300, colors=brewer.pal(8, "Set2"))

good <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Score), score = max(Score), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))

wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=50, 
          colors=brewer.pal(8, "Set2"))

bad <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Score), score = max(Score), count_word = n()) %>%
  filter(count_word>1000) %>%
  filter(!word %in% c("strong", "pretty")) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)

wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=50, 
          colors=brewer.pal(8, "Set2"))

review_summary <- reviews.afinn %>%
  group_by(ProductId) %>%
  summarise(mean_rating = mean(Score),
            sentiment = mean(Score))

datatable(head(review_summary))

x_mid = 3.5
y_mid = 0

review_summary <- review_summary %>% 
  mutate(quadrant = case_when(
    mean_rating > x_mid & sentiment > y_mid   ~ "Positive Review/Positive Sentiment",
    mean_rating <= x_mid & sentiment > y_mid  ~ "Negative Review/Positive Sentiment",
    mean_rating <= x_mid & sentiment <= y_mid ~ "Negative Review/Negative Sentiment",
    TRUE                                    ~ "Positive Review/Negative Sentiment"
  ))

ggplot(review_summary, aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept = y_mid, color = "black", size = 0.5) + 
  geom_vline(xintercept = x_mid, color = "black", size = 0.5) +
  guides(color = FALSE) +
  scale_color_manual(values = c("Positive Review/Positive Sentiment" = "#33CC33",
                                "Negative Review/Positive Sentiment" = "#FF6666",
                                "Negative Review/Negative Sentiment" = "#9966CC",
                                "Positive Review/Negative Sentiment" = "#66CCFF")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f5f5f5"),
    panel.grid.major = element_line(color = "white"),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  annotate("text", x = 4.33, y = 3.5, label = "Positive Review/Positive Sentiment",
           color = "#33CC33", size = 5) +
  annotate("text", x = 2, y = 3.5, label = "Negative Review/Positive Sentiment",
           color = "#FF6666", size = 5) +
  annotate("text", x = 4.33, y = -3, label = "Positive Review/Negative Sentiment",
           color = "#66CCFF", size = 5) +
  annotate("text", x = 2, y = -3, label = "Negative Review/Negative Sentiment",
           color = "#9966CC", size = 5) +
  geom_point(size = 3, alpha = 0.8)









