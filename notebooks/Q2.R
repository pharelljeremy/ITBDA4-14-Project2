#2.1
library(tm)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)

dir_sa_news <- "/Users/jeremypharell/Desktop/£/School/Honours/ITBDA4-B14/Assignment 2/Dataset/ITBDA4-14 Project 2 Datasets-20260729/sa_news"
dir_test    <- "/Users/jeremypharell/Desktop/£/School/Honours/ITBDA4-B14/Assignment 2/Dataset/ITBDA4-14 Project 2 Datasets-20260729/map_reduce_test_docs"

all_files <- c(
  list.files(dir_sa_news, pattern = "\\.txt$", full.names = TRUE),
  list.files(dir_test,    pattern = "\\.txt$", full.names = TRUE)
)

read_one <- function(f) {
  paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
}

news_df <- data.frame(
  file_name = basename(all_files),
  content   = sapply(all_files, read_one),
  stringsAsFactors = FALSE
)

denoise <- function(txt) {
  txt <- tolower(txt)
  txt <- gsub("http\\S+|www\\S+", " ", txt)
  txt <- gsub("@\\w+", " ", txt)
  txt <- gsub("\\S+@\\S+", " ", txt)
  txt <- gsub("[0-9]+", " ", txt)
  txt <- gsub("[^a-z\\s]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  trimws(txt)
}

news_df$clean <- sapply(news_df$content, denoise)

extra_stop <- c("said","says","say","also","will","may","one","two","three",
                "new","can","could","would","us","well","like","many","much",
                "make","made","according","however","therefore","thus","read",
                "click","learn","photo","portrait","image","advertisement",
                "usa","today","news","monday","tuesday","wednesday","thursday",
                "friday","saturday","sunday","january","february","march","april",
                "may","june","july","august","september","october","november",
                "december")

corpus <- VCorpus(VectorSource(news_df$clean))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, extra_stop)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.95)

cat("Documents:", nrow(dtm), "\n")
cat("Terms kept:", ncol(dtm), "\n")

#2.2
k <- 5
lda_model <- LDA(dtm, k = k, method = "Gibbs",
                 control = list(seed = 1234, burnin = 1000,
                                iter = 2000, thin = 100))

term_probs <- posterior(lda_model)$terms
topic_dist <- posterior(lda_model)$topics

top_k <- 10
cat("\nTop", top_k, "words per topic (beta probabilities):\n")
for (i in 1:k) {
  cat("\nTopic", i, ":\n")
  p <- sort(term_probs[i, ], decreasing = TRUE)[1:top_k]
  print(round(p, 4))
}

doc_topic <- as.data.frame(topic_dist)
doc_topic$file_name <- rownames(dtm)
doc_topic$dominant  <- apply(topic_dist, 1, which.max)
print(doc_topic[, c("file_name", "dominant")])
print(table(doc_topic$dominant))

#2.3
beta_df <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = top_k) %>%
  ungroup() %>%
  arrange(topic, -beta)

p1 <- beta_df %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "Beta probability", y = "Term",
       title = "Top 10 terms per LDA topic") +
  theme_minimal()
print(p1)
ggsave("q2_top_terms_bar.png", p1, width = 10, height = 7, dpi = 300)

png("q2_wordclouds.png", width = 1400, height = 900, res = 150)
par(mfrow = c(2, 3), mar = c(1, 1, 2, 1))
for (i in 1:k) {
  wordcloud(names(term_probs[i, ]), term_probs[i, ],
            max.words = 30, scale = c(3, 0.5),
            colors = brewer.pal(8, "Dark2"),
            main = paste("Topic", i))
}
dev.off()

beta_heat <- tidy(lda_model, matrix = "beta") %>%
  group_by(term) %>%
  mutate(total = sum(beta)) %>%
  ungroup() %>%
  filter(total > quantile(total, 0.9))

p3 <- ggplot(beta_heat, aes(x = factor(topic),
                            y = reorder(term, beta),
                            fill = beta)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Topic", y = "Term", fill = "Beta",
       title = "Term probability across topics") +
  theme_minimal()
print(p3)
ggsave("q2_beta_heatmap.png", p3, width = 8, height = 8, dpi = 300)
