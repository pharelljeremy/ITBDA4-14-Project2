library(rvest)
library(httr)
library(tm)
library(SnowballC)
library(lexRankr)
library(textrank)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(stringr)

# 3.1 Scraping
target_url <- "https://en.wikipedia.org/wiki/Big_data" # use your own chosen page

resp <- GET(
  target_url,
  user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")
)
stop_for_status(resp)
Sys.sleep(1) # polite scraping

page <- read_html(content(resp, "text", encoding = "UTF-8"))

article_text <- page %>%
  html_elements("#mw-content-text .mw-parser-output > p") %>%
  html_text2()

if (length(article_text) == 0) {
  article_text <- page %>%
    html_elements("p") %>%
    html_text2()
}

article_df <- data.frame(
  url = target_url,
  scrape_date = Sys.Date(),
  text = paste(article_text, collapse = " "),
  stringsAsFactors = FALSE
)

cat("Words scraped:", str_count(article_df$text, "\\S+"), "\n")

# 3.2 Preprocessing
raw_text <- gsub("\\[[^]]*\\]", " ", article_df$text) # [1], [citation needed]
raw_text <- gsub("\\s+", " ", raw_text)
sentences <- unlist(strsplit(raw_text, "(?<=[.!?])\\s+", perl = TRUE))
sentences <- sentences[nchar(sentences) > 40] # drop short fragments

clean_text <- tolower(raw_text)
clean_text <- gsub("http\\S+|www\\S+", " ", clean_text)
clean_text <- gsub("\\S+@\\S+", " ", clean_text)
clean_text <- gsub("[0-9]+", " ", clean_text)
clean_text <- gsub("[^a-z\\s\\.!?]", " ", clean_text)
clean_text <- gsub("\\s+", " ", clean_text)
clean_text <- trimws(clean_text)

extra_stop <- c(
  "read", "more", "related", "subscribe", "share", "click", "learn",
  "also", "will", "may", "one", "two", "three", "new", "can", "could",
  "would", "us", "well", "like", "many", "much", "make", "made",
  "according", "however", "therefore", "thus", "said", "says"
)

corpus <- VCorpus(VectorSource(clean_text))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, extra_stop)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

clean_string <- as.character(corpus[[1]])
cat("Characters after cleaning:", nchar(clean_string), "\n")

# 3.3 Summarisation
lexrank_summary <- lexRank(sentences,
  docId = rep(1, length(sentences)),
  n = 3,
  continuous = TRUE
)
cat("\n--- LexRank Summary ---\n")
print(lexrank_summary$sentence)

sent_df <- data.frame(
  sentence_id = seq_along(sentences),
  sentence = sentences,
  stringsAsFactors = FALSE
)

terms <- sent_df %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word") %>%
  select(sentence_id, word)

tr <- textrank_sentences(data = sent_df, terminology = terms)
cat("\n--- TextRank Summary ---\n")
print(summary(tr, n = 3))

tdm <- TermDocumentMatrix(corpus)
m <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)

freq_df <- data.frame(
  term = names(m), freq = as.integer(m),
  stringsAsFactors = FALSE
) %>%
  slice_head(n = 20) %>%
  mutate(term = reorder(term, freq))

p1 <- ggplot(freq_df, aes(freq, term, fill = freq)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    x = "Frequency (number of occurrences)",
    y = "Term (stemmed)",
    title = "Top 20 terms in the scraped article",
    subtitle = target_url
  ) +
  theme_minimal()
print(p1)
ggsave("q3_term_frequency_bar.png", p1, width = 8, height = 6, dpi = 300)

png("q3_wordcloud.png", width = 1200, height = 800, res = 150)
wordcloud(names(m), as.integer(m),
  max.words = 40, scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")
)
title("Word cloud of the scraped article")
dev.off()
