library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

ua <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) Eduvos-student-project")

get_page <- function(url) {
  resp <- GET(url, ua)
  stop_for_status(resp)
  Sys.sleep(1) # polite scraping
  read_html(content(resp, "text", encoding = "UTF-8"))
}

base_url <- "https://en.wikipedia.org"
category_url <- paste0(base_url, "/wiki/Category:Data_analysis")

cat_page <- get_page(category_url)
links <- cat_page %>%
  html_elements("#mw-pages .mw-category li a") %>%
  html_attr("href")

links <- links[!is.na(links)]
links <- links[str_detect(links, "^/wiki/") & !str_detect(links, ":")]
links <- unique(paste0(base_url, links))
links <- head(links, 40)
cat("Article links found:", length(links), "\n")

scrape_article <- function(url) {
  tryCatch(
    {
      pg <- get_page(url)
      title <- pg %>%
        html_element("h1") %>%
        html_text2()

      paras <- pg %>%
        html_elements("#mw-content-text p") %>%
        html_text2()
      paras <- paras[nchar(str_squish(paras)) > 0]

      data.frame(
        url = url,
        title = title,
        intro = paste(head(paras, 2), collapse = " "),
        text = paste(paras, collapse = " "),
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      message("Failed: ", url)
      NULL
    }
  )
}

raw_df <- map_dfr(links, scrape_article)

year_pattern <- "\\b(1[5-9][0-9]{2}|20[0-2][0-9])\\b"

raw_df <- raw_df %>%
  mutate(
    word_count      = str_count(text, "\\S+"),
    citation_count  = str_count(text, "\\[[0-9]+\\]"),
    citation_needed = str_count(text, "\\[citation needed\\]"),
    n_years_cited   = str_count(text, year_pattern),
    intro_year      = as.integer(str_extract(intro, year_pattern)) # NA if no year in intro
  )

cat("Total records scraped:", nrow(raw_df), "\n")
print(head(raw_df %>% select(title, word_count, citation_count, intro_year)))
cat("\nMissing values per column (raw data):\n")
print(colSums(is.na(raw_df)))

# Question 4.2 
clean_df <- raw_df %>%
  mutate(
    title = str_squish(title),
    text = text %>%
      str_remove_all("\\[[^]]*\\]") %>% # [1], [citation needed], [edit]
      str_remove_all("http\\S+|www\\S+") %>% # URLs
      str_replace_all("[^\\x20-\\x7E]", " ") %>% # non-ASCII noise
      str_squish(),
    intro = intro %>% str_remove_all("\\[[^]]*\\]") %>% str_squish()
  ) %>%
  distinct(title, .keep_all = TRUE) %>% # remove duplicates
  filter(word_count >= 100) %>% # drop stub / empty pages
  mutate(
    intro_year_imputed = is.na(intro_year), # flag before imputing
    intro_year = ifelse(is.na(intro_year),
      round(median(intro_year, na.rm = TRUE)),
      intro_year
    ),
    intro_year = as.integer(intro_year),
    clean_word_count = str_count(text, "\\S+")
  )

cat("\nRecords after cleaning:", nrow(clean_df), "\n")
cat("Remaining missing values:", sum(is.na(clean_df)), "\n")

write_csv(clean_df, "cleaned_articles.csv")
cat("Cleaned dataset exported to cleaned_articles.csv\n")
