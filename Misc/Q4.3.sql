SELECT
    title,
    citation_count,
    word_count,
    ROUND((citation_count::numeric / word_count) * 100, 2) AS citations_per_100_words
FROM articles
ORDER BY citation_count DESC
LIMIT 5;