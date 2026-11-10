SELECT
    intro_year,
    COUNT(*) AS number_of_articles,
    ROUND(AVG(word_count), 0) AS average_word_count,
    ROUND(AVG(citation_count), 1) AS average_citations
FROM articles
GROUP BY intro_year
ORDER BY intro_year DESC;