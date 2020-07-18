SELECT language, SUM(population * percentage) AS sum_of_ppl_speaking
FROM country
INNER JOIN countrylanguage
ON country.code = countrylanguage.countrycode
GROUP BY language
ORDER BY sum_of_ppl_speaking DESC
LIMIT 5
