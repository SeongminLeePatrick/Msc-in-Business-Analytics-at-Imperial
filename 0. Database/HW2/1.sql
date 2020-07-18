SELECT name, population, continent,
SUM(population) OVER(PARTITION BY continent) AS sum_conti_pop
FROM country
WHERE continent != 'Antarctica'
ORDER BY sum_conti_pop, population DESC
LIMIT 3
