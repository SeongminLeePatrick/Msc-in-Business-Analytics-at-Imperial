SELECT name, lifeexpectancy,avg_lifeexpectency, lifeexpectancy - avg_lifeexpectency AS life_diff
FROM country
INNER JOIN (
  SELECT continent, AVG(lifeexpectancy) AS avg_lifeexpectency
  FROM country
  GROUP BY continent) AS continent_avg
ON continent_avg.continent = country.continent
ORDER BY indepyear
LIMIT 20
