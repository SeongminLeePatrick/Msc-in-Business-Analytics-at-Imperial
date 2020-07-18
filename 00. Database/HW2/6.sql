SELECT city.name, city.population
FROM country
INNER JOIN city
ON country.code = city.countrycode
WHERE country.name IN (
  SELECT name
  FROM country
  ORDER BY population DESC
  LIMIT 5
)ORDER BY city.population DESC
LIMIT 3
