SELECT name, city.district, Concat(100*population/state_pop,'%') AS portion,
RANK() OVER(PARTITION By city.district ORDER BY city.population DESC)
FROM city INNER JOIN
  (SELECT district, SUM(population) AS state_pop
  FROM city
  WHERE countrycode = 'USA'
  GROUP BY district) AS state_pop
ON city.district = state_pop.district
ORDER BY population DESC
LIMIT 20
