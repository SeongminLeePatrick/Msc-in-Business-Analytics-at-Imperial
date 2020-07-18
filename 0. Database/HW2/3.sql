SELECT name, count(*) AS n_of_offlang
FROM country
INNER JOIN countrylanguage
ON country.code = countrylanguage.countrycode
WHERE isofficial = 'true'
GROUP BY name
HAVING count(*) >=3
