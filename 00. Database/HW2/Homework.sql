-- Show the 3 biggest countries (by their population) on the least populous continent (except for Antarctica).
-- (Comment: please don’t hardcode ‘Oceania’ ;).)

SELECT continent, sum(population)
FROM country
GROUP BY continent
ORDER 
