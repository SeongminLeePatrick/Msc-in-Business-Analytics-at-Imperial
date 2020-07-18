SELECT sfccode AS ZIP_Code, AVG(ttl_spend) AS AVG_spend
FROM (SELECT lines.custid, sfccode, SUM(lines.linedollar) AS ttl_spend
FROM lines
LEFT JOIN Summary
ON Summary.custid = lines.custid
GROUP BY lines.custid, Summary.sfccode) AS TTL_spend_by_cust_loc
GROUP BY sfccode
ORDER BY avg_spend DESC
LIMIT 5
