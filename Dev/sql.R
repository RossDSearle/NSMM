

SELECT Sites.SiteName, Sites.Network, Sensors.HasData
FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID
GROUP BY Sites.SiteName
HAVING
SUM(CASE WHEN HasData THEN 1 ELSE 0 END) = 0
order by Network, SiteName;