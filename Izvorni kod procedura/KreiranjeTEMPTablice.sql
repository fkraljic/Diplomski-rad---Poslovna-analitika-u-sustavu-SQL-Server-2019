USE [US_Fatal_Police_Shootings]
GO
/****** Object:  StoredProcedure [dbo].[KreiranjeTEMPTablice]    Script Date: 3.8.2020. 9:31:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER PROCEDURE [dbo].[KreiranjeTEMPTablice]
AS
BEGIN
  SET NOCOUNT ON;
 
 
DECLARE @temp TABLE
(
	race CHAR(1), 
	description VARCHAR(30), 
	ct INT,
	area VARCHAR(2),
	state_name VARCHAR(100)
)

INSERT INTO @temp
SELECT r.race,r.description, COUNT(*) as ct,l.area,l.state_name FROM [dbo].[ShootPerson] sp
JOIN [dbo].[Race] r
ON r.id=sp.fk_race
JOIN [dbo].[Location] l
ON sp.fk_location=l.id
GROUP BY r.race,r.description,l.area,l.state_name
ORDER BY l.area ASC, ct DESC

DECLARE @temp2 TABLE
(
	ct INT,
	race CHAR(1), 
	description VARCHAR(30), 	
	area VARCHAR(2),
	state_name VARCHAR(100),
	long FLOAT,
	lat FLOAT
)

INSERT INTO @temp2 SELECT DISTINCT t1.ct,t2.race,t2.description,l.area,l.state_name,l.longitude,l.latitude FROM 
(SELECT MAX(tp.ct) AS ct,tp.area FROM (select * from @temp) tp
JOIN [dbo].[Location] l
ON l.area=tp.area
GROUP BY tp.area) t1
JOIN @temp t2
ON t2.ct=t1.ct AND t2.area=t1.area
JOIN [dbo].[Location] l
ON l.area=t1.area
ORDER BY area

SELECT * FROM @temp2  
WHERE NOT (area='DE' AND race='W') 
AND NOT (area='MO' AND long IS NULL) 
ORDER BY state_name
 
DROP TABLE IF EXISTS TEMP
CREATE TABLE TEMP (
   ct INT,
	race CHAR(1), 
	description VARCHAR(30), 	
	area VARCHAR(2),
	state_name VARCHAR(100),
	long FLOAT,
	lat FLOAT
);

INSERT INTO TEMP SELECT * FROM @temp2  
WHERE NOT (area='DE' AND race='W') 
AND NOT (area='MO' AND long IS NULL) 
ORDER BY state_name

 SET NOCOUNT OFF
END
