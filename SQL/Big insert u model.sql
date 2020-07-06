INSERT INTO [dbo].[ShootPerson](id_og,name,age,mental_illness,fk_date,fk_race,fk_shooting_circ,fk_location) 
SELECT * FROM (SELECT DISTINCT TOP 2882  og.id,name,age,signs_of_mental_illness,Dates.id AS Dates,Race.id AS Race,Shoot.id AS Shoot, MIN(Loc.id) AS State FROM [dbo].[OriginalPoliceKillingsUS2] og
,(SELECT d.id,date FROM [dbo].[Dates] d) Dates
,(SELECT r.id,r.race,r.gender FROM [dbo].[Race] r) Race
,(SELECT s.id,s.manner_of_death,s.armed,s.thread_level,s.flee,s.body_camera FROM [dbo].[ShootingCircumstances] s) Shoot
,(SELECT l.id,l.area,l.city,l.city_2 FROM [dbo].[Location] l) Loc
WHERE Dates.date=og.date 
AND Race.race=og.race AND Race.gender=og.gender
AND Shoot.manner_of_death=og.manner_of_death AND Shoot.armed=og.armed AND Shoot.thread_level=og.threat_level AND Shoot.flee=og.flee AND Shoot.body_camera=og.body_camera
AND og.state=Loc.area AND DIFFERENCE(og.city,Loc.city_2)>2
 
GROUP BY og.id,og.name,age,signs_of_mental_illness,Dates.id,Race.id,Shoot.id
ORDER BY 1 desc) A order by 1 ASC