library(RODBC)
library(ggplot2)
library("maps")
library(ggmap)
library(mapdata)

cn <- odbcConnect("US_Fatal_Police_Shootings", rows_at_time = 1)
cn <- odbcDriverConnect("Persist Security Info=False;Integrated Security=true;  
    Initial Catalog=AdventureWorks;Server=MSSQL1", rows_at_time = 1)
dbhandle <- odbcDriverConnect("driver={SQL Server};server=A715-71G-51;database=US_Fatal_Police_Shootings;trusted_connection=true")

#catching data for shot people by police officers
databaseTable <- sqlQuery(cn, "
                SELECT sp.mental_illness,sc.manner_of_death,sc.armed,sp.age,r.gender,r.race,l.area,l.city,sc.thread_level,sc.flee,l.state_name FROM [dbo].[ShootPerson] sp
                JOIN [dbo].[ShootingCircumstances] sc ON sc.id=sp.fk_shooting_circ
                JOIN [dbo].[Race] r ON sp.fk_race=r.id
                JOIN [dbo].[Location] l ON l.id=sp.fk_location")


#creating table
table <- data.frame(
  databaseTable$age,
  databaseTable$manner_of_death,
  databaseTable$armed,
  databaseTable$gender,
  databaseTable$race,
  databaseTable$area,
  databaseTable$mental_illness,
  databaseTable$thread_level,
  databaseTable$flee,
  databaseTable$city,
  databaseTable$state_name
)

names(table) <- c("age", "manner", "armed", "gender", "race", "state", "sings", "thread", "flee", "city","state_name")
table


#counting number of people killed in a state
data <- table(table$state_name)
data <- data.frame(data)
names(data) <- c("region", "number")
print(data)


#getting states data
states <- map_data("state")
states <- data.frame(states)


#catching data for states and their central points
locationDB <- sqlQuery(cn, "
                select distinct area,state_name,longitude,latitude from [dbo].[Location]")
locationDB <- data.frame(
  locationDB$area,
  locationDB$state_name,
  locationDB$longitude,
  locationDB$latitude
)
names(locationDB)<-c("state","region","longitude","latitude")


#ploting state with numbers
numberOfCasesPerState <- merge(data,states,by="region")
stateWithLongAndLat <- merge(locationDB,numberOfCasesPerState,by="region")
head(states$region)
p <- ggplot(data = numberOfCasesPerState) + 
  geom_polygon(aes(x = long, y = lat, fill = number, group = group), color = "white") +
  geom_text(data=stateWithLongAndLat, aes(longitude, latitude, label = number), size=5,color="black", fontface = "bold")+
  geom_text(data=stateWithLongAndLat, aes(longitude, latitude, label = number), size=5,color="yellow")+
  coord_fixed(1.3) +
  guides(fill=FALSE) 

 
p <- p + labs(title = "Number of fatal cases by US States", size=15)
p <- p + theme(plot.title = element_text(size=22))
p <- p +theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12))
print(p)

summary(stateWithLongAndLat)
which(is.na(stateWithLongAndLat$longitude))

#ploting barchart for above
library(RColorBrewer)
coul <- brewer.pal(8, "Set2")

par(mai=c(2,1,1,1))#for us states text padding
barplot(data$number, 
        main = "Number of fatal cases by state",
        xlab = "", 
        ylab="Number of persons",
        ylim = c(0, max(data$number) + 80),
        col=coul,
        border="white",
        names=data$region, 
        las=2#rotate states
        ,
        cex.names = 1.00)


mtext("US states", side=1, line = 7, las=1)#for us states text padding



#Prilagoðeno po stanovništvu
populationDB <- sqlQuery(cn, "
                select distinct area,state_name,state_population from [dbo].[Location]")
populationDB <- data.frame(
  populationDB$area,
  populationDB$state_name,
  populationDB$state_population
)
names(populationDB)<-c("state","region","population")

populationData <- merge(data,populationDB,by="region")
options("scipen" = 100, "digits" = 4)
populationData <- cbind(populationData,((populationData$number/populationData$population)*100))
names(populationData)<- c("region","number","state","population","perPop")


par(mai=c(2,1.5,1,1))#for us states text padding
barplot(populationData$perPop, 
        main = "Fatal cases by state adjustet for population",
        xlab = "", 
        ylab="",
        ylim = c(0, max(populationData$perPop) + 0.001),
        col=coul,
        border="white",
        names=data$region, 
        las=2#rotate states
        ,
        cex.names = 1.00)


mtext("US states", side=1, line = 7, las=1)#for us states text padding
mtext("Percentage by population", side=2, line = 5, las=3)#for us states text padding


#Number of guns by state---------------------------------------
armedDB <- sqlQuery(cn, "SELECT sc.armed,l.area,l.state_name,COUNT(area) AS count, l.longitude,l.latitude  FROM [dbo].[ShootPerson] sp
                        JOIN [dbo].[ShootingCircumstances] sc
                        ON sp.fk_shooting_circ=sc.id
                        JOIN [dbo].[Location] l 
                        ON l.id=sp.fk_location
                        WHERE sc.armed='gun'
                        GROUP BY armed,area,state_name,longitude,latitude")

armedDB <- data.frame(
  armedDB$armed,
  armedDB$area,
  armedDB$state_name,
  armedDB$count,
  armedDB$longitude,
  armedDB$latutude
)
names(armedDB)<-c("armed","area","state_name","count","long","lat")


par(mai=c(2,1,1,1))#for us states text padding
barplot(armedDB$count, 
        main = "Number of fatal cases in which the suspect had a firearm",
        xlab = "", 
        ylab="Number of firearms",
        ylim = c(0, max(armedDB$count) + 50),
        col=coul,
        border="white",
        names=armedDB$state_name, 
        las=2#rotate states
        ,
        cex.names = 1.00)


mtext("US statets", side=1, line = 7, las=1)#for us states text padding


#TOP 10 MOST DANGEROUS CITIES ---------------------------------------
dangerCityDB <- sqlQuery(cn, "SELECT TOP 10 l.city,l.area,l.state_name,COUNT(city) AS count,  CONCAT(l.city,' (',l.area,')') AS concat FROM [dbo].[ShootPerson] sp
                          JOIN [dbo].[ShootingCircumstances] sc
                          ON sp.fk_shooting_circ=sc.id
                          JOIN [dbo].[Location] l 
                          ON l.id=sp.fk_location
                          WHERE sc.armed='gun' OR sc.armed='guns and explosives' OR sc.armed='bean-bag gun' OR sc.armed='gun and knife'
                          GROUP BY city,area,state_name
                          ORDER BY count DESC")

dangerCityDB <- data.frame(
  dangerCityDB$city,
  dangerCityDB$area,
  dangerCityDB$state_name,
  dangerCityDB$count,
  dangerCityDB$concat
)
names(dangerCityDB)<-c("city","area","state_name","count","concat")


par(mai=c(2.5,1,1,1))#for us states text padding
barplot(dangerCityDB$count, 
        main = "Top 10 most dangerous cities",
        xlab = "", 
        ylab="Number of persons killed",
        ylim = c(0, max(dangerCityDB$count) + 5),
        col=coul,
        border="white",
        names=dangerCityDB$concat, 
        las=2#rotate states
        ,
        cex.names = 0.70,
        srt = 60)


mtext("US states", side=1, line = 10, las=1)#for us states text padding



#TIME plot-----------------------------------------


timeDB <- sqlQuery(cn, "SELECT DATEPART(month, d.date) AS month,DATEPART(YEAR, d.date) AS year,CONCAT(DATEPART(YEAR, d.date),'-',DATEPART(month, d.date),'-1') AS concat,COUNT(*) AS count FROM [dbo].[ShootPerson] sp
                        JOIN [dbo].[Dates] d
                        ON d.id=sp.fk_date
						            WHERE d.date < '2020-06-01'
                        GROUP BY DATEPART(month, d.date), DATEPART(YEAR,d.date)
                        ORDER BY year,month")

timeDB <- data.frame(
  timeDB$month,
  timeDB$year,
  timeDB$concat,
  timeDB$count
)
names(timeDB)<-c("month","year","concat","count")


p <- ggplot(timeDB, aes(x=as.Date(concat), y=count)) +
  geom_line( color="red") + 
  xlab("Razdoblje (u mjesecima)") +
  ylab("Broj smrtnih sluèajeva") +
  ggtitle("Broj smrtih sluèajeva po mjesecima")+
  geom_smooth(method = "lm")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

p + scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")


#By age -------------------------------------------------------

ageDB <- sqlQuery(cn, "SELECT age FROM [dbo].[ShootPerson] WHERE mental_illness=1")

ageDB <- data.frame(
  ageDB$age
)
names(ageDB)<-c("age")

par(mai=c(1,1,1,1))#for us states text padding
hist(ageDB$age,
     main = "Histogram for persons with sings of mental illness by age",
     xlab = "Age",
     ylab = "Number of persons",
     border = "white",
     col = "#2b7043",
     las = 1,
     breaks = 30,
     ylim = c(0,100),
     xlim=c(-5,100))


ageDB <- sqlQuery(cn, "SELECT age FROM [dbo].[ShootPerson] WHERE mental_illness=0")

ageDB <- data.frame(
  ageDB$age
)
names(ageDB)<-c("age")

par(mai=c(1,1,1,1))#for us states text padding
hist(ageDB$age,
     main = "Histogram for persons without sings of mental illness by age",
     xlab = "Age",
     ylab = "Number of persons",
     border = "white",
     col = "#2b7043",
     las = 1,
     breaks = 30,
     ylim = c(0,350),
     xlim=c(-5,100))


#mental illness by gender ---------------
genderDB <- sqlQuery(cn, "SELECT r.gender,mental_illness FROM [dbo].[ShootPerson] sp
                    JOIN [dbo].[Race] r
                    ON r.id=sp.fk_race")

genderDB <- data.frame(
  genderDB$gender,
  genderDB$mental_illness
)
names(genderDB)<-c("gender","illness")

counts <- table(genderDB$gender, genderDB$illness)
rownames(counts) = c("female", "male")
colnames(counts) = c("no", "yes")
counts


barplot(counts, main = "US Fatal Police Shootings by gender and sings of mental illness",
        xlab = "Sings of mental illness", col = c("#ad2447", "#2b4399"), ylab="Number of persons",
        legend = rownames(counts), ylim = c(0, max(counts) + 220))


#RACE AND CASES ---------------------------------------------------
raceDB <- sqlQuery(cn, "
SELECT * FROM TEMP
")

raceDB <- data.frame(
  raceDB$ct,
  raceDB$race,
  raceDB$description,
  raceDB$area,
  raceDB$state_name,
  raceDB$long,
  raceDB$lat
)
names(raceDB)<-c("ct","race","desc","area","region","long1","lat1")


numberOfCasesByRacePerState <- merge(raceDB,states,by="region")
numberOfCasesByRacePerState <- numberOfCasesByRacePerState[order(numberOfCasesByRacePerState$group,numberOfCasesByRacePerState$order),]
a <- as.numeric(factor(numberOfCasesByRacePerState$race))

p <- ggplot(data = numberOfCasesByRacePerState) + 
  geom_polygon(aes(x = long, y = lat, fill = a, group = group), color = "white") +
  geom_text(data=numberOfCasesByRacePerState, aes(long1, lat1, label = race), size=5,color="yellow", fontface = "bold")+
  geom_text(data=numberOfCasesByRacePerState, aes(long1, lat1, label = race), size=5,color="yellow")+
  coord_fixed(1.3) +
  guides(fill=FALSE)

p <- p + labs(title = "Most fatal cases by race and by state")
p


#--------------
raceAreaDB <- sqlQuery(cn, "
SELECT r.race,l.area,l.state_name FROM [dbo].[ShootPerson] sp
JOIN [dbo].[Race] r ON r.id=sp.fk_race 
JOIN [dbo].[Location] l ON l.id=sp.fk_location
")

raceAreaDB <- data.frame(
  
  raceAreaDB$race,
  raceAreaDB$area,
  raceAreaDB$state_name
)
names(raceAreaDB)<-c("race","area","s_name")

counts <- table( raceAreaDB$race,raceAreaDB$s_name)
counts <- counts[,-5]
counts <- counts[,-43]
counts <- counts[,-9]

rownames(counts) <- c("asian","black","hispanic","native","other","white","unknown")
counts

par(mai=c(2.5,1,1,1))#for us states text padding
barplot(counts, main = "US Fatal Police Shootings by race in state",
        col = c( "#e2ee20","brown","orange","#13c34c","red","#00baf0","#D3D3D3"), 
        ylab="Number of persons killed",
        legend = rownames(counts),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.08, 0)),
        ylim = c(0, max(counts) + 150),
        las=2
        )

mtext("US states", side=1, line = 8, las=1)#for us states text padding

counts <- table( raceAreaDB$race,raceAreaDB$s_name)
counts
counts <- counts[,c(5,10,44)]
counts

rownames(counts) <- c("asian","black","hispanic","native","other","white","unknown")
counts

par(mai=c(1.5,1,1,1))#for us states text padding
barplot(counts, main = "US Fatal Police Shootings by race in state",
        col = c( "#e2ee20","brown","orange","#13c34c","red","#00baf0","#D3D3D3"), 
        ylab="Number of persons killed",
        legend = rownames(counts),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.08, 0)),
        ylim = c(0, max(counts) + 650)
)

mtext("US states", side=1, line = 3, las=1)#for us states text padding

