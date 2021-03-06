USE [US_Fatal_Police_Shootings]
GO
/****** Object:  StoredProcedure [dbo].[MapaSADa]    Script Date: 3.8.2020. 9:31:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER PROCEDURE [dbo].[MapaSADa]
AS
BEGIN
  SET NOCOUNT ON;
  DECLARE @query nvarchar(max) =  
  N'SELECT sp.mental_illness,sc.manner_of_death,sc.armed,sp.age,r.gender,r.race,l.area,l.city,sc.thread_level,sc.flee,l.state_name FROM [dbo].[ShootPerson] sp
    JOIN [dbo].[ShootingCircumstances] sc ON sc.id=sp.fk_shooting_circ
    JOIN [dbo].[Race] r ON sp.fk_race=r.id
    JOIN [dbo].[Location] l ON l.id=sp.fk_location'

	DECLARE
    @Table1_Input NVARCHAR( MAX ) = 'SELECT DISTINCT area,state_name,longitude,latitude FROM [dbo].[Location]',
    @Table1_Data VARBINARY( MAX )
	
	DECLARE @Picture VARBINARY( MAX )

  EXECUTE sp_execute_external_script
    @language = N'R',
    @script = N'

        if( nrow(InputDataSet) == 0 )
            stop("Invalid data passed in")

        # Read in the sql table, serialize it to an output string
        Output <- serialize(InputDataSet, NULL)
    ',
    @input_data_1 = @Table1_Input,
    @params = N'@Output VARBINARY( MAX ) OUTPUT',
    @Output = @Table1_Data OUTPUT;
	
  EXECUTE sp_execute_external_script @language = N'R',  
                                     @script = N' 
	library(RODBC)
	library(ggplot2)
	library("maps")
	library(ggmap)
	library(mapdata)

	#--- Ulazni podaci ---
	databaseTable <- InputDataSet
	locationDB <- unserialize(Table1_Data)

	print(databaseTable)
	print(locationDB)
	
	#--- Strukturiranje podataka ---
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

	#--- Analiza podataka ---
	data <- table(table$state_name)
	data <- data.frame(data)
	names(data) <- c("region", "number")

	#--- Dohvaćanje podataka o saveznim državama ---
	states <- map_data("state")
	states <- data.frame(states)

	#--- Središnje točke saveznih država ---
	locationDB <- data.frame(
	  locationDB$area,
	  locationDB$state_name,
	  locationDB$longitude,
	  locationDB$latitude
	)
	names(locationDB)<-c("state","region","longitude","latitude")

	#--- Kreiranje grafa ---
	numberOfCasesPerState <- merge(data,states,by="region")
	stateWithLongAndLat <- merge(locationDB,numberOfCasesPerState,by="region")
	head(states$region)
	p <- ggplot(data = numberOfCasesPerState) + 
	  geom_polygon(aes(x = long, y = lat, fill = number, group = group), color = "white") +
	  geom_text(data=stateWithLongAndLat, aes(longitude, latitude, label = number), size=8,color="black", fontface = "bold")+
	  geom_text(data=stateWithLongAndLat, aes(longitude, latitude, label = number), size=8,color="yellow")+
	  coord_fixed(1.3) +
	  guides(fill=FALSE) 

	#--- Ispis grafa ---
	print(getwd())
	png(filename="C:/Users/FabijaN/Desktop/Diplomski/US_Map_killings_by_state.png", width=1600, height=900, units = "px")
	p <- p + labs(title = "Number of fatal cases by US States")
	p <- p + theme(plot.title = element_text(size=22))
	p <- p + theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12))
	plot(p)
	dev.off();
   ',  
   @input_data_1 = @query,
   @params = N'@Table1_Data VARBINARY(MAX)',
   @Table1_Data = @Table1_Data
   --@Picture = @Picture OUTPUT

 SET NOCOUNT OFF
END
