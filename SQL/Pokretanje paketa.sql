USE [US_Fatal_Police_Shootings]
--EXEC sp_execute_external_script
--@language =N'R',
--@script=N'library(randomForest)'

CREATE EXTERNAL LIBRARY ggplot2
FROM (CONTENT = 'C:\Users\Fabijan\Desktop\Diplomski\R_paketi\ggplot2_3.3.1.zip')
WITH (LANGUAGE = 'R');

EXEC sp_execute_external_script 
                    @language = N'R', 
                    @script = N'
                    OutputDataSet <- data.frame(
                    installed.packages()[,c("Package", "LibPath")]);'
WITH RESULT SETS ((Package nvarchar(255), LibPath nvarchar(2000)));

EXEC sp_execute_external_script
@language =N'R',
@script=N'library(ggplot2)'


--EXEC sp_execute_external_script
--@language =N'R',
--@script=N'library(gtable)'