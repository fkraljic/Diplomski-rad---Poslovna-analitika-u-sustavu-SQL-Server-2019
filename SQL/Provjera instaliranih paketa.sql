EXEC sp_execute_external_script 
                    @language = N'R', 
                    @script = N'
                    OutputDataSet <- data.frame(
                    installed.packages()[,c("Package", "LibPath")]);'
WITH RESULT SETS ((Package nvarchar(255), LibPath nvarchar(2000)));

