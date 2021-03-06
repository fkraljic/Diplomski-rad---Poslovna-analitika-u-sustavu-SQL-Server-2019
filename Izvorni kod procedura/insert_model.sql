USE [US_Fatal_Police_Shootings]
GO
/****** Object:  StoredProcedure [dbo].[insert_model]    Script Date: 3.8.2020. 9:30:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [dbo].[insert_model]

AS
BEGIN


DECLARE
	@num INT,
	@model varbinary(MAX);

SELECT @num=COUNT(*) FROM [dbo].[ShootPerson]
PRINT(@num)

EXEC generiranje_modela @model OUTPUT

PRINT(@model)

INSERT INTO [dbo].[Model](model,date_created,number_of_rows_used) VALUES(@model,GETDATE(),@num)



END
