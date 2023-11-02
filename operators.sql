USE [msdb]
GO

/****** Object:  Operator [DBA]    Script Date: 02/11/2023 13:48:13 ******/
EXEC msdb.dbo.sp_add_operator @name=N'DBA', 
		@enabled=1, 
		@weekday_pager_start_time=90000, 
		@weekday_pager_end_time=180000, 
		@saturday_pager_start_time=90000, 
		@saturday_pager_end_time=180000, 
		@sunday_pager_start_time=90000, 
		@sunday_pager_end_time=180000, 
		@pager_days=0, 
		@email_address=N'vpmaciel@gmail.com', 
		@category_name=N'[Uncategorized]'
GO

