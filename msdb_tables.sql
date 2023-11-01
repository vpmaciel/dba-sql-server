USE [msdb]
GO

/****** Object:  Table [dbo].[blocktable]    Script Date: 01/11/2023 20:29:36 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[blocktable](
	[blockedsid] [int] NULL,
	[blockingsid] [int] NULL,
	[eventinfo1] [varchar](300) NULL,
	[eventinfo2] [varchar](300) NULL,
	[waittime] [int] NULL,
	[hostname] [varchar](20) NULL,
	[dat_bloqueio] [datetime] NULL,
	[program_name1] [varchar](30) NULL,
	[program_name2] [varchar](30) NULL,
	[host_blocking] [char](20) NULL
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[log_locks]    Script Date: 01/11/2023 20:29:37 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[log_locks](
	[session_id] [smallint] NOT NULL,
	[blocked_by] [smallint] NOT NULL,
	[login] [nchar](128) NOT NULL,
	[host_name] [nchar](128) NOT NULL,
	[program_name] [nchar](128) NOT NULL,
	[Query] [nvarchar](max) NULL,
	[Command] [nchar](16) NOT NULL,
	[database] [sysname] NOT NULL,
	[last_wait_type] [nchar](32) NOT NULL,
	[wait_time_sec] [bigint] NULL,
	[last_batch] [datetime] NOT NULL,
	[login_time] [datetime] NOT NULL,
	[status] [nchar](30) NOT NULL,
	[cpu] [int] NULL,
	[capture_time] [datetime] NOT NULL,
	[ID_log] [uniqueidentifier] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

/****** Object:  Table [dbo].[trace_daily]    Script Date: 01/11/2023 20:29:37 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[trace_daily](
	[cod_servidor] [varchar](255) NULL,
	[TextData] [ntext] NULL,
	[DatabaseID] [int] NULL,
	[NTUserName] [nvarchar](128) NULL,
	[HostName] [nvarchar](128) NULL,
	[ApplicationName] [nvarchar](128) NULL,
	[LoginName] [nvarchar](128) NULL,
	[Duration] [bigint] NULL,
	[StartTime] [datetime] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

