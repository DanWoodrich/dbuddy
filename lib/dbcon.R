library(RSQLite)
library(DBI)

standard_con<- function(){

con <- dbConnect(RSQLite::SQLite(),"C:/Users/daniel.woodrich/Desktop/database/lab_data.db")
dbClearResult(dbSendQuery(con, "PRAGMA foreign_keys = ON"))
dbClearResult(dbSendQuery(con, "PRAGMA busy_timeout = 60000")) #will attempt to wait for up to one minute before error
dbClearResult(dbSendQuery(con,"PRAGMA locking_mode = EXCLUSIVE"))
dbClearResult(dbSendQuery(con,"BEGIN EXCLUSIVE"))

return(con)
}