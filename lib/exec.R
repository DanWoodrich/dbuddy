#install.packages("RSQLite")
#setwd("C:/Users/daniel.woodrich/Desktop/database/dbuddy/lib")
#class definitions: 
#make a table class. Will have standard methods

source("classes.R")
source("dbcon.R")

lookup_datatype<-read.csv("../etc/DataTypeLookupR_SQLite3.csv")

#make a db connection, and set some standard pragma settings.  
con <-standard_con()


#based on the syntax, translate to a class method. 
args<-commandArgs(trailingOnly = TRUE)
print(args)

#get variables: 

#hardcode everything you can do since it needs to be a defined procedure to enforce trust
if(args[1]=='insert'){
  csvpath = args[2]
  data = read.csv(csvpath)
  if(args[3]=='soundfiles'){
    soundfiles().insert(data)
  }else if(args[3]=='bins'){
    bins().insert(data)
  }else if(args[3]=='detections'){
    known_keys = 'y' == args[which(args=="--known_keys")+1] #required argument for detection insertion, y or no
    detections().insert(data,known_keys)
  }
}  


  

bins()$getprimkey()

bins()$testinherit(TRUE,cars)

dbDisconnect(con)




#res2 = dbSendStatement(con, "DELETE FROM bins WHERE id='id'")
#dbGetRowsAffected(res2)

#dbDisconnect(con)
#
#dbListTables(con)

#
#disconnecting connection turns off EXCLUSIVE


#testing R package
#res=dbSendQuery(con, "SELECT * FROM bins LIMIT 4")
#dbFetch(res)

#dbClearResult(res)

