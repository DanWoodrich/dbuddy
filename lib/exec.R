#install.packages("RSQLite")
setwd("C:/Users/daniel.woodrich/Desktop/database/dbuddy/lib")
#class definitions: 
#make a table class. Will have standard methods

source("classes.R")
source("dbcon.R")

lookup_datatype<-read.csv("../etc/DataTypeLookupR_SQLite3.csv")
lookup_datatype$R_name[which(is.na(lookup_datatype$R_name))]<-"NA"
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
    soundfiles()$insert(data)
  }else if(args[3]=='bins'){
    bins()$insert(data)
  }else if(args[3]=='detections'){
    known_keys = 'y' == args[which(args=="--known_keys")+1] #required argument for detection insertion, y or no
    detections().insert(data,known_keys)
  }
}  

#example functions: 

#see schema of bin table
bins()$getschema()

#create some dummy data
testbindata <-data.frame(c("1","2"),c("1","2"),c(1,2),c(3,4))
colnames(testbindata)<-bins()$getschema()$name

#load in dummy data
bins()$insert(testbindata)
bins()$view_()

#try inserting data with wrong data type
wrongdatatype <-data.frame(c(1,2),c("1","2"),c(1,2),c(3,4))
bins()$insert(wrongdatatype)

#now try inserting data that worked before, again: 
bins()$insert(testbindata)
bins()$view_()

#now delete dummy data from bins 
bins()$table_delete(testbindata$id)
bins()$view_()


#insert and delete will both call other class tables, and their respective functions: soundfiles()$insert, bins_detections$modify() , etc. 
#doing this with classes allows for complex sequences that are performed procedurally. 

#lets try loading in some soundfiles. Currently, their aren't any in the table. 
#Soundfiles currently includes in it's basic import function, a call to 'deployments' import. 

deployments()$table_delete("testmanual")
deployments()$view_()

#can add an entry to deployments: 

deployments()$getschema()

deploymentsdummy<-data.frame("testmanual",30,50,as.integer(42),38,4,40,"then","later",as.integer(16384),"PST","M2",as.integer(1))
colnames(deploymentsdummy)<-deployments()$getschema()$name
deployments()$insert(deploymentsdummy)

deployments()$view_()
deployments()$insert("test2",'key')

#show how deployments is modified when soundfiles are entered
soundfiles()$view_()
soundfiles()$getschema()

soundfiledummy<-data.frame("testSF",600,"Testdeployment")
colnames(soundfiledummy)<-soundfiles()$getschema()$name
soundfiles()$insert(soundfiledummy)

#now we see that deployments and soundfiles were both updated:
soundfiles()$view_()
deployments()$view_()

soundfiles()$table_delete("testSF")
deployments()$table_delete("Testdeployment")

sfs=read.csv("C:/Users/daniel.woodrich/Desktop/database/soundfiles.csv")

soundfiles()$insert(sfs)
soundfiles()$table_clear()


soundfiles()$view_()


dbCommit(con)
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

