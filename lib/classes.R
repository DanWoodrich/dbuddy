#other functions

suppressWarnings(suppressMessages(library(foreach)))

check_datatype <-function(types1,types2,lookup){ #data1= input data, data2 = classvec
  #couldn't figure out how to do with vectors
  checks = vector(length=length(types1))
  for(i in 1:length(checks)){
    nums1 = lookup$match_id[which(lookup$R_name==types1[i])]
    nums2 = lookup$match_id[which(lookup$SQLite_name==types2[i])]
    
    if(!all(is.na(match(nums1,nums2)))){
      checks[i]<-TRUE
    }else{
      checks[i]<-FALSE
    }
  }
  
  return(checks)
  
}

#R reference classes
#generic class
dbtable <- setRefClass("dbtable", 
 #fields=list(), #or take data as field?                
  methods = list(
    view_ = function(){
    query = dbSendQuery(con,paste("SELECT * FROM",tableinfo("name")))
    View(dbFetch(query))
    dbClearResult(query)
    
    },
    head_ = function(){ #to easily look at table from R session
    
    query = dbSendQuery(con,paste("SELECT * FROM",tableinfo("name"),"LIMIT 10"))
    print(dbFetch(query))
    dbClearResult(query)
    
    },
    #probably want a method 'getkey' that will get primary key from db 
    getschema = function(){
              
      query = dbSendQuery(con,paste("PRAGMA table_info(",tableinfo("name"),")",sep=""))
      out = dbFetch(query)
      dbClearResult(query)
       
      return(out)
       
    },                   
    getprimkey = function(){
     
      schema = getschema()
     
      return(schema[which(schema$pk==1),"name"])
    },
    tableinfo = function(attribute){
      if(attribute=="name"){
        return(.self$getClass()@className[1])
      }else if(attribute=="names"){
        return(.self$getschema()$name)
      }else if(attribute=="type"){
        return(.self$getschema()$type)
      }
    },
    assert = function(data){
      namescheck = all(colnames(data)==.self$tableinfo("names"))
      if(!namescheck){
        stop("one or more column names is not an exact match of table schema")
      }
      classvec = as.vector(sapply(data, class))
      #even though R codes NA as logical, I want to distinguish from T/F and define it as NULL
      classvec[which(is.na(data))]<-"NA"
      datacheck=check_datatype(classvec,.self$tableinfo("type"),lookup_datatype)
      if(!all(datacheck)){
        print("one or more data types is not compatible with table schema")
        #print(datacheck)
        print(colnames(data[!datacheck]))
        stop()
      }
    },
    table_insert = function(data){
    
      assert(data)
    
      command = paste("INSERT OR IGNORE INTO",tableinfo("name"),"VALUES",paste("(:",paste(tableinfo("names"),collapse=",:"),")",sep="") ,sep=" ")
            
      insertnew <- dbSendQuery(con,command)
      dbBind(insertnew, params=data) # execute
      rows = dbGetRowsAffected(insertnew)
      dbClearResult(insertnew)  # release the prepared statement
      
      
      #first, query the data to see if there are any duplicate keys- this will crash dbAppendTable
      
      #query = dbSendQuery(con,paste("SELECT * FROM",tableinfo("name"),"WHICH"))
      #out = dbFetch(query)
      #dbClearResult(query)
    
      #out = dbAppendTable(con,tableinfo("name"),data)
      
      return(rows)
    },
    table_delete = function(keyvec){
    
       
    
      #data<-data[,bins()$getprimkey()]
      #print(data)
      primkey = getprimkey()
      
      command = paste("DELETE FROM",tableinfo("name"),"WHERE",primkey,"= $id",sep=" ")

      deletenew <-  dbSendStatement(con,command)
      dbBind(deletenew, params=list(id=keyvec))
      rows = dbGetRowsAffected(deletenew)
      dbClearResult(deletenew)  

      return(rows)
    },
    table_clear = function(){
    dbExecute(con, paste("DELETE FROM",tableinfo("name")))
    }
    
    
  )
)

#each table in ERD has a class

soundfiles <-setRefClass("soundfiles",
  contains="dbtable",
  methods =list(
    insert = function(data){ 
      
      #first, attempt to insert deployment. Will fill in any missing values with NULL. 
      deployments()$insert(unique(data$deployments_name),'key') #just fills in the needed key(s)
      #take data of schema of soundfile
      
      #create datetime manually, based on file name. 
      #sfs only uses 3 columns as input: file name, duration, and deployment name
      
      namelens = nchar(data$Name)
      
      datetime_str = substr(data$Name,nchar(data$Name)-16,nchar(data$Name)-4)
      
      dates = as.POSIXct(datetime_str,format="%y%m%d-%H%M%S",tz="UTC")
      DateTime = format(dates,"%Y-%m-%d %H:%M:%S")
      
      data$DateTime = DateTime
      
      affected=table_insert(data) 
      
      data$DateTime<-NULL #not needed for any more downstream inserts. 
      
      print(paste(affected,"rows inserted in",tableinfo("name")))
      
      if(affected>0){
        
        #generate and insert standard bins into bins
        
        source("standardbins.R")
        
        standardbins = make_standard_bins(data)        
        #print(data)
        #print(str(data))
        
        colnames(standardbins)<-c("id","FileName","SegStart","SegDur","Type")

        #return(standardbins)
        #insert into bins
        bins()$insert(standardbins)
      
      }
    }
  )
)

bins <-setRefClass("bins",
  contains="dbtable",
  methods =list(
    insert = function(data){ 
      
      #take data that also has type. When adding to bins, split off type and reduce to unique ids. then add other data to bintypes
      bintypes_data = data[,c("id","Type")]
      colnames(bintypes_data) <-c("bins_id","bin_type")
      bin_data = data[,c("id","FileName","SegStart","SegDur")]
      
      
      #split data into 
      
      affected=table_insert(bin_data) 
      
      print(paste(affected,"rows inserted in",tableinfo("name")))
      
      #only cascade triggers if there were any rows affected
      if(affected>0){
      
        #insert into bin_types
        affected = bintypes()$table_insert(bintypes_data)
        
        print(paste(affected,"rows inserted in bintypes"))
        
        #bins_detections - find detections in new or changed bins... 
        
        #populate new rows in bin_labels
      
      }
      #insert assumes that keys provided are not related to db keys. So, finds the max key in db, and 
      #print(.self$getprimkey())
       
    }
  )
)

bintypes <-setRefClass("bintypes",contains="dbtable")

deployments <-setRefClass("deployments",
  contains="dbtable",
  methods =list(
    insert = function(data,input='full'){

      if(input=='key'){
        #build row with NULLs to enter in to db
        data<-data.frame(data,matrix(nrow = length(data),ncol =(nrow(getschema())-1)))
        colnames(data)<-getschema()$name
      }
      
      #insert into deployments
      affected = table_insert(data) 
      print(paste(affected,"rows inserted in",tableinfo("name")))
    }
  )
)

