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
        types = .self$getschema()$type
        return(lookup_datatype$match_id[match(types,lookup_datatype$SQLite_name)])
      }
    },
    assert = function(data){
      namescheck = all(colnames(data)==.self$tableinfo("names"))
      if(!namescheck){
        stop("one or more column names is not an exact match of table schema")
      }
      datatype = match(as.vector(sapply(data, class)),lookup_datatype$R_name)
      datacheck = all(datatype==.self$tableinfo("type"))
      if(!datacheck){
        stop("one or more data types is not compatible with table schema")
      }
    },
    table_insert = function(data){
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
    table_delete = function(data){
    
      #data<-data[,bins()$getprimkey()]
      #print(data)
      primkey = getprimkey()
      
      command = paste("DELETE FROM",tableinfo("name"),"WHERE",primkey,"= $id",sep=" ")

      deletenew <-  dbSendStatement(con,command)
      dbBind(deletenew, params=list(id=data[,primkey]))
      rows = dbGetRowsAffected(deletenew)
      dbClearResult(deletenew)  

      return(rows)
    }
    
    
  )
)

#each table in ERD has a class

soundfiles <-setRefClass("soundfiles",
  contains="dbtable",
  methods =list(
    
  )
)

bins <-setRefClass("bins",
  contains="dbtable",
  methods =list(
    testinherit = function(knownkeys,data){
      .self$compare(knownkeys=knownkeys,data=data)
      print(str(con))
      print("it worked")
    },
    #call super? 
    insert = function(data){ 
      
      .self$assert(data) 
      
      reply=table_insert(data) #this inserts the data into soundfiles
      
      return(reply)
      #insert assumes that keys provided are not related to db keys. So, finds the max key in db, and 
      #print(.self$getprimkey())
       
    }
  )
)

deployments <-setRefClass("deployments",contains="dbtable")

