#R reference classes
#generic class
dbtable <- setRefClass("dbtable", 
 #fields=list(), #or take data as field?                
  methods = list(
   
    #probably want a method 'getkey' that will get primary key from db 
    getschema = function(){
       
      tableName = .self$getClass()@className[1]
       
      query = dbSendQuery(con,paste("PRAGMA table_info(",tableName,")",sep=""))
      out = dbFetch(query)
      dbClearResult(query)
       
      return(out)
       
    },                   
    getprimkey = function(){
     
      schema = getschema()
     
      return(schema[which(schema$pk==1),"name"])
    },
    compare = function(knownkeys,data){ 
     
      View(data)
      if(knownkeys==TRUE){
       
      #query db for keys- keys that aren't found will be added, those that are will be 
       
      print(.self$getprimkey())
       
      }else{
        stop('cannot compare if db keys are not known: this should be treated as an insert operation')
      }
     #query table for known keys
     
    }
  )
)

#each table in ERD has a class

soundfiles <-setRefClass("soundfiles",
  contains="dbtable",
  methods =list(
    testinherit = function(knownkeys,data){
      .self$compare(knownkeys=knownkeys,data=data)
      print(str(con))
      print("it worked")
    },
    insert = function(data){
     
    #insert new rows into SFs 
    #generate standard bins from SFs and insert into bins and bintype

     print('yes')
   }
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
    insert = function(data){
     
    #insert new rows into SFs 
    #generate standard bins from SFs and insert into bins and bintype

     print('yes')
   }
  )
)

deployments <-setRefClass("deployments",contains="dbtable")

