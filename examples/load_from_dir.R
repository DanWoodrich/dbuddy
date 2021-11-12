#scratch script with dbuddy

outpath = "//161.55.120.117/NMML_AcousticsData/Working_Folders/MetaData/WaveMetaData/"
outnames = dir(outpath)
             
depnames = substr(outnames,1,nchar(outnames)-15)

for(i in 1:length(outnames)){
  
  command = paste("dbuddy insert soundfiles",paste(outpath,outnames[i],sep="")," --no_warn")
  system(command)

}


#ok: so the following got into the database. Need to try the rest again and debug. 
deps_l=read.csv("C:/Apps/testdb/deployments_loaded.csv")
deps_l=unlist(deps_l)
deps_l = paste(deps_l,"_soundfiles.csv",sep="")

outnames<-outnames[-which(outnames %in% deps_l)]
