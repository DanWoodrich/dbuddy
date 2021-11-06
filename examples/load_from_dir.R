#scratch script with dbuddy

outpath = "//161.55.120.117/NMML_AcousticsData/Working_Folders/MetaData/WaveMetaData/"
outnames = dir(outpath)
             
depnames = substr(outnames,1,nchar(outnames)-15)

for(i in 1:length(outnames)){
  
  command = paste("dbuddy insert soundfiles",paste(outpath,outnames[i],sep="")," --no_warn")
  system(command)

}
