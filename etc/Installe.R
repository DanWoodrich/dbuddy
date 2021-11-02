options(timeout=1800)

Packages<-c("RSQLite")

for(n in Packages){
  install.packages(n, repos = "http://cran.us.r-project.org")
}
