


#Parse init_libs file for libraries names

con <- file("init_libs.R", open="r")
s <- readLines(con)
close(con)

s <- grep("library\\((.+?)\\)", s, value=TRUE)
s <- sapply(s, function(x){ substr(x,9, nchar(x)-1) } )
s <- paste0('install.packages("',s, '")')

for( s1 in s ){
  print(s1)
  #execute installation command 
  eval(parse(text=s1))
  Sys.sleep(5)
}
