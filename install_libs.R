con <- file("init_libs.R", open="r")
s <- readLines(con)
close(con)

s <- grep("library\\((.+?)\\)", s, value=TRUE)
s <- sapply(s, function(x){ substr(x,9, nchar(x)-1) } )

for( s1 in s ){
  print(s1)
  
  if( !require(s1, character.only = TRUE)) {
  #execute installation command 
    install.packages(s1)
    Sys.sleep(5)
  } else {
    print( paste0("Already installed: ", s1))
  }
}
