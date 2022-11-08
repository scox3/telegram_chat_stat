#Extract user comments functions

chat_users_list <- function(dt.data) {
  return(dt.data[ , .(nickname=first(from)),by=.("id"=from_id)])  
}

get_user_id <- function(cht.usr, user_nickname) {
  id_user <- cht.usr[ str_detect(nickname, user_nickname), first(id) ]   
  return(id_user)
}

extract_user_comments <- function(dt.data, id_user ) {
  dt.data[ from_id == id_user, .N ]
  dt1 <- dt.data[ from_id == id_user, .(date, text) ]
  return(dt1)
}

comments_word_stat <- function(dt.user.comments) {
  dt.uset.word.stat <- 
    data.table( word=str_split( paste0( dt.user.comments$text, collapse=" "), 
                                "\\W+")[[1]])[ , .(freq=.N), by=word]
  dt.uset.word.stat[ , freq:=freq/sum(freq)]
  setorderv(dt.uset.word.stat, "freq", order=-1)
  return(dt.uset.word.stat)
}


print_html_header <-function(fileConn) {
  cat( file=fileConn, "<html>")
  cat( file=fileConn, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n")
  
  cat( file=fileConn, "<style type=\"text/css\"> body { width:auto; font-family: Times New Roman;",
       "font-size: 30pt; }",
       ".leftimg { float:left;   margin: 7px 7px 7px 0;   }  ",
       ".brick {float:none; display: block; padding-left: 2em}  ",
       ".clear1 { clear:both; }  \n img {max-width: 100%;}</style>\n")
  
  cat( file=fileConn, "<body>") 
}

init_outfile <- function(fname, fileConn = NULL) {
  if(is.null(fileConn)){
    fileConn<-file(fname, "w", encoding = "UTF-8")
    print_html_header(fileConn)
  }
  return( fileConn)
}


close_outfile <- function( fileConn ) {
  cat(c("</body></html>"), file=fileConn)
  close(fileConn)
}

print_user_comments_html <- function(dt.data, id_user, fname.out= "user_comments.html") {
  
  fileConn <- init_outfile(fname.out)
  photoConn <- file("photo-list.txt", "w", encoding = "UTF-8")
  
  i1 <- c(1:dt.data[ from_id == id_user, .N ])
  dt1 <- dt.data[ from_id == id_user, .(date, text, photo, file) ]
  
  
  for( i in i1 ) {
    # if( str_detect( dt1[i, text]), "^list(type" ) ){
    #   print(class( dt1[i, text]))
    # }
    
    cat("<p>", as.character(dt1[i, date]), "    ", 
        as.character(dt1[i, text]), file= fileConn, sep="")
    if( dt1[i, photo] != "") {
      
      cat("<br/>", file= fileConn, sep="")
      cat(file=fileConn, "<img ", "src=\"", dt1[i, photo], "\">", sep=""	)
      
      cat(file=photoConn, str_remove( trimws(dt1[i, photo]), "^photos/"), "\n", sep="")
    }
    
    if( dt1[i, file] != "") {
      cat("<br/>", file= fileConn)
      cat(file=fileConn, "<img ", "src=\"", dt1[i, file], "\">", sep=""	)
      if( !str_detect(dt1[i, file], "^\\(File") ){
        cat(file=photoConn, str_remove( trimws(dt1[i, file]), "^files/"), "\n", sep="")
      }
    }
    
    cat("</p>", file= fileConn, sep="")
  }
  close_outfile(fileConn)
  close(  photoConn)
  
  return(TRUE)
}


