#Script to extract user comments
#Vlad Borkus, 2022


#Source chat_stat.R first
# source("chat_stat.R")
source("module_user_comments.R")

#read data
source("init_libs.R")
source("module1.R")

params <- read_yaml("params.yaml")

json1 <- read_tg_data_file(params$filename)

if( is.null(params$mindate)) {
  mindate <- make_date( 2019, 1,1 )
} else {
  mindate <-as.Date(params$mindate)

}

dt.data <- tg_data_json2df(json1, mindate)

#Username of interest 

if( is.null(params$userid) |  (params$userid ==0 ) ){
  tg.username <- params$username
  
  #Users list ad ids
  cht.usr <- chat_users_list(dt.data)
  #Fund user id
  id1 <- get_user_id(cht.usr, tg.username)
} else {
  id1 <- params$userid
}


#Get comments
dt.user.comments <- extract_user_comments( dt.data, id1)
#head(comments_word_stat(dt.user.comments),100)
#write.csv(file="user-word-stat.csv", dt.uset.word.stat)

#Dump comments
write.csv( file=params$commentsfilename , dt.user.comments )

#Make a book
print_user_comments_html(dt.data, id1, fname.out=params$commentshtmlbook)


# "Алаторцев"
# id1 <- "user139453999"
# dt.user.comments <- extract_user_comments( dt.data, id1)
# write.csv( file="Alatortcev-comments.txt" , dt.user.comments )
# print_user_comments_html(dt.data, id1)

