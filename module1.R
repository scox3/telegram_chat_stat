#module 1


parse_node <- function(x) {
  
  #fields to parse 
  df1 <- list(date="", type="",
             from="", from_id = "", text="", 
             photo="", file="")
  
  for( i in names(df1)) {
    if( !is.null( x[[i]])) {
      df1[[i]] <- as.character(x[[i]])
    }
  }
  df.out <- data.frame(df1, stringsAsFactors = FALSE)
  return(df.out)  

}


create_messages_dt <- function(json1)
{
  dt.data <- json1$messages %>% map( ~ parse_node(.x) ) %>% rbindlist()
  dt.data <- as.data.table(dt.data)
  dt.data <- dt.data[ type == "message"] 
  dt.data <- dt.data[ nchar(from) > 1]

  dt.data[ , ddate := as.Date(date)]
  dt.data[ , imonth := (year(ddate) - min(year(ddate)))*12+month(ddate)]
  dt.data[ , ymonth := sprintf("%.4d-%.2d", year(ddate), month((ddate)))]
  
  return(dt.data)
}



decompose_stat <- function( lst.active ) {
  
  ret <- list()
  dt1 <- data.table( date = seq(from=min(lst.active$dt.stat$date), 
                                to=max(lst.active$dt.stat$date), by=1))
  
  dt1 <- merge( dt1, lst.active$dt.stat[, .(date, n_msg1)], by=c("date"), all.x=TRUE)
  dt1[ is.na(n_msg1), n_msg1 := 0]
  
  ret$dc1<- decompose( ts(log(dt1$n_msg1+1), frequency=7, start=1))
  ret$dcdt1 <- data.table( wd=weekdays(dt1[1:7,date]), 
                           dc=exp(ret$dc1$seasonal[1:7]),
                           wd1 = as.POSIXlt(dt1[1:7,date])$wday)
  
  ret$stl <- stl( ts(log(dt1$n_msg1+1), frequency=7, start=1), 
                  s.window = "periodic",
                  t.window=90)
  return(ret)
}



create_lst_active <- function( dt.data ) {
  dt.src <- dt.data[  ,.(date=date(date), id=as.factor(from_id))]
  dt.stat <- data.table( date = unique(dt.src$date))
  return(list( dt.stat=dt.stat, dt.src=dt.src))
}

add_active_users_column <- function( lst.active, days_period = 30, 
                                     col_name = "n_active30"){
  n_active <- lst.active$dt.stat$date %>% 
    map( ~ (lst.active$dt.src[ (date <= .x) & 
                                 (date > .x - days_period), 
                               length(unique(id)) ])) %>% unlist
  
  ret <- lst.active
  ret$dt.stat[ , get("col_name") := n_active ]
  return(ret)
} 


stat_lst_active <- function(dt.data)
{

  lst.active <- 
    create_lst_active(dt.data) %>% 
    add_active_users_column( 30, "n_active30") %>%
    add_active_users_column( 60, "n_active60") %>%
    add_active_users_column( 7, "n_active7") %>%
    add_active_users_column( 1, "n_active1") %>% 
    add_message_count_column( 30, "n_msg30") %>% 
    add_message_count_column( 1, "n_msg1")
  
  return(lst.active)
  
}

collapse_from_field <- function(from){
  return( paste(sort(unique(from)), collapse="/"))
}

plot_usr_active <- function( lst.active, col_name, ndays, add.smooth = TRUE) {
  dt1 <- lst.active$dt.stat
  gg1 <- ggplot(data=dt1, 
         aes(date, get(col_name), color="data")) +
    geom_path( size=2)+
    ylab("N. users")+
    xlab("Date")+
    ggtitle(sprintf("Number of users who posted in chat within %d days", ndays))
  
  if( add.smooth ){
    gg1 <- gg1 +
      geom_smooth(aes(color="smooth"))
  }
  return(gg1)
}

add_message_count_column <- function( lst.active, days_period = 30, 
                                      col_name = "n_msg30"){
  
  print( days_period)
  n_msg <- lst.active$dt.stat$date %>% 
    map( ~ (lst.active$dt.src[ (date <= .x) & (date > .x - days_period),.N ])) %>% 
    unlist
  
  #    print(n_msg)
  ret <- lst.active
  col_name_per_day  <- paste0(col_name, "_per_day")
  
  ret$dt.stat[ , get("col_name") := n_msg ]
  ret$dt.stat[ , get("col_name_per_day") := n_msg/days_period  ]
  
  
  return(ret)
} 




stat_msg_by_user <- function( dt.data ) {
  
  dt.stat.messages <- dt.data[, .(from=collapse_from_field(from), n_messages= .N, n_chars = sum(nchar(text)), 
                                  fisrt_msg_date=min(as.Date(date)), 
                                  last_msg_date= max(as.Date(date))), by=from_id]
  
  setorderv(dt.stat.messages, "n_messages", order = -1)
  dt.stat.messages[ , share_nmsg := n_messages/sum(n_messages)]
  dt.stat.messages[ , cum_nmsg := cumsum(share_nmsg)]
  dt.stat.messages[ , avg_msg_len := n_chars/n_messages]
  
  return( dt.stat.messages)
}  


stat_by_users_by_month <- function( dt.data, ids ) {
  
  dt.stat.by.month1 <- dt.data[ from_id %in% ids, 
                                .(nmsg=.N, from=collapse_from_field(from), 
                                  ndays=max(day(ddate))), 
                                by=.(from_id, ymonth) ]
  
  dt.stat.by.month2 <- dt.data[ !(from_id %in% ids), 
                                .(nmsg=.N, from="Others", ndays=max(day(ddate)), 
                                  from_id="Others"), 
                                by=.(ymonth)]
  dt.stat.by.month <- rbindlist(list(dt.stat.by.month1[ ,
                                    c("ymonth","nmsg", "from", "from_id", 
                                      "ndays")],
                                     dt.stat.by.month2 ), use.names = TRUE)
  setorderv( dt.stat.by.month, c("from", "ymonth"))
  return(dt.stat.by.month)
}






plot_number_of_messages_per_user <- function( dt.stat.new.era, last_period_start){
  
  # dgg_n <- head( dt.stat.new.era[ , .(from, from_id, n_messages, n_chars, 
  #                                     share_nmsg=
  #                                       formatC(share_nmsg, digits=2),
  #                                     avg_msg_len=floor(avg_msg_len))], n=20 )
  
  dgg_n <- dt.stat.new.era[ , .(from, from_id, n_messages, n_chars, 
                                      share_nmsg=
                                        formatC(share_nmsg, digits=2),
                                      avg_msg_len=floor(avg_msg_len))]
  dgg_n[ , from:= collapse_from_field(from),by=.(from_id)]
  
  
  dgg_n[ , from := as.factor(from)]
  dgg_n[ , from := fct_reorder(from, n_messages, first, .desc=FALSE)]
  
  ggplot( )+
    geom_bar( data=dgg_n, 
              aes(x=from, y=n_messages),position="stack", 
              stat="identity",
              fill=global.cbbPalette[2])  +
    ylab("N. Messages")+
    xlab("User")+
    ggtitle( paste0( "Number of messages per user since ", last_period_start))+ 
    labs(color="Legend")+
    coord_flip()
}


plot_av_length_of_messages_per_user <- function( dt.stat.new.era, last_period_start){
  
  # dgg_n <- head( dt.stat.new.era[ , .(from, n_messages, n_chars, 
  #                                     share_nmsg=
  #                                       formatC(share_nmsg, digits=2),
  #                                     avg_msg_len=floor(avg_msg_len))], n=20 )
  
  dgg_n <- dt.stat.new.era[ , .(from, from_id, n_messages, n_chars, 
                                      share_nmsg=
                                        formatC(share_nmsg, digits=2),
                                      avg_msg_len=floor(avg_msg_len))]
  
  dgg_n[ , from:= collapse_from_field(from),by=.(from_id)]
  dgg_n[ , from := as.factor(from)]
  dgg_n[ , from := fct_reorder(from, avg_msg_len, first, .desc=FALSE)]
  # dgg_n[ , from := fct_reorder(from, avg_msg_len, first, .desc=FALSE)]
  
  ggplot( )+
    geom_bar( data=dgg_n, 
              aes(x=from, y=avg_msg_len),position="stack", 
              stat="identity",
              fill=global.cbbPalette[2])  +
    ylab("Characters in a message")+
    xlab("User")+
    ggtitle( paste0( "Average message length since ", last_period_start))+ 
    labs(color="Legend")+
    coord_flip()
  
}


plot_nmsg_by_user_by_month <- function(dt.stat.by.month, last_period_start
                                       = min(dt.stat.by.month$ymonth), skip_others = FALSE) {
  
  if( skip_others ) {
    dgg1 <-  dt.stat.by.month[ from != "Others", ]
  } else {
    dgg1 <-  dt.stat.by.month
  }
  nfrom <- length(unique(dgg1$from))
  mycolors <- colorRampPalette(global.cbbPalette)(nfrom)
  
  col_list <- unique( dgg1$from)
  
  # col_list <- unique( dt.stat.by.month$from)
  col_labels <- col_list
  
  mycolors <- colorRampPalette(global.cbbPalette)(nfrom)
  
  dgg1[ , from:= collapse_from_field(from),by=.(from_id)]
  
  dgg1[ , tot_messages := sum(nmsg)/ndays, by=.(from)]

  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  
  n <- length(unique(dgg1$from))
  
  ggplot( )+
    geom_bar( data=dgg1, 
              aes(x=ymonth, y=nmsg, fill=from),position="stack", 
              stat="identity") + 
    scale_fill_manual(  values=mycolors) +
    ylab("N. Messages/month")+
    xlab("Month")+
    ggtitle( paste0("Number of messages per day (month average) by top ", n, " users since ",
                    as.character(last_period_start)))+ 
    labs(color="Legend") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

#Doe not work with text axis x
plot_nmsg_by_user_by_month_area <- function(dt.stat.by.month, last_period_start
                                       = min(dt.stat.by.month$ymonth), skip_others = FALSE,
                                       show_share = FALSE) {

  if( skip_others ) {
    dgg1 <-  dt.stat.by.month[ from != "Others", ]
  } else {
    dgg1 <-  dt.stat.by.month
  }
  
  col_list <- unique( dgg1$from)
  nfrom <- length(col_list)
  
  library(RColorBrewer)
  
  # Classic palette BuPu, with 4 colors
  coul <- brewer.pal(11, "Paired") 
  
  # Add more colors to this palette :
  mycolors <- colorRampPalette(coul)(nfrom+1)
  
  #mycolors <- colorRampPalette(global.cbbPalette)(nfrom)


  # col_list <- unique( dt.stat.by.month$from)
  col_labels <- col_list

  
  #mycolors <- colorRampPalette(global.cbbPalette)(from)
  dgg1[ , from:= collapse_from_field(from),by=.(from_id)]
  
  dgg1[ , tot_messages := sum(nmsg), by=.(from)]
  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  dgg1[ , date1 := as.Date(paste0(ymonth,"-01"))]
  
  library(tidyr)
  dgg1 <- as.data.table( complete(dgg1, date1, from, fill=list(nmsg=0, ndays=1 )))
  
  setorderv( dgg1, cols=c("date1"), order=1)
  n <- length(unique(dgg1$from))
  
  if( show_share == FALSE ) {
   gg1 <- ggplot( )+
    geom_area( data=dgg1,
              aes(x=date1, y=nmsg/ndays, fill=from),stat="identity") +
     ggtitle( paste0("Number of messages per day (monthly average) by top ", n, " users since ",
                     as.character(last_period_start)))+
     ylab("N. Messages/day")
     
     
  } else {
    dgg1[ , share_nmsg := nmsg/sum(nmsg), by=.(ymonth)]
    dgg1[ is.na(share_nmsg), share_nmsg :=0]
    
    gg1 <- ggplot( )+
      geom_area( data=dgg1,
                 aes(x=date1, y=share_nmsg, fill=from),stat="identity") +
      ggtitle( paste0("Share of monthly messages by top ", n, " users since ",
                      as.character(last_period_start)))+
      ylab("Share of messages")
  }  
  gg1 <- gg1 +
    scale_fill_manual(  values=mycolors)+
    xlab("Month")+
    labs(color="Legend") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(gg1)
}


plot_stat_by_user_by_month <- function( dt.stat.by.month, skip_others = FALSE ) {
  
  if( skip_others ) {
    dgg1 <-  dt.stat.by.month[ from != "Others", ]
  } else {
    dgg1 <-  dt.stat.by.month
  }
  nfrom <- length(unique(dgg1$from))
  mycolors <- colorRampPalette(global.cbbPalette)(nfrom)
  
  col_list <- unique( dgg1$from)
  col_labels <- col_list
  
  dgg1[ , from:= collapse_from_field(from),by=.(from_id)]
  
  dgg1[ , tot_messages := sum(nmsg), by=.(from)]
  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  
  n <- length(unique(dgg1$from))
  ggplot( )+
    geom_bar( data=dgg1, 
              aes(x=ymonth, y=nmsg, fill=from),position="stack", 
              stat="identity") + 
    scale_fill_manual(  values=mycolors) +
    ylab("N. Messages/month")+
    xlab("Month")+
    ggtitle(paste0("Number of messages per month by top ", n, " users since",
                    as.character(last_period_start)))+ 
    labs(color="Legend") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}


stat_by_hour <- function(dt.data){
  dt.stat.messages <- dt.data[, .(nmsg=.N), 
          by=.("hour"=as.integer(hour(parse_date_time(date,  "YmdHMS",
                      tz = Sys.timezone()))))]
  
  
  setorderv(dt.stat.messages, "hour", order = 1)
  
  dt.stat.messages[ , share_nmsg := nmsg/sum(nmsg)]

  return( dt.stat.messages)
}  



#data file

get_data_path <- function(){
  #Find project root folder
  
  global.root.folder <<- rprojroot::is_rstudio_project$find_file()
  filename.data.path <<- paste0(global.root.folder, "/data/")
}

read_tg_data_file <- function(fname1) {
  
  fname <- paste0( filename.data.path, fname1) 
  #  json1 <- read_json(fname, simplifyDataFrame = TRUE,flatten = TRUE ) 
  json1 <- read_json(fname, simplify = FALSE)
  return(json1)
}

tg_data_json2df <- function(json1, mindate){
  dt.data <- create_messages_dt(json1)
  dt.data <- dt.data[ date >= mindate]
  return(dt.data)  
}


load_data_incremental <- function(params) {
  dt.data <- NULL
  print(params$filename)
  
  if( !( length(params$filename) >= 1 )) {
    stop("No valid filename in params.yaml ")
  }
  
  #  json1 <- read_tg_data_file(params$filename)
  #} else if( length(params$filename) > 1 ){
  
  ffnames_rdata <- str_detect(tolower(params$filename), "\\.rdata$")
  ffnames_json <- str_detect(tolower(params$filename), "\\.json$")
  
  if( sum(ffnames_rdata) > 0 ) {
    #Read Rdata
    load( paste0( filename.data.path, 
                  first(params[["filename"]][ffnames_rdata]) ))
  }
  
  if( sum(ffnames_json) > 0 ) {
    
    json1 <- params[["filename"]][ffnames_json] %>% 
      map( read_tg_data_file)  
    
    if( length(json1) >= 1 ) {
      # dt.data.json <-  json1 %>%
      #   map( ~ tg_data_json2df(.x, mindate)) %>%
      #   rbindlist
      
      dt.data.json <-  seq_along(json1) %>%
        map( ~ tg_data_json2df(json1[[.x]], mindate)[, iset:= .x]) %>%
        rbindlist
      
      if( !is.null(dt.data)){
        dt.data[ , iset:=0 ]
        dt.data <- rbindlist(list(dt.data, dt.data.json))
      } else {
        dt.data <-dt.data.json
      }
      
      dt2 <- unique(dt.data, by = c("date", "type", "from_id", "text", "file", "photo"))
      
      #      View(dt2[ date %in% dt2[ , .N,by=.(date, type, from_id, text)][ N>1, date]])
      
      dt2[ , c("group_size", "group_id") := .(.N, .GRP),
           by=.(date, type, from_id, text)]
      
      #version
      #dt2[ , group_nphotos := sum(str_detect(photo.col, "^photo")), by = .(group_id)]
      
      
      # Note: when  (photo,file) is ("", "(File)") or ("(File)", "")
      # this may generate spurious duplicates - but does not affect the 
      # final cleaning 
      cat("Number of probable groups of duplicates:", 
          dt2[ group_size > 1, length(unique(group_id))])
      
      #      View(dt2[ (group_size > 1) & nchar(text)>0])
      #      View(dt2[ group_size > 1])
      
      dt2[ , del_flag := 0 ]
      
      del_doubles<- function( photo.col, file.col, iset.col ) {
        res <- rep(0, length(photo.col))
        fphoto <- str_detect(photo.col, "^photo")
        fnonphoto <- str_detect(photo.col, "^\\(File")
        
        if( sum(fphoto) > 0 & sum(fnonphoto) > 0 )  {
          
          #Expected data structure: if photos are exported, their number for 
          # the group is >=1. If not exported than the number of "(File"
          # messages is exactly 1
          # photo file names are determited by post datetime and are unique 
          # so there is no need to worry about duplicated photos, only missed ones
          iset_with_photos <- first( iset.col[fphoto])  
          res[fnonphoto & (iset.col!=iset_with_photos)] <- 1
        }
        
        ffile <- str_detect(file.col, "^files/")
        fnonfile <- str_detect(file.col, "^\\(File")
        
        if( sum(ffile) > 0  & sum(fnonfile)>0) {
          iset_with_files <- first( iset.col[ffile])  
          
          res[fnonfile & (iset.col!=iset_with_files)] <- 1
        }
        res
      }
      
      dt2[ group_size > 1, del_flag := del_doubles(photo, file, iset), 
           by=.(group_id)]
      
      dt3 <- dt2[del_flag ==  0]
      dt3[ , c("iset", "del_flag", "group_size", "group_id") := 
             list(NULL,NULL,NULL, NULL)]
      
      dt.data <- dt3
      setorderv(dt.data, c("date"))
      
      if( !is.null(params$save.update)){
        if( is.null(params$update.filename.template)){
          fn1 <- "updated-data-"
        } else {
          fn1 <- params$update.filename.template
        }
        save( dt.data, file=paste0(fn1, dt.data[, as.Date(min(date))], "_",
                                   dt.data[, as.Date(max(date))],
                                   ".RData"))
      }
      
    } else {
      stop("Err reading input data")
    }
  } 
  return(dt.data)
}




