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

plot_usr_active <- function( lst.active, col_name, ndays, add.smooth = TRUE) {
  dt1 <- lst.active$dt.stat
  gg1 <- ggplot(data=dt1, 
         aes(date, get(col_name), color="data")) +
    geom_path( size=2)+
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
  
  dt.stat.messages <- dt.data[, .(from=last(from), n_messages= .N, n_chars = sum(nchar(text)), 
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
                                .(nmsg=.N, from=first(from), ndays=max(day(ddate))), 
                                by=.(from_id, ymonth) ]
  
  dt.stat.by.month2 <- dt.data[ !(from_id %in% ids), 
                                .(nmsg=.N, from="Others", ndays=max(day(ddate))), 
                                by=.(ymonth)]
  dt.stat.by.month <- rbindlist(list(dt.stat.by.month1[ ,
                                    c("ymonth","nmsg", "from", "ndays")],
                                     dt.stat.by.month2 ))
  setorderv( dt.stat.by.month, c("from", "ymonth"))
  return(dt.stat.by.month)
}






plot_number_of_messages_per_user <- function( dt.stat.new.era, last_period_start){
  
  dgg_n <- head( dt.stat.new.era[ , .(from, n_messages, n_chars, 
                                      share_nmsg=
                                        formatC(share_nmsg, digits=2),
                                      avg_msg_len=floor(avg_msg_len))], n=20 )
  
  
  
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
  
  dgg_n <- head( dt.stat.new.era[ , .(from, n_messages, n_chars, 
                                      share_nmsg=
                                        formatC(share_nmsg, digits=2),
                                      avg_msg_len=floor(avg_msg_len))], n=20 )
  
  
  
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
  
  dgg1[ , tot_messages := sum(nmsg)/ndays, by=.(from)]
  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  
  ggplot( )+
    geom_bar( data=dgg1, 
              aes(x=ymonth, y=nmsg, fill=from),position="stack", 
              stat="identity") + 
    scale_fill_manual(  values=mycolors) +
    ylab("N. Messages/month")+
    xlab("Month")+
    ggtitle( paste0("Number of messages per day (month average) by top ", nfrom, " users since ",
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

  dgg1[ , tot_messages := sum(nmsg), by=.(from)]
  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  dgg1[ , date1 := as.Date(paste0(ymonth,"-01"))]
  
  library(tidyr)
  dgg1 <- as.data.table( complete(dgg1, date1, from, fill=list(nmsg=0, ndays=1 )))
  
  setorderv( dgg1, cols=c("date1"), order=1)
  
  if( show_share == FALSE ) {
   gg1 <- ggplot( )+
    geom_area( data=dgg1,
              aes(x=date1, y=nmsg/ndays, fill=from),stat="identity") +
     ggtitle( paste0("Number of messages per day (month average) by top ", nfrom, " users since ",
                     as.character(last_period_start)))+
     ylab("N. Messages/day")
     
     
  } else {
    dgg1[ , share_nmsg := nmsg/sum(nmsg), by=.(ymonth)]
    dgg1[ is.na(share_nmsg), share_nmsg :=0]
    
    gg1 <- ggplot( )+
      geom_area( data=dgg1,
                 aes(x=date1, y=share_nmsg, fill=from),stat="identity") +
      ggtitle( paste0("Share of messages per day (month average) by top ", nfrom, " users since ",
                      as.character(last_period_start)))+
      ylab("Share of messages/day")
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
  
  dgg1[ , tot_messages := sum(nmsg), by=.(from)]
  dgg1[ , from := as.factor(from)]
  dgg1[ , from := fct_reorder(from, tot_messages, first, .desc=FALSE)]
  
  ggplot( )+
    geom_bar( data=dgg1, 
              aes(x=ymonth, y=nmsg, fill=from),position="stack", 
              stat="identity") + 
    scale_fill_manual(  values=mycolors) +
    ylab("N. Messages/month")+
    xlab("Month")+
    ggtitle(paste0("Number of messages per month by top ", nfrom, " users since",
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


read_tg_data_file <- function(fname1) {
  library(rprojroot)
  #Find project root folder
  
  global.root.folder <<- rprojroot::is_rstudio_project$find_file()
  filename.data.path <<- paste0(global.root.folder, "/data/")
  
  fname <- paste0( filename.data.path, fname1) 
  
  #  json1 <- read_json(fname, simplifyDataFrame = TRUE,flatten = TRUE ) 
  json1 <- read_json(fname, simplify = FALSE)
  return(json1)
}

tg_data_json2df <- function(json1, mindate){
  dt.data <- create_messages_dt(json1)
  dt.data <- dt.data[ date > mindate]
  return(dt.data)  
}




