#' ---
#' title: "Telegram chat statistics"
#' author: "Vladislav Borkus"
#' date: 2022-07-12T10:10:00-00:00
#' output:
#'   blogdown::html_page: 
#'     self_contained: no
#'     toc: yes
#'     keep_md: yes
#'     preserve_yaml: yes
#'   bookdown::html_document2: 
#'     css: style.css
#'     keep_md: yes
#'     self_contained: no
#'     preserve_yaml: yes
#' categories: ["R", "RStudio"]
#' tags: ["R", "RStudio", "Telegram", "Dataset"]
#' ---



#read data
  source("init_libs.R")
  source("module1.R")
  

  params <- read_yaml("params.yaml")
  
  json1 <- read_tg_data_file(params$filename)
  
  if( is.null(params$mindate)) {
    mindate <- make_date( 2019, 1,1 )
  } else {
    mindate <- params$mindate
  }
  
  dt.data <- tg_data_json2df(json1, mindate)
  
  print( sprintf("<H1>Data: %s</H1>", params$filename))
  
  knitr::opts_chunk$set(collapse = TRUE)
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)
  # The palette with grey:
  global.cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # The palette with black:
  global.cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  
  
  
  
#Stat1
  
  dt.stat.messages <- stat_msg_by_user( dt.data)

  ##what posted people with 1 message?
  
  print(sprintf( "Posted something: %d",  dt.stat.messages[ n_messages >=1, .N  ]))
  print(sprintf( "Posted 1 message: %d",  dt.stat.messages[ n_messages ==1, .N  ]))
  print(sprintf( "Posted 2-5 messages: %d",  dt.stat.messages[ (n_messages >1) & (n_messages <= 5),
                                                                 .N  ]))
  print(sprintf( "More than 5 messages: %d", 
                   dt.stat.messages[ n_messages >5,  .N  ]))
    
  print("Top 20 flooders")
  
  head( dt.stat.messages[, .(from, n_messages, n_chars, share_nmsg=
                               formatC(share_nmsg, digits=2),
                              cum_nmsg=formatC(cum_nmsg, digits=2),
                             av_len = floor(n_chars/n_messages))], n=20 )

  plot_number_of_messages_per_user(dt.stat.messages, dt.data[,min(ddate)])
    
  #ggplot(dt.stat, aes(n_messages)) + stat_ecdf(geom = "step", size=1, color="blue")+
  #  ggtitle("Number of messages by user in Bitza chat")

  ggplot(dt.stat.messages, aes(x=seq_along(cum_nmsg), cum_nmsg)) +
    geom_path(color="blue", size=2) +xlab("User#, sorted by nmsg")+ylab("Cumulative share of messages")+
    ggtitle("Cumulative input of users to total number of messages")
  

  lst.active <- stat_lst_active(dt.data)
    
  plot_usr_active( lst.active, "n_active30", 30, add.smooth = FALSE)
  plot_usr_active( lst.active, "n_active60", 60, add.smooth = FALSE)
  plot_usr_active( lst.active, "n_active7", 7)
  plot_usr_active( lst.active, "n_active1", 1)
  

  ggplot(lst.active$dt.stat, aes(date, n_msg30, color="data")) +
    geom_path(size=2)+
    ggtitle("Number of messages within 30 days")
  #+    geom_smooth(aes(color="smooth"))

  ggplot(lst.active$dt.stat, aes(date, n_msg30_per_day, color="data")) +
    geom_path(size=2)+
    ggtitle("Average number of messages per day within 30 days")
  #+    geom_smooth(aes(color="smooth"))
  
  ggplot(lst.active$dt.stat, aes(date, n_msg30/n_active30, color="data")) +
    geom_path(size=2)+
    ggtitle("Number of messages within 30 days per active user")

  
  ggplot(lst.active$dt.stat, aes(date, n_msg1, color="data")) +
    geom_path(size=2)+
    ggtitle("Number of messages within 1 days")+
    geom_smooth(aes(color="smooth"))
  
## avtivity by day of the week
  

  lst.dc <- decompose_stat(lst.active)
  lst.dc$dcdt1
  print( sprintf("Max/Min = %.2f", lst.dc$dcdt1[ , max(dc)/min(dc)]))
  plot(lst.dc$dc1)
  
  plot(lst.dc$stl)
  dgg1 <- lst.dc$dcdt1
  
  #dgg1[ , wd := fct_reorder(wd, seq_along(wd), .desc=FALSE)]
  dgg1[ wd1 == 0 , wd1 := 7 ] #sunday -> 7 
  
  dgg1[ , wd := fct_reorder(wd, wd1, .desc=FALSE)]
  
  ggplot( )+
    geom_bar( data=lst.dc$dcdt1, 
              aes(x=wd, y=dc, fill="blue"),position="stack", 
              stat="identity") + 
    ylab("N. messages rel.to/ weekly average")+
    xlab("Month")+
    geom_hline(yintercept = 1, linetype="dotted")+
    ggtitle( "Within week variation of number of messages")+ 
    labs(fill="Coeff") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  
##
###New era
  
  last_period_start <- dt.data[ , max( min(date(date)), max(date(date))-366)]

  dt.data.new.era <- dt.data[ date >= last_period_start]
  dt.stat.new.era <- stat_msg_by_user( dt.data.new.era )
  
  # dt.stat.new.era <- dt.data.new.era[, .(from=last(from), n_messages= .N, n_chars = sum(nchar(text)), 
  #                        fisrt_msg_date=min(as.Date(date)), last_msg_date= max(as.Date(date))), by=from_id]
  # 
  # setorderv( dt.stat.new.era, c("n_messages"), order=-1)
  # dt.stat.new.era[ , share_nmsg := n_messages/sum(n_messages)]
  
  print(paste0( "Top 20 flooders - from ", last_period_start))
  head( dt.stat.new.era[, .(from, n_messages, n_chars, share_nmsg=
                               formatC(share_nmsg, digits=2))], n=20 )
  

  # dt.stat.new.era[ , cum_nmsg := cumsum(share_nmsg)]
  #head( dt.stat.new.era[, .(from, cum_nmsg=formatC(cum_nmsg, digits=2))], n=20 )
  
  print(paste0( "Top 20 by number of chars - from ", last_period_start))
  head( dt.stat.new.era[, .(from, n_chars)][ order(-n_chars)], 20)
  

  # dt.stat.new.era[ , avg_msg_len := n_chars/n_messages]
  
  

  # Plot - number of messages  
  
  plot_number_of_messages_per_user(dt.stat.new.era, last_period_start)
  
  # Plot - number of messages  
  
  plot_av_length_of_messages_per_user(dt.stat.new.era, last_period_start)
  
    
  top_flooders_ids <- head( dt.stat.new.era[, from_id ], n=20 )
  
  dt.stat.by.user.by.month.20 <- stat_by_users_by_month(dt.data.new.era, top_flooders_ids)
  
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.20, last_period_start,
                             skip_other= FALSE, show_share = FALSE)
  
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.20, last_period_start,
                                  skip_other= FALSE, show_share = TRUE)
  
  # plot_nmsg_by_user_by_month(dt.stat.by.user.by.month.20, last_period_start)
  
  ##Top
  
  #  head( dt.stat.messages, n=20 )
  top_flooders_ids <- head( dt.stat.new.era[, from_id ], n=10 )
  dt.stat.by.user.by.month.10 <- stat_by_users_by_month(dt.data.new.era, 
                                                        top_flooders_ids)
  

  #plot_stat_by_user_by_month(dt.stat.by.user.by.month.10)
  #plot_stat_by_user_by_month(dt.stat.by.user.by.month.10, skip_other= TRUE)

  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
                             skip_other= FALSE, show_share = FALSE)
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
                                  skip_other= FALSE, show_share=TRUE)
  
  
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
                             skip_other= TRUE, show_share = FALSE)
  
  ##Within day variations
  
  dt.hour <- stat_by_hour(dt.data.new.era)
  
  ggplot( dt.hour, aes(x=hour, y=share_nmsg) )+
    geom_bar( fill="blue", 
              stat="identity") + 
    ylab("N. messages rel.to total")+
    xlab("Hour")+
    ggtitle( "Within day variation of number of messages")+ 
    labs(fill="Coeff") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

    