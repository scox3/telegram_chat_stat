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


  source("knitr_options.R")

#read data
  source("init_libs.R")
  source("module1.R")
  
  params <- read_yaml("params.yaml")
  get_data_path()
  if( is.null(params$mindate)) {
    mindate <- make_date( 2019, 1,1 )
  } else {
    mindate <- as.Date(params$mindate)
  }
  
  dt.data <-load_data_incremental(params)
#  json1 <- read_tg_data_file(params$filename)
  
#  dt.data <- tg_data_json2df(json1, mindate)
  
  
  # The palette with grey:
  global.cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # The palette with black:
  global.cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  

# Header     
#' # Telegram chat statistics 
{{params$filename}}

print( sprintf("Data file: %s", params$filename))
  
#' ## Full data statistics
  
#' ### Chat's active users  
  lst.active <- stat_lst_active(dt.data)
  
  plot_usr_active( lst.active, "n_active30", 30)
  plot_usr_active( lst.active, "n_active60", 60, add.smooth = FALSE)
  plot_usr_active( lst.active, "n_active7", 7)
  plot_usr_active( lst.active, "n_active1", 1)
  

#' ### Number of messages in the chat 
  

  plot_nmsg( lst.active, "n_msg30", 30, add.smooth = FALSE)
  plot_nmsg( lst.active, "n_msg30_per_day", 30, add.smooth = FALSE,
             title="Average number of messages per day within %s days")

  lst.active$dt.stat[ ,n_msg30_per_active_usr := n_msg30/n_active30 ]  
  plot_nmsg( lst.active, "n_msg30_per_active_usr", 30, add.smooth = FALSE,
             title="Number of messages within %s days per active user")

  plot_nmsg( lst.active, "n_msg1", 1, add.smooth = TRUE,
             title="Number of messages within 1 days")
  

  # ggplot(lst.active$dt.stat, aes(date, n_msg1, color="Data")) +
  #   geom_path(size=2)+
  #   ggtitle("Number of messages within 1 days")+
  #   geom_smooth(aes(color="Smooth"))+
  #   labs(color="Legend")

    
#' ### Chat's activity variation by day of the week
  
#' #### Averaging multiplicative decomposition   
  lst.dc <- decompose_stat(lst.active)
  plot(lst.dc$dc1)
  lst.dc$dcdt1
  print( sprintf("Max/Min = %.2f", lst.dc$dcdt1[ , max(dc)/min(dc)]))
  
#' ### STL log decomposition   
  
  plot(lst.dc$stl)
  dgg1 <- lst.dc$dcdt1
  
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


      
#' ### Activity by users
#' #### Most active users
  
  dt.stat.messages <- stat_msg_by_user( dt.data)


     

  plot_number_of_messages_per_user(head( dt.stat.messages, n=20),dt.data[,min(ddate)])

  print("Top 20 flooders")
  
  head( dt.stat.messages[, .(from, n_messages, n_chars, share_nmsg=
                               formatC(share_nmsg, digits=2),
                             cum_nmsg=formatC(cum_nmsg, digits=2),
                             av_len = floor(n_chars/n_messages))], n=20 )

#' #### Other users activity
  print(sprintf( "Posted something: %d",  dt.stat.messages[ n_messages >=1, .N  ]))
  print(sprintf( "Posted 1 message: %d",  dt.stat.messages[ n_messages ==1, .N  ]))
  print(sprintf( "Posted 2-5 messages: %d",  dt.stat.messages[ (n_messages >1) & (n_messages <= 5),
                                                               .N  ]))
  print(sprintf( "More than 5 messages: %d", 
                 dt.stat.messages[ n_messages >5,  .N  ]))
  
  #ggplot(dt.stat, aes(n_messages)) + stat_ecdf(geom = "step", size=1, color="blue")+
  #  ggtitle("Number of messages by user in Bitza chat")

  ggplot(dt.stat.messages, aes(x=seq_along(cum_nmsg), cum_nmsg)) +
    geom_path(color="blue", size=2) +xlab("User#, sorted by nmsg")+ylab("Cumulative share of messages")+
    ggtitle("Cumulative input of users to total number of messages")
  

  
#' ## New era statistics
  
  last_period_start <- dt.data[ , max( min(date(date)), max(date(date))-
                                         params$lastperiod)]

  dt.data.new.era <- dt.data[ date >= last_period_start]
  dt.stat.new.era <- stat_msg_by_user( dt.data.new.era )
  
  

#' ### Most active users
  
  # Plot - number of messages  
  
  plot_number_of_messages_per_user(head( dt.stat.new.era, n=20), last_period_start)
    
  # Plot - number of messages  
  
  plot_av_length_of_messages_per_user(head(dt.stat.new.era, n=20), last_period_start)
  print(paste0( "Top 20 flooders - from ", last_period_start))
  head( dt.stat.new.era[, .(from, n_messages, n_chars, share_nmsg=
                              formatC(share_nmsg, digits=2))], n=20 )
  
  
  print(paste0( "Top 20 by number of chars - from ", last_period_start))
  head( dt.stat.new.era[, .(from, n_chars)][ order(-n_chars)], 20)
  
#' ### Activities by month for 20 most productive users
   
  top_flooders_ids <- head( dt.stat.new.era[, from_id ], n=20 )
  
  dt.stat.by.user.by.month.20 <- stat_by_users_by_month(dt.data.new.era, top_flooders_ids)
  
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.20, last_period_start,
                             skip_other= FALSE, show_share = FALSE)
  
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.20, last_period_start,
                                  skip_other= FALSE, show_share = TRUE)
  
  # plot_nmsg_by_user_by_month(dt.stat.by.user.by.month.20, last_period_start)
  
  ##Top
  
  #  head( dt.stat.messages, n=20 )
  # top_flooders_ids <- head( dt.stat.new.era[, from_id ], n=10 )
  

#' ### Activities by month for 10 most productive users
  
  top_flooders_ids.10 <- head( dt.stat.new.era[, from_id ], n=10 )
  
  dt.stat.by.user.by.month.10 <- stat_by_users_by_month(dt.data.new.era, 
                                                        top_flooders_ids.10)
  

  #plot_stat_by_user_by_month(dt.stat.by.user.by.month.10)
  #plot_stat_by_user_by_month(dt.stat.by.user.by.month.10, skip_other= TRUE)

  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
                             skip_other= FALSE, show_share = FALSE)
  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
                                  skip_other= FALSE, show_share=TRUE)
  
# #' #### Without "others"
  
#  plot_nmsg_by_user_by_month_area(dt.stat.by.user.by.month.10, last_period_start,
#                             skip_other= TRUE, show_share = FALSE)

  
#' ### Within day variation of snumber of messages for the new era

  dt.hour <- stat_by_hour(dt.data.new.era)
  
  ggplot( dt.hour, aes(x=hour, y=share_nmsg) )+
    geom_bar( fill="blue", 
              stat="identity") + 
    ylab("N. messages rel.to total")+
    xlab("Hour")+
    ggtitle( "Within day variation of number of messages")+ 
    labs(fill="Coeff") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

    