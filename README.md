# telegram_chat_stat
This a script written in R to make some statistical charts based on telegram chat data.
This script requires RStudio 

##Installation
1. Copy files, create data subfolder for data
2. Install necessary packages by running install_libs.R in RStudio (separate installation on RTools may be required).

#Usage: chat statistics 
1. Place Telegram chat export file (result.json) in the folder "data/".
2. Place the necessary name in the field "filename" of the parameters file "params.yaml".
3. Open chat_stat.R in RStudio, then run report generation for it.
4. The report with be placed in the file "chat_stat.html" with pictures in "chat_stat/files".

#Usage: export certain user comments, create comments book
1. Place the user name in the field "username" of the parameters file "params.yaml".
2. Open extract_user_comments.R in RStudio, then run report generation for it.
3. Comments with be placed in the files specifield in "params.yaml".

