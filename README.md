# telegram_chat_stat
This a script written in R to make some statistical charts based on telegram chat data. 
Also it can extract certain user comments and make a book from them.
Statistics include general comments number dynamic, number of comments by different users and dates, within week and day variation of comments, etc. 

This script requires RStudio 


## Installation
1. Copy files, create data subfolder for data Ex. "git clone https://github.com/scox3/telegram_chat_stat.git"
2. Install necessary packages by running "source("install_libs.R")" in R/RStudio console (separate installation on RTools may be required).

## Usage: chat statistics 
1. Place Telegram chat export file (result.json) in the folder "data/". Rename it as you wish if you have more  of similar files there. 
2. Place the name of the data file in the field "filename" of the parameters file "params.yaml".
3. Run "knitr::spin("chat_stat.R")" i R console prompt.
4. The report with be placed in the file "chat_stat.html" with pictures in "figures".

## Usage: export certain user comments, create comments book
1. Place the user name in the field "username" of the parameters file "params.yaml".
2. Open extract_user_comments.R in RStudio, then run report generation for it.
3. Comments with be placed in the files specifield in "params.yaml".
4. The file photo-list.txt will be created with the list of photo files referenced from the book. Total Commander can be used to select these files in telegram backup (photos/ and files/ folders) and exctract them to some other folder.


