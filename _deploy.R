config = configr::read.config("config.yml")$default
library(tidyverse)
library(DBI)
library(RMariaDB)
source("Rfiles/cm3functions.R")
source("Rfiles/server_functions.R")
source("Rfiles/symlink_global_functions.R")
source("Rfiles/cm3functions.R")
source("constants.r")

get_conn <- function(){
  conn <- DBI::dbConnect(
    #RMySQL::MySQL(),
    drv = RMariaDB::MariaDB(),
    load_data_local_infile = T,
    timeout=6000,
    reconnect=T,#FIXME could be dangerous
    encoding="utf8mb4",
    
    # drv = RMySQL::MySQL(max.con=1000, fetch.default.rec=1000),
    idleTimeout=900000,#15 minutes
    interactive_timeout=900000,#15 minutes
    wait_timeout=900000,#15 minutes
    dbname = config$sql_cm$dbname,
    host = config$sql_cm$host,
    port = config$sql_cm$port,
    username = config$sql_cm$username,
    password = config$sql_cm$password,
    mysql=F
  )
  conn
}





if(F){
  conn <- get_conn()
  # conn <- dbConnect(
  #   drv = RMySQL::MySQL(max.con=100, fetch.default.rec=1000),
  #   dbname = config$sql$dbname,
  #   host = config$sql$host,
  #   port = config$sql$port,
  #   username = config$sql$username,
  #   password = config$sql$password
  # )
  
  
  
  # source("Rfiles-server/_global_functions.R")
}
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
if(F){
  detachAllPackages()
  devtools::install_github("stevepowell99/causalmap3functions@main",force=T,upgrade="never")#;options(shiny.fullstacktrace = TRUE)
  # rsconnect::deployApp(appName="tokyo",account="causalmap",forceUpdate = T,lint=F)
  
  
  getlogs <- function(appName)  {
    library(tidyverse)
    appPath<-file.path(getwd(),"app.R")
    target <- rsconnect:::deploymentTarget(appPath, appName, account="causalmap", NULL, NULL)
    accountDetails <- rsconnect:::accountInfo(target$account)
    client <- rsconnect:::lucidClient(rsconnect:::shinyappsServerInfo()$url, accountDetails)
    application <- rsconnect:::getAppByName(client, accountDetails, target$appName)
    logs <- client$getLogs(application$id, 99999999)
    vlogs <- (logs %>% str_split("\n"))[[1]] %>%
      keep(!str_detect(.,"JAVASCRIPT")) %>%
      keep(.!="")
    vlogs %>% paste0(collapse="\n") %>% str_replace_all("\n\n","\n") %>% writeLines("log.txt" %>% paste0(appName,.))
  }
  getlogs <- function(appName)  {
    library(tidyverse)
    showLogs(
      appPath = getwd(),
      appFile = NULL,
      appName = appName,
      account = NULL,
      server = NULL,
      entries = 1000,
      streaming = T
    ) %>% paste0(collapse="\n") %>% str_replace_all("\n\n","\n") %>% writeLines("log.txt" %>% paste0(appName,.))
  }
  
  
}
if(F){
  rsconnect::showLogs(appName = "cm3dev3",entries = 1000)
  
  getlogs(appName="dummy")
  getlogs(appName="StorySurvey2")
  getlogs(appName="CausalMap2")
  getlogs(appName="CM2test")
  getlogs(appName="tokyo")
  getlogs(appName="cm3dev3")
  
  if(F){
    rsconnect::showLogs( "C:/Users/steve/Dropbox/Projects/StorySurvey2/app.R",appName = "StorySurvey2",account = "causalmap",entries = 999,streaming = T)
    rsconnect::showLogs( "C:/Users/steve/Dropbox/Projects/StorySurvey2/app.R",appName = "StorySurvey2",account = "causalmap",entries = 999,streaming = T)
  }
}
# rsconnect::deployApp(appName="cm3main",account="causalmap",forceUpdate = T,lint=F)


# push_message(value = "test2")
# push_message(value = "test2",hostname = "causalmap.shinyapps.io//cm3dev/")

######## main
#push_message(hostname = "causalmap.shinyapps.io//cm3main/",conn = get_conn())
#rsconnect::deployApp(appName="cm3main",account="causalmap",forceUpdate = T,lint=F)
#push_message(value = "",hostname = "causalmap.shinyapps.io//cm3main/")
#
# #
# # ######## dev
# push_message(hostname = "causalmap.shinyapps.io//cm3dev/",conn = get_conn())
# rsconnect::deployApp(appName="cm3dev",account="causalmap",forceUpdate = T,lint=F)
# push_message(value = "",hostname = "causalmap.shinyapps.io//cm3dev/")

######## dev2
# push_message(hostname = "causalmap.shinyapps.io//cm3dev2/",conn = get_conn())
# rsconnect::deployApp(appName="cm3dev2",account="causalmap",forceUpdate = T,lint=F)
# push_message(value = "",hostname = "causalmap.shinyapps.io//cm3dev2/")

######## dev3
# push_message(hostname = "causalmap.shinyapps.io//cm3dev3/",conn = get_conn())
# system("git add .")
# system("git commit -a -m 'WIP'")
# system("git add .")
# rsconnect::deployApp(appName="cm3dev3",account="causalmap",forceUpdate = T,lint=F)
# push_message(value = "",hostname = "causalmap.shinyapps.io//cm3dev3/")

# servers=xc("dev2 dev3")
push_message <- function(hostname="127.0.0.1//",value="This server will restart in a few minutes with new fixes - ",add="",conn=get_conn()){
  usage=tibble(
    time_stamp=time_stamp(),
    user=SUPER_USER,
    file="",
    hostname=hostname,
    type="warning",
    value=paste0(value,add)
    
  )
  dbWriteTable(conn=conn,name="cm3usage",usage,append=T)
  
}


update <- function(servers,msg="WIP"){
  #  browser()
  res <-
    system("git add .")
  if(T | res!=0){
    system(paste0('git commit -a -m "',msg,'-',collap(servers,sep=", "),'"'))
    # system(paste0("git commit -a -m '",msg,"-",collap(servers,sep=", "),"'"))
    system("git add .")
  }
  for(s in servers){
    hname <- paste0("causalmap.shinyapps.io/cm3",s,"/")
    push_message(hostname = hname,conn = get_conn(),add=msg)
    rsconnect::deployApp(appName=paste0("cm3",s),account="causalmap",forceUpdate = T,lint=F)
    push_message(value = "",hostname = hname)
  }
}
#update(xc("dev2 dev3"),msg = "removed browser call")
# update(xc("dev2 dev3"),msg = "removed browser call and fixed upload of links only files")
# update(xc("dev3"),msg = "trying to improve restore from bookmark")
# update(xc("dev3 dev2"),msg = "fixing file chooser")
# update(xc("dev2 dev3"),msg = "fixing file chooser, adding welcome message")
# update(xc("dev2 dev3"),msg = "fixing bookmark")
# update(xc("dev3"),msg = "fixing bookmark")
# update(xc("dev2 dev3"),msg = "fixing bookmark")
#update(xc("dev2 dev3"),msg = "manual refresh of factor lists in transforms UI")
# update(xc("dev2 dev3"),msg = "re-enable restore of order")
# update(xc("dev2 dev3"),msg = "notion guide page")
# update(xc("dev2"),msg = "initial refresh of factor lists")
# update(xc("dev3"),msg = "add escape from getting things ready modal")
# update(xc("dev3"),msg = "text uploads")
# update(xc("dev3"),msg = "library recommended")
# update(xc("dev3"),msg = "hashtags")
# update(xc("dev3"),msg = "hashtags UI")
# update(xc("dev3"),msg = "text uploads again")
# update(xc("dev2"),msg = "question IDs in chooser")
# update(xc("dev3"),msg = "fixed list and unique in table functions")
update(xc("dev3"),msg = "fixed again list and unique in table functions")
