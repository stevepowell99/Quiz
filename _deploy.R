library(tidyverse)

collap <- function(vec,sep="\n"){
  vec %>% paste0(collapse=sep)
}
collapc <- function(vec){
  vec %>% collap(",")
}
uncollap <- function(vec,sep="\n"){
  vec %>% map(function(x){
    str_split(x,pattern=sep) %>% pluck(1)
    
  })
}
uncollapc <- function(vec){
  vec %>% uncollap(",")
}

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

`%notin%` <- Negate(`%in%`)

time_stamp <- function(){
  Sys.time() %>% str_replace_all(":","-")
}

update <- function(servers="",msg="WIP"){
  #  browser()
  res <-
    system("git add .")
  if(T | res!=0){
    system(paste0('git commit -a -m "',msg,'-',collap(servers,sep=", "),'"'))
    # system(paste0("git commit -a -m '",msg,"-",collap(servers,sep=", "),"'"))
    system("git add .")
  }
}
update(msg = "init 2 from rstudio")
