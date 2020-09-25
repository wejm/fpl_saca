#Code to scrape Fantasy Premier League API for team data

setwd("~/fpl/fpl_saca")
#setwd("~/Documents/FPL/2020/Lads")

#Loading packages
library('rvest')
library("jsonlite")

#Player_lookup
name <- c('Zac','Tom','Josh','Scot','Paddy','Luke','Alex')
code <- c('29649','345408','4871937','555634','172179','3136901','3528506')

name <- c('Gray','Daisy','Curt','HarryM','Charlie','Harriet','HarryB','Tom','Joe')
code <- c('2337150','4173915','2808893','1534451','3529644','2187963','2197544','4789785','3250312')



#player_lookup <- tibble(name,history.entry)
#write.csv(player_lookup,"player_lookup.csv")

j <- 1

#Get first user data to initialise csv file - always my id
test <- '58628'
json_file <- paste0('https://fantasy.premierleague.com/api/entry/', test, '/history/', sep='')
data <- fromJSON(json_file)
current <- data.frame(data[1])
current$player_id <- 58628
current$player_name <- 'Will'
write.csv(current, "data.csv")

#loop through code array to append subsequent user data
for (i in code) {
  test <- i
  json_file_app <- paste0('https://fantasy.premierleague.com/api/entry/', test, '/history/', sep='')
  data_app <- fromJSON(json_file_app)
  current <- data.frame(data_app[1])
  current$player_id <- i
  current$player_name <- name[j]
  write.table(current, "data.csv", sep = ",", col.names = F, append = T)
  j <- j+1
  
}





