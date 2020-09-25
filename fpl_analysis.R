setwd("~/fpl/fpl_saca")

library(ggplot2)
library(tidyverse)

data <- read.csv('data.csv')
#drop_names <- c('Josh','Alex','Luke')
#drop_weeks <- c(30:38)
main <- data
#removing boys not on chat and removing blank covid Gameweeks

#main <- filter(data,!(player_name %in% drop_names) ) %>%
#       filter(!(current.event %in% drop_weeks))

#for(i in 1:length(main$current.event)){
#  if(main[i,"current.event"] >=39){
#    main[i,"current.event"] <- main[i,"current.event"]-9}
#}


#making scot gap data

# wide_points <- select(main,player_name,current.event, current.total_points) %>%
#                 spread(player_name,current.total_points)
# 
# wide_points[, "leader"] <- apply(wide_points[, 2:6], 1, max)
# wide_points[, "4th"] <- apply(wide_points[, c(2,4,5,6)], 1, min)
# wide_points$scot_leader <- -(wide_points$leader - wide_points$Scot)
# wide_points$gap_4th <- wide_points$`4th` - wide_points$Scot
# wide_points$paddy_leader <- -(wide_points$leader - wide_points$Paddy)
# wide_points$will_leader <- -(wide_points$leader - wide_points$Will)
# wide_points$zac_leader <- -(wide_points$leader - wide_points$Zac)
# wide_points$tom_leader <- -(wide_points$leader - wide_points$Tom)



#reformatting value
main$current.value <- main$current.value/10


#transfers made

t_made <- select(main,player_name, current.event_transfers) %>%
          group_by(player_name) %>%
          summarise(current.event_transfers=sum(current.event_transfers))

#transfers cost

t_cost <- select(main,player_name, current.event_transfers_cost) %>%
  group_by(player_name) %>%
  summarise(current.event_transfers_cost=sum(current.event_transfers_cost))

#bench points

b_points <- select(main,player_name, current.points_on_bench) %>%
  group_by(player_name) %>%
  summarise(current.points_on_bench=sum(current.points_on_bench))

summary(main$player_name)

str(data)

p <- ggplot(main,aes(x=current.event,y=current.total_points)) + geom_line(aes(color=player_name)) + theme_bw()

print(p)

p <- ggplot(main,aes(x=current.event,y=current.value)) + geom_line(aes(color=player_name)) + theme_bw()

print(p)


p <- ggplot(wide_points,aes(x=current.event,y=gap_leader)) + geom_line() + theme_bw() + ggtitle("Scot's gap from 1st")

print(p)

p <- ggplot(wide_points,aes(x=current.event,y=paddy_leader)) + geom_line() + theme_bw() + ggtitle("Paddy's gap from 1st")

print(p)

p <- ggplot(wide_points,aes(x=current.event,y=will_leader)) + geom_line() + theme_bw() + ggtitle("Will's gap from 1st")

print(p)

p <- ggplot(wide_points,aes(x=current.event,y=zac_leader)) + geom_line() + theme_bw() + ggtitle("Zac's gap from 1st")

print(p)

p <- ggplot(wide_points,aes(x=current.event,y=tom_leader)) + geom_line() + theme_bw() + ggtitle("Tom's gap from 1st")

print(p)


p <- ggplot(wide_points,aes(x=current.event,y=gap_4th)) + geom_col() + theme_bw() + ggtitle("Scot's gap from 4th")

print(p)

p <- ggplot(t_made,aes(x=player_name,y=current.event_transfers)) + geom_col() + theme_bw() + ggtitle("Transfers made")

print(p)


p <- ggplot(t_cost,aes(x=player_name,y=current.event_transfers_cost)) + geom_col() + theme_bw() + ggtitle("Transfer cost")

print(p)


p <- ggplot(b_points,aes(x=player_name,y=current.points_on_bench)) + geom_col() + theme_bw() + ggtitle("Bench points")

print(p)

?gather


p <- ggplot(wide_points,aes(x=current.event)) + geom_line(aes(y=paddy_leader, colour = 'red')) + geom_line(aes(y=zac_leader, colour = 'green')) + theme_bw() + ggtitle("All gap from 1st")

print(p)


#Analysis on biggest ground made up - ie gameweek variation

wide_GWpoints <- select(main,player_name,current.event, current.points) %>%
  spread(player_name,current.points)

wide_GWpoints[, "leader"] <- apply(wide_GWpoints[, 2:6], 1, max)
wide_GWpoints[, "4th"] <- apply(wide_GWpoints[, c(2,4,5,6)], 1, min)
wide_GWpoints$scot_leader <- -(wide_GWpoints$leader - wide_GWpoints$Scot)
wide_GWpoints$gap_high_low <- wide_GWpoints$leader - wide_GWpoints$`4th`
wide_GWpoints$paddy_leader <- -(wide_GWpoints$leader - wide_GWpoints$Paddy)
wide_GWpoints$will_leader <- -(wide_GWpoints$leader - wide_GWpoints$Will)
wide_GWpoints$zac_leader <- -(wide_GWpoints$leader - wide_GWpoints$Zac)
wide_GWpoints$tom_leader <- -(wide_GWpoints$leader - wide_GWpoints$Tom)
wide_GWpoints$zac_ <- (wide_GWpoints$zac - wide_GWpoints$leader)

p <- ggplot(wide_GWpoints, aes(x=gap_high_low)) + geom_histogram(binwidth = 5) + xlab('Max gap made up in a gameweek (5 point bins)') + ylab('Count')
print(p)
