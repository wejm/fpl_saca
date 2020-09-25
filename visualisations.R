library(tidyverse)
library(ggplot2)
library(data.table)
library(plotly)
library(directlabels)
library(grid)
library(ggthemes) # Load

setwd("~/fpl/fpl_saca")

#cleaning up data

df <- fread(file="data.csv", drop=1)
df <- rename(df, Player = player_name) #For renaming dataframe column
df <- rename(df, Total_points = current.total_points)
df <- rename(df, Gameweek_points = current.points)
df <- rename(df, Gameweek = current.event)
df$value_m <- df$current.value/10



GWleader <- subset(df,Gameweek==max_GW)
firstGW <- max(GWleader$Gameweek_points)
GWleader <- subset(df, Gameweek_points==firstGW)
first <- max(df$Total_points)
leader <- subset(df, Total_points==first)
l <- as.character(leader$player_name)
tp <- as.numeric(leader$Total_points)

df2 <- df %>%
  group_by(Player) %>%
  summarise(total_transfer_cost = sum(current.event_transfers_cost))
max_transfer_cost <- if(max(df2$total_transfer_cost)==0){4}else{max(df2$total_transfer_cost)}



?subset

head(df)

#useful variables
max_GW <- max(df$Gameweek)

#Line chart - total points

p1 <- ggplot(df, aes(x = Gameweek, y = Total_points, colour = Player, group = Player)) + 
  geom_line() + 
  scale_colour_discrete(guide = 'none')  + 
  scale_x_continuous(breaks=seq(0, max_GW, 1), expand = c(0, 0)) +
  geom_dl(aes(label = Player), method = list(dl.trans(x = x + .3), "last.bumpup")) +
  theme_bw() +
  theme(plot.margin = unit(c(1,4,1,1), "lines")) 
# Code to turn off clipping
gt1 <- ggplotGrob(p1)
gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
grid.draw(gt1)

#Line plot - value

vl <- ggplot(df,aes(x=Gameweek,y=value_m, color=Player)) + geom_line() 
vl <- vl + scale_x_continuous(breaks=seq(0, max_GW, 1))
vl <- vl + ggtitle("Value by GW") +ylab('Value (£m)')
vl <- vl + theme_hc()+ scale_colour_hc()
vl

print(ggplotly(vl))

# Bar chart - bench points

bp <- ggplot(df,aes(x=reorder(Player,current.points_on_bench), y=current.points_on_bench)) + geom_bar(stat='Identity') 
bp <- bp + ggtitle("Bench points") +ylab('Points on bench')
bp <- bp + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()+
  geom_text(aes(label=sum(current.points_on_bench)))

bp

# Bar chart - Transfers made
df2 <- df %>%
  group_by(Player) %>%
  summarise(total_transfers = sum(current.event_transfers))
max_transfers <- max(df2$total_transfers)

tm <- ggplot(df,aes(x=reorder(Player,current.event_transfers), y=current.event_transfers)) + geom_bar(stat='Identity') 
tm <- tm + ggtitle("Transfers") + xlab('Transfers') + ylab('Transfers') + scale_y_continuous(breaks=seq(0, 1, 1))
tm <- tm + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()
tm


# Bar chart - Transfer spend

ts <- ggplot(df,aes(x=reorder(Player,current.event_transfers_cost), y=current.event_transfers_cost)) + geom_bar(stat='Identity') 
ts <- ts + ggtitle("Transfer spend") +ylab('Transfer spend') + scale_y_continuous(breaks=seq(0, 1, 1))
ts <- ts + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()
ts

Sys.Date()
install.packages('leaflet')
