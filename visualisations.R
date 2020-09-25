library(tidyverse)
library(ggplot2)
library(data.table)
library(plotly)
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


?subset

head(df)

#useful variables
max_GW <- max(df$Gameweek)

#Line chart - total points

pl <- ggplot(df,aes(x=Gameweek,y=Total_points, color=Player)) + geom_line() 
pl <- pl + scale_x_continuous(breaks=seq(0, max_GW, 1))
pl <- pl + ggtitle("Total points by GW")+ylab('Points')
pl <- pl + theme_hc()+ scale_colour_hc()
pl
print(ggplotly(pl))

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
bp <- bp + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()
bp

# Bar chart - Transfers made

tm <- ggplot(df,aes(x=reorder(Player,current.event_transfers), y=current.event_transfers)) + geom_bar(stat='Identity') 
tm <- tm + ggtitle("Transfers") + xlab('Transfers') + ylab('Transfers') + scale_y_continuous(breaks=seq(0, 16, 1))
tm <- tm + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()
tm


# Bar chart - Transfer spend

ts <- ggplot(df,aes(x=reorder(Player,current.event_transfers_cost), y=current.event_transfers_cost)) + geom_bar(stat='Identity') 
ts <- ts + ggtitle("Transfer spend") +ylab('Transfer spend') + scale_y_continuous(breaks=seq(0, 16, 1))
ts <- ts + theme_hc()+ scale_colour_hc() + theme(legend.position = "none") + coord_flip()
ts

Sys.Date()
install.packages('leaflet')
