library(tidyverse)
pdf("visualization.pdf") 
DATASET <- read_csv("DATASET.csv",show_col_types = FALSE)
Required <- c("SteamSpyOwners","GenreIsStrategy", "GenreIsRPG")
newDataset = DATASET[Required]
final<-newDataset[(newDataset$GenreIsStrategy=="TRUE" | newDataset$GenreIsRPG=="TRUE"),]
d<-final[!(final$GenreIsStrategy=="TRUE" & final$GenreIsRPG=="TRUE"),]
d <- d %>%
  add_column(Genre = 
               if_else(.$GenreIsRPG =="TRUE", "RPG", "strategy"),
             .after="SteamSpyOwners")
d<-d[!(d$SteamSpyOwners==0),]
d <- d %>%
  add_column(Log_steamplayer = 
               log10(d$SteamSpyOwners))

boxplot(d$Log_steamplayer~d$Genre,
        main = "Distribution of players",
        xlab = "Genres",
        ylab = expression('Log'[10]),
        col = "orange",
        border = "brown",
        vertical = TRUE,
)


h1<-hist(d$Log_steamplayer,
         col = rgb(1,0.8,0,1),main = "Distribution of player's",xlab =expression('Log'[10]*'(Number of Players)'))

x1<-seq(0,max(d$Log_steamplayer),1)
y1<-d$Log_steamplayer
mn<-mean(y1)
stDev <- sd(y1)
yn <- dnorm(x1,mean = mn, sd = stDev)
box.size<- diff(h1$mids[1:2]) * length(y1)
yn<-yn*box.size
lines(x1, yn, col="red" )


dev.off()