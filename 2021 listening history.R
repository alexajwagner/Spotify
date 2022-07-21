install.packages(spotifyr)
install.packages(ggplot2)

library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)

#this will be specific to you (Session-> set working directory-> choose directory)
setwd("~/spotify stuffs/MyData 1-4-22")

# pull all files from your downloaded data
streamHistory <- fromJSON("StreamingHistory0.json", flatten = TRUE)   
streamHistory1 <- fromJSON("StreamingHistory1.json", flatten = TRUE)   
streamHistory2 <- fromJSON("StreamingHistory2.json", flatten = TRUE) 
streamHistory3 <- fromJSON("StreamingHistory3.json", flatten = TRUE)   
streamHistory4 <- fromJSON("StreamingHistory4.json", flatten = TRUE)
streamHistory5 <- fromJSON("StreamingHistory5.json", flatten = TRUE)

#combining files into one dataframe
total <- rbind(streamHistory,
               streamHistory1,
               streamHistory2,
               streamHistory3,
               streamHistory4,
               streamHistory5)
view(total)

#removing non-2021 songs NOT ENTIRELY NECESSARY
#this allows you to remove rows 55510-56040, replace numbers from your "total"
total <- total[-c(55510:56040), ]  
view(total)

access_token <- get_spotify_access_token(
  client_id = "Insert your client id here",
  client_secret = "insert your client secret here")


#pulling playlists
playlist <- fromJSON("Playlist1.json", flatten = TRUE)  
view(playlist)

#gettting a specific playlist
#you can name this whatever, make sure to change it in other lines of code
doctor <- ((playlist[["playlists"]])[[3]][[9]])
view(doctor)

#searches (just for fun)
searches <- fromJSON("SearchQueries.json", flatten = TRUE)

#track information :)
trackinfo <- get_track("6UryEVkqPDLliZOG4UmFi9", market = NULL, get_spotify_access_token(
  client_id = "insert your client id",
  client_secret = "insert your client secret"))
view(trackinfo)

get_track_audio_analysis("6UryEVkqPDLliZOG4UmFi9", get_spotify_access_token(
  client_id = "insert your client id",
  client_secret = "insert your client secret"))

get_track_audio_features("6UryEVkqPDLliZOG4UmFi9", get_spotify_access_token(
  client_id = "insert your client id",
  client_secret = "insert your client secret"))

#this removes the spotify:track: text in front of ID so R can read it 
doctor$track.trackUri<-gsub("spotify:track:","",as.character(doctor$track.trackUri))
view(doctor)

#gettrackfeats only lets me use 100 ids at a time so we have to do it in chunks
#here i am breaking my 365 song playlist into 4 dataframes
first <- doctor[-c(101:365), ]
view(first)

second <- doctor[-c(1:100, 201:365), ]
view(second)

third <- doctor[-c(1:200, 301:365), ]
view(third)

fourth <- doctor[-c(1:300), ]

#getting feats for all groups
#now I am getting the features for each group
firstfeats <- get_track_audio_features(first$track.trackUri, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(firstfeats)

secondfeats <- get_track_audio_features(second$track.trackUri, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(secondfeats)

thirdfeats <- get_track_audio_features(third$track.trackUri, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(thirdfeats)

fourthfeats <- get_track_audio_features(fourth$track.trackUri, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(fourthfeats)


#combing all of the groups 
allfeats <- rbind(firstfeats, secondfeats, thirdfeats, fourthfeats)
view(allfeats)

#including title and artist
final <- cbind(allfeats, doctor$track.trackName, doctor$track.artistName)
view(final)

#cleaning up column names
names(final)[names(final) == "doctor$track.trackName"] <- "title"
view(final)
names(final)[names(final) == "doctor$track.artistName"] <- "artist"
view(final)

#realized the dates were missing
dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days")
ffinal <- cbind(final, dates)
view(ffinal)

#basic plots
plot(ffinal$dates, ffinal$energy)
plot(ffinal$dates, ffinal$danceability)
plot(ffinal$dates, ffinal$key)
plot(ffinal$dates, ffinal$loudness)
plot(ffinal$dates, ffinal$mode)
plot(ffinal$dates, ffinal$acousticness)
plot(ffinal$dates, ffinal$instrumentalness)
plot(ffinal$dates, ffinal$liveness)
plot(ffinal$dates, ffinal$valence)
plot(ffinal$dates, ffinal$tempo)

#this has lots of info
doctorstats <-get_playlist_audio_features("lexijewell", "4oS46HAvvek3W6FtOAjeRw", get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(doctorstats)                 

###plots from doctorstats###
install.packages("ggplot2")
library(ggplot2)

#Distribution of the popularity of tracks
ggplot(doctorstats, aes(x = doctorstats$track.popularity))+
  theme_bw() +
  geom_density( alpha= 0.5, fill = "#FF6666") +
  labs( x ="Popularity of tracks",
        title = "Distribution of the popularity of tracks")

#Number of track vs. keys
doctorstats$key <- as.factor(doctorstats$key)
ggplot(doctorstats, aes(x = key, fill = key))+
  theme_bw()+
  geom_bar()+
  scale_fill_discrete(name="Key", labels=c("0-C", "1-C# Db", "2-D","3-D# Eb","4-E","5-F","6-F# Gb","7-G","8-G# Ab","9-A","10-A# Bb","11-B"))+
  labs(x = "Keys of tracks",
       y = "Number of tracks",
       title = "Number of tracks Vs. Keys")

# Does loud songs mean more energetic songs? most of the energy measured by loudness? 
ggplot(doctorstats, aes(x = loudness, y = energy))+ 
  theme_bw()+ 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Loudness of track",
       y = "Energy of tracks",
       title = "Energy Vs. Loudness")

#acousticness
ggplot(doctorstats, aes(x = acousticness))+
  theme_bw() +
  geom_density( alpha= 0.5, fill = "#0a5b96") +
  labs( x ="Acousticness of tracks",
        title = "Distribution of the Acousticness of tracks")

key_energy <- ggplot(doctorstats, aes(x=energy, fill=key)) + 
  geom_boxplot() +
  scale_fill_discrete(name="Keys", labels=c("0-C", "1-C# Db", "2-D","3-D# Eb","4-E","5-F","6-F# Gb","7-G","8-G# Ab","9-A","10-A# Bb","11-B"))+
  facet_wrap(~key)+
  labs( x ="Energy of tracks",
        title = "Key wise boxplot of Enerygy")
key_energy


