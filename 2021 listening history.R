library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)

setwd("~/spotify stuffs/MyData 1-4-22")

# pulling all files
streamHistory <- fromJSON("StreamingHistory0.json", flatten = TRUE)   
streamHistory1 <- fromJSON("StreamingHistory1.json", flatten = TRUE)   
streamHistory2 <- fromJSON("StreamingHistory2.json", flatten = TRUE) 
streamHistory3 <- fromJSON("StreamingHistory3.json", flatten = TRUE)   
streamHistory4 <- fromJSON("StreamingHistory4.json", flatten = TRUE)
streamHistory5 <- fromJSON("StreamingHistory5.json", flatten = TRUE)

#combing files into one dataframe
total <- rbind(streamHistory,
               streamHistory1,
               streamHistory2,
               streamHistory3,
               streamHistory4,
               streamHistory5)
#removing non-2021 songs
total <- total[-c(55510:56040), ]  
view(total)

access_token <- get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731")


#pulling playlists
playlist <- fromJSON("Playlist1.json", flatten = TRUE)  
view(playlist)

doctor <- ((playlist[["playlists"]])[[3]][[9]])
view(doctor)

#searches
searches <- fromJSON("SearchQueries.json", flatten = TRUE)

#woooo this get me track information :)
trackinfo <- get_track("6UryEVkqPDLliZOG4UmFi9", market = NULL, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(trackinfo)

get_track_audio_analysis("6UryEVkqPDLliZOG4UmFi9", get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))

get_track_audio_features("6UryEVkqPDLliZOG4UmFi9", get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))

#this removes the spotify:track: text in front of ID so R can read it 
doctor$track.trackUri<-gsub("spotify:track:","",as.character(doctor$track.trackUri))
view(doctor)

#gettrackfeats only lets me use 100 ids at a time 
first <- doctor[-c(101:365), ]
view(first)

second <- doctor[-c(1:100, 201:365), ]
view(second)

third <- doctor[-c(1:200, 301:365), ]
view(third)

fourth <- doctor[-c(1:300), ]

#getting feats for all groups
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

#plots
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


#tyring new functions
tops <- get_my_top_artists_or_tracks(type = "artists", limit= 20, offset = 0)

#this has lots of info
doctorstats <-get_playlist_audio_features("lexijewell", "4oS46HAvvek3W6FtOAjeRw", get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(doctorstats)                 

#i need to figure out how to redirct url
install.packages("httr")
library(httr)
library(devtools) 
library(tidyr) 
library(knitr)
library(tidyverse)

oauth_callback("http://localhost:1410/")
?oauth_callback()

#genius lyrics
install.packages("geniusr")
genius_token("ctdXebuaMxOPt1AWWOA2QLpymD7weaNWQBvPu9j1pLpyBSd7FgfuK-iCdkXzoKC-")
token <- "ctdXebuaMxOPt1AWWOA2QLpymD7weaNWQBvPu9j1pLpyBSd7FgfuK-iCdkXzoKC-"

library(geniusr)
library(dplyr)
library(tidytext)
install.packages("genius")
library(genius)
#these work
genius_album(artist = "Atta Boy", album = "Out of Sorts", info = "simple")
#this is how i find song_id
oor <- search_song("Out of Sorts",10, "ctdXebuaMxOPt1AWWOA2QLpymD7weaNWQBvPu9j1pLpyBSd7FgfuK-iCdkXzoKC-")
view(oor)
get_lyrics_id("3825527", "token")
view(ooslyrics)
get_lyrics_search(artist_name = "Atta Boy", song_title = "Out of Sorts")

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


