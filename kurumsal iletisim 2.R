library(tidyverse)  #
library(gganimate)  #animated bar chart
library(gifski)     #gif renderer
library(readxl)     #excel okuma
library(dplyr)      #select, filter
library(ggplot2)    #plotting the chart
library(av)         #video renderer
library(stringr)

Sys.setenv(LANG = "en") #turns error language to eng to make it easier to research

# memory.limit(16000)     #increases dedicated limit
topn = 20                   #top n countries
ExcelName = "covid.xlsx"    #the name excel is saved in the file
SheetName = "Tidy"             #the sheet to study on
cor_last_data <- read_excel(ExcelName, 
                            sheet = SheetName)
MyInterval <- 43200            #smoothing parameters
IkinciInterval <- MyInterval/4

MyTitle = 'Coronavirus Deaths Top {topn} Countries: {substring((closest_state), 1, 10)}'  #title of the barchart
vidname = 'Dunyada olumler nufusa gore feb 7.mp4'          #title of media saved                                   

MySubtitle  =  "Deaths per million people (5M or more population countries)"           #subtitle 
Caption  = "IU Faculty of Economics \n twitter: @iuiktisatedu    instagram: @istanbuliktisat     facebook: /iuiktisat1936"                    #credits


str(cor_last_data)             #displays coloumn names
# WantedData <- c("Continent")     #category to study on
# Names <- select(cor_last_data, WantedData)     # displays unique names in category to 
# unique(Names)                                  # make it easier to copy + paste

WantedData = "Death/Population"
Category = "Population"
Kriter = "Africa"

if (WantedData =="Death" || WantedData == "Death/Population") {
  Arkaplan = "black"
  style <- deadcolours
} else if (WantedData =="Infection" || WantedData == "Infected/Population") {
  Arkaplan= "white"
  style <- infectedcolours
} else {
    Arkaplan= "cornsilk2"
    style <- recoveredcolours
}

cor_processed <- cor_last_data[,c("Countries", "Date", "Day", WantedData, Category)] #reduces the total data to necessities
#cor_processed <- cor_last_data[,c("Countries", "Date","Day", WantedData)] #reduces the total data to necessities
names(cor_processed)[names(cor_processed) == WantedData] <- "Total"
#cor_processed <- filter(cor_processed, Total == Kriter) #filters data in desired category

cor_processed <- cor_processed %>% filter(Population > 5000000)
#og: 2000000
cor_processed <- cor_processed %>% filter(Total > 0)
Daycount <- cor_processed %>% count(Date)

Daycount


#infection world 40 
#death world 55
#

cor_date <- cor_processed %>% filter(Day >= 55)
cor_date

cor_processed <- cor_date


lastday = max(cor_processed$Day)
lastday
last_frame <- filter(cor_processed, Day == lastday)
last_frame
# mins = 0
# cor_last <- last_frame  #[c(TRUE, FALSE,TRUE,TRUE,TRUE)]
# for (mins in seq(0,86359,5400)) {
#   minframe <- mutate(last_frame, Date = Date + mins)
#   cor_last <- rbind(cor_last, minframe)
# }
lastdate = max(cor_processed$Date)
lastdate = lastdate + 86400 #Bir gün ekleniyor
lastdate
lastframe <- mutate(last_frame, Date = lastdate)
lastframe


cor_processed <- rbind(cor_processed, lastframe)
cor_processed
arrange(cor_processed, desc(Date))

gc()            #tidies memory

#smoothing 
corrank_smoother <- cor_processed %>%                
  group_by(Countries) %>%                            
  # Do somewhat rough interpolation for ranking
  # (Otherwise the ranking shifts unpleasantly fast.)
  complete(Date = full_seq(Date, MyInterval)) %>%
  mutate(Total = approx(x = Date, y = Total, method = "linear", xout = Date)$y) %>%
  group_by(Date) %>%
  mutate(rank = min_rank(-Total) * 1) %>%
  ungroup()

# Then interpolate further to quarter Days for fast number ticking.
# Interpolate the ranks calculated earlier.
corrank_s <- corrank_smoother %>% 
  group_by(Countries) %>%
  complete(Date = full_seq(Date, IkinciInterval)) %>%
  mutate(Total = approx(x = Date, y = Total, method = "linear", xout = Date)$y) %>%
  # "approx" below for linear interpolation. "spline" has a bouncy effect.
  mutate(rank =      approx(x = Date, y = rank,      xout = Date)$y) %>%
  filter(rank<=topn)%>% 
  mutate(Total = round(Total,0)) %>%
  ungroup()  %>% 
  arrange(Countries,Date)


# corrank_ss<- corrank_s %>%
#   filter(Date > "2020-06-06")
# corrank_ss
# 
# corrank_new <- corrank_s[!corrank_ss,]

# last_frame <-
#   filter(corrank_s,  Date == "2020-06-06 00:00:00") %>%
#   i= 0
#   # for i in seq(0,86359,5400){
# 
#   }
# 
# last_frame <- filter(corrank_s, Day == "137")
# last_frame
# # mins = 0
# # cor_last <- last_frame  #[c(TRUE, FALSE,TRUE,TRUE,TRUE)]
# # for (mins in seq(0,86359,5400)) {
# #   minframe <- mutate(last_frame, Date = Date + mins)
# #   cor_last <- rbind(cor_last, minframe)
# # }
# lastframe <- mutate(last_frame, Date = "2020-06-07 03:00:00")
# lastframe
# 
# 
# corrank_s <- rbind(corrank_s, lastframe)
# corrank_s


# Animation


anim <- ggplot(corrank_s, aes(rank, group = Countries, 
                              fill = as.factor(Countries), color = as.factor(Countries))) +
  geom_tile(aes(y = Total/2,
                height = Total,
                width = 0.9), alpha = 0.8, color = NA) +
  
  geom_text(aes(y = 0, label = paste(Countries, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Total,label = scales::comma(Total, accuracy = 1)), hjust=0) +
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=18, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=16, hjust=0.5, color="dodgerblue3"),
        # plot.background=element_blank(),     #for recovers and infected
        plot.background=element_rect(Arkaplan), #for deaths
        plot.margin = margin(0.75, 2, 0.75, 4, "cm")) +
  transition_states(Date, transition_length = 5, state_length = 0, wrap = TRUE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = MyTitle,  
       subtitle  =  MySubtitle,
       caption  = Caption) 

#add colours to the animation
anim2 <- anim + scale_color_manual(values=style) + scale_fill_manual(values=style)   

#mp4 renderer

df <- animate(anim2, 3000, renderer = av_renderer(vidname), 
              width = 1920, height = 1080, res = 200, fps = 30)
#og 3000
#w: 1920 h: 1080
#vertical w: 720 h: 1280

