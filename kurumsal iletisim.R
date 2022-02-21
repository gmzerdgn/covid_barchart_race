library(tidyverse)  #
library(gganimate)  #animated bar chart
library(gifski)     #gif renderer
library(readxl)     #excel okuma
library(dplyr)      #select, filter
library(ggplot2)    #plotting the chart
library(av)         #video renderer

Sys.setenv(LANG = "en") #turns error language to eng to amke it easier to research

# memory.limit(16000)     #increases dedicated limit
style <- deadcolours    #sets the colour scheme of the bars
topn = 15                   #top n countries
ExcelName = "last day.xlsx"    #the name excel is saved in the file
SheetName = "Tidy"             #the sheet to study on
cor_last_data <- read_excel(ExcelName, 
                            sheet = SheetName)
MyInterval <- 21600            #smoothing parameters
IkinciInterval <- MyInterval/4

MyTitle = 'Coronavirus Deaths in Europe : {substring((closest_state), 1, 10)}'  #title of the barchart
vidname = 'Avrupada Olumler 6 Haziran.mp4'          #title of media saved                                   

MySubtitle  =  "Top {topn} countries"           #subtitle 
Caption  = "Informatics Department"                    #credits



str(cor_last_data)             #displays coloumn names
# WantedData <- c("Continent")     #category to study on
# Names <- select(cor_last_data, WantedData)     # displays unique names in category to 
# unique(Names)                                  # make it easier to copy + paste

WantedData = "Death"
Category = "Continent"
cor_processed <- cor_last_data[,c("Countries", "Date", "Day", WantedData, Category)] #reduces the total data to necessities
# cor_processed <- cor_last_data[,c("Countries", "Date","Day", WantedData)] #reduces the total data to necessities
names(cor_processed)[names(cor_processed) == WantedData] <- "Total"
cor_processed <- filter(cor_processed, Continent == "Europe") #filters data in desired category

cor_processed <- cor_processed %>% filter(Total > 0)
Daycount <- cor_processed %>% count(Date)

Daycount

cor_date <- cor_processed %>% filter(Day >= 76)
cor_date

cor_processed <- cor_date

gc()            #tidies memory

#smoothing 
corrank_smoother <- cor_processed %>%                
  group_by(Countries) %>%                            
  # Do somewhat rough interpolation for ranking
  # (Otherwise the ranking shifts unpleasantly fast.)
  complete(Date = full_seq(Date, MyInterval)) %>%
  mutate(Total = spline(x = Date, y = Total, xout = Date)$y) %>%
  group_by(Date) %>%
  mutate(rank = min_rank(-Total) * 1) %>%
  ungroup()

# Then interpolate further to quarter Days for fast number ticking.
# Interpolate the ranks calculated earlier.
corrank_s <- corrank_smoother %>% 
  group_by(Countries) %>%
  complete(Date = full_seq(Date, IkinciInterval)) %>%
  mutate(Total = spline(x = Date, y = Total, xout = Date)$y) %>%
  # "approx" below for linear interpolation. "spline" has a bouncy effect.
  mutate(rank =      approx(x = Date, y = rank,      xout = Date)$y) %>%
  filter(rank<=topn)%>% 
  mutate(Total = round(Total,0)) %>%
  ungroup()  %>% 
  arrange(Countries,Date)

# corrank_ss<- corrank_s %>% 
#   filter(Countries == "Ecuador") %>% 
#   filter(Date > "2020-04-10")
# 
# corrank_new <- corrank_s[!corrank_ss,]
# 
# last_frame <-
#   filter(corrank_s,  Date == "2020-04-10 00:00:00") %>%
#   i= 0
#   # for i in seq(0,86359,5400){
# 
#   }
# 
# last_frame <- filter(corrank_s, Date == "2020-04-10 00:00:00")
# mins = 0
# cor_last <- last_frame  #[c(TRUE, FALSE,TRUE,TRUE,TRUE)]
# for (mins in seq(0,86359,5400)) {
#   minframe <- mutate(last_frame, Date = Date + mins)
#   cor_last <- rbind(cor_last, minframe)
# }
# cor_last
# unique(cor_last, Date)
# corrank_s <- rbind(corrank_s, cor_last)
# corrank_s


# Animation


anim <- ggplot(corrank_s, aes(rank, group = Countries, 
                                  fill = as.factor(Countries), color = as.factor(Countries))) +
  geom_tile(aes(y = Total/2,
                height = Total,
                width = 0.9), alpha = 0.8, color = NA) +

  geom_text(aes(y = 0, label = paste(Countries, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Total,label = scales::comma(Total)), hjust=0) +
  
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
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        # plot.background=element_blank(),     #for recovers and infected
        plot.background=element_rect("black"), #for deaths
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(Date, transition_length = 5, state_length = 0, wrap = TRUE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = MyTitle,  
       subtitle  =  MySubtitle,
       caption  = Caption) 

#add colours to the animation
anim2 <- anim + scale_color_manual(values=style) + scale_fill_manual(values=style)   


#mp4 renderer

df <- animate(anim2, 2000, renderer = av_renderer(vidname), 
              width = 1920, height = 1080, res = 200, fps = 30)
