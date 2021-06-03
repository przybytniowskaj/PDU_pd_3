library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(wesanderson)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

Posts <- read.csv("gaming.stackexchange.com/Posts.xml.csv")
PostHistory <- read.csv("gaming.stackexchange.com/PostHistory.xml.csv")


#Wykres przedstawiający jaka liczba postów została napisana o xbox a jaka o ps(1,2,3,4 lub 5) 
#w poszczególnych latach zaczynając na 2017 konczac na 2020 

not_deleted <- PostHistory %>%
  filter(PostHistoryTypeId != 12)%>%
  inner_join(Posts, by = c("PostId" = "Id"))%>%
  distinct(PostId, .keep_all = TRUE)

xbox_questions <- filter(not_deleted, str_detect(Tags, 'xbox'))
  
ps_questions <- filter(not_deleted, str_detect(Tags, 'ps1') | str_detect(Tags, "ps2")| str_detect(Tags, "ps3") | str_detect(Tags, "ps4") | str_detect(Tags, "ps3"))

xbox2017 <- as.integer(
filter(xbox_questions, str_detect(CreationDate.y, "2017"))%>%
  count())

xbox2018 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2018"))%>%
    count())

xbox2019 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2019"))%>%
    count())

xbox2020 <- as.integer(
  filter(xbox_questions, str_detect(CreationDate.y, "2020"))%>%
    count())

ps2017 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2017"))%>%
    count())

ps2018 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2018"))%>%
    count())

ps2019 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2019"))%>%
    count())

ps2020 <- as.integer(
  filter(ps_questions, str_detect(CreationDate.y, "2020"))%>%
    count())

LiczbaWyszukan <- c(xbox2017, ps2017, xbox2018, ps2018, xbox2019, ps2019, xbox2020, ps2020)
Urzadzenie <- c("Xbox", "PlayStation", "Xbox", "PlayStation", "Xbox", "PlayStation", "Xbox", "PlayStation")
Rok <- c("2017",'2017', "2018","2018", "2019", "2019", "2020", "2020")

data <- data.frame(Rok, Urzadzenie, LiczbaWyszukan)

ggplot(data, aes(fill=Urzadzenie, y=LiczbaWyszukan, x=Rok)) +
  geom_bar(position="stack", stat="identity") + scale_fill_manual("Urzadzenie", values = c("PlayStation" = "lightblue", "Xbox" = "darkblue"))
