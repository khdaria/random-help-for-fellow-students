library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(mallet)


page <- html("https://en.wikipedia.org/wiki/Category:American_rappers_by_city")
links <- page %>% html_nodes(xpath = "//div[@class='CategoryTreeItem']/a") %>% html_attr("href")
locations <- page %>% html_nodes(xpath = "//div[@class='CategoryTreeItem']/a") %>% html_text %>%
  str_sub(start = 14)

rap <- data.frame()
for(i in 1:length(links)){
  Sys.sleep(1)
  print(i)
  rappers <- html(str_c("https://en.wikipedia.org" , links[i], collapse = "")) %>% html_nodes(xpath = "//div[@class='mw-category']//li") %>% html_text()
  rap <- rbind(rap, data.frame(rapper = rappers, location = rep(locations[i], times = length(rappers))))
}

rap$rapper <- str_replace_all(rap$rapper, fixed(" (rapper)"), "")
rap$rapper <- str_replace_all(rap$rapper, fixed(" (group)"), "")
rap$rapper <- str_replace_all(rap$rapper, fixed(" (musician)"), "")
rap$rapper <- str_replace_all(rap$rapper, fixed(" (singer)"), "")
rap$rapper <- str_replace_all(rap$rapper, fixed(" (producer)"), "")

rap <- rap[-1,]


page <- html("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate_(2014)")
crime <- page %>% html_node(xpath = "//*[@id='mw-content-text']/table[1]") %>% html_table()
names(crime) <- str_replace_all(names(crime), " ", "_")
names(crime)[5] <- "Murder_and_Nonnegligent_Manslaughter"
for(i in 3:13){
  crime[,i] <- str_replace_all(crime[,i], ",", "")
  crime[,i] <- as.numeric(crime[,i])
  
}


crime_aggr <- crime %>% select(-City) %>% group_by(State) %>% summarise_each( funs(mean(., na.rm = TRUE)), Population, Violent_Crime, Murder_and_Nonnegligent_Manslaughter, Rape, Robbery, Aggravated_Assault, Property_Crime, Burglary, `Larceny-Theft`, Motor_Vehicle_Theft, Arson)
for(i in 3:12){
  crime_aggr[,i] <- crime_aggr[,i] / crime_aggr$Population
}

page <- html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_GDP")
gdp <- page %>% html_node(xpath = "//*[@id='mw-content-text']/table[1]") %>% html_table()


rap <- rap %>% separate(location, c("City", "State"), ", ")
rap$State[rap$State == "D.C."] <- "District of Columbia"
rap$State[rap$City == "New York City"] <- "New York"
rap$State[rap$City == "the San Francisco Bay Area"] <- "California"
rap_count <- rap %>% group_by(State) %>% tally()



gdp$State <- str_trim(gdp$State)
gdp <- left_join(gdp, rap_count, by = "State")
names(gdp)[3] <- "gdp"
gdp$gdp <- str_replace_all(gdp$gdp, ",", "")
gdp$gdp <- as.numeric(gdp$gdp)
gdp$gdp2 <- log(gdp$gdp)
ggplot(gdp, aes(gdp2, n)) + geom_point()
gdp2 <- select(gdp, gdp, gdp2, n, State) %>% na.omit()
cor(gdp2$gdp, gdp2$n)
cor(gdp2$gdp2, gdp2$n)

crimerap <- left_join(gdp2, crime_aggr, by = "State")
crimerap_m <- as.matrix(select(crimerap, gdp, gdp2, n, Population, Violent_Crime, Murder_and_Nonnegligent_Manslaughter, Rape, Robbery, Aggravated_Assault, Property_Crime, Burglary, `Larceny-Theft`, Motor_Vehicle_Theft, Arson))
corrgram(cor(crimerap_m))


page <- html("http://www.ohhla.com/all.html")
links <- page %>% html_nodes(xpath = "//pre/a") %>% html_attr("href")
links <- na.omit(links)

#texts <- data.frame()
for (i in links[113:length(links)]){
  print(i)
  if(str_detect(i, "anonymous")){
    link <- str_c("http://ohhla.com/", i)
    page <- html(link)
    albums <- page %>% html_nodes(xpath = "//li/a") %>% html_attr("href")
    albums <- albums[-1]
    for(j in albums){
      url <- str_c(link, j)
      print(j)
      page <- html(url)
      songs <- page %>% html_nodes(xpath = "//li/a") %>% html_attr("href")
      songs <- songs[-1]
      for(k in songs){
        url_all <- str_c(url, k)
        page <- html(url_all)
        song <- page %>% html_nodes(xpath = "//pre") %>% html_text()
        if(length(song) == 0){
          song <- page %>% html_nodes(xpath = "//p") %>% html_text()
        }
        texts <- rbind(texts, data.frame(artist = i, album = j, song = k, text = song))
      }
    }
  }
}

write.csv(texts, "rap_lyrics.csv")


page <- html("https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2012/crime-in-the-u.s.-2012/tables/1tabledatadecoverviewpdf/table_1_crime_in_the_united_states_by_volume_and_rate_per_100000_inhabitants_1993-2012.xls")
crime <- page %>% html_node(xpath = '//*[@id="table-data-container"]/table') %>%html_table()
crime$Year <- str_sub(crime$Year, end = 4)
crime <- na.omit(crime)
for(i in 3:20){
  crime[,i] <- str_replace_all(crime[,i], ",", "")
  crime[,i] <- as.numeric(crime[,i])
  
}

crime <- tidyr::gather(crime, "type", "n", 3:20)
crimerate <- filter(crime, str_detect(crime$type, "rate"))

ggplot(crimerate, aes(Year, n, group = type, color = type)) + geom_line(size = 3) + scale_colour_brewer(palette = "Set1") + theme(text = element_text(size=15))

c <- VCorpus(VectorSource(ria_df$text_lem), readerControl = list(language = "ru"))
c <- tm_map(c, removeWords, stopwords('ru'))
dtm <- DocumentTermMatrix(c)
findFreqTerms(dtm, 1000)


ria_df$id <- 1:nrow(ria_df) %>% as.character

mallet.instances <- mallet.import(ria_df$id, ria_df$text_lem, "stopwords.txt", token.regexp = "[\\p{L}\\p{N}-]*\\p{L}+")
topic.model <- MalletLDA(num.topics=100) 
topic.model$loadDocuments(mallet.instances) 
topic.model$setAlphaOptimization(20, 50)

word.freqs <- mallet.word.freqs(topic.model)
topic.model$train(1000)
topic.model$maximize(10)


doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

top <- c()
for (k in 1:nrow(topic.words)) {
  top <- c(top,paste(mallet.top.words(topic.model, topic.words[k,], 30)$words,collapse=" "))
}
write.table(top, "top.txt")