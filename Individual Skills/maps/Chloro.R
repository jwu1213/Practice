library(tidyverse)
library(mapproj)
library(scales)
library(rworldmap)
library(maps)

load("shootings.Rda")
View(shootings)
table(shootings$State)
unique(shootings$State)


#NY State Coordinates
x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11)
y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)

#Plot1: Map Load
ggplot()+geom_polygon(aes(x,y), fill="firebrick1",border=NA)

#Fix State name spacing
shootings$State<-gsub("^\\s|\\s$","",shootings$State) 

#Create attribute representing the total number of victims. 
agg.dat <- shootings %>% 
  group_by(State) %>%
  summarise(victims = sum(Total.Number.of.Victims))
agg.dat
states <- map_data("state")
unique(states$region) #gives all unique values

#Plot 2: Black and white
#Issues: Lines are having trouble connecting, cutting several states in half
ggplot(states)+
  geom_polygon(aes(long,lat, fill=region), alpha=0, color="black")+
  theme(legend.position="none")

#Plot3: Filled in colors, state boundaries are colored in. Looks better
#Issues: Color doesnt have much meaning
ggplot(states)+
  geom_polygon(aes(long,lat, group=group, fill=region), alpha=0.5, color="black")+
  theme(legend.position="none")

#Assigns a random number to attribute fakedata to test color
chloro<-states %>% 
  group_by(region) %>% 
  mutate(fakedata= floor(runif(1,0,300)))

#Plot4: Adds color scale as a dimension. 
#issues doesn't represent anything
ggplot(chloro)+
  geom_polygon(aes(long,lat,group=group,fill=fakedata))+
  theme(legend.position = "bottom")

#States are capitalized here, but not in chloro so I can't join them together.
agg.dat<-agg.dat %>%
  mutate(region = tolower(State))
agg.dat 
chloro <- states %>% left_join(agg.dat, by="region")
chloro

#Plot5: Adds victim as a color scale for the states. 
#issue: bad color scheme, hard to tell difference
ggplot(chloro)+
  geom_polygon(aes(long,lat,group=group,fill=victims))+
  scale_fill_viridis_c(option="inferno")+
  theme(legend.position = "bottom")

#Plot6: Better looking color scheme
#Issues: Information is missing in certain states
ggplot(chloro)+geom_polygon(aes(long,lat,group=group,fill=victims))+
  scale_fill_gradient(low="lightyellow", high=muted("red"))+ 
  theme(legend.position = "bottom", legend.key.width=unit(1,"in"))+
  labs(title="US Shooting Victims 1966-2015")

#Plot7: Added latitude lines to make map more realistic. Looks much better!
#Issues: Same as before
ggplot(chloro)+geom_polygon(aes(long,lat,group=group,fill=victims))+
  scale_fill_gradient(low="lightyellow", high=muted("red"))+ 
  theme(legend.position = "bottom", legend.key.width=unit(1,"in"))+ 
  ggtitle("US Shooting Victims 1966-2015")+
  coord_map("albers", parameters = c(45.5,29.5))

                          
###############################################
#Practice with mapproj, scales libraries
head(us.cities)

#Capital information is shown in 1's and 0's. Clean by adding a new attribute as capital which actually takes all capitals. Alaska and Hawaii are not in the map, so I have to filter them out
cities <- us.cities %>%
  mutate(captial=capital>0)%>%
  filter(!(country.etc %in% c("AK","HI")))

#Plot 8: Charts instances of cities, with the color indicating if its a capital and the population indicated by size of point
#Issues: Hard to read, too many dimensions and cant tell the scale. 
ggplot()+
  geom_polygon(data=states, aes(long,lat,group=group),fill="white")+
  geom_point(data=cities,aes(long,lat, color=capital,size=pop), alpha=.3)

ggplot()+geom_col(aes(x=1,y=1), fill=rgb(1,0,0))
ggplot()+geom_col(aes(x=1,y=1), fill=rgb(.9,.6,.4,.7))

#Plot9: Better but not grea,better color scheme and such, but still a lot going on
ggplot()+
  geom_polygon(data=states, aes(long,lat,group=group),fill="white", color="tan")+
  geom_point(data=cities,aes(long,lat, color=capital,size=pop), alpha=.2)+
  scale_size_area(max_size=15)+
  scale_fill_gradient(low="lightyellow", high=muted("red"))+
  coord_map("mercator")

###############################################
#Information is too messy, practically unreadable (delimited).
#Loads Country data including continent, population, country code etc.
countries<-read_delim("countries.csv",delim=";")
countries

#Get rid of the formatting because I want to combine this dataset
colnames(countries)<-gsub("\\s","_", colnames(countries))
colnames(countries)<-gsub("[()]","", colnames(countries))

#Loading world geographic data and joining them together
world<-map_data("world")
View(world)
w.names<-tibble(world = unique(world$region),type="w")
w.names
c.names<-tibble(country = unique(countries$Country_en),type="c")
c.names
View(w.names %>% full_join(c.names))

#country codes aren't right so change them into working ones
countries$Country_en<-recode(countries$Country_en,"United States"="USA", "United Kingdom"="UK")

#join the world and countries data togehter. 
mapping<-world %>% left_join(countries, by=c("region"="Country_en"))
mapping

#Plot 10: Whole world map, with the life expectancy as the fill for each country. 
#No information in Antarctica. and a country in Africa. Not correct. 
ggplot(mapping)+
  geom_polygon(aes(long,lat, group=group, fill=Life_expectancy))+
  scale_fill_viridis_c(option="magma")
 
#Plot 11: Take out incorrect, data and to just portray as NA.
mapping<-mapping %>% mutate(Life_expectancy=na_if(Life_expectancy,0))
ggplot(mapping)+
  geom_polygon(aes(long,lat, group=group, fill=Life_expectancy))+
  scale_fill_viridis_c(option="magma")

#Practice with rwm
myISO3fun <- Vectorize(rwmGetISO3)
myISO3fun(c("United States", "United Kingdom"))

#Get all the country's country codes. 
countries <- countries %>% 
  mutate(ISO3 = myISO3fun(Country_en)) %>% 
  filter(Life_expectancy>0)
View(countries)

#Add the country codes to the world map information also
world_codes <- world %>% group_by(region) %>% summarise() %>%
  mutate(ISO3 = myISO3fun(region))

#Join the information all together
world_codes <- world_codes %>% inner_join(world)
final <- world_codes %>% left_join(countries,by="ISO3")

#Plot 12: World map with all the na information being gray. Can clearly see that Africa is not doing well in life expectancy 
ggplot(final)+
  geom_polygon(aes(long, lat, group=group, fill=Life_expectancy))+
  scale_fill_viridis_c(option = "magma") + 
  theme(legend.position = "bottom", legend.key.width = unit(1,"in"))+ 
  ggtitle("Life expectancy across the world")

