library(tidyverse)
# Exercise 1:   Let's build a chloropleth of county level data for all crimes, all ages in 2009
# Read the data
data<-read_delim("./ICPSR_30763/DS0001/30763-0001-Data.tsv",delim = "\t")
unique(data$STUDYNO)
unique(data$FIPS_ST)

view(data)

# Read the FIPS table
fips <- read_csv("county_fips_master.csv")
View(fips)
# Let's generate the key
data <- data %>%
  mutate(fips = FIPS_ST*1000 + FIPS_CTY)

# Check it out
View(data)

# Let's look at the non-matches
View(fips %>% 
       select(fips) %>% 
       full_join(data %>% select(fips),keep=T) %>% 
       filter(is.na(fips.x+fips.y)))

#Check it out
View(fips)

# Can't really figure out what's up?  Let's not worry, we'll just keep what matches
View(data %>% 
       filter(FIPS_CTY==777))

# Join everything up
data<- data %>% 
  inner_join(fips)
View(data)

# Ok now, let's get the county data
counties<-map_data("county")

# Let check out some of the counties that have weird names
View(fips %>% filter(state_name== "Maryland"))
View(counties %>% group_by(region,subregion) %>% summarise %>% filter(region=="maryland"))
View(counties %>% group_by(region,subregion) %>% summarise %>% filter(region=="louisiana"))

# So, we see that the last word (county / parish) is removed, spaces are retained, but all punctuation removed
unique(data$county_name)

# We will use extract to extract part of the string into a new column
# (see https://tidyr.tidyverse.org/reference/extract.html)
data <- data %>% 
  extract(county_name,c("county_name_simplified"),"(.*)\\s[^\\s]+$",remove = F) %>%
  mutate(county_name_simplified = tolower(str_remove_all(county_name_simplified,"['.,]")),
         state_name = tolower(state_name))

# Let's check out the error?
data[1798,]$county_name  #Hmm.  Let's not worry about that one right now.  But remember to fix it up later

# Let's check and see how we did
just_regions <- counties %>% 
  group_by(region,subregion) %>% 
  summarise()

View(data %>% 
       select(state_name,county_name_simplified) %>%
       full_join(just_regions,by=c("state_name"="region",
                                   "county_name_simplified"="subregion")
                 ,keep=T) %>% filter(!complete.cases(.)))

# Ok, this is a good diagnostic.  We see a problem with words starting with "de" for some reason.  Maybe different
# regional spellings?  Let's figure it out later.  But I'm not sure what's up with VA?

View(just_regions %>% filter(region=="virginia"))

# Why so many missing counties?  Let's see if we can find one
View(just_regions %>% filter(subregion=="winchester"))

# Googling around (https://en.wikipedia.org/wiki/Winchester,_Virginia), it seems winchester is a city, not a county!  Well, we'll have to do something about that later.  For now, let's just move on.

plot_data <- counties %>% 
  left_join(data, by=c("region"="state_name",
                       "subregion"="county_name_simplified"))

ggplot(plot_data) + 
  geom_polygon(aes(long,lat,fill=GRNDTOT,group=group))+
  scale_fill_viridis_c()

# Turn off scientific notation!!
options(scipen=999)
ggplot(plot_data) + 
  geom_polygon(aes(long,lat,fill=GRNDTOT,group=group))+
  scale_fill_viridis_c(trans="log10")

View(data %>% filter(state_name=="florida")) #no Florida data

############## NOT WORKING ################ EXERCISE 2 (you do it):  Do the same thing for just adults & juveniles.  Compare the two maps using facet_wrap.  If you want to see them on the same scale, use "bind_rows"
view(data)
View(plot_data)

adult_data <- data %>% mutate(type="adult")
jv_data <- data %>% mutate(type="jv")

str(data)
plot_data <- bind_rows(adult_data, jv_data)
View(plot_data)

ggplot(plot_data) +
  geom_polygon(aes(long,lat,fill=GRNDTOT, group=group))+
  scale_fill_viridis_c(trans="log10")+ 
  facet_wrap(~type)


###############################################################
# EXERCISE 3:  Let's just compare crimes for the interesting ones.  First, let's look at
View(data %>% 
       select(MURDER:RUNAWAY) %>% 
       summarise_all(sum) %>% 
       pivot_longer(everything(),names_to="type",values_to="count")%>% 
       arrange(-count))

# Ok, I'm interested to see what the relationship is between MURDER, ARSON, DRGPOSS, DUI
# Note, that if we want to see these things in a facet_wrap, we need to pivot_longer
sub_data <- data %>% 
  select(state_name,county_name_simplified,MURDER,ARSON,DRGPOSS,DUI) %>% 
  pivot_longer(MURDER:DUI,names_to="type",values_to="count")

plot_data <- counties %>% 
  left_join(sub_data, by=c("region"="state_name","subregion"="county_name_simplified"))

ggplot(plot_data) + 
  geom_polygon(aes(long,lat,fill=count,group=group))+
  scale_fill_viridis_c()+
  facet_wrap(~type)
# again, log scale, and let's get rid of the NAs

View(plot_data %>% filter(is.na(type)))

# Ok, this is underlying matching errors.  We'll have to go back and fix this up
ggplot(plot_data %>% filter(!is.na(type))) + 
  geom_polygon(aes(long,lat,fill=count,group=group))+
  scale_fill_viridis_c(trans="log10")+facet_wrap(~type)

# seems like pretty good correlations.  We'll test this next

###############################################################
# EXERCISE 5: Let's use a simple scatter to look at correlations by county with DUI

View(data)
sub_data <- data %>% 
  select(state_name,county_name_simplified,MURDER,ARSON,DRGPOSS,DUI) %>%
  pivot_longer(MURDER:DRGPOSS,names_to="type",values_to="count")
View(sub_data)
#plot_data <- counties %>% left_join(sub_data, by=c("region"="state_name","subregion"="county_name_simplified"))

ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count))+facet_wrap(~type)

# Ok, very hard to see because of scaling.  Let's see if we can fix!
ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count))+facet_wrap(~type,scales="free_y")

# There is a pretty good correlation after all!  How about we log scale to reduce the clustering?
ggplot(sub_data %>% filter(!is.na(type))) + 
  geom_point(aes(DUI,count),alpha=.3)+facet_wrap(~type,scales="free_y")+
  scale_x_log10()+
  scale_y_log10()

#Interesting!  Much stronger correlation at the low end

###############################################################
# EXERCISE 6 (you do it): Choose a different baseline (instead of DUI) and look at correlations
rob_data <- data %>% 
  select(state_name,county_name_simplified,ROBBER,MURDER,AGASSLT,LARCENY,GAMBLE) %>%
  pivot_longer(ROBBERY:LARCENY, names_to = "type", values_to = "count")

ggplot(rob_data %>% filter(!is.na(type)))+ 
  geom_point(aes(GAMBLE,count))+
  facet_wrap(~type,scales="free_y")


################### Copy #################

# EXERCISE 6 (you do it): Choose a different baseline (instead of DUI) and look at correlations
#Seeing if theres a correlation between Gambling and violation and theft
rob_data <- data %>% 
  select(state_name, county_name_simplified, ROBBERY, 
         BURGLRY, MVTHEFT, MURDER,AGASSLT, LARCENY, GAMBLE) %>%
  pivot_longer(ROBBERY:LARCENY, names_to = "type", values_to = "count")

ggplot(rob_data %>% filter(!is.na(type))) + 
  geom_point(aes(GAMBLE,count))+
  facet_wrap(~type,scales="free_y")+
  scale_x_log10()+
  scale_y_log10()

# EXERCISE 7 (let's look at a heatmap of all crimes by state)
names(data)
sub_data <- data %>% select(state_name,MURDER:RUNAWAY) %>% group_by(state_name) %>% summarise_all(sum) 
View(sub_data)

# We want to order by totals on both sides, so lets get our factor levels
state_order <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count") %>% 
  group_by(state_name) %>% 
  summarise(total = sum(count)) %>% 
  arrange(total)
state_order

crime_order <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count")%>% 
  group_by(type)%>% 
  summarise(total = sum(count))%>% 
  arrange(total)
crime_order

# Ok, apply factor levels and plot
sub_data <- sub_data %>% 
  pivot_longer(-state_name, names_to="type", values_to = "count") %>%
  mutate(state_name = factor(state_name,state_order$state_name),
         type = factor(type,crime_order$type))

ggplot(sub_data) + 
  geom_tile(aes(state_name,type,fill = count))+
  scale_fill_viridis_c()

# Meant to screen those out
ggplot(sub_data %>% filter(!(type %in% c("ALLOTHR","DRUGTOT"))))+ 
  geom_tile(aes(state_name,type,fill = count))+
  scale_fill_viridis_c()

ggplot(sub_data %>% 
         filter(!(type %in% c("ALLOTHR","DRUGTOT"))))+ 
  geom_tile(aes(state_name,type,fill = count))+
  scale_fill_viridis_c(trans="log10")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

# Not seeing a lot of correlation?  Let's try scaling by total number of crimes per state

sub_data <- sub_data %>% 
  filter(!(type %in% c("ALLOTHR","DRUGTOT"))) %>% 
  group_by(state_name) %>% 
  mutate(scaled_count = count / sum(count,na.rm = T))

ggplot(sub_data %>% 
         filter(!(type %in% c("ALLOTHR","DRUGTOT")))) + 
  geom_tile(aes(state_name,type,fill = scaled_count)) +
  scale_fill_viridis_c(trans="log10")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

###############################################
#Extra Credit: Violin chart representing the proportion of the top 10 crimes for each state
#STEPS:
#1 Filtering out all the the top two as they aren't specific crimes.
#2 Filter just the top 10 crimes
#3 Find total number of crimes per state
#4 Take count per type then divide by total number of crime
#5 Totals per crime
#6 Plot
View(sub_data)

#1 + 2
ec <- sub_data %>%
  filter(type != "ALLOTHR")%>%
  filter(type != "DRUGTOT") %>%
  arrange(desc(type), desc(count))%>%
  slice(1:510)
ec
#3
ec <- ec %>%
  group_by(state_name) %>%
  mutate(`Total in State` = sum(count))
ec
#4
ec<- ec %>%
  mutate(Scaled_count=count/`Total in State`)
ec
#5
ec <- ec %>% 
  group_by(type) %>%
  mutate(`Total per Crime`= sum(count))
ec
#6
ggplot(ec, aes(x=type, y=Scaled_count, fill= `Total per Crime`))+ 
  scale_fill_viridis_c()+
  geom_violin(scale="width")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        text = element_text(size=8))+
  ggtitle("Relative Proportion of top 10 adult crimes by state (colored by total count), 2009", )
  

