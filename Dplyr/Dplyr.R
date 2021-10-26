library(tidyverse)
flights<-read_csv("nycflights.csv")
flights

##################### Filtering
View(flights) #looks at the data
filter(flights, month==1, day==1 )#only shows flights on jan1
view(filter(flights, month==1, day==1))
jan1<-filter(flights, month==1, day==1)#must use == bc it's numbers
dec25<-filter(flights, month==12, day==25)#prints out all the flights in one line

# And&, Not!, Or|
view(filter(flights, month==11 & month==12))
nov_dec<-filter(flights,month %in% c(11,12))
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#NA's: contagious
NA * 5
is.na(NA)
df<-tibble( x= c(1,NA,3))
df
filter(df,x>1)
filter(df,x>1 | is.na(x))
filter(df, !is.na(x))

#had an arrival delay of two or more hours
view(filter(flights, arr_delay>=120))
#Were operated by United, American,or Delta
view(filter(flights, carrier=="UA" | carrier=="AA" | carrier=="DL"))
#were delyaed by at least an hour, but made up over 30 minutes in flight
view(filter(flights, dep_delay>=60 & arr_delay <= dep_delay-30))


####################### Arrange - changing the order of rows
view(arrange(flights, year,month,day))
view(arrange(flights, year, month, desc(day)))
df <- tibble (df,x)
arrange(df,desc(x))
arrange(flights,air_time)

########################Select - select columns you want
select(flights,year,month,day)
str(flights)
select(flights,year:dep_delay)
select(flights,year:day,dep_delay)
select(flights,-(year:day))
select(flights,year:day,contains("dep"))
select(flights,time_hour,air_time,everything())
rename(flights,tail_num = tailnum)

test<-flights
colnames(test)
colnames(test)[4]<-"DEPTIME"
test

########################Mutate- Adding a column
flights_sml<-select(flights,year:day,ends_with("delay"),distance,air_time)
flights
flights_sml
mutate(flights_sml, gain = dep_delay - arr_delay, 
       hours = air_time / 60, 
       gain_per_hour = gain/hours)

ifelse(c(1,2,3) < c(1,1,10) , "Smaller", "Bigger")
ifelse(c(1,2,3) < 2 , "Smaller", "Bigger")
mutate(flights_sml, logged_delay=log2(dep_delay))
mutate(flights_sml, sqrt_delay=sqrt(dep_delay))
mutate(flights_sml, sqr_delay=dep_delay^2)
mutate(flights_sml, sum_delay=sum(dep_delay))

#########################Lead and lag
x <- 1:10
x
lag(x)#shoves everything forward 1
lag(x,default=0)
x<-tibble(time=sample(1:1000,100,replace=T),event=sample(letters[1:3],100,replace=T))
x<-arrange(x,time)
x

mutate(x,delay = time - lag(time,default=0))

#another good one - cumsum, cumprod, cummin,cummax
x<-sample(1:1000)
cumsum(x)
cummin(x)
cumprod(x)

x<-sample(1:100,200,replace=T)
min_rank(sort(x))
dense_rank(sort(x))
sort(x)

#Mutate exercise
ifelse(TRUE==FALSE,"what?","ok then")

#use ifelse to create a column called departure_precision filled with values "on time" ,late, early based on the vales in the departure delay column
mutate(flights,departure_precision=ifelse(dep_delay==0,"On time",ifelse(dep_delay>0,"late","early")))

######################summarize
summarise(flights,delay = mean(dep_delay,na.rm=T))
by_day<-group_by(flights,year, month,day)
by_day

#Looking for average delay by day
result<-summarise(by_day,delay=mean(dep_delay,na.rm=T))
result

#Looking for average delay by destination
by_dest<-group_by(flights,dest)
delay<-summarise(by_dest,count=n(),dist = mean(distance,na.rm=T), 
                 delay=mean(arr_delay,na.rm=T))
delay
busy_delay<-filter(delay,count>20,dest!="HNL")
busy_delay

#Plot to see delay by distance with a line showing a rough pattern
ggplot(data=busy_delay,mapping=aes(x=dist,y=delay))+geom_point(aes(size=count),alpha=1/3)+geom_smooth(se=FALSE)

#Adding pipes to clean up
delays<-flights %>%
  group_by(dest) %>%
  summarise(count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=T)) %>%
  filter(count>20,dest!="HNL")

ggplot(data=delay,mapping=aes(x=dist,y=delay))+geom_point(aes(size=count),alpha=1/3)+geom_smooth(se=FALSE)


flights%>%
  group_by(year,month,day)%>%
  summarise(mean=mean(dep_delay, na.rm=T))

#gets rid of the NA's
flights%>%
  group_by(year,month,day)%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))%>%
  summarise(mean=mean(dep_delay))

#Finding where the majority of delays are. 
not_cancelled<-flights%>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
delays<-not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay=mean(arr_delay,na.rm=T),n=n())

#Shown with a line
ggplot(delays,mapping = aes (x = delay)) +geom_freqpoly(binwidth=10)            
#shown with points
ggplot(delays,mapping=aes(x=n,y=delay))+geom_point(alpha=1/10)

#number of delays
delays %>%
  filter(n>50) %>%
  ggplot(mapping = aes (x=n,y=delay))+
  geom_point(alpha=1/10)

not_cancelled %>%
  group_by(year,month,day)%>%
  summarise(avg_delay1=mean(arr_delay),avg_delay2=mean(arr_delay[arr_delay>0]))

not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance),distance_med=median(distance))%>%
  arrange(desc(distance_sd)) %>%
  ggplot(aes(distance_med,distance_sd))+geom_point()
not_cancelled %>% group_by(year,month,day)%>% summarise(n_early=sum(dep_time<500))

early<-not_cancelled%>%
  group_by(year,month,day)%>%
  summarise(n_early=sum(dep_time<500))%>%
  mutate(percent_early=n_early/max(n_early))


early %>% ungroup()

#Relationship between delay and cancel rate. Makes sense as delay increases, cancel rate also follows
flights %>% 
  group_by(year,month,day)%>% 
  summarise(cancel_rate = mean(is.na(arr_delay) | is.na(dep_delay)),
            mean_delay = mean(arr_delay + dep_delay, na.rm = T))%>% 
  ggplot(mapping=aes(mean_delay,cancel_rate))+
  geom_point()
