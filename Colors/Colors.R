library(tidyverse)
library(RColorBrewer)
library(scales)
library(ggrepel)

display.brewer.all()

rand.data <- tibble(name=rep(letters[1:8], 35), val=rnorm(35*8, 35,sd=1.5))
rand.data

ggplot(rand.data)+
  geom_boxplot(aes(val, fill=name))+
  coord_flip()+
  scale_fill_brewer(palette = "Set3")

#1
num.colors <- 8
FUN <- colorRampPalette(c("blue","red"))
FUN(10)

my.cols <- FUN(num.colors)
ggplot(rand.data)+
  geom_boxplot(aes(val, fill=name))+
  coord_flip()+
  scale_fill_manual(values = my.cols)

#2
ggplot(rand.data)+
  geom_boxplot(aes(val, fill=name))+
  coord_flip()+scale_fill_grey()

ggplot(rand.data)+
  geom_boxplot(aes(val, group=name, fill=as.numeric(as.factor(name))))+
  coord_flip()+
  scale_fill_gradient("name", guide='legend', breaks=1:8, low="blue", high="red")

##continuous color palette
ndata = 1000
rand.data<- tibble(altitude = runif(ndata, 30,50000), 
                   temperature = runif(ndata,-10,120),
                   pain= altitude+(temperature-20)^20 + 
                     temperature*altitude) %>%
  mutate(pain=floor(rescale(pain,c(0,10))))

ggplot(rand.data)+
  geom_point(aes(temperature,altitude,color=pain))+
  scale_color_viridis_c()

ggplot(rand.data)+
  geom_point(aes(temperature,altitude,color=pain))+
  scale_color_viridis_c(breaks = 1:5*2, trans="log")

ggplot(rand.data)+geom_point(aes(temperature,altitude,color=pain+1))+
  scale_color_viridis_c(breaks = 1:5*2, trans="log")

ggplot(rand.data)+
  geom_point(aes(temperature,altitude,color=pain+1, alpha=.3, size=3))+
  scale_color_viridis_c(breaks = 1:5*2, trans="log")

########
sales<-read_csv("sales(1).csv")

ggplot(sales)+geom_point(aes(expenses,recipt))

ggplot(sales)+geom_point(aes(expenses,recipt, color=rep.sex==0))

ggplot(sales)+
  geom_point(aes(expenses,recipt, color=rep.sex==0))+ 
  scale_color_discrete("Gender")

ggplot(sales)+geom_point(aes(expenses,recipt, color=type)) + scale_color_discrete("Wine")

ggplot(sales)+geom_point(aes(expenses,recipt, color=unit.price)) + scale_color_viridis_c()

ggplot(sales)+geom_point(aes(expenses,recipt, color=unit.price>20))

ggplot(sales)+geom_point(aes(expenses,recipt, color=unit.price>10))

ggplot(sales)+
  geom_point(aes(expenses,recipt, color=unit.price>15), size=1, alpha=.5)

x<-read_delim("countries.csv", delim=";")

x.s <- x[sample(1:nrow(x),20,F),]

x.s

x.s <- x.s %>% mutate(`Population (millions)` = Population / 10^6)

ggplot(x.s,aes(Birthrate, `Population (millions)`))+
  geom_point(aes(color = `Country (en)`, size=4))+ 
  scale_y_log10()+geom_text(aes(label=`Country (en)`))+
  theme(legend.position = "none")

ggplot(x.s,aes(Birthrate, `Population (millions)`))+
  geom_point(aes(color = `Country (en)`, size=4))+ 
  scale_y_log10()+
  geom_text_repel(aes(label=`Country (en)`))+
  theme(legend.position = "none")

mpg %>%
  ggplot(aes(x=displ,y=hwy))+ geom_point()
