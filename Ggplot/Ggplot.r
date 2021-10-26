library(tidyverse)

?mpg
str(mpg)

ggplot(data=mpg)+geom_point(aes(x=displ, y=hwy))

ggplot()+geom_point(data=mpg, mapping=aes(x=displ,y=hwy))

ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+geom_point()

ggplot(data=mpg,mapping=aes(x=displ,y=hwy, label=manufacturer))+geom_point()+geom_text()

# ggplot is an object, and when you evaluate it plot
g<-ggplot(mpg,mapping=aes(displ,hwy)) 
g+geom_point()
g+geom_col()
g<-g+geom_point()
g
g+geom_col(alpha=.5)

#Other mappings
ggplot(data=mpg)+geom_point(aes(displ,hwy))
ggplot(data=mpg)+geom_point(aes(displ,hwy,color=class))
ggplot(data=mpg)+geom_point(aes(displ,hwy,alpha=class))
ggplot(data=mpg)+geom_point(aes(displ,hwy,size=class))
ggplot(data=mpg)+geom_point(aes(displ,hwy,shape=class))
ggplot(data=mpg)+geom_point(aes(displ,hwy,size=class,color=class,alpha=class))

#fixed Setting
ggplot(data=mpg)+geom_point(aes(displ,hwy),color="blue")
ggplot(data=mpg)+geom_point(aes(displ,hwy,color="blue"))#doesn't work bc it doesn't know the string blue as a color so it just uses the first one

ggplot()+geom_point(aes(x=1:10,y=runif(10)))
dim(mpg)
tmp<-1:dim(mpg)[1]
tmp
ggplot(data=mpg)+geom_point(aes(displ,hwy,color=tmp))
ggplot(mpg)+geom_point(aes(displ,hwy,color=class))

#Other geoms
ggplot(mpg)+geom_smooth(aes(displ,hwy))

ggplot(mpg,aes(displ,hwy))+geom_smooth()+geom_point(aes(color=class))
ggsave("lab2_plot2.pdf")

ggplot(mpg,aes(displ,hwy))+
  geom_smooth(aes(linetype=class))+
  geom_point(aes(color=class))

#Statistical Transformations
ggplot(data=mpg)+geom_bar(aes(x=class))

ggplot(data=mpg)+stat_count(aes(x=class))
d2a
d2a<- group_by(mpg,class)                                       
d2a<- summarise(d2a,count=n())
d2a
ggplot(d2a)+geom_col(aes(class,count))

#How to sort a Barchart

d2a
d2a<-d2a[order(d2a$count),]
d2a

ggplot(d2a)+geom_col(aes(class,count)) #is still the same chart despite #'s being flipped
?factor
d2a$class
str(d2a)

d2a$class<-factor(d2a$class,levels=d2a$class)
d2a
str(d2a)
ggplot(d2a)+geom_bar(aes(class))

#ordering the data
data<-tibble(category = letters[1:10],value=runif(10,0,100))
ggplot(data)+geom_col(aes(category,value))
data<-data[order(-data$value),]
data$category<-factor(data$category, data$category)
ggplot(data)+geom_col(aes(category,value))

#Next class
ggplot(mpg)+stat_count(aes(x=class,fill=trans))
class<-d2a$class


sortedclass<-factor(mpg$class,d2a$class)
ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans))

 
library(RColorBrewer)
display.brewer.all()

ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans))+scale_fill_brewer(palette="Set3")
ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans))+scale_fill_manual(values=colorlist)

colors()
colorlist<-sample(colors(),length(unique(mpg$trans)))
colorlist

ggplot(mpg)+geom_point(aes(displ,hwy,color=class),size=5)+scale_fill_virdis_d(option="magma")
ggplot(mpg)+geom_point(aes(displ,hwy,color=class),size=5)+scale_color_viridis_d(option="magma")
ggplot(mpg)+geom_point(aes(displ,hwy,color=class),size=5)+scale_color_viridis_d(option="magma")+scale_x_reverse()
ggplot(mpg)+geom_point(aes(displ,hwy,color=class),size=5)+scale_color_viridis_d(option="magma")+scale_x_reverse()+scale_y_log10()
ggplot(mpg)+geom_point(aes(displ,hwy,color=class),size=5)+scale_color_viridis_d(option="magma")+scale_x_reverse()+scale_x_continuous(breaks=c(1,3,5,7),labels=c("tiny","bigger","still bigger","huge"))+scale_y_log10

#other adjustments
ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans), position = position_dodge())

ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans), position = position_dodge(preserve="single"))+scale_fill_viridis_d()+theme(legend.position = "none")

#faceting
ggplot(mpg)+geom_point(aes(displ,hwy),size=4)+ggtitle("Highway MPG x Dsiplacement")+facet_wrap(~trans)+theme(legend.position="none")

ggplot(mpg)+geom_point(aes(displ,hwy,color=trans),size=4)+scale_color_viridis_d(option="magma")+ggtitle("Highway MPG x Dsiplacement")+facet_wrap(~trans,nrow=2)+theme(legend.position="none")

ggplot(mpg)+geom_point(aes(displ,hwy,color=trans),size=1)+scale_color_viridis_d(option="magma")+ggtitle("Highway MPG x Dsiplacement")+facet_grid(cyl~trans)+theme(legend.position="none")

#theming
ggplot(mpg)+geom_point(aes(displ,hwy))+theme(axis.title=element_text(size = 20),axis.line.x=element_line(size=1,arrow=arrow()),axis.line.y=element_line(size=2))
ggplot(mpg)+stat_count(aes(x=sortedclass,fill=trans), position = position_dodge(preserve="single"))+scale_fill_viridis_d()+theme(legend.position = "none")
ggplot(mpg)+geom_bar(aes(manufacturer))
ggplot(mpg)+geom_bar(aes(manufacturer))+theme(axis.text.x=element_text(angle=45,hjust=1))

mpg
view(mpg)
table(mpg$manufacturer)
names(sort(table(mpg$manufacturer)))
mf_order<-names(sort(table(mpg$manufacturer)))
mf_order
mpg$manufacturer<-factor(mpg$manufacturer,mf_order)
chart
ggplot(mpg)+geom_bar(aes(manufacturer))+theme(axis.text.x=element_text(angle=45,hjust=1))+coord_flip()

ggplot(data=mpg)+geom_bar(aes(x=displ, y=hwy))
