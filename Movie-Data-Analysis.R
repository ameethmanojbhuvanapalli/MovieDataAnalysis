library(plotrix)
library(dplyr)
#reading csv file
indian_movies = read.csv("indian movies 1949-2021.csv")
#cleaning the data set
indian_movies_clean = indian_movies[indian_movies$Rating.10. != '-' & indian_movies$Genre != '-',]
#taking the data of the year 2021
indian_movies_2021 = indian_movies_clean[indian_movies_clean$Year == "2021",]
#counting the occurrence of each language
#this is imported from dplyr library
language_count = count(indian_movies_clean,Language)
language_count_2021 = count(indian_movies_2021,Language)
#No.of movies in each language
png(file = "No.of Movies.png")
barplot(language_count[['n']],names.arg = language_count[['Language']],main = "No.of Movies in each Language",xlab = "Languages",
        ylab = "No.of Movies",col = rainbow(19),ylim = c(0,8000))
legend("topright",legend = language_count[['Language']],cex = 0.4,fill = rainbow((nrow(language_count))))
dev.off()
#No.of movies based on language of 2021
png(file = "No.of Movies in 2021.png")
pie(language_count_2021[['n']],labels = language_count_2021[['n']],radius = 1,main = "No of movies in 2021",
    col=rainbow(nrow(language_count_2021)))
legend(x=-1.5,y=1,legend = language_count_2021[['Language']],cex = 0.4,fill = rainbow((nrow(language_count_2021))))
dev.off()
#taking movies rated above 9 
movies_rated = subset(indian_movies_clean, indian_movies_clean$Rating.10 >= 9.0) 
language_count_rated = count(movies_rated,Language)
#No.of movies rated above 9
png("hist.png")
hist(as.numeric(movies_rated$Rating.10.),breaks = 10,main = "No.of Movies rated above 9",xlab = "Rating",ylab="No of movies",
     col = "orange",xlim = c(9,10),ylim = c(0,150))
dev.off()
#No.of movies released in past 6 years
hf1 = subset(indian_movies,as.numeric(indian_movies$Year)>2015)
png("No of movies in last 6 years.png")
hist(as.numeric(hf1$Year),xlab = "Year",ylab = "No.of Movies",main = "No.of Movies in last 6 years",
     col = c("skyblue","cyan"),breaks = 10, xlim = c(2016,2022),ylim = c(0,3500))
dev.off()
#adding a popularity column
#(According to IMDB)
#pop = (v/(v+m))*r + (m/(v+m))*c  
#v = no of votes,m = min no of votes
#r = Rating of movie,c = mean of Rating
min = 5000
mean_rating = mean(as.numeric(indian_movies_2021$Rating.10.))
indian_movies_2021$pop = (as.numeric(indian_movies_2021$Votes)/(as.numeric(indian_movies_2021$Votes)+min))*
  (as.numeric(indian_movies_2021$Rating.10.)) + ((min/(as.numeric(indian_movies_2021$Votes)+min))*mean_rating)
#most popular movies in 2021
#sorting of data based on popularity in decreasing order
df = indian_movies_2021[order(indian_movies_2021$pop,decreasing=TRUE),]
ndf = subset(df,as.numeric(df$Votes)>min)
top_movies = head(ndf)
most_popular_2021 = subset(indian_movies_2021,indian_movies_2021$pop == max(pop))
#most popular industry in 2021
lan = language_count_2021[['Language']]
lan_list = split(indian_movies_2021, f = indian_movies_2021$Language)
sum_list = list()
#user defined function to calculate the sum of pop column of each language
my_function = function(da)
{
  temp = dplyr::select(.data = da, dplyr::all_of('pop'))
  x = sum(temp[,'pop'])
  return(x)
}
sum_list[1] = my_function(lan_list$assamese)
sum_list[2] = my_function(lan_list$bengali)
sum_list[3] = my_function(lan_list$hindi)
sum_list[4] = my_function(lan_list$kannada)
sum_list[5] = my_function(lan_list$malayalam)
sum_list[6] = my_function(lan_list$marathi)
sum_list[7] = my_function(lan_list$nepali)
sum_list[8] = my_function(lan_list$oriya)
sum_list[9] = my_function(lan_list$punjabi)
sum_list[10] = my_function(lan_list$rajastani)
sum_list[11] = my_function(lan_list$tamil)
sum_list[12] = my_function(lan_list$telugu)
sum_list[13] = my_function(lan_list$urdu)
#converting the list obtained to a dataframe
sum_data = as.data.frame(sum_list,col.names = lan,row.names = "pop")
#taking the transpose of dataframe
sum_data_transpose = as.data.frame(t(sum_data))
#arranging the data in descending order
#arrange function can be used by importing from dplyr
df2 = arrange(sum_data_transpose,desc(sum_data_transpose['pop']))
#taking the top industries
top = head(df2)
#Most popular film industry of india in 2021
png(file = "Most Popular Film Industry of India in 2021.png")
barplot((top$pop),main = "Most Popular Film Industry in 2021",names.arg = row.names(top),xlab = "Industry",
        ylab = "Popularity",col = "red",ylim = c(0,500))
dev.off()
#Analysis of the movie with respect to ratings and votes
png("Scattrtplot.png")
plot(x = as.numeric(ndf$Rating.10.),y = as.numeric(ndf$Votes)/1000,
     xlab = "Rating",
     ylab = "Votes x 1000",
     ylim = c(0,55),
     xlim = c(6,10),
     main = "Rating vs Votes of most popular movies in 2021",
     col = "darkgreen",
     pch = 19,
     las = 1
)
text(as.numeric(ndf$Rating.10.),as.numeric(ndf$Votes)/1000,labels = ndf$Movie.Name,pos=3,
     col = "darkred",cex = 0.8)
dev.off()
#reading the next csv file
demo=read.csv("indianmovies3.csv")
print(demo)
#displayingdata
head(demo)
tail(demo)
View(demo)
str(demo)
#statistical analysis on Ratings
lowest_rated=subset(demo,demo$Rating.10.==min(Rating.10.))
highest_rated=subset(demo,demo$Rating.10.==max(Rating.10.))
print(range(demo$Rating.10.))
print(mean(demo$Rating.10.))
#6 best movies based on votes
piedata<-data.frame(as.numeric(demo$Votes),demo$Movie.Name)
colnames(piedata) = c("Votes","Movie")
piedata1 = piedata[order(as.numeric(piedata$Votes),decreasing = TRUE),]
newpiedata<-head(piedata1)
png(file="MovieVotes.jpg")
pie3D(newpiedata$Votes,labels = newpiedata$Votes,explode = 0.05,col = terrain.colors(6),
      main = "6 Best Movies Based on Votes")
legend(x=0.65,y=0.95,legend = newpiedata$Movie,cex=0.8,fill = terrain.colors(6))
dev.off()
#Top movies with highest votes
demo$Year_1<- as.numeric(demo$Year)
demo$Votes_1<- as.numeric(demo$Votes)
linedf <-  demo[order(demo$Votes_1,decreasing=TRUE),]
newlinedf<- head(linedf)
plot(x = as.numeric(newlinedf$Votes_1),y = newlinedf$Year_1,type="o",ylim = c(1980,2022),xlim = c(5000,60000),
     xlab = "Votes",ylab = "Year",col="navy",main = "Year vs Votes",las=1,pch=19)
text(as.numeric(newlinedf$Votes_1),newlinedf$Year_1,labels = newlinedf$Movie.Name,cex=0.8,col="red",pos = 3)