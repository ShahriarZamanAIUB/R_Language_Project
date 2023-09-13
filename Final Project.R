#A : We will use the rvest library for scraping or harvesting web data
#B : read_html() function will read the entire webpage and store it in a variable
#C : html_nodes() function will return the specific nodes/tags requested from the webpage
#D : html_text() will return the specific text in those returned tags


library(rvest) #A
web_page <- read_html("https://www.the-numbers.com/movie/budgets/all")  #B
movie_name<- html_nodes(web_page, "b a") #C
name<-html_text(movie_name) #D

 



name<-name[1:100] # cancelling out some unwanted scraped data

 

#library(rvest) #A
web_page <- read_html("https://www.the-numbers.com/movie/budgets/all") #B
movie_release_date<- html_nodes(web_page, "td:nth-child(2)") #C
release_date<-html_text(movie_release_date) #D

 


#library(rvest) #A
web_page <- read_html("https://www.the-numbers.com/movie/budgets/all") #B
movie_budget<- html_nodes(web_page, ".data:nth-child(4)") #C
budget<-html_text(movie_budget) #D




#library(rvest) #A
web_page <- read_html("https://www.the-numbers.com/movie/budgets/all") #B
movie_gross<- html_nodes(web_page, ".data:nth-child(5)") #C
gross<-html_text(movie_gross) #D


dataset1<-data.frame(name,release_date,budget,gross, stringsAsFactors = FALSE)
# storing all the vectors(name,release_date,budget and gross) into a single dataframe

# as of now we don't want numeric values of our string so we set stringsAsFactors = FALSE


View(dataset1)


#####################################################################################

#A : We will use the rvest library for scraping or harvesting web data
#B : read_html() function will read the entire webpage and store it in a variable
#C : html_nodes() function will return the specific nodes/tags requested from the webpage
#D : html_text() will return the specific text in those returned tags

library(rvest) #A 
web_page <- read_html("https://www.imdb.com/list/ls056349599/") #B
movie_name<- html_nodes(web_page, ".lister-item-header a") #C 
name<-html_text(movie_name) #D




#library(rvest) #A 
web_page <- read_html("https://www.imdb.com/list/ls056349599/") #B
movie_genre<- html_nodes(web_page, ".genre") #C 
genre<-html_text(movie_genre) #D



#library(rvest) #A 
web_page <- read_html("https://www.imdb.com/list/ls056349599/") #B
movie_rating<- html_nodes(web_page, ".ipl-rating-star.small .ipl-rating-star__rating") #C 
rating<-html_text(movie_rating) #D



dataset2<-data.frame(name,genre,rating, stringsAsFactors = FALSE)

# storing all the vectors(name,release_date,budget and gross) into a single dataframe

# as of now we don't want numeric values of our string so we set stringsAsFactors = FALSE

View(dataset2)


############date formatting

dataset1$release_date<-as.Date(dataset1$release_date,format="%b %d, %Y")



verdict_grader<- function(budget,gross){
  
  
  if(gross<budget){return("Flop")}
  
  else if(gross>=budget && gross<1.25*budget  ){return("Average")}
  
  else if(gross>=1.25*budget && gross<1.75*budget  ){return("Hit")}
  
  else if(gross>=1.75*budget && gross<2*budget  ){return("Superhit")}
  
  else if(gross>=2*budget){return("Blockbuster")}
  
  else{return("error")}
  
}

for(i in 1:100)
{
  
  dataset1$verdit[i]<-verdict_grader(dataset1$num_budget[i],dataset1$num_gross[i])
}

######hidden

dataset1$budget[24]=NA
dataset1$budget[81]=NA
dataset1$budget[83]=NA
########

library(sqldf)

dataset1<-sqldf("select * from dataset1 where name!='The Gray Man'")

#here we are using the sqldf library, using this we can manipulate our data like database tables

#Here we have passed a query which will select the rows from dataset1 where budget is not null/NA

View(dataset1)


write.csv(dataset1,"C:\\Users\\Asus\\Desktop\\Semester 9\\Introduction to Data Science\\Dataset1.csv" )

write.csv(dataset2,"C:\\Users\\Asus\\Desktop\\Semester 9\\Introduction to Data Science\\Dataset2.csv" )
 
library("stringr") 
for(i in 1:100){
  genre[i]<-str_remove_all(genre[i],"\\s+")
}


datasetx<-dataset1



library(stringr) 
dataset1 <- within(dataset1,{ 
   budget<- str_remove_all(budget,"\\s+") #removing whitespace
   budget<- str_remove_all(budget,",")    #removing comma
   budget<- str_remove_all(budget,"\\$")  #removing $ sign
   
   
   gross<- str_remove_all(gross,"\\s+") #removing whitespace
   gross<- str_remove_all(gross,",")    #removing comma
   gross<- str_remove_all(gross,"\\$")  #removing $ sign
   
   num_budget<-strtoi(budget) #converting budgets from char to int
   num_gross<-strtoi(gross)   #converting gross values from char to int
   
   release_date<-as.Date(release_date,format="%b %d, %Y") 
   #formatting the dates from %b %d, %Y to default format
   
   
   }) 

library(dplyr)
columns <- names(dataset2) %in% c("rating")  
dataset2 <- dataset2[!columns] 
dataset2$num_rating<-as.double(dataset2$rating)
columns <- names(dataset2) %in% c("rating")  
dataset2 <- dataset2[!columns] 
dataset2 <- rename(dataset2, rating = "num_rating") 

#first we are selecting the variable names to be deleted 
#then were deleting those columns entirely

 
 

###################################################################################
Animation,Adventure,Comedy

library(stringr) 
dataset2 <- within(dataset2,{ 
  
  genre<- str_remove_all(genre,"\\s+")
  
  genre[genre=='Action,Adventure,Comedy']<-"Action"
  
  genre[genre=='Action,Adventure,Sci-Fi']<-"Action"
  
  genre[genre=='Action,Comedy,Crime']<-"Action"
  
  genre[genre=='Action,Drama']<-"Action"
  
  genre[genre=='Animation,Adventure,Comedy']<-"Animation"
  
  genre[genre=='Animation,Action,Adventure']<-"Animation"
  
  genre[genre=='Animation,Adventure,Family']<-"Animation"
  
  genre[genre=='Adventure,Drama,Family']<-"Adventure"
  
  genre[genre=='Action,Adventure,Fantasy']<-"Adventure"
  
  genre[genre=='Action,Adventure']<-"Adventure"
  
  genre[genre=='Action,Adventure,Horror']<-"Adventure"
  
  genre[genre=='Adventure,Family,Fantasy']<-"Fantasy"
  
  genre[genre=='Adventure,Fantasy']<-"Fantasy"
  
  genre[genre=='Action,Adventure,Family']<-"Fantasy"
  
  genre[genre=='Action,Adventure,Thriller']<-"Thriller"
  
  genre[genre=='Drama,Romance']<-"Drama"
  
   
 
})  

View(dataset2$genre)






 
  agecat <- NA 
  agecat[age > 75] <- "Elder" 
 
  agecat[age >= 55 & age <= 75] <- "Middle Aged" 
  agecat[age < 55] <- "Young"  
  
  
  
  
  dataset2$genre[5]<-'Sci-Fi'
  dataset2$genre[22]<-'Sci-Fi'
  dataset2$genre[31]<-'Sci-Fi'
  dataset2$genre[37]<-'Sci-Fi'
  dataset2$genre[47]<-'Sci-Fi'
  dataset2$genre[59]<-'Sci-Fi'
  dataset2$genre[68]<-'Sci-Fi'
  dataset2$genre[69]<-'Sci-Fi'
  dataset2$genre[92]<-'Fantasy'
  
  dataset2$genre[c(74,15,66,12)]<-'Action'
  
  ########################################################
  
  datasetx<-dataset1
  datasetx$budget_millions<-round(dataset1$budget/1000000) 
  datasetx$gross_millions<-round(dataset1$gross/1000000) 
  
  View(datasetx)
  
  
  library(ggplot2)
  
  ggplot(data = datasetx, mapping = aes(x = budget_millions, y = gross_millions)) + geom_point()+ geom_smooth(formula = y ~ x,method = "lm")
  
  
  
  
  
  #######################################
   
  
  newdata2 <- subset(dataset2, name %in% dataset1$name,)
  
  newdata1 <- subset(dataset1, name %in% newdata2$name,)
  
  View(newdata1)
  View(newdata2)
  
  
  ###########################################
  
  write.csv(movies,"C:\\Users\\Asus\\Desktop\\Semester 9\\Introduction to Data Science\\movies.csv" )
  
  
  library(dplyr)
  movies <- within(movies,{ 
    
    budget<-round(budget/1000000)
    
    gross<-round(gross/1000000)
   })
  
  movies <- rename(movies, budget_millions = "budget", gross_millions = "gross")
  
  ########################################################################33
  
  mean(movies$rating)
  
  as.double("4.566")
  
  movies$num_rating<-as.double(movies$rating)
  
  movies[!rating]
  
  columns <- names(movies) %in% c("rating")  
  movies <- movies[!columns]
  
  movies <- rename(movies, rating = "num_rating") 
  
  ##################################################################3
  
  median(movies$rating)
  
  library(sqldf)
  
  df<-sqldf("select  genre,avg(rating) from movies group by genre")
  
  
  
  mode <- function(x) { 
    unique_values <- unique(x) 
    table <- tabulate(match(x, unique_values)) 
    unique_values[table ==max(table)] 
  } 
  
  values <- c(4,7,3,8,11,7,10,19,6,9,12,12) 
  
  mode(values) 
  View(movies)
  
  
  mean(movies$rating)
  mean(movies$gross_millions)
  mean(movies$budget_millions)
  
  median(movies$rating)
  median(movies$gross)
  median(movies$budget)
  
  mode(movies$rating)
  mode(movies$gross)
  mode(movies$budget)
  
  
  var(movies$rating)
  var(movies$gross_millions)
  var(movies$budget_millions)
  
  
  
  
  
  
  ######## making the mode function 
  
  mode <- function(x) { 
    unique_values <- unique(x) 
    table <- tabulate(match(x, unique_values)) 
    unique_values[table ==max(table)] 
  }
  ######## mean
  
  mean(movies$rating)
  mean(movies$gross_millions)
  mean(movies$budget_millions) 
  
  ######## median
  median(movies$rating)
  median(movies$gross_millions)
  median(movies$budget_millions)  
  
  ######## mode
  mode(movies$rating)
  mode(movies$gross_millions)
  mode(movies$budget_millions)  
  
  var(movies$rating)
  
  e<-movies$gross_millions
  
  var(dataset1$num_gross)
  
  
  var(c(60,55,50,65,59))
  
  
  
  library(ggplot2)
  
  ggplot(data = movies, mapping = aes(x = budget_millions, y = gross_millions)) + geom_point()+ geom_smooth(formula = y ~ x,method = "lm")
  
  
  var(movies$rating)
  sd(movies$gross_millions)
  sd(movies$budget_millions) 
  
  
  quantile(movies$rating)
  quantile(movies$gross_millions)
  quantile(movies$budget_millions)
  
  
  #######################################################
  
  
  
  plotdata <- movies %>% #1 
    group_by(genre) %>% 
    summarize(n=n(), 
              mean = mean(gross_millions), 
              se = sd(gross_millions)/sqrt(n)) 
  
  
  ggplot(plotdata, aes(x=reorder(genre, mean), y=mean)) + #2 
    geom_bar(stat="identity", fill="green") + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) + #3 
    labs(x="Genre", 
         y="Earning ($Millions)", 
         title = "Mean Domestic Earnings of Hollywood movies", 
         subtitle = "With standard error bars")
  