rm(list = ls())
library(class)
library(MASS)
library(e1071)
setwd("/Users/sireeshapandeti/Documents/Rfiles")
getwd()

##########
library(rjson)
library(plyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(glmnet)
library(googleVis)
library(DT)
library(scales)
###############
con <- file("yelp_academic_dataset_business.json", "r")
input <- readLines(con, -1L)
close(con)
#########
yelpdata <- input %>%
  lapply(function(x) t(unlist(fromJSON(x)))) %>% 
  ldply()
save(yelpdata, file= 'yelpdata.rdata')
load("yelpdata.rdata")
##################
clean.names <- function(df){
  colnames(df) <- gsub("[^[:alnum:]]", "", colnames(df))
  colnames(df) <- tolower(colnames(df))
  return(df)
}
yelpdata <- clean.names(yelpdata)
yelpdata <- yelpdata[,!duplicated(colnames(yelpdata))]

# Features 
yelpdata$stars <- as.numeric(as.character(yelpdata$stars))
yelpdata$reviewcount <- as.numeric(as.character(yelpdata$reviewcount))
names(yelpdata)[names(yelpdata)=="attributeshappyhour"] <- "happyhour"
names(yelpdata)[names(yelpdata)=="attributesacceptscreditcards"] <- "acc"
names(yelpdata)[names(yelpdata)=="attributesgoodforgroups"] <- "groups"
names(yelpdata)[names(yelpdata)=="attributesoutdoorseating"] <- "outdoor"
names(yelpdata)[names(yelpdata)=="attributespricerange"] <- "price"
names(yelpdata)[names(yelpdata)=="attributesalcohol"] <- "alcohol"
names(yelpdata)[names(yelpdata)=="attributesnoiselevel"] <- "noiselevel"
names(yelpdata)[names(yelpdata)=="attributesambienceclassy"] <- "classy"
names(yelpdata)[names(yelpdata)=="attributesparkingvalet"] <- "valet"
names(yelpdata)[names(yelpdata)=="neighborhoods"] <- "nhood"
names(yelpdata)[names(yelpdata)=="attributesdrivethru"] <- "drivethru"
names(yelpdata)[names(yelpdata)=="attributesparkinglot"] <- "parkinglot"
names(yelpdata)[names(yelpdata)=="attributesparkinglot"] <- "parkinglot"
names(yelpdata)[names(yelpdata)=="attributespaymenttypescashonly"] <- "cash"
names(yelpdata)[names(yelpdata)=="attributesambiencecasual"] <- "casual"
names(yelpdata)[names(yelpdata)=="attributesgoodfordancing"] <- "dance"
names(yelpdata)[names(yelpdata)=="attributesdelivery"] <- "delivery"
names(yelpdata)[names(yelpdata)=="attributescoatcheck"] <- "ccheck"
names(yelpdata)[names(yelpdata)=="attributestakeout"] <- "takeout"
names(yelpdata)[names(yelpdata)=="attributestakesreservations"] <- "res"
names(yelpdata)[names(yelpdata)=="attributeswaiterservice"] <- "service"
names(yelpdata)[names(yelpdata)=="attributesparkingstreet"] <- "street"
names(yelpdata)[names(yelpdata)=="attributesparkinggarage"] <- "garage"
names(yelpdata)[names(yelpdata)=="attributesgoodforlatenight"] <- "late"
names(yelpdata)[names(yelpdata)=="attributesgoodfordessert"] <- "desert"
names(yelpdata)[names(yelpdata)=="attributescaters"] <- "caters"
names(yelpdata)[names(yelpdata)=="attributeswifi"] <- "wifi"
names(yelpdata)[names(yelpdata)=="attributesattire"] <- "attire"
names(yelpdata)[names(yelpdata)=="attributesgoodforkids"] <- "goodforkids"
names(yelpdata)[names(yelpdata)=="attributeshastv"] <- "tv"
names(yelpdata)[names(yelpdata)=="attributesambienceromantic"] <- "romantic"
names(yelpdata)[names(yelpdata)=="attributesambiencetrendy"] <- "trendy"
names(yelpdata)[names(yelpdata)=="attributesambienceupscale"] <- "upscale"
names(yelpdata)[names(yelpdata)=="attributesambiencedivey"] <- "divey"
names(yelpdata)[names(yelpdata)=="attributeswheelchairaccessible"] <- "wheelchair"
names(yelpdata)[names(yelpdata)=="attributesmusicbackgroundmusic"] <- "bkgmusic"
names(yelpdata)[names(yelpdata)=="attributesmusiclive"] <- "livemusic"
names(yelpdata)[names(yelpdata)=="attributesbyob"] <- "byob"
names(yelpdata)[names(yelpdata)=="attributesdogsallowed"] <- "dogsallowed"
names(yelpdata)[names(yelpdata)=="attributesopen24hours"] <- "open24hrs"
names(yelpdata)[names(yelpdata)=="attributespaymenttypesamex"] <- "amex"
names(yelpdata)[names(yelpdata)=="attributesorderatcounter"] <- "orderatcounter"
names(yelpdata)[names(yelpdata)=="attributespaymenttypesvisa"] <- "visa"


# Identify N/A as "dnr" (did not respond).
addDNR <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "dnr")))
  return(x)
}
yelpdata <- as.data.frame(lapply(yelpdata, addDNR))
yelpdata[is.na(yelpdata)] <- "dnr"
#############################
yelpdata <- mutate(yelpdata, loc = ifelse(yelpdata$state=="NV", "Las Vegas, NV",
                                          ifelse(yelpdata$state=="PA", "Pittsburg, PA",
                                                 ifelse(yelpdata$state=="NC", "Charlotte, NC",
                                                        ifelse(yelpdata$state=="AZ", "Phoenix, AZ",
                                                               ifelse(yelpdata$state=="IL", "Urbana-Champaign, IL",
                                                                      ifelse(yelpdata$state=="WI", "Madison, WI",
                                                                             ifelse(yelpdata$state=="MLN", "Edinburgh, UK",
                                                                                    ifelse(yelpdata$state=="BW", "Karlsruhe, Germany",
                                                                                           ifelse(yelpdata$state=="QC", "Montreal, Canada",  
                                                                                                  ifelse(yelpdata$state=="ON", "Waterloo, Canada",
                                                                                                         ifelse(yelpdata$state=="SC", "Charlotte, NC",
                                                                                                                ifelse(yelpdata$state=="EDH", "Edinburgh, UK",
                                                                                                                       ifelse(yelpdata$state=="KHL", "Edinburgh, UK",
                                                                                                                              ifelse(yelpdata$state=="XGL", "Edinburgh, UK",
                                                                                                                                     ifelse(yelpdata$state=="NTH", "Edinburgh, UK",
                                                                                                                                            ifelse(yelpdata$state=="SCB", "Edinburgh, UK",
                                                                                                                                                   NA)))))))))))))))))

# Filter the restaurants.
all_restaurants <- filter(yelpdata, categories == "Restaurants" |
                            categories1 == "Restaurants" | 
                            categories2 == "Restaurants"| 
                            categories3 == "Restaurants"|
                            categories4 == "Restaurants"|
                            categories5 == "Restaurants"|
                            categories6 == "Restaurants"|
                            categories7 == "Restaurants"|
                            categories8 == "Restaurants"|
                            categories9 == "Restaurants"|
                            categories10 == "Restaurants") 

# Showy all of the categories of a restaurants 
bigcat <- c(as.character(all_restaurants$categories1), 
            as.character(all_restaurants$categories2), 
            as.character(all_restaurants$categories3),
            as.character(all_restaurants$categories4), 
            as.character(all_restaurants$categories5), 
            as.character(all_restaurants$categories6),
            as.character(all_restaurants$categories7), 
            as.character(all_restaurants$categories8), 
            as.character(all_restaurants$categories9),
            as.character(all_restaurants$categories10),
            as.character(all_restaurants$categories)) %>% 
  table() %>% 
  sort()

# Let's have a look at the most important categories
# tail(bigcat,65)

# "Varmaker" function creates a column for a category
# 1 = yes, 0 = no
varmaker <- function(x){
  all_restaurants <- mutate(all_restaurants, 
                            a = 
                              ifelse(
                                categories == x |
                                  categories1 == x | 
                                  categories2 == x | 
                                  categories3 == x | 
                                  categories4 == x | 
                                  categories5 == x | 
                                  categories6 == x | 
                                  categories7 == x | 
                                  categories8 == x | 
                                  categories9 == x | 
                                  categories10 == x , 1, 0) )
  all_restaurants$a <- as.factor(all_restaurants$a)
  names(all_restaurants)[names(all_restaurants)=="a"] <- gsub(" ", "", x, fixed = TRUE)
  return(all_restaurants)
}

# Create new columns associated to the most important categories
all_restaurants <- varmaker("Fast Food")
all_restaurants <- varmaker("Pizza")
all_restaurants <- varmaker("Mexican")
all_restaurants <- varmaker("American (Traditional)")
all_restaurants <- varmaker("Nightlife")
all_restaurants <- varmaker("Sandwiches")
all_restaurants <- varmaker("Bars")
all_restaurants <- varmaker("Food")
all_restaurants <- varmaker("Italian")
all_restaurants <- varmaker("Chinese")
all_restaurants <- varmaker("American (New)")
all_restaurants <- varmaker("Burgers")
all_restaurants <- varmaker("Breakfast & Brunch")
all_restaurants <- varmaker("Cafes")
all_restaurants <- varmaker("Japanese")
all_restaurants <- varmaker("Sushi Bars")
all_restaurants <- varmaker("Delis")
all_restaurants <- varmaker("Steakhouses")
all_restaurants <- varmaker("Seafood")
all_restaurants <- varmaker("Chicken Wings")
all_restaurants <- varmaker("Sports Bars")
all_restaurants <- varmaker("Coffee & Tea")
all_restaurants <- varmaker("Mediterranean")
all_restaurants <- varmaker("Barbeque")
all_restaurants <- varmaker("Thai")
all_restaurants <- varmaker("Asian Fusion")
all_restaurants <- varmaker("French")
all_restaurants <- varmaker("Buffets")
all_restaurants <- varmaker("Indian")
all_restaurants <- varmaker("Pubs")
all_restaurants <- varmaker("Greek")
all_restaurants <- varmaker("Diners")
all_restaurants <- varmaker("Bakeries")
all_restaurants <- varmaker("Vietnamese")
all_restaurants <- varmaker("Tex-Mex")
all_restaurants <- varmaker("Vegetarian")
all_restaurants <- varmaker("Salad")
all_restaurants <- varmaker("Hot Dogs")
all_restaurants <- varmaker("Middle Eastern")
all_restaurants <- varmaker("Event Planning & Services")
all_restaurants <- varmaker("Specialty Food")
all_restaurants <- varmaker("Lounges")
all_restaurants <- varmaker("Korean")
all_restaurants <- varmaker("Canadian (New)")
all_restaurants <- varmaker("Arts & Entertainment")
all_restaurants <- varmaker("Wine Bars")
all_restaurants <- varmaker("Gluten-Free")
all_restaurants <- varmaker("Latin American")
all_restaurants <- varmaker("British")
all_restaurants <- varmaker("Gastropubs")
all_restaurants <- varmaker("Ice Cream & Frozen Yogurt")
all_restaurants <- varmaker("Southern")
all_restaurants <- varmaker("Vegan")
all_restaurants <- varmaker("Desserts")
all_restaurants <- varmaker("Hawaiian")
all_restaurants <- varmaker("German")
all_restaurants <- varmaker("Bagels")
all_restaurants <- varmaker("Caterers")
all_restaurants <- varmaker("Juice Bars & Smoothies")
all_restaurants <- varmaker("Fish & Chips")
all_restaurants <- varmaker("Ethnic Food")
all_restaurants <- varmaker("Tapas Bars")
all_restaurants <- varmaker("Soup")
all_restaurants <- varmaker("Halal")
#############################
ggplot(all_restaurants, aes(as.factor(stars))) + 
  geom_histogram(fill = "blue", col="black") +
  xlab("Stars") +
  ylab("Number of Restaurants") +
  theme_classic()
############################
ggplot(all_restaurants, aes(reviewcount)) + 
  geom_histogram(binwidth = .15, fill = "blue", col="white") + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Number of Reviews") +
  ylab("Number of Restaurants") + 
  theme_classic()
#########################
ggplot(all_restaurants, aes(as.factor(stars), reviewcount)) + 
  geom_boxplot(col = "blue") +
  scale_y_log10() +
  xlab("Stars") +
  ylab("Number of Reviews") + 
  theme_classic()
#######################
###

# localisation in US and Europe
all_restaurants$latlong <- paste(all_restaurants$latitude, all_restaurants$longitude, sep=":")
counts <- all_restaurants %>% 
  group_by(loc) %>%
  summarize(Restaurants = n(),  Avg_Rating = round(mean(stars),2))

locdata <- data.frame(latlong = all_restaurants$latlong, 
                      loc = all_restaurants$loc)

counts <- inner_join(counts, locdata, by="loc") %>%
  group_by(loc) %>%
  summarize(Restaurants = first(Restaurants), 
            latlong = first(latlong),
            Avg_Rating = first(Avg_Rating))

require(datasets)

USmap <- gvisGeoChart(counts, "loc", 
                      sizevar="Restaurants",
                      colorvar="Avg_Rating", 
                      options=list(region = 'US',
                                   displayMode = "markers",
                                   colorAxis="{colors:['white', 'blue']}"))

Europemap <- gvisGeoChart(counts, "loc", 
                          sizevar = "Restaurants",
                          colorvar = "Avg_Rating", 
                          options=list(region = '150',
                                       displayMode = "markers",
                                       colorAxis="{colors:['white', 'blue']}"
                          ))


print(gvisMerge(USmap, Europemap, horizontal=TRUE), "chart")

#######################
### Table 1

dstate <- all_restaurants %>%
  group_by(loc) %>%
  summarise(num_rest = n(),
            avg_stars = round(mean(stars), digits =  2), 
            avg_num_rev = round(mean(reviewcount), digits =  2)) %>%
  tbl_df()

dstate <- dstate[order(-dstate$avg_stars),]
kable(dstate, col.names = c("City","Restaurants","Average Star Rating of Restaurants","Average Number of Ratings per Restaurant") , align  = "c")

#######################
### 

library(dplyr)
# Make dataset with predictors.
dataset <- all_restaurants %>%
  select(businessid,
         stars,   
         city,
         price,
         alcohol,
         noiselevel,
         classy,
         valet,
         cash,
         nhood,
         drivethru,
         parkinglot,
         casual,
         dance,
         delivery,
         ccheck,
         takeout,
         res,
         service,
         street,
         garage,
         late,
         desert,
         caters,
         wifi,
         goodforkids,
         tv,
         romantic,
         trendy,
         upscale,
         divey,
         wheelchair,
         bkgmusic,
         livemusic,
         byob,
         dogsallowed,
         open24hrs,
         amex,
         orderatcounter,
         visa
  )

dataset <- left_join(dataset, all_restaurants[c(1,119:(length(all_restaurants)-1))], by = "businessid")
dataset <- subset(dataset, select = -businessid)

# Define make predictors into a matrix.
x <- model.matrix(stars ~ ., data = dataset)[,-1]
y <- dataset$stars

#####################
#########
# Define training and test sets.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.train <- y[train]
y.test <- y[! (1:nrow(x)) %in% train]
grid=10^seq(10,-2, length =100)
#############################
#####Figure 5
# Train the LASSO model. Make plot of coefficients for increasing "s".
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

#############################
#####Figure 6
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out, col = "blue")
bestlam <- cv.out$lambda.min

#############################
########Figure 7

lasso <- data.frame(y.act = y.test)
lasso$y.act <- lasso$y.act %>%
  as.character() %>%
  as.numeric()
lasso$pred <- predict(lasso.mod, s=bestlam, newx=x[test,])  %>%
  as.character() %>%
  as.numeric()

# R-squared value estimate
1-(mean((lasso$pred -y.test)^2)/var(y.test))


# Plot of actual vs. predicted values for the test data set.
ggplot(lasso, aes(jitter(y.act), pred)) + 
  geom_point() + 
  geom_smooth(method = "lm", colour="#000099") + 
  geom_abline(intercept = 0, colour="#000099", size = 2) +
  coord_cartesian(ylim = c(1,5)) +
  xlab("Actual Yelp Star Ratings") +
  ylab("LASSO Predicted Star Ratings") + 
  theme_classic()


#############################
######### Table 2
out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam) [-1,] %>% data.frame()
names(lasso.coef)[names(lasso.coef)=="row.names"] <- "Variable"
names(lasso.coef)[names(lasso.coef)=="."] <- "Coefficient"
lasso.coef$Coefficient <- round(as.numeric(lasso.coef$Coefficient), 3) 
datatable(lasso.coef)
















