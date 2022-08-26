library(tidyverse)
library(ggplot2)
library(highcharter)
library(leaflet)
library(priceR)
library(leaflet)
library(sp)
library(stringr)
options(scipen=999)

# Loading data ---------------------------------------------

data <- read.csv("House_Rent_Dataset.csv")
data <- janitor::clean_names(data)
glimpse(data)

### Date posted seems to be in string datatype, lets change it

data$posted_on <- as.Date(data$posted_on)

### We will also change bhk and bathroom to factors

data$bathroom <- as.factor(data$bathroom)
data$bhk <- as.factor(data$bhk)

# Descriptive analysis ----------------------------------------------------

## Distribution of rent prices ----

ggplot(data = data)+
  geom_histogram(aes( x = rent))


### Huge rents detected , lets check rent price "anomalies" by doing a box plot:

ggplot(data = data)+
  geom_boxplot(aes( y = rent, x  = city,
                    color = city))

### It seems Bangalore has an extremely high rent price, maybe its a huge mansion?,
### a typo?, but for this analysis we will ignore it (just this one).

data <- data %>% 
  filter(rent < 3500000)

### Lets box plot again without that extremely high rent

ggplot(data = data)+
  geom_boxplot(aes( y = rent, x  = city,
                    color = city),
               outlier.stroke = 0.5)+
  coord_flip()

### We can visually identify that mumbai has the most amount of rare high prices
### and its distribution of its rents is higher than most of the cities in this
### dataset. Also, most of the cities have lots of outliers

## Distribution of rent prices by furnishing status----

ggplot(data = data)+
  geom_boxplot(aes( y = rent, x  = city,
                    color = furnishing_status))+
  coord_flip()

## Boxplotted distribution ----
ggplot(data = data)+
  geom_boxplot(aes( y = rent, x  = city,
                    color = furnishing_status))+
  labs(title = "Price by city Boxplot",
       subtitle = "By furnishing status")+
  xlab("City")+
  ylab("Rent price")+
  scale_y_continuous(labels=scales::label_dollar(prefix="₹"))+
  coord_flip()


## Leaflet map----

### In order to use leaflet we will get latitudes and longitudes from latlong.net
### so we can use it to plot in our leaflet map.


data <- data %>% 
  mutate(lat = case_when(city == "Bangalore" ~ 12.972442,
                          city == "Kolkata" ~ 22.572645,
                         city == "Mumbai" ~ 19.076090,
                         city == "Delhi" ~ 28.644800,
                         city == "Chennai" ~ 13.067439,
                         city == "Hyderabad" ~ 17.387140)) %>% 
  mutate(long = case_when(city == "Bangalore" ~ 77.580643,
                          city == "Kolkata" ~ 88.363892,
                          city == "Mumbai" ~ 72.877426,
                          city == "Delhi" ~ 77.216721,
                          city == "Chennai" ~ 80.237617,
                          city == "Hyderabad" ~ 78.491684))

### Just in case we set it as a numeric value
data$lat <- as.numeric(data$lat)
data$long <- as.numeric(data$long)

### For  future use
data.SP <- SpatialPointsDataFrame(data[,c(13,14)], data[,-c(13,14)])

map <- leaflet(options = leafletOptions()) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data = data, lng = ~long, lat = ~lat,color = "#246e8b",
                   popup = ~paste(str_glue('<h3>{city}</h3>'),
                                  str_glue('<h4>AVG Rent : ₹ {round(mean(data$rent),2)}')))
### The map!
map

### As expected furnished houses tend to have a higher price than semi and
### unfurnished prices, this behaviour changes on rents out of the "normal"
### IQR ranges.

## Point plot through time--

ggplot(data =data)+
  geom_point(aes(x = posted_on, y = rent,
                 color = city), position = "jitter")+
  scale_x_date(date_breaks = "1 month")+
  scale_y_continuous(labels=scales::label_dollar(prefix="₹"))+
  labs(title = "Rent through time",
       subtitle = "By city",
       color = "City")+
  xlab("Date posted")+
  ylab("Rent price")


# Outlier treament --------------------------------------------------------

### My idea here is to remove any outlier depending on the IQR of the rent
### by each city.

## Obtaining 98% ICR for observation by city----

for (i in unique(data$city)){
  test <- data %>% 
   filter(city == i) %>%
    select(rent) %>% 
    unlist() %>% 
    quantile(c(0.98), type = 1) #This one  calculates de 98th percentile.
  print(paste0(i,":",test))
}

### Since I didn't found a quick way to assign the corresponding IQR to every row
### depending on its city I used a case_when.

data <- data %>% 
  mutate(city_98iqr = case_when(city == "Kolkata" ~ 40000,
                              city == "Mumbai" ~ 360000,
                              city == "Bangalore" ~ 110000,
                              city == "Delhi" ~ 150000,
                              city == "Chennai" ~ 100000,
                              city == "Hyderabad" ~ 80000))

### "If the rent is  higher than the  98th percentile replace it with the 98th value" 
## Obtaining new rent values of outliers----

data_wz <- data %>% 
  mutate(rent_wz = ifelse(rent > city_98iqr, city_98iqr,rent))

## Boxplot of winsorized outliers ----

ggplot(data = data_wz)+
  geom_boxplot(aes( y = rent_wz, x  = city,
                    color = furnishing_status))+
  coord_flip()
### Values larger than the 95% still exists, but we can now say that there are
### no extremely high values on each city.

## Histogram of winsorized outliers ----

ggplot(data = data_wz)+
  geom_histogram(aes(x = rent_wz),bins = 50)

### We can see that the distribution is still not simmetrical due to the
### difference in the rent of different cities.




# Linear Regression -------------------------------------------------------

## First try ----

## A simple model without rent,posted_on,area_locality, tenant_preferred,
## point_of_contact,city_iqcr,city_98iqr
full_model <- lm(rent_wz ~ bhk + size + city + furnishing_status +
                  bathroom, data = data_wz)

summary(full_model)


## Second try (with forward step) ----

null_model <- lm(rent_wz ~ 1 , data = data_wz)

step_model <- step(null_model,
                  direction="forward",
                  scope = formula(full_model))

summary(step_model)

## Same adjusted R-squared as the first try, so we'll use "full_model" from now on
## As a reminder,  the intercept is : bhk = 1, area_type = "Built Area",
## city = "Bangalore", furnishing_status = "Furnished",bathroom = 1


# Model quality -----------------------------------------------------------

## Kolmogorov Smirnov test----

nortest::lillie.test(full_model$residuals)

## p value near 0, we can reject that the distribution is from a normal distribution.

## Homoscedasticity ----

plot(full_model, 3)

### We can see a huge cluster at the start of the  plot!!!
### So no homoscedasticity for this model.

### Breusch-Pagan Test ----
lmtest::bptest(full_model)

### We can see that the p value is almost 0, we reject that he errors are not
### homoscedastic.

## Residuals tendency ----

plot(full_model, 1)

### Again, we can see a huge cluster at the start of the plot.

### Test Durbin Watson

lmtest::dwtest(full_model)

### p value is 0.5 which means there is correlation but,
### there is no first-order autocorrelation because DW = 2.0028



# Predicting --------------------------------------------------------------

predict_me <-tibble(bhk = 4,
                    size = 2000,
                    city =  "Bangalore",
                    furnishing_status = "Furnished",
                    bathroom = 3
)

predict_me$bhk <- as.factor(predict_me$bhk)
predict_me$bathroom <- as.factor(predict_me$bathroom)

predict(full_model, newdata =  predict_me)

## The rent of a 4 bedroom, 1000 square feet in Bangalore, furnished with 3 bathrooms
## should cost 75262 rupees.

data_wz %>% 
  filter(bhk == 4, size >= 1000,
         city == "Bangalore",
         furnishing_status == "Furnished",
         bathroom == 3)

## In the dataset there is a similar floor that costs
## 75000!