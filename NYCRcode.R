
## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")

library(plyr)
library(dplyr)
library(tidyverse)

library(Hmisc)


#GET /resource/5ebm-myj7.json HTTP/1.1
#Host: data.cityofnewyork.us
#Accept: application/json
#X-App-Token: xJDuhvTW865W8J5dj3KK7KjSa


df2 <- read.socrata(
  "https://data.cityofnewyork.us/resource/5ebm-myj7.json?year=2020",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)




df3 <- read.socrata(
  "https://data.cityofnewyork.us/resource/usep-8jbt.json",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)


df2016 <- read.socrata(
  "https://data.cityofnewyork.us/resource/w2pb-icbu.json",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)


dim(df2016)


nopv <- read.socrata(
  "https://data.cityofnewyork.us/resource/8vgb-zm6e.json",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)


head(nopv)




dplyr::filter(nopv, grepl('PHEASANT', address_2))


dplyr::filter(nopv, grepl('33', lot), grepl('7351', block))



condo <- read.socrata(
  "https://data.cityofnewyork.us/resource/9ck6-2jew.json",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)


head(condo)



dplyr::filter(condo, grepl('41-40 UNION', address))




ipis <- read.socrata(
  "https://data.cityofnewyork.us/resource/n5mv-nfpy.json",
  app_token = "xJDuhvTW865W8J5dj3KK7KjSa",
  email     = "tammanho@hotmail.com",
  password  = "Job123456#"
)




dplyr::filter(ipis, grepl('1096', lot))


head(ipis)


map(fildf6, class)




fildf6 <- filter(df3, df3$sale_price != 0)
fildf6 <- filter(df3, df3$sale_price >= 10000)
fildf6 %>% drop_na(sale_price)
fildf6$sale_price <- as.numeric(gsub(",","",fildf6$sale_price))
fildf6$land_square_feet <- as.numeric(gsub(",","",fildf6$land_square_feet))
fildf6$gross_square_feet <- as.numeric(gsub(",","",fildf6$gross_square_feet))
fildf6$year_built <- as.numeric(gsub(",","",fildf6$year_built))
fildf6 <- filter(fildf6, fildf6$sale_price >= 10000)
fildf6 <- filter(fildf6, fildf6$sale_price <= 1000000)
fildf6 <- filter(fildf6, fildf6$gross_square_feet >= 100)
fildf6 <- filter(fildf6, fildf6$tax_class_at_time_of_sale != 3)
fildf6 <- filter(fildf6, fildf6$tax_class_at_time_of_sale != 4)



#fildf6 <- filter(df3, df3$sale_price != 0)



#ddply(fildf6, .(neighborhood), mutate, count = length(unique(type)))




filfrom2016 <- filter(df2016, df2016$sale_price != 0)
filfrom2016 <- filter(df2016, df2016$sale_price >= 10000)
filfrom2016 %>% drop_na(sale_price)
filfrom2016$sale_price <- as.numeric(gsub(",","",filfrom2016$sale_price))
filfrom2016$land_square_feet <- as.numeric(gsub(",","",filfrom2016$land_square_feet))
filfrom2016$gross_square_feet <- as.numeric(gsub(",","",filfrom2016$gross_square_feet))
filfrom2016$year_built <- as.numeric(gsub(",","",filfrom2016$year_built))
filfrom2016 <- filter(filfrom2016, filfrom2016$sale_price >= 10000)
filfrom2016 <- filter(filfrom2016, filfrom2016$sale_price <= 1000000)
filfrom2016 <- filter(filfrom2016, filfrom2016$gross_square_feet >= 100)
filfrom2016 <- filter(filfrom2016, filfrom2016$tax_class_at_time_of_sale != 3)
filfrom2016 <- filter(filfrom2016, filfrom2016$tax_class_at_time_of_sale != 4)


build07 <- filter(filfrom2016, filfrom2016$building_class_category == "07 RENTALS - WALKUP APARTMENTS")

filfrom2016excl07 <- filter(filfrom2016, filfrom2016$building_class_category != "07 RENTALS - WALKUP APARTMENTS")


typeof(fildf6$neighborhood)


dim(df3)


dim(fildf6)

dim(filfrom2016)

head(fildf6)


newresult <- filfrom2016 %>% filter(sale_price <= 1000000)


dim(newresult)




fildf6$year_built_category <- as.numeric(cut2(fildf6$year_built, g=2))



fildf6$sale_date_category <- as.Date(cut2(fildf6$sale_date, g=5))


table(unique(fildf6)$neighborhood)[fildf6$neighborhood]

abc = fildf6 %>%
  group_by(neighborhood) %>%
  mutate(unique_types = n_distinct(character()))




dplyr::filter(df3, grepl('41-40 UNION', address))



#train <- fildf6 %>% dplyr::sample_frac(.75)
#test  <- dplyr::anti_join(fildf6, train, by = 'address')


#resulttrain <- lm((sale_price) ~ (gross_square_feet) + factor(neighborhood), data = fildf6)



result <- lm((sale_price) ~ (gross_square_feet) + factor(neighborhood) + factor(building_class_category), data = filfrom2016)
summary(result)


plot(result)


pred1 <- predict(result, newdata = fildf6, interval = "confidence")

summary(pred1)

rmse <- sqrt(sum((exp(pred1) - fildf6$sale_price)^2)/length(fildf6$sale_price))
c(RMSE = rmse, R2=summary(resulttrain)$r.squared)


# ggplot model
# https://stackoverflow.com/questions/14146005/multivariable-regression-with-ggplot2

model <- lm((sale_price) ~ (gross_square_feet) + factor(neighborhood) + factor(building_class_category) + factor(year_built), data = filfrom2016)
fildf6$model <- stats::predict(model, newdata=fildf6)
err <- stats::predict(model, newdata=fildf6, se = TRUE)
fildf6$ucl <- err$fit + 1.96 * err$se.fit
fildf6$lcl <- err$fit - 1.96 * err$se.fit

g <- ggplot(fildf6)
g <- g + geom_point(aes(x=sale_price, y = model), size = 2, colour = "blue")
g <- g + geom_smooth(data=fildf6, aes(x=sale_price, y=model, ymin=lcl, ymax=ucl), size = 1.5, 
                     colour = "red", se = TRUE, stat = "smooth")

# Display the g plot
g