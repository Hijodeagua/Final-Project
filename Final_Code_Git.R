#Clear prior data environment set working directories, load proper programs and create glm function
##----
rm(list = ls())

setwd("Working Directory")
pacman::p_load(tidyverse,ggplot2, stringi, scales, lubridate, ggrepel)

glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

#Data Cleaning 
##----

city <- read_csv("BigCity.csv")

city_simp = city %>% 
  select(date_label, metric_item_label, metric_cat_label,metric_item_label_subtitle, geo_label_city, geo_label_state, value, geo_strata_poverty,
         geo_strata_Population, strata_race_label, strata_sex_label, geo_strata_region)

city_simp <- city_simp[city_simp$strata_sex_label == "Both",]

city_simp <- city_simp[city_simp$strata_race_label == "All",]


city_simp <- filter(city_simp, metric_item_label %in% 
                      c("Minority Population","Population Density", "Unemployment", "Violent Crime",
                        "Adult Mental Distress", "Homicides", "Income Inequality",
                        "Per-capita Household Income", "Racial Segregation, White and non-White",
                        "Poverty and Near Poverty in All Ages"))

city_simp <- filter(city_simp, date_label %in% 
                      c(2019))


city_simp <- filter(city_simp, geo_label_city != "U.S. Total")
city_simp = select(city_simp, -metric_cat_label)
city_simp = select(city_simp, -metric_item_label_subtitle)
city_simp = select(city_simp, -geo_strata_region)
city_simp = select(city_simp, -geo_strata_Population)
city_simp = select(city_simp, -geo_strata_poverty)
city_simp = select(city_simp, -strata_race_label)
city_simp = select(city_simp, -strata_sex_label)


city_amd <- city_simp[city_simp$metric_item_label == "Adult Mental Distress",]
city_amd = city_amd %>% rename(AMD = value)

city_murd <- city_simp[city_simp$metric_item_label == "Homicides",]
city_murd = city_murd %>% rename(Homicides = value)

city_II <- city_simp[city_simp$metric_item_label == "Income Inequality",]
city_II = city_II %>% rename(Inc_Ineq = value)

city_mino <- city_simp[city_simp$metric_item_label == "Minority Population",]
city_mino = city_mino %>% rename(Minority_Pop = value)

city_HI <- city_simp[city_simp$metric_item_label == "Per-capita Household Income",]
city_HI = city_HI %>% rename(Per_Cap_HI = value)

city_pop <- city_simp[city_simp$metric_item_label == "Population Density",]
city_pop = city_pop %>% rename(Pop_Dens = value)

city_unemp <- city_simp[city_simp$metric_item_label == "Unemployment",]
city_unemp = city_unemp %>% rename(Unemprate = value)

city_vc <- city_simp[city_simp$metric_item_label == "Violent Crime",]
city_vc = city_vc %>% rename(Violent_Crime = value)

city_seg <- city_simp[city_simp$metric_item_label == "Racial Segregation, White and non-White",]
city_seg = city_seg %>% rename(Segregation = value)

city_pov <- city_simp[city_simp$metric_item_label == "Poverty and Near Poverty in All Ages",]
city_pov = city_pov %>% rename(Poverty = value)

df_list <- list(city_amd, city_murd, city_II, city_mino, 
                city_HI, city_pop, city_unemp, city_vc,
                city_seg, city_pov)

final <- df_list %>% reduce(full_join, by=c('geo_label_city', "date_label", "geo_label_state"))

final = final %>%
  select(date_label, geo_label_city,geo_label_state, AMD, Homicides, 
         Inc_Ineq, Minority_Pop, Per_Cap_HI, Pop_Dens, Violent_Crime,
         Poverty, Segregation, Unemprate)

final = final %>% rename(City = geo_label_city)


final$Violent_Crime <- round(final$Violent_Crime)

#Load in 1 year acs numbers and final cleaning steps
##----
y2014 <- read_csv("2014.csv")
y2014 = select(y2014, -`metro city`)
y2014 = y2014 %>% rename(u2014 = Unemployment)
y2019 <- read_csv("2019.csv")
y2019 = select(y2019, -`Metro city, State`)
y2019 = y2019 %>% rename(u2019 = Unemployment)
unemp <- left_join(y2019,y2014,by="City")
unemp$u2019 <- parse_number(unemp$u2019)
unemp$u2014 <- parse_number(unemp$u2014)
unemp$change <- unemp$u2019 - unemp$u2014
x <- mean(unemp$change)
unemp$average <- as.character(ifelse(unemp$change < x, 'A', 'B'))

#Drop louisville
final <-subset(final, City!='Louisville')

final <- left_join(final, unemp,by="City")

final <- na.omit(final)

hist(final$Violent_Crime)

final <- final[!duplicated(final$City), ]




#Regression analysis
##----
vcpoiss <- glm(Violent_Crime ~ u2019 + change + Pop_Dens + Per_Cap_HI + Minority_Pop + Segregation,
               family = poisson,
              data = final)

summary(vcpoiss)
glm.RR(vcpoiss, 3) 


vc_nb <- glm.nb(Violent_Crime ~ u2019 + change + Pop_Dens + Per_Cap_HI + Minority_Pop+Segregation ,
                data = final)
summary(vc_nb)
lrtest(vcpoiss, vc_nb)

vc_est <- cbind(Estimate = exp(coef(vc_nb)), exp(confint(vc_nb)))
vc_est


vc_nb_solo <- glm.nb(Violent_Crime ~ u2019,
                data = final)
vc_est_solo <- cbind(Estimate = exp(coef(vc_nb_solo)), exp(confint(vc_nb_solo)))
vc_est_solo
amdpoiss <- glm(Violent_Crime ~  AMD + Pop_Dens + Per_Cap_HI + Minority_Pop + Segregation,
                 family = poisson,
               data = final)

summary(amdpoiss)
glm.RR(amdpoiss, 3) 


amd_nb <- glm.nb(Violent_Crime ~ AMD + Pop_Dens + Per_Cap_HI + Minority_Pop + Segregation
                 ,
                 data = final)
summary(amd_nb)
lrtest(amdpoiss, amd_nb)

amd_est <- cbind(Estimate = exp(coef(amd_nb)), exp(confint(amd_nb)))
amd_est

amd_nb_solo <- glm.nb(Violent_Crime ~ AMD,
                 data = final)
amd_est_solo <- cbind(Estimate = exp(coef(amd_nb_solo)), exp(confint(amd_nb_solo)))
amd_est_solo

#Create table 1 and export final data set
##----
final <- final %>% rename(Unemployment_2019 = u2019,
                          Income_Inequality = Inc_Ineq,
                          Population_Density = Pop_Dens,
                          Adult_Mental_Distress = AMD
                          )
table1(~Violent_Crime + Unemployment_2019 + Adult_Mental_Distress + Population_Density + Per_Cap_HI + Segregation +
       change + Minority_Pop, final)

write.csv(final,"File Path/finaldata.csv", row.names = FALSE)


