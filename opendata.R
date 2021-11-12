library(dplyr)
library(ggplot2)
library(ggcorrplot)

df <- read.csv(file = 'broadband_data_opendatachallenge.csv', header = T)

# Data Processing (aka Data Cleaning)
# remove rows that contain NA and NULL values
clean_df <- df[df == "NULL"] <- NA
clean_df <- na.omit(df)

# remove zip and county columns
clean_df <- clean_df %>% select(Population, State:ends_with("All100_3.1"))

# switch columns positionsso it makes sense
clean_df <- clean_df %>% relocate(State, Population)
clean_df <- clean_df %>% relocate(TestCount:X.Access.to.Terrestrial.Broadband, .before = WiredCount_2020)

# convert column classes to appropriate types
clean_df <- clean_df %>% mutate(Population = as.numeric(Population),
                                X.Access.to.Terrestrial.Broadband = as.numeric(sub("%","",X.Access.to.Terrestrial.Broadband))/100,
                                Lowest.Priced.Terrestrial.Broadband.Plan = as.numeric(Lowest.Priced.Terrestrial.Broadband.Plan))

# Z-score normalizing all numeric data
z_df <- clean_df %>% mutate_at(vars(-('State')), scale)

### End of Data Process ###

# Plot - population
population <- aggregate(clean_df$Population, by=list(State=clean_df$State), FUN=sum)
ggplot(population, mapping = aes(x = State, y = x, label=State, group=1)) + geom_area() + theme_light() +
  labs(title = 'Total Population by State') +
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 12), axis.text.y = element_text(size=12)) + 
  scale_y_continuous(name="Population", labels = scales::comma) + coord_flip() 


# Plot - Percent avg coverage by state
percentavg <- aggregate(clean_df$X.Access.to.Terrestrial.Broadband, by=list(State=clean_df$State), FUN=mean)
percentavg <- percentavg %>% rename(y = x)
percentavg <- percentavg[-c(40,48), ] # drop Puerto Rico and Virgin Islands

ggplot(percentavg, mapping = aes(x = as.numeric(row.names(percentavg)), y = y,label=State)) + 
  labs(title = "Percent Avg of States's Population that has Access to Terrestrial Broadband", x = "", y = "Percent Avg") + 
  geom_point() + geom_text(aes(label=State), check_overlap = TRUE, angle=0, hjust=1.1) +
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 12), axis.text.y = element_text(size=20))


# Inside look into # of providers / population by state

allProvider2015 <- aggregate(clean_df$AllProviderCount_2015, by=list(State=clean_df$State), FUN=sum)
allProvider2020 <- aggregate(clean_df$AllProviderCount_2020, by=list(State=clean_df$State), FUN=sum)

# all25_2015 <- aggregate(clean_df$All25_3_2015, by=list(State=clean_df$State), FUN=sum)
# all25_2020 <- aggregate(clean_df$All25_3_2020, by=list(State=clean_df$State), FUN=sum)
# 
# all100_2015 <- aggregate(clean_df$All100_3.1, by=list(State=clean_df$State), FUN=sum)
# all100_2020 <- aggregate(clean_df$All100_3, by=list(State=clean_df$State), FUN=sum)

ggplot(allProvider2020, mapping = aes(x = State, y = x, label=State, group=1)) + theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(title = "Provider Count by State (2015 vs 2020)", x = "", y = "Number of Providers") +
  geom_point(shape = 19, size = 3) + geom_line(linetype = 'dashed') +
  geom_point(data = allProvider2015, shape = 1, size = 3) + coord_flip() + 
  theme(axis.text.x = element_text(angle=90, hjust = 1, size = 12), axis.text.y = element_text(size=12))

ggcorrplot(allProvider2015)
  
# # Summary on numeric values
# overview <- clean_df %>% select(-State)
# overview <- sapply(overview, summary)
# 
# # z-score normalizing all numeric data
# z_df <- clean_df %>% select(-State) %>% mutate(scale(clean_df))
# overview2 <- sapply(z_df, summary)


