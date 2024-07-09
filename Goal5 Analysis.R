setwd("C:/Users/flavi/Downloads")

# Clear the inventory
rm(list = ls())

library(readxl) # Import excel file 

df <- read_excel("Goal5.xlsx")

# Descriptive statistics with different functions

str(df) 
head(df)
summary(df)

df$Value<-round(as.numeric(df$Value),2)

# Remove na values from the dataset

df<- df[!is.na(df$Value),]

summary(df)

df<-df[ colSums(is.na(df))==0]

library(ggplot2)
library(gridExtra)

# Filter dataset where the target is 5.1
df_5.1 <- subset(df, Target == "5.1")

# Convert TimePeriod to a factor
df_5.1$TimePeriod <- factor(df_5.1$TimePeriod)

# With aggregate function we can calcute the mean of Values for every country, SeriesCode
df_5.1_mean=aggregate(df_5.1$Value, by= list(df_5.1$GeoAreaName, df_5.1$SeriesCode), FUN=mean, na.rm=TRUE)

#plots=list() 
list_quadri_giuridici=list("Area 3: occupazione e benefici economici","Area 1: quadri giuridici generali e vita pubblica",
                           "Area 4: matrimonio e famiglia","Area 2: violenza contro le donne")
k=0 #Inizialize an index 
plots=list()

# Loop through unique series codes
for(i in unique(df_5.1$SeriesCode)) {
  
  k=k+1 # k is an index and increments to update it every iteration
  
  # Subset data for the current series code
  temp_df <- subset(df_5.1, SeriesCode == i)
  
  temp_df_clear <- temp_df[temp_df$GeoAreaName %in% names(which(table(temp_df$GeoAreaName) >= 3)),]
  
  print(paste("Iterazione numero:", k, "Paesi rimossi per insufficienza di valori:",
              paste(setdiff(temp_df$GeoAreaName, temp_df_clear$GeoAreaName), collapse=", ")))
  
  
# Create plot
  
  p <- ggplot(temp_df_clear, aes(x=Value, y=GeoAreaName, fill=TimePeriod)) + 
    labs(x="Percentuale di raggiungimento", y="Paesi dell'Asia", fill="Anni di riferimento :")+
    scale_fill_manual(values=c("turquoise1", "royalblue1", "midnightblue")) +
    geom_bar(stat="identity",position = position_dodge()) +
    theme_minimal()+
    xlim(c(0,100))+
    ggtitle(paste("Grafico",k, ":",list_quadri_giuridici[k]))
  
# Print the plot
  #print(p)
  plots[[i]]=p
}

#WARNING remove # from line 66 to see single plot for each graph and remove line 67 and 72

grid.arrange(grobs=plots, ncol=2, nrow=2) #if you want to see four graphic in only one window 

# In the second phase of the analysis, objective 5.5 is analyzed with indicator 5.5.1

# Filter dataset where the target is 5.5
df_5.5<-subset(df,Target=="5.5")

# Create a new subset called df.parl, df_5.5.map where we isolate the variables to analyze
df.parl=subset(df_5.5[,c("GeoAreaName","TimePeriod","Value")],df_5.5$SeriesCode=="SG_GEN_PARL")
summary(df.parl) # descriptive statistics of df.parl dataset

df_5.5.map = df.parl[df.parl$TimePeriod==2024,]
df_5.5.map$GeoAreaName=ifelse(df_5.5.map$GeoAreaName=="Viet Nam","Vietnam",df_5.5.map$GeoAreaName)

library(ggplot2)
library(rnaturalearth) # Use this library to get Asia data to plot a Geographic Map
library(rnaturalearthdata) 
library(sf)
library(scales)  # To use a different scales such as percent scale

# Import Asia dataset from rnaturalearth library
asia <- ne_countries(continent = "asia", returnclass = "sf")

# Merge df_5.5.map with Asia dataset 

asia_df <- merge(asia, df_5.5.map, by.x = "name", by.y = "GeoAreaName")

# Plot Asia map 
p= ggplot(data = asia_df) +
  geom_sf(aes(fill = Value),alpha=0.8) +  
  scale_fill_viridis_c(
    limits = c(0, 50),    
    name = "Percentuale Donne: ",
    labels = scales::percent_format(scale = 1),
    option = "C" 
  ) +
  geom_sf_text(aes(label = name), size = 4, nudge_x = 0.5, nudge_y = 0.5,check_overlap = TRUE, fontface = "bold") +  # Posiziona le etichette dei paesi
  theme_minimal() +
  labs(title = "Percentuale di seggi occupati da donne nei parlamenti nazionali dell'Asia nel 2024")


# Change theme and title font
p+theme_void() + theme(plot.title = element_text(size = 20, face = "bold"))

# With aggregate function we can calcute the standard deviation and mean of Values for every country 
df.parl_mean_var = aggregate(df.parl$Value,by=list(df.parl$GeoAreaName), FUN = function(data){
  c(mean=mean(data), sd=sd(data)) 
})

#TIME TREND AND BOOTSTRAP FORECAST

# Choose the countries which have the highest standard deviation values 

Armenia =  subset(df.parl[,c("GeoAreaName","TimePeriod","Value")],df.parl$GeoAreaName=="Armenia")
Emirati_Arabi = subset(df.parl[,c("GeoAreaName","TimePeriod","Value")],df.parl$GeoAreaName=="United Arab Emirates")
Uzbekistan = subset(df.parl[,c("GeoAreaName","TimePeriod","Value")],df.parl$GeoAreaName=="Uzbekistan")

# Combine the chosen countries in one Data Frame with rbind function (union for rows)
combined_df = rbind(Armenia,Emirati_Arabi,Uzbekistan)

# Trasform GeoAreaName and TimePeriod column to factors

combined_df$GeoAreaName=factor(combined_df$GeoAreaName)
names(combined_df)[1]="Paesi"
combined_df$TimePeriod=factor(combined_df$TimePeriod)

# Plot the time series graph 

ggplot(data=combined_df,aes(x=TimePeriod,y=Value, group=Paesi, color=Paesi))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  scale_x_discrete(breaks = seq(2000, 2024, by = 3))+
  labs(title= "Trend temporali Armenia, Emirati Arabi, Uzbekistan dagli anni 2000 al 2024", x="Anni" , y="Percentuale di seggi occupati")+
  theme(plot.title = element_text(hjust = 0.5)) 

# Initialize bootstrap function 

index=c(1:25)
bootstrap_regression <- function(data, index) {
  fit <- lm(Value ~ as.numeric(TimePeriod), data = data[index,])
  return(coef(fit))
}
df_boot <- list(Armenia = Armenia, "United Arab Emirates" = Emirati_Arabi, Uzbekistan = Uzbekistan)

library(boot) #import boot library

# To predict future values up to 2030 we use a loop and 
# the bootstrap_regression function defined previously

for (country_name in names(df_boot)) {
  set.seed(123)
  country_data <- df_boot[[country_name]]
  boot_results <- boot(country_data, bootstrap_regression, R = 1000)
  mean_coef <- colMeans(boot_results$t)
  future_years <- data.frame(TimePeriod = as.factor(2025:2030), 
                             Value = mean_coef[1] + mean_coef[2] * as.numeric(2025:2030), 
                             Paesi = rep(country_name, times = 6))
  combined_df <- rbind(combined_df, future_years)
}

# Plot the new time series graph with forecast

ggplot(combined_df, aes(x = TimePeriod, y = Value, group = Paesi, color = Paesi)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  scale_x_discrete(breaks = seq(2000, 2030, by = 3)) +
  labs(title = "Trend temporali fino al 2030 Armenia, Emirati Arabi,Uzbekistan",
       x = "Anni", y = "Percentuale di seggi occupati") +
  theme(plot.title = element_text(hjust = 0.5))


  