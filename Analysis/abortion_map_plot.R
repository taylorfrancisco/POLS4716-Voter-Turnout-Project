# Spardha Sharma
# the original dataset can be found here: https://www.kff.org/other/state-indicator/abortion-policy-tracker/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

# loading necessary libraries
library(plotly)
library(readr)

# reading file (note that state codes were added)
ab_df <- read_csv("/Users/Spardha/Downloads/abortion_ban_data.csv")

# creating a new varaible to classify abortion statuses
ab_df$Ban_Type <- ifelse(ab_df$Status == "Abortion banned", "High",
                         ifelse(ab_df$Status == "Abortion legal beyond 22 weeks LMP", "Low",
                                ifelse(ab_df$Status == "Gestational limit between 15 and 22 weeks LMP","Moderately Less",
                                       ifelse(ab_df$Status == "Gestational limit between 6 and 12 weeks LMP", "Moderately High", NA))))

# factorizing Ban type
ab_df$Ban_Type <- factor(ab_df$Ban_Type, levels = c("Low", "Moderately Less", "Moderately High", "High"))

# creating a color scale to represent ban intensity
ab_df$numeric_Ban_Type<- as.numeric(ab_df$Ban_Type)

# adding hover column for hover text on the map
ab_df$hover <- with(ab_df, paste(State, '<br>', "Status", Status))

# plotting the map
plot_ly(
  data = ab_df,
  type = "choropleth",
  locations = ~code,
  locationmode = "USA-states",
  text = ab_df$hover,
  z = ~numeric_Ban_Type,
  colors  = c("blue", "skyblue2", "tomato3", "red"),
  colorbar = list(
    title = "Ban Type",
    tickvals = c(1, 2, 3, 4),  # Adjust tick values as needed
    ticktext = c("Low", "Moderately Low", "Moderately High", "High"),
    tickmode = "array",
    tickcolor = "white"  # Optional: Set tick color
  )
) %>%
  layout(
    geo = list(scope = "usa"),
    title = "Status of Abortion Bans by State After Dobbs",
    margin = list(t = 70)
  )

