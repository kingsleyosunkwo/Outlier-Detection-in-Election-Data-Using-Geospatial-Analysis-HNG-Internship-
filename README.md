# Outlier-Detection-in-Election-Data-Using-Geospatial-Analysis-HNG-Internship-
This project ensures election integrity in Imo State, Nigeria, by detecting potential voting irregularities. By identifying outlier polling units where results deviate significantly from neighbouring units, we aim to highlight possible influences or rigging, supporting transparency and accuracy in election results.

## TABLE OF CONTENT
- [INTRODUCTION](#introduction)
- [METHODOLOGY](#methodology)
- [NEIGHBOUR IDENTIFICATION](#neighbour-identification)
- [OUTLIER SCORE CALCULATION](#outlier-score-calculation)
- [EXPORTING THE DATA](#exporting-the-data)
- [SORTING AND REPORTING](#sorting-and-reporting)
- [FINDINGS](#findings)
- [CONCLUSION](#conclusion)


# INTRODUCTION
In this report, I aim to uncover potential voting irregularities in the recently concluded election in Imo State, Nigeria. The Independent National Electoral Commission (INEC) has faced multiple legal challenges concerning the integrity and accuracy of the election results. Allegations of vote manipulation and irregularities have prompted a thorough investigation into the matter. Our analysis will focus on identifying outlier polling units based on the votes each party received, using geospatial techniques to find neighboring polling units and calculate an outlier score for each party.

# METHODOLOGY
Dataset Preparation
Data Source: The dataset was obtained from the provided link, focusing on Imo State.
Geocoding: Longitude and latitude values were added to each polling unit using geocoding techniques.
Cleaning: The dataset was cleaned to remove any inconsistencies and ensure accuracy.

```
install.packages("tidyverse")
library(dplyr)

# Display the first few rows to ensure it is loaded correctly
head(geocoded_data)

State LGA       Ward  PU_Code PU_Name Accredited_Voters Registered_Voters Results_Found
  <chr> <chr>     <chr> <chr>   <chr>               <dbl>             <dbl> <lgl>        
1 IMO   ABOH MBA... ENYI... 16-01-... ALADIN...               175               905 TRUE         
2 IMO   ABOH MBA... ENYI... 16-01-... OKWUAK...               243               852 TRUE         
3 IMO   ABOH MBA... ENYI... 16-01-... UMUNKW...               183               704 TRUE         
4 IMO   ABOH MBA... ENYI... 16-01-... IBEKU ...               153               646 TRUE         
5 IMO   ABOH MBA... ENYI... 16-01-... EZIALA...               209               750 TRUE         
6 IMO   ABOH MBA... ENYI... 16-01-... COMMUN...               203               750 TRUE         
# ℹ 13 more variables: Transcription_Count <dbl>, Result_Sheet_Stamped <lgl>,


# Load necessary libraries
install.packages("geosphere")
library(geosphere)
```

### Neighbour Identification
Proximity Calculation: We defined a radius of 1 km to identify neighbouring polling units.
Neighbour List Creation: For each polling unit, a list of neighbouring units within the 1 km radius was created.

```
# Define a function to calculate neighbours within a given radius
  identify_neighbours <- function(data, radius = 1) {
  # Convert radius to meters
  radius_m <- radius * 1000

  # Initialize an empty list to store neighbours
  neighbours_list <- vector("list", nrow(data))

 # Loop through each polling unit
  for (i in 1:nrow(data)) {
    # Calculate distances to all other polling units
    distances <- distGeo(matrix(c(data$lon[i], data$lat[i]), ncol = 2), 
                         matrix(c(data$lon, data$lat), ncol = 2))
    
  # Identify neighbours within the specified radius
    neighbours_list[[i]] <- which(distances <= radius_m & distances > 0)
  }
  
 # Add the neighbours list as a new column
  data$neighbours <- neighbours_list
  return(data)
}

# Apply the function to the dataset
geocoded_data <- identify_neighbours(geocoded_data, radius = 1)

# Display the first few rows to ensure neighbours are identified
head(geocoded_data)
```

### Outlier Score Calculation
Vote Comparison: For each polling unit, the votes each party received were compared with those of its neighbouring units.
Score Calculation: An outlier score for each party was calculated based on the deviation of votes from neighbouring units. This score was stored in new columns: APC_outlier_score, LP_outlier_score, PDP_outlier_score, and NNPP_outlier_score.

```
# Define a function to calculate outlier scores
calculate_outlier_scores <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(
      APC_outlier_score = ifelse(length(neighbours) > 0, abs(APC - mean(data$APC[neighbours])), NA),
      LP_outlier_score = ifelse(length(neighbours) > 0, abs(LP - mean(data$LP[neighbours])), NA),
      PDP_outlier_score = ifelse(length(neighbours) > 0, abs(PDP - mean(data$PDP[neighbours])), NA),
      NNPP_outlier_score = ifelse(length(neighbours) > 0, abs(NNPP - mean(data$NNPP[neighbours])), NA)
    )
  return(data)
}

# Apply the function to the dataset
geocoded_data <- calculate_outlier_scores(geocoded_data)

# Display the first few rows to ensure outlier scores are calculated
head(geocoded_data)
```
# EXPORTING THE DATA
```
# Remove the 'neighbours' column
geocoded_data_no_neighbours <- geocoded_data %>%
  select(-neighbours)

# Sort the dataset by outlier scores for each party
sorted_data_APC <- geocoded_data_no_neighbours %>% arrange(desc(APC_outlier_score))
sorted_data_LP <- geocoded_data_no_neighbours %>% arrange(desc(LP_outlier_score))
sorted_data_PDP <- geocoded_data_no_neighbours %>% arrange(desc(PDP_outlier_score))
sorted_data_NNPP <- geocoded_data_no_neighbours %>% arrange(desc(NNPP_outlier_score))

# Save the sorted datasets
write.csv(sorted_data_APC, "sorted_data_APC.csv", row.names = FALSE)
write.csv(sorted_data_LP, "sorted_data_LP.csv", row.names = FALSE)
write.csv(sorted_data_PDP, "sorted_data_PDP.csv", row.names = FALSE)
write.csv(sorted_data_NNPP, "sorted_data_NNPP.csv", row.names = FALSE)

## Export geo_data
# Convert the neighbours list column to a character column
geocoded_data$neighbours <- sapply(geocoded_data$neighbours, function(x) paste(x, collapse = ","))

# Now write the data frame to a CSV file
write.csv(geocoded_data, "geocoded_data.csv", row.names = FALSE)
```

### Sorting and Reporting
Sorting: The dataset was sorted by the outlier scores for each party to identify the most significant outliers.
Top Outliers Identification: The top 3 outliers for each party were identified, along with their closest neighbouring polling units.

# FINDINGS
Summary of Outlier Scores
The following table summarizes the top 3 outlier polling units for each party, along with their respective outlier scores and neighbouring polling units:


# CONCLUSION
My analysis revealed significant outliers in the voting results for each party, suggesting potential irregularities in the election process. By identifying these outliers, we have highlighted areas that warrant further investigation to ensure election integrity. The geospatial approach provided a robust method for detecting deviations and enhancing transparency in the electoral process.





