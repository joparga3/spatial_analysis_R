library(tmap)
library(plotly)
library(ggthemes)
library(leaflet)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(spdep)
library(ggplot2)
library(sqldf)

# ---------------------------------------------------------------------------------------------------------
# Load data price sold data
# ---------------------------------------------------------------------------------------------------------

driver = JDBC(driverClass = 'org.postgresql.Driver', '/home/DS/postgresql-42.0.0.jre6.jar')
connection = dbConnect(driver, 'jdbc:postgresql://localhost:5432/DS03_DB', 'ds_user', 'netezzaBsl0w')
d_raw = dbGetQuery(connection, 'SELECT * FROM lookup_zz_final_tables.land_registry')
dbDisconnect(connection)

d_raw = as.data.frame(d_raw)

head(d_raw)

# Analysing the last 5 years
# 1 subset for the past 5 years
# 2 take the mean of the prices for those 5 years per lsoa code
d = subset(d_raw, year >= 2012 & year <= 2017)
any(is.na(d$clean_lsoa_2011))

nrow(d) # 207884

d_1 = d %>% group_by(clean_lsoa_2011) %>% dplyr::summarize(Mean = mean(avg_price, na.rm = TRUE))
d_1 = as.data.frame(d_1)
# d_1 = na.omit(d_1)
head(d_1)

nrow(d_1) # 34751

# not unique LSOA codes
df = sqldf('SELECT clean_lsoa_2011, count(*) FROM d_1 GROUP BY clean_lsoa_2011 HAVING count(*) > 1') 
df

# ---------------------------------------------------------------------------------------------------------
# Load LSOA layers
# ---------------------------------------------------------------------------------------------------------

# load data
lsoa = readOGR("/home/DS/DE/Lookup/UK_Shapefiles_Census_Boundaries_data/Data Sources/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp")
lsoa$geo_code = as.character(lsoa$geo_code)
lsoa$geo_label = as.character(lsoa$geo_label)
lsoa$geo_labelw = NULL

nrow(lsoa) # 42619 -> clearly more LSOA layers than we have in our sold houses dataset.
length(unique(lsoa$geo_code)) # 42619 - unique codes, which makes me wonder why the house prices dataset have got duplicate LSOA layers

# Due to this, we will be performing INNER JOINS if we have to merge both datasets. Before doing that, let's check if there are any
# areas in the house prices dataset that are not present in the LSOA dataset. Hopefully, this will not be the case and the areas in
# the house prices are a subset of the entire LSOA layer dataset.
subset(d_1, !(clean_lsoa_2011 %in% unique(lsoa$geo_code))) # All ok to perform inner join

# ---------------------------------------------------------------------------------------------------------
# Load LSOA neighbours
# This dataset is a list where, for each area, you have the neighbour areas
# ---------------------------------------------------------------------------------------------------------

lsoa_nb = readRDS(file = '/home/DS/DE/Lookup/UK_Shapefiles_Census_Boundaries_data/Data Sources/infuse_lsoa_lyr_2011_clipped/polygon_neighbours.RData')

# ---------------------------------------------------------------------------------------------------------
# Join the house price data with the LSOA layers
#   The only problem with the join is that we only have the LSOA geo_code, and not the full LSOA and MSOA layers
#   like in the house prices. Let's double check what happens with this join.
# ---------------------------------------------------------------------------------------------------------
LSOA.house_prices = merge(x = lsoa, y = d_1, by.x = c("geo_code"), by.y = c("clean_lsoa_2011"))

names(LSOA.house_prices)[names(LSOA.house_prices) == 'geo_code'] = 'lsoa_code'
names(LSOA.house_prices)[names(LSOA.house_prices) == 'geo_label'] = 'lsoa_name'
names(LSOA.house_prices)[names(LSOA.house_prices) == 'Mean'] = 'lsoa_avg_price_2012_2017'




# As you can see, there are records with NAs. We will exclude those areas from the analysis.
summary(LSOA.house_prices)

# Excluding the areas that are NA
LSOA.house_prices = LSOA.house_prices[-which(is.na(LSOA.house_prices$lsoa_avg_price_2012_2017)),]
LSOA.house_prices_sp = LSOA.house_prices

summary(LSOA.house_prices) # 34751 areas

head(LSOA.house_prices)

# ---------------------------------------------------------------------------------------------------------
# Running a spatial autocorrelation
# ---------------------------------------------------------------------------------------------------------
# A spatial autocorrelation measures how distance influences a particular variable. In other words, it quantifies the degree of 
# which objects are similar to nearby objects. Variables are said to have a positive spatial autocorrelation when similar values 
# tend to be nearer together than dissimilar values.
# Waldo Tober’s first law of geography is that “Everything is related to everything else, but near things are more related than 
# distant things.” so we would expect most geographic phenomena to exert a spatial autocorrelation of some kind. In population 
# data this is often the case as persons with similar characteristics tend to reside in similar neighbourhoods due to a range 
# of reasons including house prices, proximity to workplaces and cultural factors.
# We will be using the spatial autocorrelation functions available from the spdep package.

# Finding neighbours
# ---------------------------------------------------------------------------------------------------------
# In order for the subsequent model to work, we need to work out what polygons neighbour each other. We have already done this running
# the following commented code. The results have been saved in a data object that we have loaded above.

# start = Sys.time()
# all_nb = poly2nb(lsoa, queen=TRUE, row.names = lsoa$geo_code) 
# saveRDS(all_nb, file = '/home/DS/DE/Lookup/UK_Shapefiles_Census_Boundaries_data/Data Sources/infuse_lsoa_lyr_2011_clipped/polygon_neighbours.RData')
# Sys.time() - start

# Running a global spatial autocorrelation
# ---------------------------------------------------------------------------------------------------------
# With the neighbours defined. We can now run a model. First, we need to convert the data types of the neighbours object. 
# This file will be used to determine how the neighbours are weighted

# Convert the neighbour data to a listw object
listw = nb2listw(lsoa_nb, zero.policy=TRUE)
listw2 = nb2listw(lsoa_nb, zero.policy=TRUE, style = "W")

# global spatial autocorrelation -> moran statistic is 0.7 - which is basically no correlation. This can perfectly be because of 
# the amount of areas we are considering
moran.test(LSOA.house_prices$lsoa_avg_price_2012_2017, listw, na.action = na.omit, zero.policy=TRUE)

# ---------------------------------------------------------------------------------------------------------
# Local autocorrelation example - Let's filter the data for areas in Wandsworth. - EXAMPLE!!
# ---------------------------------------------------------------------------------------------------------
areas_to_subset = sqldf("SELECT clean_lsoa_2011 FROM d WHERE msoa_name LIKE '%Wandsworth%'")
areas_to_subset = areas_to_subset[,1]

# Subsetting data to plot less areas
LSOA.house_prices_small = subset(LSOA.house_prices, lsoa_code %in% areas_to_subset)

# plotting only for that area
tm_shape(LSOA.house_prices_small) + tm_fill("lsoa_avg_price_2012_2017"
                                            , palette = "Reds"
                                            , style = "quantile"
                                            , title = "Avg price") + tm_borders(alpha=.4)

listw2 = nb2listw(poly2nb(LSOA.house_prices_small, queen=TRUE, row.names = LSOA.house_prices_small$lsoa_code)
                  , zero.policy=TRUE, style = "W")
local = localmoran(x = LSOA.house_prices_small$lsoa_avg_price_2012_2017, listw = listw2)
head(local)
class(local)

# By considering the help page for the localmoran function (run ?localmoran in R) we can observe the arguments and outputs. We get a number of useful statistics from the model which are as defined:
#
# Name	  Description
# Ii	    local moran statistic
# E.Ii	  expectation of local moran statistic
# Var.Ii	variance of local moran statistic
# Z.Ii	  standard deviate of local moran statistic
# Pr()	  p-value of local moran statistic

# First, we will map the local moran statistic (Ii). A positive value for Ii indicates that the unit is surrounded by units with similar values.

# binds results to our polygon shapefile
moran.map = cbind(LSOA.house_prices_small, local)
head(moran.map)

# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

# From the map, it is possible to observe the variations in autocorrelation across space. We can interpret that there seems to be
# a geographic pattern to the autocorrelation. However, it is not possible to understand if these are clusters of high or low values.
# Why not try to make a map of the P-value to observe variances in significance across Wandsworth? Use names(moran.map@data)
# to find the column headers.
# One thing we could try to do is to create a map which labels the features based on the types of relationships they share with
# their neighbours (i.e. high and high, low and low, insignificant, etc…)

# to create LISA cluster map - just creates a vector of 0s with length equal to the lsoa_codes we have
quadrant = vector(mode="numeric", length=nrow(local))
LSOA.house_prices_small$quadrant_numeric = quadrant
head(quadrant)
length(quadrant)

# centers the variable of interest around its mean
m.avg_price = LSOA.house_prices_small$lsoa_avg_price_2012_2017 - mean(LSOA.house_prices_small$lsoa_avg_price_2012_2017)
LSOA.house_prices_small$lsoa_avg_price_2012_2017_around_mean = LSOA.house_prices_small$lsoa_avg_price_2012_2017 - mean(LSOA.house_prices_small$lsoa_avg_price_2012_2017)

# centers the local Moran's around the mean
m.local = local[,1] - mean(local[,1])
LSOA.house_prices_small$local_moran_stat = local[,1]
LSOA.house_prices_small$local_moran_stat_around_mean = local[,1] - mean(local[,1])

head(LSOA.house_prices_small)

# Now we assign 5 different groups comparing both the variable of interest and the local moran statistic.
# The idea is to identify groups of clusters. For example:
#   - if the avg price is higher than average and also has a high correlation with it's neighbours, we have found a cluster of high value

# local area price above average price of borough & local moran stat above average local moran stat - highest value
quadrant[LSOA.house_prices_small$lsoa_avg_price_2012_2017_around_mean > 0 & LSOA.house_prices_small$local_moran_stat_around_mean > 0] = 4
# local area price above average price of borough & local moran stat above average local moran stat - mid highest value.
quadrant[LSOA.house_prices_small$lsoa_avg_price_2012_2017_around_mean > 0 & LSOA.house_prices_small$local_moran_stat_around_mean < 0] = 3
# local area price below average price of borough & local moran stat above average local moran stat - mid lowest value.
quadrant[LSOA.house_prices_small$lsoa_avg_price_2012_2017_around_mean < 0 & LSOA.house_prices_small$local_moran_stat_around_mean > 0] = 2
# local area price below average price of borough & local moran stat below average local moran stat - lowest value
quadrant[LSOA.house_prices_small$lsoa_avg_price_2012_2017_around_mean < 0 & LSOA.house_prices_small$local_moran_stat_around_mean < 0] = 1

# significance threshold - this is used to detect insignificant areas based on the p-values we have from local moran statistic. The lower the significance level
# the more restrictive and sure we will be about our results.
# We assign a 0 to anything that is not statistically significant
signif = 0.1
quadrant[local[,5]>signif] = 0

LSOA.house_prices_small$quadrant_numeric = quadrant
LSOA.house_prices_small$quadrant_category = ifelse(LSOA.house_prices_small$quadrant_numeric == 0, 'Statistically insignificant'
                                                   , ifelse(LSOA.house_prices_small$quadrant_numeric == 1, 'Area price below average - Negatively correlated with neighbouring areas'
                                                            , ifelse(LSOA.house_prices_small$quadrant_numeric == 2, 'Area price below average - Positively correlated with neighbouring areas'
                                                                     , ifelse(LSOA.house_prices_small$quadrant_numeric == 3, 'Area price above average - Negatively correlated with neighbouring areas'
                                                                              , ifelse(LSOA.house_prices_small$quadrant_numeric == 4, 'Area price above average - Positively correlated with neighbouring areas'
                                                                                       ,LSOA.house_prices_small$quadrant_numeric
                                                                              )))))

table(LSOA.house_prices_small$quadrant_category)

# plot in r
brks = c(0,1,2,3,4)
colors = c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(LSOA.house_prices_small, border="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
legend("bottomleft",legend=c("Statistically insignificant"
                             ,"Area price below average \n - Negatively correlated with neighbouring areas"
                             ,"Area price below average \n - Positively correlated with neighbouring areas"
                             ,"Area price above average \n - Negatively correlated with neighbouring areas"
                             ,"Area price above average \n - Positively correlated with neighbouring areas"),
       fill=colors,bty="n")


# -------------------------------------------------------------------------------------------------------------
# As we are happy with the result shown in the example, we need to apply this categorization to all the dataset
# All the following metrics are being applied to the whole dataset, so the distribution calculated will be 
# the distribution using the whole UK dataset. This is fine for the moment, but we want to get to a local analysis.
# You will see that after the this code section
# -------------------------------------------------------------------------------------------------------------
# Convert the neighbour data to a listw object
start_time = Sys.time()
listw2 = nb2listw(poly2nb(LSOA.house_prices, queen=TRUE, row.names = LSOA.house_prices$lsoa_code)
                  , zero.policy=TRUE, style = "W")
Sys.time() - start_time # ~5min

local = localmoran(x = LSOA.house_prices$lsoa_avg_price_2012_2017, listw = listw2, na.action = na.omit, zero.policy=TRUE)

# binds results to our polygon shapefile
moran.map = cbind(LSOA.house_prices, local)
head(moran.map)

head(LSOA.house_prices)

# centers the variable of interest around its mean
m.avg_price = LSOA.house_prices$lsoa_avg_price_2012_2017 - mean(LSOA.house_prices$lsoa_avg_price_2012_2017)     

LSOA.house_prices = as.data.frame(LSOA.house_prices)
LSOA.house_prices = LSOA.house_prices %>% mutate(quantile = ntile(lsoa_avg_price_2012_2017,5))
names(LSOA.house_prices)[names(LSOA.house_prices) == 'quantile'] = 'global_distribution_avg_price_quantile'

LSOA.house_prices$global_spatial_distribution_mean_avg_price_2012_2017 = mean(LSOA.house_prices$lsoa_avg_price_2012_2017)


# centers the local Moran's around the mean
m.local = local[,1] - mean(local[,1])    
LSOA.house_prices$global_spatial_distribution_local_moran = local[,1]
LSOA.house_prices = LSOA.house_prices %>% mutate(quantile = ntile(global_spatial_distribution_local_moran,5))
names(LSOA.house_prices)[names(LSOA.house_prices) == 'quantile'] = 'global_spatial_distribution_local_moran_quantile'
LSOA.house_prices$global_spatial_distribution_mean_local_moran = mean(local[,1])
LSOA.house_prices$global_spatial_distribution_local_moran_pvalue = local[,5]

# to create LISA cluster map - just creates a vector of 0s with length equal to the lsoa_codes we have
quadrant = vector(mode="numeric", length=nrow(local))
LSOA.house_prices$global_spatial_distribution_quadrant_numeric = quadrant
head(quadrant)
length(quadrant)

# Now we assign 5 different groups comparing both the variable of interest and the local moran statistic.
# The idea is to identify groups of clusters. For example:
#   - if the avg price is higher than average and also has a high correlation with it's neighbours, we have found a cluster of high value

# local area price above average price of borough & local moran stat above average local moran stat - highest value
quadrant[LSOA.house_prices$lsoa_avg_price_2012_2017 - LSOA.house_prices$global_spatial_distribution_mean_avg_price_2012_2017 > 0 
         & LSOA.house_prices$global_spatial_distribution_local_moran - LSOA.house_prices$global_spatial_distribution_mean_local_moran > 0] = 4  
# local area price above average price of borough & local moran stat above average local moran stat - mid highest value. 
quadrant[LSOA.house_prices$lsoa_avg_price_2012_2017 - LSOA.house_prices$global_spatial_distribution_mean_avg_price_2012_2017 > 0 
         & LSOA.house_prices$global_spatial_distribution_local_moran - LSOA.house_prices$global_spatial_distribution_mean_local_moran < 0] = 3
# local area price below average price of borough & local moran stat above average local moran stat - mid lowest value. 
quadrant[LSOA.house_prices$lsoa_avg_price_2012_2017 - LSOA.house_prices$global_spatial_distribution_mean_avg_price_2012_2017 < 0 
         & LSOA.house_prices$global_spatial_distribution_local_moran - LSOA.house_prices$global_spatial_distribution_mean_local_moran > 0] = 2
# local area price below average price of borough & local moran stat below average local moran stat - lowest value
quadrant[LSOA.house_prices$lsoa_avg_price_2012_2017 - LSOA.house_prices$global_spatial_distribution_mean_avg_price_2012_2017 < 0 
         & LSOA.house_prices$global_spatial_distribution_local_moran - LSOA.house_prices$global_spatial_distribution_mean_local_moran < 0] = 1   

# significance threshold - this is used to detect insignificant areas based on the p-values we have from local moran statistic. The lower the significance level
# the more restrictive and sure we will be about our results. 
# We assign a 0 to anything that is not statistically significant
signif = 0.1 
# quadrant[local[,5]>signif] = 0  
quadrant[LSOA.house_prices$global_spatial_distribution_local_moran_pvalue>signif] = 0

LSOA.house_prices$global_spatial_distribution_quadrant_numeric = quadrant
LSOA.house_prices$global_spatial_distribution_quadrant_category = ifelse(LSOA.house_prices$global_spatial_distribution_quadrant_numeric == 0, 'Statistically insignificant'
                                             , ifelse(LSOA.house_prices$global_spatial_distribution_quadrant_numeric == 1, 'Area price below average - Negatively correlated with neighbouring areas'
                                                      , ifelse(LSOA.house_prices$global_spatial_distribution_quadrant_numeric == 2, 'Area price below average - Positively correlated with neighbouring areas'
                                                               , ifelse(LSOA.house_prices$global_spatial_distribution_quadrant_numeric == 3, 'Area price above average - Negatively correlated with neighbouring areas'
                                                                        , ifelse(LSOA.house_prices$global_spatial_distribution_quadrant_numeric == 4, 'Area price above average - Positively correlated with neighbouring areas'
                                                                                 ,LSOA.house_prices$global_spatial_distribution_quadrant_numeric
                                                                        )))))

head(LSOA.house_prices)

# Dataframe backup

LSOA.house_prices1 = LSOA.house_prices
LSOA.house_prices1 = as.data.frame(LSOA.house_prices1)

# write.csv(LSOA.house_prices1, '/home/DS/DE/Lookup/Land_registry_data/land_registry_geospatial_analysis.csv')


# ----------------------------------------------------------------------------------------------------------------------------------
# Trying to perform the same analysis as before but at a local focus
#
# STEPS:
# 1. Get the unique MSOA name layers grouped. For example layer 001, layer 002 will be grouped as layer.
#   1.1. To do this we use a regex expression get the MSOA name without numbers
#
# 2. Once we have those, we will loop through each one of these areas and perform the following actions:
#   2.1. Subset the big dataframe to the rows that match the MSOA unique. Basically we use a regex expression that will look for
#        a pattern matching the unique MSOA name, and pick the subset of the big dataset for those matching records.
#   2.2. Once we have the dataset subsetted with the matching MSOA layers, we get the information from the LSOA + house sold prices
#        information that contains the LSOA layers within the MSOA layer of interest.
#   2.3. Then we calculate the 5 quantiles for the average sold prices in that local MSOA layer. So for example, you will see LSOA layers
#        that have a global average sold price quantile of 5 in London, meaning that, compared to the rest of the country this area
#        is top in sold house prices, but then it might be a 3 locally, which means that this area is less expensive when compared to 
#        it's neighbours.
#
# ----------------------------------------------------------------------------------------------------------------------------------
# 1.1
MSOA_unique = unique(gsub("[?!^0-9\\]", "", d$msoa_name))
MSOA_unique = sort(MSOA_unique)
# MSOA_unique = MSOA_unique[68:70]
# MSOA_unique = c("Wyre ","Wyre Forest ")

# txt = c("Wyre ","Wyre Forest ")
# grep("Wyre ",txt)

rm(LSOA_house_prices3)

for(i in 1:length(MSOA_unique)){
  
  print(paste("MSOA layer:",i))
  print(MSOA_unique[i])
  print("")
  
  # 2.1. Subset the dataset for the MSOA layers
  # Given that we are using regular expressions, we need to be careful with a specific area. 
  # The regular expression will look for a pattern. There are 2 areas that share a common pattern = "Wyre".
  # Therefore, in order to now have duplication, if we are analysing "Wyre", we will ensure the "Wyre Forest" records are taking out of
  # the subset we are going to analyse.
  # Any other record doesn't need this consideration.
  if(MSOA_unique[i] == "Wyre "){
    d1 = d[-grep("Wyre Forest",d$msoa_name),]
    d1 = d[grep(as.character(MSOA_unique[i]),d1$msoa_name),]
  }else{
    d1 = d[grep(as.character(MSOA_unique[i]),d$msoa_name),]
  }
  
  # 2.2. Get the Spatial Dataframe for the LSOA layers that fall within the MSOA layer
  LSOA.house_prices2 = subset(LSOA.house_prices1, lsoa_code %in% d1$clean_lsoa_2011
                                                  & lsoa_name %in% d1$lsoa_name)
  LSOA.house_prices2 = as.data.frame(LSOA.house_prices2)
  
  # 2.3. Calculate the 5 group quantiles for the avg price
  LSOA.house_prices2$local_spatial_distribution_mean_avg_price_2012_2017 = mean(LSOA.house_prices2$lsoa_avg_price_2012_2017)
  LSOA.house_prices2 = LSOA.house_prices2 %>% mutate(quantile = ntile(lsoa_avg_price_2012_2017,5))
  names(LSOA.house_prices2)[names(LSOA.house_prices2) == 'quantile'] = 'local_distribution_avg_price_quantile'
  
  # 2.4. Calculating the moran statistic for a specific area and appending to dataframe
  # - Subsetting the spatial polygon dataframe for only the area we are analysing
  # - Calculate the neighbours for that specific area
  # - Calculate the moran statistc for that area
  # - Append the information to the dataframe
  
  LSOA.house_pricesw2 = subset(LSOA.house_prices_sp, lsoa_code %in% as.character(d1$clean_lsoa_2011))
  
  if(MSOA_unique[i] %in% c("Coldstream and Area","Isles of Scilly ")){
    
    local = 0
    LSOA.house_prices2$local_spatial_distribution_local_moran = local
    LSOA.house_prices2 = LSOA.house_prices2 %>% mutate(quantile = ntile(local_spatial_distribution_local_moran,5))
    names(LSOA.house_prices2)[names(LSOA.house_prices2) == 'quantile'] = 'local_spatial_distribution_local_moran_quantile'
    LSOA.house_prices2$local_spatial_distribution_mean_local_moran = mean(local)
    LSOA.house_prices2$local_spatial_distribution_local_moran_pvalue = 1
    
  } else {
    listw2 = nb2listw(poly2nb(LSOA.house_pricesw2, queen=TRUE, row.names = LSOA.house_pricesw2$lsoa_code)
                      , zero.policy=TRUE, style = "W")
    local = localmoran(x = LSOA.house_pricesw2$lsoa_avg_price_2012_2017, listw = listw2, na.action = na.omit, zero.policy=TRUE) 
    LSOA.house_prices2$local_spatial_distribution_local_moran = local[,1]
    LSOA.house_prices2 = LSOA.house_prices2 %>% mutate(quantile = ntile(local_spatial_distribution_local_moran,5))
    names(LSOA.house_prices2)[names(LSOA.house_prices2) == 'quantile'] = 'local_spatial_distribution_local_moran_quantile'
    LSOA.house_prices2$local_spatial_distribution_mean_local_moran = mean(local[,1])
    LSOA.house_prices2$local_spatial_distribution_local_moran_pvalue = local[,5]
  }
  
  # to create LISA cluster map - just creates a vector of 0s with length equal to the lsoa_codes we have
  quadrant = vector(mode="numeric", length=nrow(LSOA.house_prices2))
  LSOA.house_prices2$local_spatial_distribution_quadrant_numeric = quadrant
  
  # local area price above average price of borough & local moran stat above average local moran stat - highest value
  quadrant[LSOA.house_prices2$lsoa_avg_price_2012_2017 - LSOA.house_prices2$local_spatial_distribution_mean_avg_price_2012_2017 > 0 
           & LSOA.house_prices2$local_spatial_distribution_local_moran - LSOA.house_prices2$local_spatial_distribution_mean_local_moran > 0] = 4  
  # local area price above average price of borough & local moran stat above average local moran stat - mid highest value. 
  quadrant[LSOA.house_prices2$lsoa_avg_price_2012_2017 - LSOA.house_prices2$local_spatial_distribution_mean_avg_price_2012_2017 > 0 
           & LSOA.house_prices2$local_spatial_distribution_local_moran - LSOA.house_prices2$local_spatial_distribution_mean_local_moran < 0] = 3
  # local area price below average price of borough & local moran stat above average local moran stat - mid lowest value. 
  quadrant[LSOA.house_prices2$lsoa_avg_price_2012_2017 - LSOA.house_prices2$local_spatial_distribution_mean_avg_price_2012_2017 < 0 
           & LSOA.house_prices2$local_spatial_distribution_local_moran - LSOA.house_prices2$local_spatial_distribution_mean_local_moran > 0] = 2
  # local area price below average price of borough & local moran stat below average local moran stat - lowest value
  quadrant[LSOA.house_prices2$lsoa_avg_price_2012_2017 - LSOA.house_prices2$local_spatial_distribution_mean_avg_price_2012_2017 < 0 
           & LSOA.house_prices2$local_spatial_distribution_local_moran - LSOA.house_prices2$local_spatial_distribution_mean_local_moran < 0] = 1   
  
  # significance threshold - this is used to detect insignificant areas based on the p-values we have from local moran statistic. The lower the significance level
  # the more restrictive and sure we will be about our results. 
  # We assign a 0 to anything that is not statistically significant
  signif = 0.1 
  quadrant[LSOA.house_prices2$local_spatial_distribution_local_moran_pvalue>signif] = 0  
  
  LSOA.house_prices2$local_spatial_distribution_quadrant_numeric = quadrant
  LSOA.house_prices2$local_spatial_distribution_quadrant_category = ifelse(LSOA.house_prices2$local_spatial_distribution_quadrant_numeric == 0, 'Statistically insignificant'
                                                                           , ifelse(LSOA.house_prices2$local_spatial_distribution_quadrant_numeric == 1, 'Area price below average - Negatively correlated with neighbouring areas'
                                                                                    , ifelse(LSOA.house_prices2$local_spatial_distribution_quadrant_numeric == 2, 'Area price below average - Positively correlated with neighbouring areas'
                                                                                             , ifelse(LSOA.house_prices2$local_spatial_distribution_quadrant_numeric == 3, 'Area price above average - Negatively correlated with neighbouring areas'
                                                                                                      , ifelse(LSOA.house_prices2$local_spatial_distribution_quadrant_numeric == 4, 'Area price above average - Positively correlated with neighbouring areas'
                                                                                                               ,LSOA.house_prices2$local_spatial_distribution_quadrant_numeric
                                                                                                      )))))
  
  # Append results
  if(i == 1){
    LSOA_house_prices3 = LSOA.house_prices2
  } else {
    LSOA_house_prices3 = rbind(LSOA_house_prices3, LSOA.house_prices2)  
  }
  
}

head(LSOA_house_prices3)

# backup of dataset
LSOA_house_prices4 = LSOA_house_prices3

# Difference between categories of quadrant category if we analyse the dataset for the whole of the UK or at a local
# MSOA layer
table(LSOA_house_prices4$global_spatial_distribution_quadrant_category)
table(LSOA_house_prices4$local_spatial_distribution_quadrant_category)

# Adding a column to check which areas have the same behaviour globally and locally
LSOA_house_prices4$same_global_local_behaviour = ifelse(LSOA_house_prices4$global_spatial_distribution_quadrant_category == LSOA_house_prices4$local_spatial_distribution_quadrant_category
                                                        , "Y", "N")

# Finally, adding some type of score

# ----------------------------------------------------------------------------------------------------------------------------------
# Saving the datasets
# ----------------------------------------------------------------------------------------------------------------------------------
summary(LSOA_house_prices4)
LSOA_house_prices4$global_spatial_distribution_local_moran_pvalue = ifelse(is.na(LSOA_house_prices4$global_spatial_distribution_local_moran_pvalue),1,LSOA_house_prices4$global_spatial_distribution_local_moran_pvalue)
# write.csv(LSOA_house_prices4, '/home/DS/DE/Lookup/Land_registry_data/land_registry_geospatial_analysis.csv', quote = TRUE, row.names = FALSE)



# ----------------------------------------------------------------------------------------------------------------------------------
# maps the results - because we cant plot 42k regions quickly in rstudio, let's plot 2 neighbouring areas from the dataset
# ----------------------------------------------------------------------------------------------------------------------------------
toMatch = c('Wandsworth','Lambeth', 'Eastbourne','Adur')
toMatch = c('Adur', 'Allerdale')
lsoa_information = d_raw[grep(paste(toMatch,collapse="|"),d_raw$lsoa_name),]
lsoa_polygons = lsoa[grep(paste(toMatch,collapse="|"), lsoa$geo_label),]

toMatch = 'Adur'
length(unique(d_raw[grep(paste(toMatch,collapse="|"),d_raw$lsoa_name),c('lsoa_name')])) # 42


# LSOA.house_prices_small = subset(LSOA_house_prices4, lsoa_code %in% areas_to_subset)
LSOA.house_prices_small = merge(x = lsoa_polygons, y = unique(lsoa_information[,c('clean_lsoa_2011'
                                                                   ,'lsoa_name'
                                                                   ,'lsoa_avg_price_2012_2017'
                                                                   ,'global_distribution_avg_price_quantile'
                                                                   ,'local_distribution_avg_price_quantile'
                                                                   ,'global_distribution_avg_price_quantile_category'
                                                                   ,'local_distribution_avg_price_quantile_category')])
                                , by.x = c("geo_code","geo_label")
                                , by.y = c("clean_lsoa_2011","lsoa_name"))

head(LSOA.house_prices_small)

# Change the projections so that it can match the base map projection
pj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
LSOA.house_prices_small = spTransform(LSOA.house_prices_small, pj)

# Create a color palette
# pal = colorFactor("YlOrRd", levels = as.factor(c("Cheap","Mid-Cheap","Mid-Expensive","Mid","Expensive")))
pal = colorBin("YlOrRd", c(1,2,3,4,5), bins = 10)

ttt = previewColors(pal,LSOA.house_prices_small$global_distribution_avg_price_quantile)


# rrr = ttt[[3]][3]$children

color_list = c("")
# Create a list of colors for the palette
for(i in 1:length(ttt[[3]][3]$children[[1]])){
  color_list = c(color_list,names(ttt[[3]][3]$children[[1]][i]))
}
color_list = color_list[2:length(color_list)]


m = leaflet() %>% 
    addProviderTiles(provider = 'CartoDB.Positron') %>%
    setView(lng = -1.34, lat = 53.3, zoom = 6) 
m = m %>% addPolygons(data = LSOA.house_prices_small
                      , weight = 0.5
                      , fillOpacity = 1
                      , opacity = 1
                      , color = "black"
                      #, fillColor = ~pal(global_distribution_avg_price_quantile)) %>%
                      , fillColor = color_list) %>%
          addLegend(pal = pal
              , values = LSOA.house_prices_small$global_distribution_avg_price_quantile
              , title = "Avg price 2012-2017"
              )   


m


ttt1 = previewColors(pal,LSOA.house_prices_small$local_distribution_avg_price_quantile)

color_list1 = c("")
# Create a list of colors for the palette
for(i in 1:length(ttt1[[3]][3]$children[[1]])){
  color_list1 = c(color_list1,names(ttt1[[3]][3]$children[[1]][i]))
}
color_list1 = color_list1[2:length(color_list1)]

m1 = leaflet() %>% 
  addProviderTiles(provider = 'CartoDB.Positron') %>%
  setView(lng = -1.34, lat = 53.3, zoom = 6) 
m1 = m1 %>% addPolygons(data = LSOA.house_prices_small
                      , weight = 0.5
                      , fillOpacity = 1
                      , opacity = 1
                      , color = "black"
                      #, fillColor = ~pal(global_distribution_avg_price_quantile)) %>%
                      , fillColor = color_list1) %>%
  addLegend(pal = pal
            , values = LSOA.house_prices_small$local_distribution_avg_price_quantile
            , title = "Avg price 2012-2017"
  )   


m1






# moran
small_subset = subset(moran.map, lsoa_code %in% areas_to_subset)


tm_shape(small_subset) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 

# quadrants
brks = c(0,1,2,3,4)
colors = c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(LSOA.house_prices_small, border="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
legend("topleft",legend=c("Statistically insignificant"
                             ,"Area price below average \n - Negatively correlated with neighbouring areas \n"
                             ,"Area price below average \n - Positively correlated with neighbouring areas \n"
                             ,"Area price above average \n - Negatively correlated with neighbouring areas \n"
                             ,"Area price above average \n - Positively correlated with neighbouring areas \n"),
       fill=colors,bty="n")


head(LSOA.house_prices)


# # plotting only for that area - tmmap
# m = tm_shape(LSOA.house_prices_small) + tm_fill("lsoa_avg_price_2012_2017"
#                                             , palette = "Reds"
#                                             , style = "quantile"
#, title = "Avg price") + tm_borders(alpha=.4) 
