#---------------------------
#---------------------------


# Preliminaries -- import data & packages
#---------------------------

# importing packages
require( dplyr )
require( lubridate )
require( geofacet )
require( ggplot2 )
require( scales )

# importing data
# training.data <- read.csv('../input/train.csv', header = TRUE, stringsAsFactors = FALSE)
# testing.data <- read.csv('../input/test.csv', header = TRUE, stringsAsFactors = FALSE)

zillow.data <- read.csv( 'State_time_series.csv' , header = T , stringsAsFactors = F )

# Define specific color scheme
#---------------------------
# dark.gold = rgb( 151/255 , 138/255 , 63/255 )
# light.gold = rgb( 246/255 , 180/255 , 14/255 )
# navy.blue = rgb( 10/255 , 55/255 , 130/255 )
# navy.black = rgb( 39/255 , 41/255 , 39/255 )
# silver.gray = rgb( 209/255 , 212/255 , 211/255 )
# navy.white = rgb( 251/255 , 255/255 , 255/255 )
# navy.red = rgb( 174/255 , 27/255 , 33/255 )
# navy.brown = rgb( 109/255 , 48/255 , 17/255 )

zillow.blue = rgb( 85/255, 119/255, 187/255 , alpha = 0.25 )
zillow.green = rgb( 136/255, 187/255, 68/255 , alpha = 1 )


# Filter & Clean the Data
#---------------------------

# we only want to plot one of the columns
working.data <- zillow.data[ which( zillow.data$RegionName != 'UnitedStates' )
                             , c( 'Date'
                                  , 'RegionName'
                                  , 'MedianRentalPrice_AllHomes') ]

working.data$Date <- working.data$Date %>% as.Date

# remove any miss values
working.data <- na.omit( working.data )

# need to fix all the state names with just two words
working.data$RegionName <- sapply( working.data$RegionName , FUN = function(state) {
  if( state == 'NewYork' ) { 'New York' } else
    if( state == 'SouthDakota' ) { 'South Dakota' } else
      if( state == 'NorthDakota' ) { 'North Dakota' } else
        if( state == 'NewHampshire' ) { 'New Hampshire' } else
          if( state == 'NewJersey' ) { 'New Jersey' } else
            if( state == 'NewMexico' ) { 'New Mexico' } else
              if( state == 'NorthCarolina' ) { 'North Carolina' } else
                if( state == 'RhodeIsland' ) { 'Rhode Island' } else
                  if( state == 'SouthCarolina' ) { 'South Carolina' } else
                    if( state == 'WestVirginia' ) { 'West Virginia' } else
                      if( state == 'DistrictofColumbia' ) { 'District of Columbia' } else
                      { state }
})

# Make Our State Grid Plot
#---------------------------
# aes(colour=Bgrnd_All), size =1)

quartz()
ggplot(  working.data  , aes(  Date ,  MedianRentalPrice_AllHomes ) ) + 
  theme(  axis.text=element_text(size = 6)
          , strip.text.x = element_text(size = 4)
          , strip.text = element_text(color = 'black' )
          , strip.background = element_rect( fill = zillow.blue )
          , panel.background = element_rect( fill = zillow.blue ,
                                             colour = zillow.blue ,
                                             size = 0.1, linetype = "solid")
          
  )  + 
  geom_line( color = zillow.green  ) +
  scale_color_discrete(  guide = FALSE ) +
  facet_geo( ~ RegionName, grid = "us_state_grid2", label = "name") +
  scale_y_continuous(
    labels = NULL
    , limits = c(0,4000)
  ) +
  labs( #title = "Median Rental Price All Homes",
    caption = "Source: Zillow",
    x = "Year",
    y = "Median Rental Price All Homes") +
  scale_x_date(date_breaks = "2 years", 
               labels=date_format("%y"),
               limits = as.Date(c('2010-01-01','2017-01-01'))
  )  