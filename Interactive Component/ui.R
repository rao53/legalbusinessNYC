library(tidyr)  # loads dplyr, tidyr, ggplot2 packages
library(dplyr)
library(plyr)       # for aggregate
library(ggplot2)
library(sf)         # simple features package - vector
library(raster)     # raster
library(mapview)    # quick interactive viewing of spatial objects
library(leaflet)
library(leaflet.extras) #For heatmaps
library(gdtools)
library(geojsonio)


industry_list<-c("All Industries","Tobacco Retail Dealer", "Home Improvement Contractor","Laundries","Electronics Store","Electronic Cigarette Dealer","Garage","Electronic & Appliance Service","Employment Agency","Amusement Device Portable",     
  "Newsstand","Pedicab Business","Games of Chance","Parking Lot","Stoop Line Stand","Secondhand Dealer - General",   
  "Debt Collection Agency","General Vendor Distributor","Sightseeing Bus","Laundry Jobber","Auction House Premises","Pawnbroker","Laundry","Sidewalk Cafe","Cabaret",                       
  "Dealer In Products","Ticket Seller Business","Garage and Parking Lot", "Amusement Device Temporary","Scrap Metal Processor","Special Sale",                  
  "Scale Dealer Repairer","Process Serving Agency","Storage Warehouse","Secondhand Dealer - Auto","Catering Establishment","Gaming Cafe",                   
  "Tow Truck Company","Pool or Billiard Room","Amusement Arcade","Car Wash","Amusement Device Permanent","Horse Drawn Cab Owner","Secondhand Dealer - Firearms","Bingo Game Operator","Commercial Lessor","Tow Truck Exemption","Booting Company")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage("EDAV (STATW 5702) Final Project",
    
    tabPanel("NYC: Business License Acceptance Rates",
    
        sidebarPanel(
            
            selectInput(inputId="mapType", label="Borough vs Zip-code level",
                        choices=c("Borough Level","Zip-code Level"), selected = "Borough Level"),
            selectInput(inputId="year", label="Year",
                        choices=c("All Years(2014-2021)","2014","2015","2016","2017","2018","2019","2020","2021"), selected = "All Years(2014-2021)"),
            selectInput(inputId="industry", label="Industry",
                        choices=industry_list, selected = "All Industries")
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
                     leafletOutput("LicenseMap"),
                     h5("Notes"),
                     h6("1 : NA means no applications were filed for a particular industry in the particular year in that particular region"),
                     h6("2 : Acceptance rate calculated as percentage of total applicanions accepted i.e, license issues/(licenses applied)")

        )
    )
    )
    
))
    
     
    
    

