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

# Loading leaflet
nyc<- geojsonio::geojson_read("nyc_zips.json",what = "sp") 
nyc_zip_leaflet<-leaflet(nyc) %>%
    setView(-74.00, 40.71, zoom=10) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE))

nyc_borough<- geojsonio::geojson_read("NYC.geojson",what = "sp") 
nyc_borough_leaflet<-leaflet(nyc_borough) %>%
    setView(-74.00, 40.71, zoom=10) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE))

dataset_applications<-read.csv('dataset_applications.csv')
#order_zip<-read.csv('order_zip.csv')
#order_borough<-read.csv('order_borough.csv')
order_zip=data.frame(as.numeric(nyc@data$postalCode))
order_zip$City=nyc@data$PO_NAME
order_zip$index <- 1:nrow(order_zip)
order_zip<-order_zip %>% arrange(order_zip$as.numeric.nyc.data.postalCode.)
order_zip$Address.ZIP<-order_zip$as.numeric.nyc.data.postalCode.

order_borough=data.frame((nyc_borough@data$boro_name))
order_borough$index <- 1:nrow(order_borough)
order_borough$Borough<-order_borough$X.nyc_borough.data.boro_name.


library(shiny)


shinyServer(function(input, output,session) {
    
    yearFilter <- reactive({
        
        if (input$year=="All Years(2014-2021)"){
            yearFilterData<-dataset_applications
        } else{
            yearFilterData<-dataset_applications[dataset_applications$Decision.Date==as.numeric(input$year),]}
    })
    
    mapType<-reactive({
        input$mapType
    })
    
    industryFilter <- reactive({
        
        if (input$industry=="All Industries"){
            industryFilterData<-dataset_applications
        } else{
            industryFilterData<-dataset_applications[dataset_applications$License.Category==input$industry,]}
        })

    



    output$LicenseMap <- renderLeaflet({
        

        data<-inner_join(yearFilter(),industryFilter())
        issued_applications<-data[data$Status=="Issued",]
        not_issued_applications<-data[data$Status!="Issued",]
        
        if(mapType()=="Borough Level"){
            
            boroughIssuedCount<-count(issued_applications,vars="nyc.data.borough")
            boroughIssuedCount$Borough<-boroughIssuedCount$nyc.data.borough
            boroughIssuedCount<-merge(x=order_borough,y=boroughIssuedCount,by="Borough",all.x=TRUE)
            boroughIssuedCount<-boroughIssuedCount %>% arrange(boroughIssuedCount$index)

            
            
            boroughNotIssuedCount<-count(not_issued_applications,vars="nyc.data.borough")
            boroughNotIssuedCount$Borough<-boroughNotIssuedCount$nyc.data.borough
            boroughNotIssuedCount<-merge(x=order_borough,y=boroughNotIssuedCount,by="Borough",all.x=TRUE)
            boroughNotIssuedCount<-boroughNotIssuedCount %>% arrange(boroughNotIssuedCount$index)
            
            
            
            # Acceptance calculation & choropleth
            borough_acceptance<-boroughIssuedCount[,c("Borough","freq")]
            borough_acceptance$acceptance_rate<-(100*boroughIssuedCount$freq/(boroughIssuedCount$freq+boroughNotIssuedCount$freq))
            
            bins<-seq(floor(min(borough_acceptance$acceptance_rate,na.rm=T)),100,ceiling((100-min(borough_acceptance$acceptance_rate,na.rm=T))/10))
            pal <- colorBin("YlGn", domain = borough_acceptance$acceptance_rate, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%s Acceptance",
                borough_acceptance$Borough,paste(round(borough_acceptance$acceptance_rate,2),"%")
            ) %>% lapply(htmltools::HTML)
            
            
            borough_acceptance_choropleth<-nyc_borough_leaflet %>% addPolygons(
                fillColor = ~pal(borough_acceptance$acceptance_rate),
                weight = 2,
                opacity = 1,
                dashArray = "3",
                color = "white",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    bringToFront = TRUE),
                label = labels) %>% 
                addLegend(pal = pal, values = ~borough_acceptance$acceptance_rate, opacity = 0.7, title = "License Acceptance (%)",
                          position = "bottomright")
            
        }
        
        else if (mapType()=="Zip-code Level"){
            

        zipIssuedCount<-count(issued_applications,vars="Zip")
        zipIssuedCount$Address.ZIP<-as.numeric(zipIssuedCount$Zip)
        zipIssuedCount<-merge(x=order_zip,y=zipIssuedCount,by="Address.ZIP",all.x=TRUE)
        zipIssuedCount<-zipIssuedCount %>% arrange(zipIssuedCount$index)
        zipIssuedCount[is.na(zipIssuedCount)] <- 0
        
        not_issued_applications<-data[data$Status!="Issued",]
        zipNotIssuedCount<-count(not_issued_applications,vars="Zip")
        zipNotIssuedCount$Address.ZIP<-as.numeric(zipNotIssuedCount$Zip)
        zipNotIssuedCount<-merge(x=order_zip,y=zipNotIssuedCount,by="Address.ZIP",all.x=TRUE)
        zipNotIssuedCount<-zipNotIssuedCount %>% arrange(zipNotIssuedCount$index)
        zipNotIssuedCount[is.na(zipNotIssuedCount)] <- 0
        
        # Acceptance calculation & choropleth
        zip_acceptance<-zipIssuedCount[,c("Address.ZIP","freq","City")]
        zip_acceptance$acceptance_rate<-(100*zipIssuedCount$freq/(zipIssuedCount$freq+zipNotIssuedCount$freq))
        #bins<-seq(floor(min(zip_acceptance$acceptance_rate,na.rm=T)),100,ceiling((100-min(zip_acceptance$acceptance_rate,na.rm=T))/10))
        bins=c(0,80,82,84,86,88,90,92,94,96,98,100)
        pal <- colorBin("YlGn", domain = zip_acceptance$acceptance_rate, bins = bins)
        
        labels <- sprintf(
            "<strong>%s</strong> (%s)<br/>%s Businesses",
            zip_acceptance$City,zip_acceptance$Address.ZIP,paste(round(zip_acceptance$acceptance_rate,2),"%")
        ) %>% lapply(htmltools::HTML)
        
        
        zip_acceptance_choropleth<-nyc_zip_leaflet %>% addPolygons(
            fillColor = ~pal(zip_acceptance$acceptance_rate),
            weight = 2,
            opacity = 1,
            dashArray = "3",
            color = "white",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                bringToFront = TRUE),
            label = labels) %>% 
            addLegend(pal = pal, values = ~zip_acceptance$acceptance_rate, opacity = 0.7, title = "License Acceptance (%)",
                      position = "bottomright")
        }
        
            
})
    

    
})
