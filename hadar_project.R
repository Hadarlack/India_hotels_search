
library("shiny")
library("leaflet")
library("sqldf")
library("htmltools")
library("dplyr")

#-----------------------Data Preparation----------------------------

hotels <- read.csv("C:\\Users\\Administrator\\Downloads\\makemytrip.csv",sep = ",")
hotels <- hotels[,1:20]

#only relevent features
hotels <- sqldf("select area, city, country, latitude, longitude, hotel_star_rating, mmt_location_rating, mmt_review_count, mmt_review_rating, mmt_review_score , mmt_traveller_type_review_count, pageurl, property_address, property_name, property_type, room_types, state, traveller_rating
             from hotels
             where (area!='') and (city!='') and (country!='') and (latitude!='') and (longitude!='') and (hotel_star_rating!='') and (mmt_location_rating!='') and (mmt_review_count!='' ) and (mmt_review_rating!='') and (mmt_review_score!='') and (mmt_traveller_type_review_count!='') and (pageurl!='') and (property_address!='') and (property_name!='') and (property_type!='') and (room_types!='') and (state!='') and (traveller_rating!='')")

# path <- "C:/Users/Administrator/Downloads/updated_hotels.csv"
# write.csv(hotels,path)

#separating traveller_rating
hotels$traveller_rating <- as.character(hotels$traveller_rating)

ratings <- as.character(hotels$traveller_rating)

location <- substr(ratings,10,12)
hotels <- mutate(hotels,Location=location)
hotels$Location <- as.numeric(hotels$Location)

Hospitality <- substr(ratings,30,32)
hotels <- mutate(hotels,Hospitality=Hospitality)
hotels$Hospitality <- as.numeric(hotels$Location)

Facilities <- substr(ratings,49,51)
hotels <- mutate(hotels, Facilities= Facilities)
hotels$Facilities <- as.numeric(hotels$Location)

Cleanliness <- substr(ratings,69,71)
hotels <- mutate(hotels,Cleanliness=Cleanliness)
hotels$Cleanliness <- as.numeric(hotels$Location)

Value_for_Money <- substr(ratings,93,95)
hotels <- mutate(hotels,Value_for_Money=Value_for_Money)
hotels$Value_for_Money <- as.numeric(hotels$Value_for_Money)

Food <- substr(ratings,106,108)
hotels <- mutate(hotels,Food=Food)

#mmt_review_score from factor to num
hotels$mmt_review_score <- as.numeric(levels(hotels$mmt_review_score))[hotels$mmt_review_score]

#latitude from factor to num
hotels$latitude <- as.numeric(levels(hotels$latitude))[hotels$latitude]

#hotel_star_rating from factor to num
hotels$hotel_star_rating <- as.numeric(levels(hotels$hotel_star_rating))[hotels$hotel_star_rating]

#getting mmt_traveller_type_review_count
for_type <- read.csv("C:\\Users\\Administrator\\Downloads\\updated_hotels_with_type.csv",sep = ",")
family_score <- for_type[,12]
family_score <- as.character(family_score)
family_score <- substr(family_score,8,11)
family_score <- as.numeric(family_score)

couple_score <- for_type[,13]
couple_score <- as.numeric(couple_score)


solo_score <- for_type[,14]
solo_score <- as.numeric(solo_score)

friends_score <- for_type[,15]
friends_score <- as.numeric(friends_score)

business_score <- for_type[,16]
business_score <- as.numeric(business_score)

hotels$family <- family_score
hotels$couple <- couple_score
hotels$solo <- solo_score
hotels$friends <- friends_score
hotels$business <- business_score

#making mmt_traveller_type_review_count a binary feature
for(i in 1:dim(hotels)[1])
{
if (hotels[i,25]>0)
  {hotels[i,25]=1}
if (hotels[i,26]>0)
  {hotels[i,26]=1}
if (hotels[i,27]>0)
  {hotels[i,27]=1}
if (hotels[i,28]>0)
  {hotels[i,28]=1}
if (hotels[i,29]>0)
  {hotels[i,29]=1}
}
    
#pageurl from factor to char
hotels$pageurl <- as.character(hotels$pageurl)

#property_name from factor to char
hotels$property_name <- as.character(hotels$property_name)

#property_address from factor to char
hotels$property_address <- as.character(hotels$property_address)

#location range for colors
locationRange = range(0,5)

#uniqe values for property type
property_types <- c("Hotel", "Resort", "Guest House","Specials" )
ppp <- as.data.frame(unique(hotels$property_type))
#-----------------------ui----------------------------

ui <- bootstrapPage(

  tags$style(type = "text/css", "html, body {width:100%;height:100%} 
             #controls {
             background-color: white;
             padding: 0 20px 20px 20px;
             cursor: move;
             opacity: 0.85;
             zoom: 0.9;
             transition: opacity 500ms 1s;
             }
             h1 {text-align: center; background-color: #ffdb00; font-family : 'Arial Black', Gadget, sans-serif}
             h4 {text-align: center; font-weight: bold}
           h6{font-weight: bold} "
           ),
  
  h1("Hotels in India"),
  h4 ("You can find all this hotels in makeMyTrip.com       ")
  ,

  leafletOutput("mymap", width="100%", height="100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = -25, left = "auto", right = -20, bottom = "auto",
                width = 330, height = "auto",
                
                h2("Analysis parametrs"),
                
                sliderInput("Stars", "Hotel stars rating", 0, 5,
                            value = c(2,4), step = 1 ,sep=""
                ),
                
                h6("Choose the type of"),
                h6("hotel you're looking for"),
                
                #hotel type radio buttons
                radioButtons("type",label = NULL,choices=c("all"="all", "family"= "family", "friends" = "friends", "couple"="couple", "solo"="solo", "business"="business"),selected='all' ),
                
                # Drop list 
                selectInput("Property_type", "property_types", property_types ,selected="Hotel"
                ),
                
                # Checkbox for legend
                checkboxInput("legend", "Show legend", TRUE),
                
                h3("The five cleanest hotels"),
                tableOutput("cleanliness2")
                
  )
  
)

#-----------------------server----------------------------

server <- function(input, output, session) {
  
  # # Create the map

  output$mymap <- renderLeaflet(
    leaflet(data = hotels) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = 82, lat = 22, zoom = 5))

    # Checks the current boundaries of the map and return hotels inside
    hotelsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(filteredData()[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      #only relevant hotels
      subset(filteredData(),
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])})
    
      # Reactive expression for the data subsetted to the user's selections
      filteredData <- reactive({
     
      #property types - drop list 
      if (input$Property_type=="Specials")
      {hotels2 <- subset(hotels,hotels$property_type=="Houseboat")
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Homestay"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Camp"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Beach Hut"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Villa"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Apartment"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Cottage"))
      hotels2 <- rbind.data.frame(hotels2,subset(hotels,hotels$property_type=="Tree house"))}
      else
      {hotels2 <- subset(hotels,hotels$property_type==input$Property_type) }

      #stars - slider
      hotels3 <- subset(hotels2,((hotels2$hotel_star_rating >= input$Stars[1])&(hotels2$hotel_star_rating <= input$Stars[2])))
      
      #type- radio buttons 
        if (input$type == "all")
          {hotels4 <- hotels3}
        else
          {Type <- input$type
          hotels4 <- subset(hotels3,hotels3[,Type]==1)}
      
        #recalculate radius
          hotels4$radius <- ifelse(hotels4$mmt_review_score<2, 5,hotels4$mmt_review_score*2.5)
    
   hotels4
    })
   
    colorpal <- reactive({
      colorNumeric('RdYlGn',locationRange)
    })
    
    observe({
      pal <- colorpal() # Circle's color palette will be chosen according to mmt_location_rating
      leafletProxy("mymap", data = filteredData()) %>%
        clearMarkers() %>%
        addCircleMarkers(radius = ~ radius, weight = 1, color = "#777777", #layerId=~ind,
                   fillColor = ~pal(Location), fillOpacity = 0.7,stroke=FALSE, popup=~property_name)
    }) 

    # Legend is created here
    observe({
      proxy <- leafletProxy("mymap", data = hotels)

      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "topright",
                            pal = pal, values = ~locationRange ,
                            title = 'Location rating',
                            bins = 8,
                            labFormat = labelFormat(
                              prefix = '', between = ', '
                            ))}
      })
    
    output$cleanliness2 <- renderTable(filteredData()%>%arrange(desc(Cleanliness)) %>% slice(1:5)%>%subset(,c(14,2)))
    
}

shinyApp(ui, server)

