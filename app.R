library(shiny)
library(readxl)
library(sf)
library(tmap)
library(dplyr)
library(leaflet)
library(RColorBrewer)


#get and clean numerator data
noncorp2015_gor <- read_excel("/Users/reka/Dropbox (The University of Manchester)/CDRC/analysis/data/E&W 2001-2015/2015/2015 Aggregated Data Non-Corporate.xlsx",
                              sheet = "GOR")
colnames(noncorp2015_gor) <-  c(noncorp2015_gor[6,1:10], noncorp2015_gor[2,11:17])
noncorp2015_gor <- noncorp2015_gor[-1:-6,]
noncorp2015_gor$actualCount <- as.numeric(noncorp2015_gor$`Total Value of CCJs`)/as.numeric(noncorp2015_gor$`Average Value of CCJs`)

#get shapefile
gor <- st_read("/Users/reka/Dropbox (The University of Manchester)/CDRC/analysis/data/Regions_December_2015_Ultra_Generalised_Clipped_Boundaries_in_England/Regions_December_2015_Ultra_Generalised_Clipped_Boundaries_in_England.shp")
#join numerator to shapefile
nc2015gor <- left_join(gor, noncorp2015_gor, by = c("rgn15nm"="GOR_NAME"))

#get and clean denominatory data
denom <- read.csv("/Users/reka/Dropbox (The University of Manchester)/CDRC/analysis/data/businessesperregion.csv")
denom$businesses <- as.numeric(gsub(",","", denom$businesses))
#make sure region names line up with shapefile
denom$region <- ifelse(denom$region =="Yorkshire and the Humber", "Yorkshire and The Humber", as.character(denom$region))

#join denominator data
nc2015gor <- left_join(nc2015gor, denom, by = c("rgn15nm"="region"))

#calc the rate
nc2015gor$rateperbus <- nc2015gor$actualCount/nc2015gor$businesses*10000

#get alpha and beta for beta distribution
a <-  sum(nc2015gor$actualCount)
b <- sum(nc2015gor$businesses)# - sum(nc2015gor$actualCount)

#get prior, likelihood, and posterior
th = seq(0,1,length=10000)
for(i in 1:9){

  #make prior/likelihood/posterior distributions
  a = a
  b = b
  n = nc2015gor$businesses[i]
  x = nc2015gor$actualCount[i]
  prior = dbeta(th,a,b)
  like = dbeta(th,x+1,n-x+1)
  post = dbeta(th,x+a,n-x+b)

  #assign to dataframe
  nc2015gor$priordist[i] <- list(prior/sum(prior))
  nc2015gor$likelidist[i] <- list(like/sum(like))
  nc2015gor$posteriordist[i] <- list(post/sum(post))


}

calc_KL <- function(prior, posterior) {
  return(log(sd(unlist(posterior))/sd(unlist(prior))) +
           (((sd(unlist(prior))^2 + (mean(unlist(prior)) - mean(unlist(posterior)))^2) /(2*sd(unlist(posterior))^2))) - 1/2)
}

nc2015gor$kl <- mapply(calc_KL, nc2015gor$priordist, nc2015gor$posteriordist)


nc2015gor$kl_signed <- ifelse(nc2015gor$rgn15nm %in% c("London", "East of England", "South East"),
                              nc2015gor$kl*-1,
                              nc2015gor$kl)



ui <- fluidPage(
  titlePanel("CCJs in England"),

  sidebarLayout(
    sidebarPanel(
      h4("Map to display CCJs for 2015 in regions across England"),
      #display selector goes here
      selectInput("display", "Display:",
                   c("Count" = "actualCount",
                     "Rate" = "rateperbus",
                     "Bayesian Surprise" = "kl_signed"))

    ),
    mainPanel(
      #leaflet output goes here
      leafletOutput("map", height = 800)

    )
  )
)

server <- function(input, output) {


  #reactive map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 1.5491, lat = 53.8008, zoom = 5)
  })

  observe({
    if(!is.null(input$display)){

      pal <- colorBin("RdYlBu", eval(parse(text=paste0("nc2015gor$",input$display)))*-1, bins=4, na.color = "#bdbdbd")


      leafletProxy("map", data = nc2015gor) %>%
        addTiles() %>%
        clearShapes() %>%
       addPolygons(data = nc2015gor, fillColor = ~pal(eval(parse(text=input$display))*-1), fillOpacity = 0.7,
        color = "white", weight = 2)
    }})


}


shinyApp(ui = ui, server = server)


