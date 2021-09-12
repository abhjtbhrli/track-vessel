# Track the Vessel R/Shiny application
# Author: Abhijit Bharali

library(shiny)
library(shiny.semantic)
library(tidyverse)
library(geosphere)
library(leaflet)


# Loading the dataset
# ships = readr::read_csv(unzip("ships_1.zip", "ships.csv"))

# Adding a column 'id' that will assist future data manipulation
# ships = bind_cols('id'=ships%>%row.names()%>%as.integer(),ships)

# Adding a column 'distance' that will assist future data manipulation
# ships$distance = double(nrow(ships))

# Saving the edited dataset as an RDS file for it to be used in the Shiny app. The earlier csv file is not included in the app package. We instead use the RDS file.
# write_rds(ships,"shipsrds.rds")

# The 'ships' dataset to use in the deployed Shiny app.
ships = read_rds("shipsrds.rds")

# Initialising the grid template required for shiny semantic UI
myGridTemplate = grid_template(
    default = list(
        areas = rbind(
            c("title", "map"),
            c("user", "map"),
            c("info", "map")
        ),
        cols_width = c("400px", "1fr"),
        rows_height = c("50px", "auto", "auto")
    ),
    mobile = list(
        areas = rbind(
            "title",
            "user",
            "map",
            "info"
        ),
        rows_height = c("50px", "100px", "auto", "200px"),
        cols_width = c("100%")
    )
)

# Writing module for UI
choice_mod = function(id) {
    ns <- NS(id)
    tagList(
        shiny.semantic::dropdown_input(ns("vesselchoice"),
                                       default_text = tags$strong("Select Vessel Type"),
                                       choices = str_sort(unique(ships$ship_type)),
                                       value = "Cargo"),
        br(),
        shiny::uiOutput(ns("Vesselname"))
    )
}

# Writing server side module for 'uiOutput'
choice_server_mod = function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            output$Vesselname = renderUI({
                ns = session$ns
                shiny.semantic::dropdown_input(ns("name"),
                                               default_text = tags$strong("Select Vessel Name"),
                                               choices = str_sort(unique((ships%>%
                                                                              filter(ship_type==input$vesselchoice))$SHIPNAME)),
                                               value = "KERLI")
            })
            return(reactive({input$name}))
            
        }
    )
}

# Defining UI 
ui = semanticPage(
    suppress_bootstrap = T,
    tags$head(tags$style(HTML('* {font-family: "Comic Sans MS"; font-weight: bold};'))),
    grid(
        myGridTemplate,
        container_style = "border: 5px solid #807777; grid-gap: 10px; background: #efefef",
        area_styles = list(title = "margin: 20px;", 
                           info = "margin: 20px; ", 
                           user = "margin: 20px; padding: 5px", 
                           map = "margin: 0px; border-radius: 0px; border: 3px solid #807777"),
        title = h1(style = "text-align: center;",class = "ui header", icon("ship"), div(class = "content", "Track the Vessel")),
        user = card(style = "border-radius: 2px; width: 100%; background: #efefef; padding: 5px;text-align: center; border: 3px solid #807777; border-radius: 3px",
                    h3(style = "font-family: Comic Sans MS;","Select Vessel Type & Name"),
                    br(),
                    choice_mod("Module")),
        info = div(uiOutput("stats"),
                   br(),
                   plotOutput("trend")),
        map = leafletOutput("map",width = "100%",height = "100%")
    )
)

# Defining server logic 
server = function(input, output, session) {
    
    new_var = choice_server_mod("Module")
    
    z = eventReactive(new_var(),{
        
        new_ship = ships%>%
            filter(SHIPNAME%in%new_var())
        
        
        new_ship$distance[2:nrow(new_ship)] = sapply(2:nrow(new_ship),
                                                     function(x) {if_else((new_ship[x-1,'id']+1)%>%as.integer()==(new_ship[x,'id'])%>%as.integer(), distm(new_ship[x-1,c('LON', 'LAT')], new_ship[x,c('LON', 'LAT')], fun = distHaversine),0)})
        
        new_ship
    })
    
    
    x = eventReactive(new_var(),{
        
        index = z()%>%
            arrange(desc(distance),desc(DATETIME))%>%
            head(1)%>%
            pull(id)
        
        index
        
    })
    
    output$map = renderLeaflet({
        
        
        leaflet()%>%
            addTiles()%>%
            setView(lng = ships$LON[x()],lat = ships$LAT[x()], zoom = 10)%>%
            addMarkers(lng = ships$LON[x()-1],
                       lat = ships$LAT[x()-1])%>%
            addMarkers(lng = ships$LON[x()],
                       lat = ships$LAT[x()],
                       label = HTML(paste0("Longest distance sailed: ",br(),round((z())$distance[which((z())$id==x())],0)," metres")),
                       labelOptions = labelOptions(noHide = T, direction = "auto",
                                                   style = list(
                                                       "color" = "red",
                                                       "font-family" = "Comic Sans MS",
                                                       "font-style" = "italic",
                                                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                       "font-size" = "12px",
                                                       "border-color" = "rgba(0,0,0,0.5)")))
        
    })
    
    output$stats = renderUI({
        paste("Ship ",(z())$SHIPNAME[which((z())$id==x())]," sailed the longest distance (in metres) of ",round((z())$distance[which((z())$id==x())],0),"m between two consecutive observations. The below graph shows the daily average speed of the selected vessel.")
    })
    
    output$trend = renderPlot({
        ships%>%
            filter(SHIPNAME%in%new_var())%>%
            group_by(date)%>%
            summarise(av_speed=mean(SPEED))%>%
            ggplot(aes(x=date,y=av_speed*1.852, colour = -av_speed))+
            geom_line(size=2,show.legend = F)+
            xlab(NULL)+
            ylab(NULL)+
            labs(title = paste0((z())$SHIPNAME[which((z())$id==x())],": Average Vessel Speed in km/h"))+
            theme(panel.background = element_rect(fill = "#efefef"),
                  plot.background = element_rect(fill = "#efefef"),
                  aspect.ratio = NULL,
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.text = element_text(colour = "black"),
                  plot.title = element_text(hjust = 1,face = "bold"),
                  text = element_text(colour = "black", family = "Comic Sans MS",face = "bold"))
    })
    
}

# Running the application 
shinyApp(ui = ui, server = server)
