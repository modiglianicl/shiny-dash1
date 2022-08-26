library(shiny) # App web
library(shinydashboard) # Para formato dashboard
library(shinyjs) # Para usar entorno javascript
library(highcharter) # Para graficos interactivos
library(DT) # Para tablas
library(dplyr) # Para manipulacion de bases de datos
library(dashboardthemes)
library(scales)
library(ggplot2)
library(leaflet)
library(stringr)
library(htmltools)
options(scipen=999)


# Creando modelo de RL ----------------------------------------------------


### Base de datos a utilizar (todo el analisis esta en otro script)

data <- read.csv("House_Rent_Dataset.csv")
data <- janitor::clean_names(data)
data$bathroom <- as.factor(data$bathroom)
data$bhk <- as.factor(data$bhk)

data <- data %>% 
  filter(rent < 3500000)

data <- data %>% 
  mutate(city_98iqr = case_when(city == "Kolkata" ~ 40000,
                                city == "Mumbai" ~ 360000,
                                city == "Bangalore" ~ 110000,
                                city == "Delhi" ~ 150000,
                                city == "Chennai" ~ 100000,
                                city == "Hyderabad" ~ 80000))

data <- data %>% 
  mutate(lat = case_when(city == "Bangalore" ~ 12.972442,
                         city == "Kolkata" ~ 22.572645,
                         city == "Mumbai" ~ 19.076090,
                         city == "Delhi" ~ 28.644800,
                         city == "Chennai" ~ 13.067439,
                         city == "Hyderabad" ~ 17.387140)) %>% 
  mutate(long = case_when(city == "Bangalore" ~ 77.580643,
                          city == "Kolkata" ~ 88.363892,
                          city == "Mumbai" ~ 72.877426,
                          city == "Delhi" ~ 77.216721,
                          city == "Chennai" ~ 80.237617,
                          city == "Hyderabad" ~ 78.491684))

data$lat <- as.numeric(data$lat)
data$long <- as.numeric(data$long)

data_wz <- data %>% 
  mutate(rent_wz = ifelse(rent > city_98iqr, city_98iqr,rent))

full_model <- lm(rent_wz ~ bhk + size + city + furnishing_status +
                   bathroom, data = data_wz)

# Variables
ciudades <- unique(data$city)
ciudades <- c("Every City", ciudades)
min_fecha <- as.character(min(data$posted_on))
max_fecha <- as.character(max(data$posted_on))
rango_fechas <- paste(min_fecha,"|",max_fecha)
total_cities <- as.numeric(length(as.vector(unique(data$city))))
avg_rent_price <- round(mean(data$rent),2)
# App ---------------------------------------------------------------------


## Barra superior del dashboard----
header <- dashboardHeader(title = "India rent price Analysis",
                          titleWidth = 250)


## Menu de navegacion del dashboard----
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id="inicio",
    menuItem("Home and context",
             tabName = "inicio"),
    menuItem("Descriptive Analysis",
             tabName =  "menu1",
             startExpanded = F,
             div(id = "hijo11",
                 menuItem("Price by City by Date",
                          tabName = "descriptivo1" 
                 ),
                 menuItem("Price distribution",
                          tabName = "descriptivo2"),
                 conditionalPanel("input.inicio == menu1",
                                  selectInput("ciudad","City",
                                              ciudades)))),
    menuItem("Map with data",
             tabName = "menu2"),
    menuItem("Rent price prediction",
             tabName = "prediction")
  ),
  p("Developed by Felipe Villarroel",style = "font-size:15px;")
)


## Cuerpo de cada vineta del menu----

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  tabItems(
    tabItem(tabName = "inicio",
            HTML("<h1>Home and presentation</h1>"),
            column(box(status = "primary",
                width = 8,
                p("This dashboard created using Shiny library (and many more used...) 
              is intended to show my skills gained during my post graduate Data Science 
              studies done on the Pontificia Universidad Católica de Chile, 
              this is my first 'complete' shiny dashboard in which we will do a 
              quick look on India's rent prices through 
              different cities and we will end on predicting a price 
              using a linear regression made on R.")),
                box(width = 8,title = h2("Context"), status = "primary",
                    HTML("<p>Housing in India varies from palaces of erstwhile 
                    maharajas to modern apartment buildings in big cities to 
                    tiny huts in far-flung villages. There has been tremendous
                    growth in India's housing sector as incomes have risen. 
                    The Human Rights Measurement Initiative finds that India 
                    is doing 60.9% of what should be possible at its level of 
                    income for the right to housing.</p>
                    <p>Renting, also known as hiring or letting, is an agreement 
                    where a payment 
                    is made for the temporary use of a good, service, or 
                    property owned by another. 
                    A gross lease is when the tenant pays a flat rental amount 
                    and the landlord pays 
                    for all property charges regularly incurred by the ownership. 
                    Renting can be an 
                    example of the sharing economy.</p>")),
                width=12
                ),
            ),
    tabItem(tabName = "descriptivo1",
            h1("Price by City by Date"),
            fluidRow(box(plotOutput("grafico1"),
                         title = "Price by city by date",
                         width = 8,
                         status = "primary",
                         collapsible = TRUE,
                         solidHeader = TRUE
            ),
            box(p("At simple view we can see that Mumbai is the city with the highest
                           rent prices, followed by Delhi and Bangalore. This dataset contains 4745
                           observations and we pretend to create a linear regression model in order
                           to predict the rent price of a specific place in any of this six cities.")
                ,title = "Rent price description",
                background = "light-blue",
                width = 8)
            )),
    tabItem(tabName = "descriptivo2",
            fluidRow(
              column(width = 7,
                     box(title = "Rent price distribution histogram",
                         plotOutput("grafico2"),
                         status = "primary",
                         width = 12,
                         solidHeader = TRUE,
                         collapsible = TRUE
                     ),
                     box(title = "Price distribution Boxplot by furnishing status",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         plotOutput("grafico3")
                     )
              ),
              column(width = 5,
                     infoBox("Total Cities", total_cities,
                             icon = icon("fa-solid fa-city",
                                         lib="font-awesome"),width= 10,
                             fill = TRUE),
                     infoBox("Average Rent Price", paste("₹",avg_rent_price), 
                             icon = icon("credit-card"),width = 10),
                     infoBox("POSTINGS", nrow(data), 
                             icon = icon("fa-solid fa-clipboard-check",
                                         lib="font-awesome"),width = 10,
                             fill = TRUE),
                     infoBox("DATE RANGE", rango_fechas,
                             icon = icon("fa-solid fa-calendar-days",
                                         lib="font-awesome"),width = 10)
              )
            )),
    tabItem(tabName = "menu2",
            box(HTML("<h2>Average price by City</h2>"),
                status = "primary",
                width = 8,
                footer = HTML('Map created with leaflet. Info obtained from 
              <a href="https://www.kaggle.com/datasets/iamsouravbanerjee/house-rent-prediction-dataset">
                            Kaggle : "House Rent Prediction Dataset"</a>'),
                leafletOutput("mapa")
            )
    ),
    tabItem(tabName = "prediction",
            h1("Price prediction"),
            fluidRow(box(width = 4, status="primary",
                         selectInput("bhk",
                                     label = h3("Number of Bedrooms"),
                                     choices = list("1" = 1 , "2" = 2,
                                                    "3" = 3, "4" = 4,
                                                    "5" = 5),
                                     selected = 1),
                         selectInput("bathroom",
                                     label = h3("Number of Bathrooms"),
                                     choices = list("1" = 1 , "2" = 2,
                                                    "3" = 3, "4" = 4),
                                     selected = 1)
            ),
            box(width = 4, status="primary",
                selectInput("city",
                            label = h3("City"),
                            choices = list("Kolkata" = "Kolkata",
                                           "Mumbai" = "Mumbai",
                                           "Bangalore" = "Bangalore",
                                           "Delhi" = "Delhi",
                                           "Chennai" = "Chennai",
                                           "Hyderabad" = "Hyderabad"),
                            selected = "Kolkata"),
                selectInput("furnishing",
                            label = h3("Furnishing Status"),
                            choices = list("Unfurnished" = "Unfurnished",
                                           "Semi-Furnished" = "Semi-Furnished",
                                           "Furnished" = "Furnished"),
                            selected = "Furnished") 
            ),
            box(width = 4,status = "primary",
                sliderInput("size",
                            label = h3("Place Size (square feet)"),
                            min = min(data_wz$size) , max = max(data_wz$size),
                            value = mean(data_wz$size))
            ),
            actionButton("action", label = "Predict Price!",width=350,
                         style = 'height:75px;
                                  width:300px;'
            ),
            box(title = h3("Price predicted"),
                width = 5,
                status= "success",
                solidHeader = FALSE,
                uiOutput("prediction"))
            
            
            
            
            )## Aca termina fluidRow
    ) ## Aca termina el tabItem
  )## Aca termina tabItems(cerramos todo)
)


ui <- dashboardPage(header, sidebar, body)
server <- function(input, output) {
  
  ## Reactives ----
  ### Grafico 1
  base_grafico1 <- reactive({
    ggplot(data = data)+
      geom_point(aes(x = as.Date(posted_on) , y = rent,
                     color = city), position = "jitter")+
      scale_x_date(date_breaks = "1 month")+
      scale_y_continuous(labels=scales::label_dollar(prefix="₹"))+
      labs(title = "Rent through time of every city",
           subtitle = "By City",
           color = "City")+
      xlab("Date posted")+
      ylab("Rent price")
    
  })
  ### Grafico dependiendo que variable está seleccionada
  filtered_data <- reactive({
    if (input$ciudad == "Every City"){
      data 
    } else{
      data %>% 
        filter(city == input$ciudad)
    }
  })
  
  ### Boton predict
  
  predictnow<- eventReactive(eventExpr = input$action,
                             valueExpr = {
                               predict_me <-tibble(bhk = input$bhk,
                                                   size = input$size,
                                                   city = input$city,
                                                   furnishing_status = input$furnishing,
                                                   bathroom = input$bathroom
                               )
                               
                               predict_me$bhk <- as.factor(predict_me$bhk)
                               predict_me$bathroom <- as.factor(predict_me$bathroom)
                               
                               prediction <- predict(full_model, newdata =  predict_me)
                               
                               p(as.character(paste("₹",round(prediction,2))),style = "font-size:50px;")
                               
                             },
  )
  
  ##Fin Reactives
  
  ## Outputs ----
  
  ### Output Precios en el tiempo----
  output$grafico1 <- renderPlot(
    
    if (input$ciudad == "Every City"){
      base_grafico1()
    } else {
      ggplot(data = data %>% 
               filter(city == input$ciudad))+
        geom_point(aes(x = as.Date(posted_on) , y = rent,
                       color = city), position = "jitter")+
        scale_x_date(date_breaks = "1 month")+
        scale_y_continuous(labels=scales::label_dollar(prefix="₹"))+
        labs(title = "Rent through time",
             subtitle = input$ciudad,
             color = "City")+
        xlab("Date posted")+
        ylab("Rent price")
    }
  )
  ### Output Histograma precios
  
  output$grafico2 <- renderPlot(
    
    ggplot(filtered_data())+
      geom_histogram(aes(x = rent)) + 
      labs(title = paste0("Price distribution of ",input$ciudad))+
      xlab("Rent")+
      ylab("Count")+
      scale_x_continuous(labels=scales::label_dollar(prefix="₹"))
    
    
  )
  
  ### Output Boxplot----
  
  output$grafico3 <- renderPlot(
    
    ggplot(filtered_data())+
      geom_boxplot(aes( y = rent, x  = city,
                        color = furnishing_status))+
      labs(color = "Furnishing Status")+
      xlab("City")+
      ylab("Rent price")+
      scale_y_continuous(labels=scales::label_dollar(prefix="₹"))+
      coord_flip()
    
    
    
  )
  ### Output mapa ----
  
  output$mapa <- renderLeaflet({
    
    
    leaflet(options = leafletOptions()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(data = data, lng = ~long, lat = ~lat,color = "#246e8b",
                       popup = ~paste(str_glue('<h3>{city}</h3>'),
                                      str_glue('<h4>AVG Rent : ₹ {round(mean(data$rent),2)}')))
    
    
  })
  ### Output Prediccion ----
  
  output$prediction <- renderUI({
    
    predictnow()
    
  })
  
}
shinyApp(ui = ui, server = server)