#install.packages('magrittr')
#install.packages('tidyverse')
#install.packages('shiny')
library('shiny')
library('tidyverse')
library('magrittr')


#### Loading the data file
df<-read.csv("G:/communication & visualization/clean_file1.csv")


#### Calculating avergae child mortality rates per continent
df1<- df %>%
  group_by(region) %>%
  summarise(average_region_mortality = mean(child_mortality, na.rm = TRUE))

df1

p <- sort(df1$average_region_mortality)


low_mortality_region  <- subset(df, df$region == c("America", "East Asia & Pacific", "Europe & Central Asia"))
high_mortality_region <- subset(df, df$region == c("Middle East & North Africa", "South Asia", "Sub-Saharan Africa"))
low_mortality_region$region<-factor(low_mortality_region$region, levels=c("America", "East Asia & Pacific", "Europe & Central Asia"))
high_mortality_region$region<-factor(high_mortality_region$region, levels=c("Middle East & North Africa", "South Asia", "Sub-Saharan Africa"))


#Mali Subsets
mali<- subset(df, Country=="Mali" ) 


#Sweden Subsets
Sweden<- subset(df, Country=="Sweden")

#Israel Subsets
israel<-  subset(df, Country=="Israel")

#Timor-Leste Subsets
timor<- subset(df, Country=="Timor-Leste")

Year <- Sweden$Year


ui<-navbarPage(title = "Mapping Child Mortality",
               
                tabPanel("Download the file",
                         downloadButton("downloadData", "Download"),
                         mainPanel(
                           
                           tableOutput("table")
                           
                         )
                ),
                tabPanel("Regional Cluster Distribution",
                        navlistPanel(
                          tabPanel(actionButton("action", label = "Region wise analysis"),
                                   hr(),
                                   fluidRow(column(2, verbatimTextOutput("value"))),
                                   fluidRow(column(12, plotOutput('plot1')))))),
                tabPanel("All in one with outliers",
                         sidebarPanel(
                           
                           radioButtons("z", "Regions",
                                        list("High Mortality Region"='i', "Low Moratality Region"='j'))),
                         mainPanel(
                           plotOutput("boxPlot"))
                         ),
               tabPanel("Outlier Comparison on the basis of regional clusters", 
                        sidebarPanel(
                          
                          radioButtons("x", label= "High Mortality region",
                                       choices= list("Mali"='a', "Israel"='b'),
                                       selected = "a"),
                          
                          
                         radioButtons("y", label= "Low Mortality region",
                                             choices= list("Sweden"='e', "Timor-Leste"='f'),
                                      selected = "e"
                                            ),
                         
                         sliderInput("slider1", label = "Year", min = 1964, 
                                     max = 2013, value = c(1964,2013))
                        ),
                      
                        
  mainPanel(
  plotOutput("distPlot"),
  column(4, verbatimTextOutput("value1"))
)
))
server<- function(input, output){
  
 # output$value <- renderPrint({ input$action })
  
  output$table <- renderTable({
    df
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df, file, row.names = T)
    }
  )
  output$plot1 <- renderPlot(barplot(p, 
                                     names.arg = c("America", "East Asia & Pacific",
                                                   "Europe & Central Asia", 
                                                    "Middle East",
                                                    "South Asia", "Sub-Saharan Africa"),
                                     xlab = "Regions", ylab = "Average child mortality rates", col= c("Green", "Green", "Green", "red", "red","red")))
  output$boxPlot <- renderPlot({
    if(input$z == 'i'){
      boxplot(high_mortality_region$child_mortality ~ high_mortality_region$region, 
              id.method = 'high_mortality_region$Country')
      title(main="Outliers in High mortality region: Mali, Israel")
     
      
    }
    else if(input$z == 'j'){
      boxplot(low_mortality_region$child_mortality ~ low_mortality_region$region, 
              id.method = 'low_mortality_region$Country')
      title(main="Outliers in Low child mortality region: Timor-Leste, Sweden")
      
    }
  })
  
  output$distPlot <- renderPlot({
    
     if(input$x == 'a' & input$y == 'e'){
       plot(x=Year, y=mali$child_mortality, type='l',
            main='Mali and Sweden Child Mortality Rates', xlab='Year', 
            xlim= input$slider1,
            ylim= c(min(Sweden$child_mortality),max(mali$child_mortality)),
            ylab='Child Mortality Rate', col="red", lwd = 3)
       par(new = TRUE)
       plot(x= Year, y= Sweden$child_mortality, 
            xlim= input$slider1,
            ylim= c(min(Sweden$child_mortality),max(mali$child_mortality)),  
            ylab='Child Mortality Rate', xlab ='Year',type='l', col = 'darkgreen', lwd=3)
       legend("topright",  lty=c(1,1), c("Mali", "Sweden"), lwd= c(3,3), 
              col = c("red", "darkgreen"))
     }
    
    
    else if(input$x == 'a' & input$y == 'f'){
      plot(x=Year, y=mali$child_mortality, type='l',
           main='Mali and Timor-Leste Child Mortality Rates',xlab='Year', 
           xlim= input$slider1,
           ylim=c(min(timor$child_mortality), max(mali$child_mortality)),
           ylab='Child_Mortality_Rate', col="red", lwd =3)
      par(new = TRUE)
      plot(x= Year, y= timor$child_mortality, 
           xlim= input$slider1,
           ylim=c(min(timor$child_mortality), max(mali$child_mortality)),
           ylab='Child_Mortality_Rate',type='l', col = 'blue', lwd =3)
      legend("topright",  lty=c(1,1), c("Mali", "Timor-Leste"), lwd= c(3,3), 
             col = c("red", "blue"))
    }
    
    else if(input$x == 'b' & input$y == 'e'){
    plot(x=Year, y=israel$child_mortality, type='l', 
           main='Israel and Sweden Child Mortality', xlab='Year',
          xlim= input$slider1,
          ylim = c( min(Sweden$child_mortality), max(israel$child_mortality)),
          ylab='Child Mortality Rate', col="grey", lwd =3)
      par(new = TRUE)
      plot(x= Year, y= Sweden$child_mortality, xlim= input$slider1,
           ylim = c( min(Sweden$child_mortality), max(israel$child_mortality)),
           ylab='Child Mortality Rate',type='l', col = 'darkgreen', lwd=3)
      legend("topright",  lty=c(1,1), c("Irael", "Sweden"), lwd= c(3,3), 
             col = c("grey", "darkgreen"))
    }
    
    else if(input$x == 'b' & input$y == 'f'){
      plot(x=Year, y=israel$child_mortality, type='l', 
           main='Israel and Timor-Leste Child Mortality ', xlab='Year', 
           xlim= input$slider1,
           ylim=c(min(israel$child_mortality),max(timor$child_mortality)),
           ylab='Child Mortality Rate',col="grey", lwd =3)
      par(new = TRUE)
      plot(x= Year, y= timor$child_mortality, type='l', xlim= input$slider1, 
           ylim=c(min(israel$child_mortality),max(timor$child_mortality)),
           ylab='Child Mortality Rate',col = 'blue', lwd =3)
      legend("topright",  lty=c(1,1), c("Israel", "Timor-Leste"), lwd= c(3,3),
             col = c("grey", "blue"))
    }
    
  })
}
  
shinyApp(ui,server)

