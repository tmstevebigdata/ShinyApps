# ui.R

library(shiny)

AppTitle <- 'Top US Severe Weather Events and its Effects on Public Health and the Economy (1980 - 2011)'

shinyUI(fluidPage(

  ## -- Include Cascading Style Sheet --
  includeCSS('www/styles.css'),

  headerPanel(AppTitle),

  sidebarPanel(
      helpText('US Severe Weather Events'),

      radioButtons('TopSelection', label = 'Selection',
                   choices = list('Top 3' = 3,
                                  'Top 5' = 5,
                                  'Top 10' = 10,
                                  'Top 15' = 15,
                                  'Top 20' = 20),
                                  selected = 20),

      # Adding the new div tag to the sidebar
      tags$div(class = 'header', checked = NA,
               tags$p('For the documentation on how to use this App:-'),
               tags$a(href='https://tmstevebigdata.shinyapps.io/ShinyApp-TopUSSevereWeatherEvents/documentation.html', target='_blank', 'Click Here!')
      )
  ),

  mainPanel(
      tabsetPanel(
          tabPanel('Plot', plotOutput(outputId = 'main_plot', height = '710px', width = '850px')),
          tabPanel('Table', tableOutput(outputId = 'main_table'))
      )
  )

))
