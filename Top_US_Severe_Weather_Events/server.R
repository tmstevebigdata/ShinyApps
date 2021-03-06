# server.R

## Initial setup
options(warn = -1)

options(digits = 2)
options(scipen = 999)

## Define Libraries
library(shiny)
library(gridExtra)
library(grid)
library(gtable)
library(xtable)

## Load data
df_Fatalities <- readRDS('StormData-Subset-Fatalities.rds')
df_Injuries <- readRDS('StormData-Subset-Injuries.rds')
df_Property_Damages <- readRDS('StormData-Subset-Property-Damages.rds')
df_Crop_Damages <- readRDS('StormData-Subset-Crop-Damages.rds')

shinyServer(function(input, output, session) {

## -- PLOT --

  output$main_plot <- renderPlot({

  ## Set variables
  TopNum <- as.integer(input$TopSelection)

  ## Define Functions
  fPlot <- function() {
     barplot(vY, names.arg = vX,
             horiz = vHoriz,
             width = vWidth, col = vCol, las = vLas,
             cex.names = vCexNames, cex.axis = vCexAxis, cex.lab = vCexLab,
             col.main = vColMain, col.lab = vColLab,
             main = vMain, xlab = vXLab, ylab = vYLab)
  }

  ## Subset data
  df_Fatalities_SS <- df_Fatalities[order(df_Fatalities$Fatalities, decreasing = TRUE), ]
  df_Fatalities_SS <- head(df_Fatalities_SS, TopNum)
  df_Fatalities_SS <- df_Fatalities_SS[order(df_Fatalities_SS$Fatalities, decreasing = FALSE), ]

  df_Injuries_SS <- df_Injuries[order(df_Injuries$Injuries, decreasing = TRUE), ]
  df_Injuries_SS <- head(df_Injuries_SS, TopNum)
  df_Injuries_SS <- df_Injuries_SS[order(df_Injuries_SS$Injuries, decreasing = FALSE), ]

  df_Property_Damages_SS <- df_Property_Damages[order(df_Property_Damages$Value_Mil, decreasing = TRUE), ]
  df_Property_Damages_SS <- head(df_Property_Damages_SS, TopNum)
  df_Property_Damages_SS <- df_Property_Damages_SS[order(df_Property_Damages_SS$Value_Mil, decreasing = FALSE), ]

  df_Crop_Damages_SS <- df_Crop_Damages[order(df_Crop_Damages$Value_Mil, decreasing = TRUE), ]
  df_Crop_Damages_SS <- head(df_Crop_Damages_SS, TopNum)
  df_Crop_Damages_SS <- df_Crop_Damages_SS[order(df_Crop_Damages_SS$Value_Mil, decreasing = FALSE), ]

  ## RESULTS

  ## -- Top Fatalities --
  Total_Fatalities <- sum(df_Fatalities_SS$Fatalities)
  Total_Fatalities <- format(Total_Fatalities, big.mark = ',')

  ## -- Top Injuries --
  Total_Injuries <- sum(df_Injuries_SS$Injuries)
  Total_Injuries <- format(Total_Injuries, big.mark = ',')

  ## -- Top Property Damages --
  Total_Property_Damages <- sum(df_Property_Damages_SS$Value_Mil)
  Total_Property_Damages <- format(Total_Property_Damages, big.mark = ',')

  ## -- Top Crop Damages --
  Total_Crop_Damages <- sum(df_Crop_Damages_SS$Value_Mil)
  Total_Crop_Damages <- format(Total_Crop_Damages, big.mark = ',')

  ## -- Set up variables for plot --
  vY <- df_Fatalities_SS$Fatalities
  vX <- df_Fatalities_SS$EVType
  vHoriz <- TRUE
  vWidth <- 1
  vCol <- 'Red'
  vLas <- 2
  vCexNames <- 1 # 0.85
  vCexAxis <- 1.2 # 0.85
  vCexLab <- 1.2
  vColMain <- 'Purple'
  vColLab <- 'Red'
  vMain <- paste('Top', TopNum, 'Fatalities caused by Severe Weather\n(Years 1980 - 2011)\nTotal Fatalities =', Total_Fatalities)
  vXLab <- 'Weather Event'
  vYLab <- 'Number of Fatalities'

  ## -- Output Plots --
  opar <- par()

  par(mar = c(5.5, 9.2, 3.5, 1), mgp = c(0.75, 2, 0), oma = c(0, 0, 0, 0), lwd = 0.1)
  par(mfrow = c(2, 2))

  ## Plot Fatalities
  fPlot()

  vY <- df_Injuries_SS$Injuries
  vX <- df_Injuries_SS$EVType
  vMain <- paste('Top', TopNum, 'Injuries caused by Severe Weather\n(Years 1980 - 2011)\nTotal Injuries =', Total_Injuries)
  vYLab <- 'Number of Injuries'

  ## Plot Injuries
  fPlot()

  vCol <- 'Green'
  vColMain <- 'Blue'

  vY <- df_Property_Damages_SS$Value_Mil
  vX <- df_Property_Damages_SS$EVType
  vMain <- paste('Top', TopNum, 'Property Damages caused by Severe Weather\n(Years 1980 - 2011)\nTotal Property Damage (USD Millions) =', Total_Property_Damages)
  vYLab <- 'Value of Damage (USD Millions)'

  ## Plot Property Damages
  fPlot()

  vY <- df_Crop_Damages_SS$Value_Mil
  vX <- df_Crop_Damages_SS$EVType
  vMain <- paste('Top', TopNum, 'Crop Damages caused by Severe Weather\n(Years 1980 - 2011)\nTotal Crop Damage (USD Millions) =', Total_Crop_Damages)

  ## Plot Crop Damages
  fPlot()

  par(opar)

  })

## -- TABLE --

  output$main_table <- renderTable({

  ## Set variables
  TopNum <- as.integer(input$TopSelection)

  ## Subset data
  df_Fatalities_SS <- df_Fatalities[order(df_Fatalities$Fatalities, decreasing = TRUE), ]
  df_Fatalities_SS <- head(df_Fatalities_SS, TopNum)

  df_Injuries_SS <- df_Injuries[order(df_Injuries$Injuries, decreasing = TRUE), ]
  df_Injuries_SS <- head(df_Injuries_SS, TopNum)

  df_Property_Damages_SS <- df_Property_Damages[order(df_Property_Damages$Value_Mil, decreasing = TRUE), ]
  df_Property_Damages_SS <- head(df_Property_Damages_SS, TopNum)

  df_Crop_Damages_SS <- df_Crop_Damages[order(df_Crop_Damages$Value_Mil, decreasing = TRUE), ]
  df_Crop_Damages_SS <- head(df_Crop_Damages_SS, TopNum)

  ## RESULTS

  ## -- Top Fatalities --
  Total_Fatalities <- sum(df_Fatalities_SS$Fatalities)
  Total_Fatalities <- format(Total_Fatalities, big.mark = ',')

  ## -- Top Injuries --
  Total_Injuries <- sum(df_Injuries_SS$Injuries)
  Total_Injuries <- format(Total_Injuries, big.mark = ',')

  ## -- Top Property Damages --
  Total_Property_Damages <- sum(df_Property_Damages_SS$Value_Mil)
  Total_Property_Damages <- format(Total_Property_Damages, big.mark = ',')

  ## -- Top Crop Damages --
  Total_Crop_Damages <- sum(df_Crop_Damages_SS$Value_Mil)
  Total_Crop_Damages <- format(Total_Crop_Damages, big.mark = ',')

  ## -- Top Fatalities --
  df_Fatalities_SS$Fatalities <- format(df_Fatalities_SS$Fatalities, big.mark = ',')

  ## -- Top Injuries --
  df_Injuries_SS$Injuries <- format(df_Injuries_SS$Injuries, big.mark = ',')

  ## -- Top Property Damages --
  df_Property_Damages_SS$Value_Mil <- format(df_Property_Damages_SS$Value_Mil, big.mark = ',')

  ## -- Top Crop Damages --
  df_Crop_Damages_SS$Value_Mil <- format(df_Crop_Damages_SS$Value_Mil, big.mark = ',')

  ## -- Output Results in a Table --
  df_Result <- data.frame(c(df_Fatalities_SS, df_Injuries_SS, df_Property_Damages_SS, df_Crop_Damages_SS))
  row.names(df_Result) <- NULL
  names(df_Result) <- c('Fatality Event', '# Fatalities', 'Injury Event', '# Injuries', 'Property Damage Event', 'Damage (Mil)', 'Crop Damage Event', 'Damage (Mil)')

  Result_Total <- data.frame(matrix(data = c('Total Fatalities', Total_Fatalities, 'Total Injuries', Total_Injuries, 'Total Property Damage', Total_Property_Damages, 'Total Crop Damage', Total_Crop_Damages), nrow = 1, ncol = 8))
  names(Result_Total) <- names(df_Result)
  df_Final_Result <- rbind(df_Result, Result_Total)

  xtable(df_Final_Result)

  })

})
