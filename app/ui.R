require(shiny)
require(markdown)
require(rCharts)
library(ggplot2)
library(dygraphs); #library(dplyr); library(xts)
#require(morris)
options(RCHART_LIB = 'polycharts')

Sys.setlocale("LC_CTYPE","Chinese")
## test Chinese: 测试，汉语。
Sys.setenv(TZ='Europe/Stockholm')

shinyUI(
    fluidPage(
        titlePanel("Traffic Analysis System")
        ,singleton(tags$head(tags$script(includeHTML("s.js"))))

        ,tabsetPanel(
             type="tabs"
            ,tabPanel(
                "Per Accident"
                ,fluidRow( # input & echo
                    column(6
                        ,h3("User's Selection", style = "color:#0B7FC3")
                        ,checkboxGroupInput("accidentPlotMetricArray"
                                        ,"Metrics to Display"
                                        ,choices = c(
                                            'Vehicle Count' = "FLOW_RATE"
                                            ,'Average Speed' = "AVR_SPEED"
                                            ,'Hourly Outlier' = 'HOUR_OUTLIER'
                                            ,'Updatable Hourly Outlier' = 'HOUR_OUTLIER_UPDATE'
                                            ,'Differential Hourly Outlier' = 'DIFF_HOUR_OUTLIER'
                                            ,'Updatable Differential Hourly Outlier'  = 'DIFF_HOUR_OUTLIER_UPDATE'
                                            )
                                       ,selected = c(
                                            'FLOW_RATE'
                                            ,'AVR_SPEED'
                                            ,'HOUR_OUTLIER_UPDATE'
                                            ,'DIFF_HOUR_OUTLIER_UPDATE'
                                            )
                        )
                        ,sliderInput('enlargeFactor', 'Enlarge Factor', value = 100, min = 20, max = 160, step = 20)
                    )

                    ,column(6
                        ,h3('Echo Input', style = "color:#0B7FC3")
                        ,h4('Metrics to Display')
                        ,verbatimTextOutput("accidentPlotMetricArray")
                        ,h4('Enlarge Factor')
                        ,verbatimTextOutput("enlargeFactor")
                        #,h4('h4h4h4h4h4h4h4')
                        #,verbatimTextOutput("test")
                    )
                )

                ,tags$hr()
                ,fluidRow(
                    column(6
                        ,h3('When & Where to Analyse', style = "color:#0B7FC3")
                        ,selectInput(
                            "inputSelectSource"
                            ,label = ("Select Input Source:")
                            ,choices = list(
                                "User Input"='text'
                                ,"Selected Accident"='accident'
                            )
                            ,selected = 'accident'
                        )
                    )
                    ,column(3
                        ,h5('User Input:', style = "color:#0B7FC3")
                        ,textInput("ksMeterInputText", "KS Meter:", "63711")
                    )
                    ,column(3
                        ,dateInput('dateInput',
                                   label = 'Date: (yyyy-mm-dd)',
                                   value = '2013-04-04'
                        )
                        ,sliderInput('hourInput', 'Hour:',value = 11, min = 0, max = 23, step = 1)
                        #,sliderInput('minuteInput', 'Minute',value = 10, min = 0, max = 50, step = 10)
                    )
                )

                ,tags$hr()
                ,fluidRow(
                    h5('Select One Accident:', style = "color:#0B7FC3")
                    ,dataTableOutput('accidentTable')
                )


                ,tags$hr()
                ,fluidRow(
                    column(
                        3
                        ,actionButton("userInputConfirmButton", "Confirm Input Source & Input Info")
                    )
                    ,column(
                        5
                        ,verbatimTextOutput('accidentListSelected')
                    )
                    ,column(
                        4
                        ,actionButton("previousAccidentButton", "Previous Accident")
                        ,actionButton("nextAccidentButton", "Next Accident")
                    )
                )

                ,tags$hr()
                ,fluidRow( # To K <==
                    column(6
                        ,h5(textOutput("rChart1Title"), style = "color:#0B7FC3")
                        ,dygraphOutput("device1Plot", height = '350px')
                    )
                    ,column(6
                        ,h5(textOutput("rChart2Title"), style = "color:#0B7FC3")
                        ,dygraphOutput("device2Plot", height = '350px')
                    )
                )
                ,fluidRow(
                    column(5 ### device1 accident
                        ,fluidRow( ### device1 accident occurred
                             column(3
                                ,HTML('<strong>To K <== <br/> Occurred :</strong>')
                            )
                            ,column(3 # date of occur
                                ,dateInput('device1AccidentOccurTimeDateInput',label = 'Date',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device1AccidentOccurTimeHourSelectInput',label = 'Hour',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device1AccidentOccurTimeMinuteSelectInput',label = 'Minute',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                        ,fluidRow( ### device1 accident ended
                             column(3
                                ,HTML('<strong>To K <== <br/> Ended :</strong>')
                            )
                            ,column(3 # date of end
                                ,dateInput('device1AccidentEndTimeDateInput',label = '',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device1AccidentEndTimeHourSelectInput',label = '',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device1AccidentEndTimeMinuteSelectInput',label = '',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                    )
                    ,column(1
                        , HTML('<center><br/> Indicator:')
                        , textOutput("accidentIndicatorDevice1")
                        , HTML('</center>')
                        , actionButton("device1AccidentConfirm",HTML("Confirm <br/> To K <== <br/> By Device1"))
                    )
                    ,column(5 ### device2 accident
                        ,fluidRow( ### device2 accident occurred
                             column(3
                                ,HTML('<strong>To K <== <br/> Occurred :</strong>')
                            )
                            ,column(3 # date of occur
                                ,dateInput('device2AccidentOccurTimeDateInput',label = 'Date',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device2AccidentOccurTimeHourSelectInput',label = 'Hour',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device2AccidentOccurTimeMinuteSelectInput',label = 'Minute',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                        ,fluidRow( ### device2 accident ended
                             column(3
                                ,HTML('<strong>To K <== <br/> Ended :</strong>')
                            )
                            ,column(3 # date of end
                                ,dateInput('device2AccidentEndTimeDateInput',label = '',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device2AccidentEndTimeHourSelectInput',label = '',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device2AccidentEndTimeMinuteSelectInput',label = '',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                    )
                    ,column(1
                        , HTML('<center><br/> Indicator:')
                        , textOutput("accidentIndicatorDevice2")
                        , HTML('</center>')
                        , actionButton("device2AccidentConfirm",HTML("Confirm <br/> To K <== <br/> By Device2"))
                    )
                )

                ,fluidRow(
                    #h5('The Selected Accident:', style = "color:#0B7FC3"),
                    dataTableOutput('selectedOneAccidentTable')
                )

                ,fluidRow( # ==> To S
                    column(6
                        ,h5(textOutput("rChart3Title"), style = "color:#0B7FC3")
                        ,dygraphOutput("device3Plot", height = '350px')
                    )
                    ,column(6
                        ,h5(textOutput("rChart4Title"), style = "color:#0B7FC3")
                        ,dygraphOutput("device4Plot", height = '350px')
                    )
                )
                ,fluidRow(
                    column(5 ### device3 accident
                        ,fluidRow( ### device3 accident occurred
                             column(3
                                ,HTML('<strong> ==> To S <br/> Occurred :</strong>')
                            )
                            ,column(3 # date of occur
                                ,dateInput('device3AccidentOccurTimeDateInput',label = 'Date',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device3AccidentOccurTimeHourSelectInput',label = 'Hour',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device3AccidentOccurTimeMinuteSelectInput',label = 'Minute',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                        ,fluidRow( ### device3 accident ended
                             column(3
                                ,HTML('<strong> ==> To S <br/> Ended :</strong>')
                            )
                            ,column(3 # date of end
                                ,dateInput('device3AccidentEndTimeDateInput',label = '',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device3AccidentEndTimeHourSelectInput',label = '',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device3AccidentEndTimeMinuteSelectInput',label = '',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                    )
                    ,column(1
                        , HTML('<center><br/> Indicator:')
                        , textOutput("accidentIndicatorDevice3")
                        , HTML('</center>')
                        , actionButton("device3AccidentConfirm",HTML("Confirm <br/> ==> To S <br/> By Device3"))
                    )
                    ,column(5 ### device4 accident
                        ,fluidRow( ### device4 accident occurred
                             column(3
                                ,HTML('<strong> ==> To S <br/> Occurred :</strong>')
                            )
                            ,column(3 # date of occur
                                ,dateInput('device4AccidentOccurTimeDateInput',label = 'Date',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device4AccidentOccurTimeHourSelectInput',label = 'Hour',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device4AccidentOccurTimeMinuteSelectInput',label = 'Minute',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                        ,fluidRow( ### device4 accident ended
                             column(3
                                ,HTML('<strong> ==> To S <br/> Ended :</strong>')
                            )
                            ,column(3 # date of end
                                ,dateInput('device4AccidentEndTimeDateInput',label = '',value = '')
                            )
                            ,column(3 # hour
                                ,selectInput('device4AccidentEndTimeHourSelectInput',label = '',choices = seq(0:23),selected = 0)
                            )
                            ,column(3 # minute
                                ,selectInput('device4AccidentEndTimeMinuteSelectInput',label = '',choices = seq(0, 55, 5),selected = 0)
                            )
                        )
                    )
                    ,column(1
                        , HTML('<center><br/> Indicator:')
                        , textOutput("accidentIndicatorDevice4")
                        , HTML('</center>')
                        , actionButton("device4AccidentConfirm",HTML("Confirm <br/> ==> To S <br/> By Device4"))
                    )
                )

                ,tags$hr()
                ,fluidRow(
                    h3('Related Devices', style = "color:#0B7FC3")
                    ,dataTableOutput('selectedAccidentDeviceTable')
                )

                ,tags$hr()
                ,fluidRow(
                    h3('Posibility of Having Accident', style = "color:#0B7FC3")
                    ,verbatimTextOutput("accidentPossibility")
                )

            ) # end tabPanel()
            ,tabPanel(
                "Statistics"
                ,fluidRow(
                    h4('Accidents in Space Scale:', style = "color:#0B7FC3")
                    ,sliderInput('nBin_KS_METER_km', 'Number of bins:',value = 80, min = 20, max = 120, step = 10)
                    ,plotOutput('statisticalPlot_KS_METER_km')
                )
                ,fluidRow(
                    h4('Accidents in Time Scale:', style = "color:#0B7FC3")
                    ,selectInput(
                        "nBy_ACCIDENT_timely"
                        ,label = ("Display By:")
                        ,choices = list(
                            "Hour of Day"='hour'
                            ,"Day of Week"='wday'
                            ,"Month of Year"='mon'
                            )
                        ,selected = 'hourOfDay'
                    )
                    ,showOutput("statisticalPlot_ACCIDENT_timely", lib="morris")
                )
                ,fluidRow(
                    h4('Accidents in History Time Scale:', style = "color:#0B7FC3")
                    ,sliderInput('nBin_ACCIDENT_OCCUR_TIME', 'Number of bins:',value = 80, min = 20, max = 120, step = 10)
                    ,plotOutput('statisticalPlot_ACCIDENT_OCCUR_TIME')
                )
            )
            ,tabPanel(
                "test"
                ,fluidRow(
                    sliderInput('mu', 'Guess at the mean:',value = 70, min = 62, max = 74, step = 0.05)
                    ,plotOutput('testPlot')
                )
                ,fluidRow(
                    #textOutput("accidentIndicatorDevice1")
                )
            )
            ,tabPanel(
                "Device List"
                ,fluidRow(
                    dataTableOutput('deviceTable')
                )
            )
            ,tabPanel(
                "Details"
                ,tags$style(type = "text/css"
                            ,".dygraph-legend {background: transparent !important; }"
                )
                ,fluidRow(
                    column(12
                        ,checkboxGroupInput(
                            "detailPlotMetricArray"
                            ,"Select Extra Metrics"
                            ,choices = c(
                                "LANE","OCCUPANCY","NUM_LARGE","NUM_MIDDLE","NUM_SMALL","NUM_MOTOR","NUM_OTHER","AVR_SPACE"
                            )
                            ,selected = c(
                                #"LANE"
                            )
                            ,inline = TRUE
                        )
                    )
                )
                ,fluidRow(
                    column(3
                           , selectInput(
                               "deviceForDetail"
                               , label = ("Select Device")
                               , choices = list(
                                   "LD36013026","LD36013028","LD36013030","LD36013032","LD36013034","LD36013036","LD36013040","LD36013042","LD36013044","LD36013046" ,"LD36023002","LD36023004","LD36023006","LD36023008","LD36023010","LD36023012","LD36023014","LD36023016","LD36023018","LD36023020" ,"LD36023022","LD36023024","LD36033048","LD36033050","LD36033052","LD36033054","LD36033056","LD36033058"
                               )
                               , selected = 'LD36013028'
                           ) # selectInput
                    )
                    ,column(3
                        ,dateInput('dateForDetailInput',
                                   label = 'Date: (yyyy-mm-dd)',
                                   value = '2013-04-04'
                        )
                    )
                    ,column(3
                        ,sliderInput('hourForDetailInput', 'Hour:',value = 11, min = 0, max = 23, step = 1)
                    )
                    ,column(3
                        ,actionButton(
                            "userForDetailInputConfirmButton"
                            , HTML("Confirm <br/> Date & Hour")
                        )
                    )
                )
                ,fluidRow(
                    dygraphOutput("detailDygraph", height = '350px')
                )
                ,tags$hr()
                ,fluidRow(
                    dataTableOutput('detailTable')
                )
            )
            ,br(),br(),br()
            ,h5(HTML(sprintf('<center>Copyright &copy; SunnyBingoMe, 2014~%s</center>', format(Sys.time(), "%Y"))))
        ) # end tabsetPanel
    ) # end fluidPage
) # end shiny ui
