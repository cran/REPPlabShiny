#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(REPPlab)

# Define UI
shinyUI(fluidPage(
  titlePanel("A shiny app for exploratory projection pursuit"),
  sidebarLayout(
    navbarPage(
      title = NULL,
      tabPanel("EPPlab",
               fluidPage(column(
                 width = 12,
                 column(
                   width = 6,
                   navlistPanel(
                     tabPanel(
                       "EppLab1",
                       #Calculate first EPPLAB Object
                       column(
                         width = 12,
                         textInput(
                           inputId = "eppvarselect",
                           label = "Choose your variables",
                           value = paste("1:", length(X), sep =
                                           "")
                         ),
                         textInput(
                           inputId = "eppobsselect",
                           label = "Choose your observations",
                           value = paste("1:", length(X[, 1]), sep =
                                           "")
                         ),
                         selectInput(
                           inputId = "epp1sphereselect",
                           label = "Should the data be whitened?",
                           choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp1ppalgselect",
                           label = "Choose your PPalg",
                           choices = list(
                             "Tribe" = "Tribe",
                             "PSO" = "PSO",
                             "GA" = "GA"
                           ),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp1ppindexselect",
                           label = "Choose your PPindex",
                           choices = list(
                             "KurtosisMax" = "KurtosisMax",
                             "Discriminant" =
                               "Discriminant",
                             "Friedman" =
                               "Friedman",
                             "FriedmanTukey" =
                               "FriedmanTukey",
                             "KurtosisMin" =
                               "KurtosisMin"
                           ),
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp1simulationselect",
                           label = "Choose your numbers of simulation",
                           value = 100,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp1maxiterationselect",
                           label = "Choose your maximum numbers of iterations",
                           value = 200,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp1seed",
                           label = "Set seed",
                           value = "124"
                         ),
                         actionButton(inputId = "epp1epcalc", label =
                                        "Calculate the EPPlab1")
                       )
                     ),
                     tabPanel(
                       "EppLab2",
                       #Calculate second EPPLAB Object
                       column(
                         width = 12,
                         selectInput(
                           inputId = "epp2ppalgselect",
                           label = "Choose your PPalg",
                           choices = list(
                             "Tribe" = "Tribe",
                             "PSO" = "PSO",
                             "GA" = "GA"
                           ),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp2ppindexselect",
                           label = "Choose your PPindex",
                           choices = list(
                             "KurtosisMax" = "KurtosisMax",
                             "Discriminant" =
                               "Discriminant",
                             "Friedman" =
                               "Friedman",
                             "FriedmanTukey" =
                               "FriedmanTukey",
                             "KurtosisMin" =
                               "KurtosisMin"
                           ),
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp2simulationselect",
                           label = "Choose your numbers of simulation",
                           value = 100,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp2maxiterationselect",
                           label = "Choose your maximum numbers of iterations",
                           value = 200,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp2seed",
                           label = "Set seed",
                           value = "124"
                         ),
                         actionButton(inputId = "epp2epcalc", label =
                                        "Calculate the EPPlab2")
                       )
                     ),
                     tabPanel(
                       "EppLab3",
                       #Calculate third EPPLAB Object
                       column(
                         width = 12,
                         selectInput(
                           inputId = "epp3ppalgselect",
                           label = "Choose your PPalg",
                           choices = list(
                             "Tribe" = "Tribe",
                             "PSO" = "PSO",
                             "GA" = "GA"
                           ),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp3ppindexselect",
                           label = "Choose your PPindex",
                           choices = list(
                             "KurtosisMax" = "KurtosisMax",
                             "Discriminant" =
                               "Discriminant",
                             "Friedman" =
                               "Friedman",
                             "FriedmanTukey" =
                               "FriedmanTukey",
                             "KurtosisMin" =
                               "KurtosisMin"
                           ),
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp3simulationselect",
                           label = "Choose your numbers of simulation",
                           value = 100,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp3maxiterationselect",
                           label = "Choose your maximum numbers of iterations",
                           value = 200,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp3seed",
                           label = "Set seed",
                           value = "124"
                         ),
                         actionButton(inputId = "epp3epcalc", label =
                                        "Calculate the EPPlab3")
                       )
                     ),
                     tabPanel(
                       "EppLab4",
                       #Calculate fourth EPPLAB Object
                       column(
                         width = 12,
                         selectInput(
                           inputId = "epp4ppalgselect",
                           label = "Choose your PPalg",
                           choices = list(
                             "Tribe" = "Tribe",
                             "PSO" = "PSO",
                             "GA" = "GA"
                           ),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp4ppindexselect",
                           label = "Choose your PPindex",
                           choices = list(
                             "KurtosisMax" = "KurtosisMax",
                             "Discriminant" =
                               "Discriminant",
                             "Friedman" =
                               "Friedman",
                             "FriedmanTukey" =
                               "FriedmanTukey",
                             "KurtosisMin" =
                               "KurtosisMin"
                           ),
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp4simulationselect",
                           label = "Choose your numbers of simulation",
                           value = 100,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp4maxiterationselect",
                           label = "Choose your maximum numbers of iterations",
                           value = 200,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp4seed",
                           label = "Set seed",
                           value = "124"
                         ),
                         actionButton(inputId = "epp4epcalc", label =
                                        "Calculate the EPPlab4")
                       )
                     ),
                     tabPanel(
                       "EppLab5",
                       #Calculate fourth EPPLAB Object
                       column(
                         width = 12,
                         selectInput(
                           inputId = "epp5ppalgselect",
                           label = "Choose your PPalg",
                           choices = list(
                             "Tribe" = "Tribe",
                             "PSO" = "PSO",
                             "GA" = "GA"
                           ),
                           width = NULL
                         ),
                         selectInput(
                           inputId = "epp5ppindexselect",
                           label = "Choose your PPindex",
                           choices = list(
                             "KurtosisMax" = "KurtosisMax",
                             "Discriminant" =
                               "Discriminant",
                             "Friedman" =
                               "Friedman",
                             "FriedmanTukey" =
                               "FriedmanTukey",
                             "KurtosisMin" =
                               "KurtosisMin"
                           ),
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp5simulationselect",
                           label = "Choose your numbers of simulation",
                           value = 100,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp5maxiterationselect",
                           label = "Choose your maximum numbers of iterations",
                           value = 200,
                           min = 1,
                           width = NULL
                         ),
                         numericInput(
                           inputId = "epp5seed",
                           label = "Set seed",
                           value = "124"
                         ),
                         actionButton(inputId = "epp5epcalc", label =
                                        "Calculate the EPPlab5")
                       )
                     )
                   )
                 ),
                 column(
                   width = 6,
                   textOutput("textpoint8"),
                   textOutput("textpoint10"),
                   textOutput("textpoint11"),
                   textOutput("textpoint3"),
                   textOutput("textpoint4"),
                   textOutput("textpoint5"),
                   textOutput("textpoint6"),
                   textOutput("textpoint7"),
                   actionButton("close1", "Close Session")
                   
                 )
               ))),
#------------------------------------Summary-------------------------------------------
      tabPanel(
        "Summary",
        htmlOutput("SummaryTableTitle"),
        DTOutput("summary1"),
        verbatimTextOutput("summary2")
      ),
#------------------------------------Cluster-------------------------------------------
      tabPanel("Cluster",
               fluidPage(
                 navlistPanel(
                   tabPanel("Screeplot",
                            plotOutput("plot"),
                            hr(),
                            wellPanel(fluidRow(
                              column(
                                width = 12,
                                align = "center",
                                h3("Control Panel"),
                                column(
                                  width = 12,
                                  sliderInput(
                                    'sampleSize',
                                    'Sample Size',
                                    min = 1,
                                    max = 100,
                                    value = c(1, 100)
                                  )
                                ),
                                column(
                                  width = 12,
                                  selectInput(
                                    'chooseObjectscree',
                                    'Choose your Object',
                                    choices = list(
                                      "EppLab1" = "Epplabobject1",
                                      "EppLab2" = "Epplabobject2",
                                      "EppLab3" = "Epplabobject3",
                                      "EppLab4" = "Epplabobject4",
                                      "EppLab5" = "Epplabobject5"
                                    )
                                  )
                                )
                              )
                            ))),
                   tabPanel("Angles",
                            plotOutput("plot2"),
                            hr(),
                            wellPanel(fluidRow(
                              column(
                                width = 12,
                                align = "center",
                                h3("Control Panel"),
                                column(
                                  width = 12,
                                  sliderInput(
                                    'sampleSizeangles',
                                    'Sample Sizeangles',
                                    min = 1,
                                    max = 100,
                                    value = c(1, 100)
                                  )
                                ),
                                column(
                                  width = 12,
                                  selectInput(
                                    'chooseObjectangles',
                                    'Choose your Object',
                                    choices = list(
                                      "EppLab1" = "Epplabobject1",
                                      "EppLab2" = "Epplabobject2",
                                      "EppLab3" = "Epplabobject3",
                                      "EppLab4" = "Epplabobject4",
                                      "EppLab5" = "Epplabobject5"
                                    )
                                  )
                                )
                              )
                            ))),
                   tabPanel("Projections",
                            plotOutput("plot3"),
                            hr(),
                            wellPanel(fluidRow(
                              column(
                                width = 12,
                                align = "center",
                                h3("Control Panel"),
                                column(
                                  width = 12,
                                  textInput(
                                    inputId = "run",
                                    label = "Projections",
                                    value = "1,2,3",
                                    width = NULL
                                  )
                                ),
                                column(
                                  width = 12,
                                  selectInput(
                                    'chooseObjectruns',
                                    'Choose your Object',
                                    choices = list(
                                      "EppLab1" = "Epplabobject1",
                                      "EppLab2" = "Epplabobject2",
                                      "EppLab3" = "Epplabobject3",
                                      "EppLab4" = "Epplabobject4",
                                      "EppLab5" = "Epplabobject5"
                                    )
                                  )
                                )
                              )
                            ))),
                   tabPanel(
                     "Scatter plot with colored groups",
                     plotOutput("plot4"),
                     hr(),
                     wellPanel(fluidRow(
                       column(
                         width = 12,
                         align = "center",
                         h3("Control Panel"),
                         column(
                           width = 12,
                           column(
                             width = 6,
                             textInput(
                               inputId = "colruns2",
                               label = "Projections",
                               value = "1,2",
                               width = NULL
                             )
                           ),
                           column(width = 6,
                                  radioButtons(
                                    "colruns", "Color", c("No" = "no", "Yes" = "yes")
                                  ))
                         ),
                         column(
                           width = 12,
                         column(
                           width = 6,
                         numericInput(
                           inputId = "maxargcolselect",
                           label = "Restrict variables for coloring to have at most these many unique values:",
                           value = length(X[,1]),
                           min = 1,
                           width = NULL
                         )),
                         column(
                           width = 6,
                           selectInput('choosecolor', 'Choose your Color', choices = names(X))
                         )),
                         column(
                           width = 12,
                           selectInput(
                             'chooseObjectcol',
                             'Choose your Object',
                             choices = list(
                               "EppLab1" = "Epplabobject1",
                               "EppLab2" = "Epplabobject2",
                               "EppLab3" = "Epplabobject3",
                               "EppLab4" = "Epplabobject4",
                               "EppLab5" = "Epplabobject5"
                             )
                           )
                         )
                       )
                     ))
                   ),
                   tabPanel(
                     "Marginal Plots",
                     plotOutput("plot10"),
                     hr(),
                     wellPanel(fluidRow(
                       column(
                         width = 12,
                         align = "center",
                         h3("Control Panel"),
                         column(
                           width = 12,
                           textInput(
                             inputId = "marg",
                             label = "Runs",
                             value = "1,2,3",
                             width = NULL
                           )
                         ),
                         column(
                           width = 12,
                           selectInput(
                             'chooseObjectmarg',
                             'Choose your Object',
                             choices = list(
                               "EppLab1" = "Epplabobject1",
                               "EppLab2" = "Epplabobject2",
                               "EppLab3" = "Epplabobject3",
                               "EppLab4" = "Epplabobject4",
                               "EppLab5" = "Epplabobject5"
                             )
                           )
                         )
                       )
                     ))
                   ),
                   tabPanel(
                     "Aggregation of the Results",
                     textOutput("rankagg"),
                     plotOutput("plot7"),
                     hr(),
                     wellPanel(fluidRow(
                       column(
                         width = 12,
                         align = "center",
                         h3("Control Panel"),
                         column(
                           width = 12,
                           checkboxGroupInput(
                             "aggselect",
                             "Choose your objects",
                             inline = TRUE,
                             choices = list(
                               "EppLab1" = "Epplabobject1",
                               "EppLab2" = "Epplabobject2",
                               "EppLab3" = "Epplabobject3",
                               "EppLab4" = "Epplabobject4",
                               "EppLab5" = "Epplabobject5"
                             ),
                             selected = list(
                               "Object1" = "Epplabobject1",
                               "Object2" = "Epplabobject2",
                               "Object3" = "Epplabobject3"
                             )
                           )
                         ),
                         column(width = 6,
                                radioButtons(
                                  "aggcolruns", "Color", c("No" = "no", "Yes" = "yes")
                                )),
                         column(
                           width = 6,
                           selectInput('aggchoosecolor', 'Choose your Color', choices = names(X))
                         ),
                         column(
                           width = 12,
                           selectInput(
                             'chooseaggmethod',
                             'Choose your method',
                             choices = list("inverse" = "inverse",
                                            "cumulative" = "cumulative")
                           )
                         )
                       )
                     ))
                   )
                 )
               )),
#----------------------------------Outlier Detection---------------------------------
      tabPanel(
        "Outlier detection",
        navlistPanel(
          tabPanel(
            "Outlier",
            textInput(
              inputId = "locationselect",
              label = "Choose your location",
              value = "mean",
              width = NULL,
              placeholder = "mean"
            ),
            textInput(
              inputId = "scaleselect",
              label = "Choose your scale",
              value = "sd",
              width = NULL,
              placeholder = "sd"
            ),
            numericInput(
              inputId = "kselect",
              label = "k",
              value = 3,
              width = NULL
            ),
              selectInput(
                'chooseObjectoutlier',
                'Choose your Object',
                choices = list(
                  "EppLab1" = "Epplabobject1",
                  "EppLab2" = "Epplabobject2",
                  "EppLab3" = "Epplabobject3",
                  "EppLab4" = "Epplabobject4",
                  "EppLab5" = "Epplabobject5"
                )
              )
            
          ),
          tabPanel("Boxplot",
                   plotOutput("plot5")),
          tabPanel("Epplapp Outlier",
                   plotOutput("plot6")),
          tabPanel(
            "Visual inspection of the outlier candidates",
            plotOutput("plotoutliercandidates"),
            hr(),
            wellPanel(fluidRow(
              column(
                width = 12,
                align = "center",
                h3("Control Panel"),
                column(
                  width = 6,
                  numericInput(
                    inputId = "visualrun1",
                    label = "Projection 1",
                    value = 1,
                    width = NULL
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    inputId = "visualrun2",
                    label = "Projection 2",
                    value = 2,
                    width = NULL
                  )
                )
              ),
              column(width = 6,
                     numericInput(
                       inputId = "numoutlier",
                       label = "How often should an observation be marked as outlier to be colored?",
                       value = 3,
                       width = NULL
                     )
                     )
            ))
          ),
          tabPanel("Summary",
                   verbatimTextOutput("summaryoutlier"))
        )
      ),
#------------------------------Code Output-----------------------------------------
      tabPanel(
        "Codeoutput",
        downloadLink("downloadCode", "Get your code"),
        actionButton("close", "Close Session"),
        verbatimTextOutput("codeoutput"),
      )
    ),
    mainPanel()
  )
))
