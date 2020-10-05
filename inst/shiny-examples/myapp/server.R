library(shiny)

# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  #------------------------------------DataSwitch-------------------------------------------------------
  dataSwitch <- reactive(function(objectSelect) {
    switch(
      objectSelect,
      Epplabobject1 =
        {
          validate(need(ReactiveEpplab$epp1 != "", "Epplaboject 1 not calculated"))
          ReactiveEpplab$epp1
        },
      Epplabobject2 =
        {
          validate(need(ReactiveEpplab$epp2 != "", "Epplaboject 2 not calculated"))
          ReactiveEpplab$epp2
        },
      Epplabobject3 =
        {
          validate(need(ReactiveEpplab$epp3 != "", "Epplaboject 3 not calculated"))
          ReactiveEpplab$epp3
        },
      Epplabobject4 =
        {
          validate(need(ReactiveEpplab$epp4 != "", "Epplaboject 4 not calculated"))
          ReactiveEpplab$epp4
        },
      Epplabobject5 =
        {
          validate(need(ReactiveEpplab$epp5 != "", "Epplaboject 5 not calculated"))
          ReactiveEpplab$epp5
        }
    )
    
  })
  
  ReactiveEpplab <- reactiveValues()
  
  #------------------------------------EPPLAB--------------------------------------------------------
  output$textpoint1 <- renderText(":")
  output$textpoint2 <- renderText(":")
  output$textpoint3 <- renderText("EppLab1: Not calculated")
  output$textpoint4 <- renderText("EppLab2: Not calculated")
  output$textpoint5 <- renderText("EppLab3: Not calculated")
  output$textpoint6 <- renderText("EppLab4: Not calculated")
  output$textpoint7 <- renderText("EppLab5: Not calculated")
  output$textpoint8 <-
    renderText("Choose up to 5 different settings for the EPP computations.")
  output$textpoint10 <-
    renderText(
      "Note that depending on the number of simulations and EPP objects the computations might take a while."
    )
  output$textpoint11 <-
    renderText("----------------------------------------------------------------------")
  
  observeEvent(input$epp1epcalc,
               {
                 output$textpoint3 <- renderText("EppLab1: Calculated")
                 set.seed(input$epp1seed)
                 
                 ReactiveEpplab$epp1 <- EPPlab(
                   X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)],
                   PPalg = input$epp1ppalgselect,
                   PPindex = input$epp1ppindexselect,
                   n.simu = as.numeric(input$epp1simulationselect),
                   maxiter = as.numeric(input$epp1maxiterationselect),
                   sphere = input$epp1sphereselect
                 )
                 })
  
  
  observeEvent(input$epp2epcalc,
               {
                 output$textpoint4 <- renderText("EppLab2: Calculated")
                 set.seed(input$epp2seed)
                 ReactiveEpplab$epp2 <- EPPlab(
                   X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)],
                   PPalg = input$epp2ppalgselect,
                   PPindex = input$epp2ppindexselect,
                   n.simu = as.numeric(input$epp2simulationselect),
                   maxiter = as.numeric(input$epp2maxiterationselect),
                   sphere = input$epp1sphereselect
                 )
               })
  
  observeEvent(input$epp3epcalc,
               {
                 output$textpoint5 <- renderText("EppLab3: Calculated")
                 set.seed(input$epp3seed)
                 ReactiveEpplab$epp3 <- EPPlab(
                   X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)],
                   PPalg = input$epp3ppalgselect,
                   PPindex = input$epp3ppindexselect,
                   n.simu = as.numeric(input$epp3simulationselect),
                   maxiter = as.numeric(input$epp3maxiterationselect),
                   sphere = input$epp1sphereselect
                 )
               })
  
  observeEvent(input$epp4epcalc,
               {
                 output$textpoint6 <- renderText("EppLab4: Calculated")
                 set.seed(input$epp4seed)
                 ReactiveEpplab$epp4 <- EPPlab(
                   X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)],
                   PPalg = input$epp4ppalgselect,
                   PPindex = input$epp4ppindexselect,
                   n.simu = as.numeric(input$epp4simulationselect),
                   maxiter = as.numeric(input$epp4maxiterationselect),
                   sphere = input$epp1sphereselect
                 )
               })
  
  observeEvent(input$epp5epcalc,
               {
                 output$textpoint7 <- renderText("EppLab5: Calculated")
                 set.seed(input$epp5seed)
                 ReactiveEpplab$epp5 <- EPPlab(
                   X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)],
                   PPalg = input$epp5ppalgselect,
                   PPindex = input$epp5ppindexselect,
                   n.simu = as.numeric(input$epp5simulationselect),
                   maxiter = as.numeric(input$epp5maxiterationselect),
                   sphere = input$epp1sphereselect
                 )
               })
  
  #-----------------------------------Summary---------------------------------------------------------
  
  output$SummaryTableTitle <-
    renderUI(HTML(paste(
      "<h3> Your Datatable rounded to two Digits </h3>"
    )))
  
  output$summary1 <- renderDT(datatable(
    X,
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      searching = FALSE,
      digits = 2
    )
  )  %>% formatRound(c(1:length(X)), 2))
  
  output$summary2 <- renderPrint(summary(X))
  #----------------------------------------Cluster---------------------------------------------------------------
  #----------------Screeplot-------------
  output$plot <- renderPlot({
    screeplot(
      dataSwitch()(input$chooseObjectscree),
      which = input$sampleSize[1]:input$sampleSize[2],
      xaxt = "n",
      xlab = "Projections"
    )
    axis(
      1,
      at = 1:(input$sampleSize[2] - input$sampleSize[1] + 1),
      labels = c(input$sampleSize[1]:input$sampleSize[2])
    )
  })
  
  observe({
    val <- input$epp1simulationselect
    # Control the value, min, max.
    updateSliderInput(session, "sampleSize", value = c(1,val),
                      min = 1, max = val)
  })
 
  
  #---------------Angles-----------------
  output$plot2 <- renderPlot({
    plot(
      dataSwitch()(input$chooseObjectangles),
      type = "angles",
      which = input$sampleSizeangles[1]:input$sampleSizeangles[2],
      xaxt = "n"
    )
    axis(
      1,
      at = 1:(input$sampleSizeangles[2] - input$sampleSizeangles[1] + 1),
      labels = c(input$sampleSizeangles[1]:input$sampleSizeangles[2])
    )
  })
  observe({
    val <- input$epp1simulationselect
    # Control the value, min, max.
    updateSliderInput(session, "sampleSizeangles", value = c(1,val),
                      min = 1, max = val)
  })
  #------------------Runs------------------------------
  output$plot3 <-
    renderPlot(pairs(dataSwitch()(input$chooseObjectruns), which =
                       as.numeric(unlist(
                         strsplit(input$run, split = ",")
                       )), labels = paste(
                         cbind("Projection",as.numeric(unlist(strsplit(input$run, split = ","))))[,1],
                         cbind("Projection",as.numeric(unlist(strsplit(input$run, split = ","))))[,2])
                     ))
  #-------------------------Color---------------------------------------
  
  observe({
    x <- input$maxargcolselect
    choicevector <- rep(TRUE,length(X))
    for (i in 1:length(X)){
      if(length(unique(X[,i])) > x){
        choicevector[i] = FALSE
      }
    }
    # Can also set the label and select items
    updateSelectInput(session, "choosecolor",
                      choices = names(X)[choicevector],
    )
  })
  
  
  dataColor <- reactive(function(objectSelect) {
    switch(
      objectSelect,
      Epplabobject1 =
        {
          validate(need(ReactiveEpplab$epp1 != "", "Epplaboject 1 not calculated"))
          fitted(ReactiveEpplab$epp1, which = as.numeric(unlist(
            strsplit(input$colruns2, split = ",")
          )))
        },
      Epplabobject2 =
        {
          validate(need(ReactiveEpplab$epp2 != "", "Epplaboject 2 not calculated"))
          fitted(ReactiveEpplab$epp2, which = as.numeric(unlist(
            strsplit(input$colruns2, split = ",")
          )))
        },
      Epplabobject3 =
        {
          validate(need(ReactiveEpplab$epp3 != "", "Epplaboject 3 not calculated"))
          fitted(ReactiveEpplab$epp3, which = as.numeric(unlist(
            strsplit(input$colruns2, split = ",")
          )))
        },
      Epplabobject4 =
        {
          validate(need(ReactiveEpplab$epp4 != "", "Epplaboject 4 not calculated"))
          fitted(ReactiveEpplab$epp4, which = as.numeric(unlist(
            strsplit(input$colruns2, split = ",")
          )))
        },
      Epplabobject5 =
        {
          validate(need(ReactiveEpplab$epp5 != "", "Epplaboject 5 not calculated"))
          fitted(ReactiveEpplab$epp5, which = as.numeric(unlist(
            strsplit(input$colruns2, split = ",")
          )))
        }
    )
    
  })
  
  output$plot4 <- renderPlot(if (length(as.numeric(unlist(
    strsplit(input$colruns2, split = ",")
  ))) == 2) {
    switch(
      input$colruns,
      yes = plot(
        dataColor()(input$chooseObjectcol),
        col = rainbow(length(levels(factor(
          X[, input$choosecolor]
        ))))[factor(X[, input$choosecolor])],
        pch = 16, 
        xlab =  paste("Projection ",unlist(strsplit(input$colruns2, split = ","))[1]), 
        ylab =  paste("Projection ",unlist(strsplit(input$colruns2, split = ","))[2])
      ),
      no = plot(dataColor()(input$chooseObjectcol), pch = 16, 
                xlab =  paste("Projection ",unlist(strsplit(input$colruns2, split = ","))[1]), 
                ylab =  paste("Projection ",unlist(strsplit(input$colruns2, split = ","))[2]))
    )
    if (input$colruns == "yes") {
      legend(
        "topright",
        y = 0.92,
        legend = levels(factor(X[, input$choosecolor])),
        col = rainbow(length(levels(factor(
          X[, input$choosecolor]
        )))),
        pch = 16
      )
    }
  }
  else{
    switch(
      input$colruns,
      yes = pairs(
        dataSwitch()(input$chooseObjectcol),
        which = as.numeric(unlist(strsplit(
          input$colruns2, split = ","
        ))),
        col = rainbow(length(levels(factor(
          X[, input$choosecolor]
        ))))[factor(X[, input$choosecolor])], labels = paste(
          cbind("Projection",as.numeric(unlist(strsplit(input$colruns2, split = ","))))[,1],
          cbind("Projection",as.numeric(unlist(strsplit(input$colruns2, split = ","))))[,2])
      ),
      no = pairs(dataSwitch()(input$chooseObjectcol), which = as.numeric(unlist(
        strsplit(input$colruns2, split = ",")
      )), labels = paste(
        cbind("Projection",as.numeric(unlist(strsplit(input$colruns2, split = ","))))[,1],
        cbind("Projection",as.numeric(unlist(strsplit(input$colruns2, split = ","))))[,2]))
    )
  })
  
  #----------------------------------------- Marginal density ---------------------------------------
  output$plot10 <- renderPlot(plot(
    dataSwitch()(input$chooseObjectmarg),
    which =
      as.numeric(unlist(strsplit(
        input$marg, split = ","
      ))),
    layout = c(3, ceiling(length(
      strsplit(input$marg, split = ",")[[1]]
    ) / 3)),
    xlab = "Projections"
  ))
  
  #--------------------------------------Aggregation of the result ----------------------------------
  
  dataAggregation <- reactive({
    validate(need(length(input$aggselect) > 1, "Not enough objects selected"),
             if ("Epplabobject1" %in% input$aggselect) {
               need(ReactiveEpplab$epp1 != "", "Epplaboject 1 not calculated")
             },
             if ("Epplabobject2" %in% input$aggselect) {
               need(ReactiveEpplab$epp2 != "", "Epplaboject 2 not calculated")
             },
             if ("Epplabobject3" %in% input$aggselect) {
               need(ReactiveEpplab$epp3 != "", "Epplaboject 3 not calculated")
             },
             if ("Epplabobject4" %in% input$aggselect) {
               need(ReactiveEpplab$epp4 != "", "Epplaboject 4 not calculated")
             },
             if ("Epplabobject5" %in% input$aggselect) {
               need(ReactiveEpplab$epp5 != "", "Epplaboject 5 not calculated")
             })
    c(
      list(ReactiveEpplab$epp1),
      list(ReactiveEpplab$epp2),
      list(ReactiveEpplab$epp3),
      list(ReactiveEpplab$epp4),
      list(ReactiveEpplab$epp5)
    )
  })
  
  output$plot7 <-
    renderPlot(switch(
      input$aggcolruns,
      yes = pairs(
        as.matrix(X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)]) %*% EPPlabAgg(dataAggregation()[c(
          "Epplabobject1" %in% input$aggselect,
          "Epplabobject2" %in% input$aggselect,
          "Epplabobject3" %in% input$aggselect,
          "Epplabobject4" %in% input$aggselect,
          "Epplabobject5" %in% input$aggselect
        )], method = input$chooseaggmethod)$O,
        col = rainbow(length(levels(factor(
          X[, input$aggchoosecolor]
        ))))[factor(X[, input$aggchoosecolor])],
        pch = 16
      ),
      no = pairs(
        as.matrix(X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)]) %*% EPPlabAgg(dataAggregation()[c(
          "Epplabobject1" %in% input$aggselect,
          "Epplabobject2" %in% input$aggselect,
          "Epplabobject3" %in% input$aggselect,
          "Epplabobject4" %in% input$aggselect,
          "Epplabobject5" %in% input$aggselect
        )], method = input$chooseaggmethod)$O
      )
    ))
  
  output$rankagg <-
    renderText({
      paste(
        "The rank of the average orthogonal projection matrix:",
        EPPlabAgg(dataAggregation()[c(
          "Epplabobject1" %in% input$aggselect,
          "Epplabobject2" %in% input$aggselect,
          "Epplabobject3" %in% input$aggselect,
          "Epplabobject4" %in% input$aggselect,
          "Epplabobject5" %in% input$aggselect
        )], method = input$chooseaggmethod)$k
      )
    })
  
  #----------------------------------------Outlier Detection -----------------------------------------------------
  
  output$plot5 <-
    renderPlot(boxplot(X[auswahl(input$eppobsselect), auswahl(input$eppvarselect)]))
  
  output$summaryoutlier <-
    renderPrint({
      b <-
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )
      b$location <- input$locationselect
      b$scale <- input$scaleselect
      summary(b)
    })
  
  output$plot6 <- renderPlot(plot(
    EPPlabOutlier(
      dataSwitch()(input$chooseObjectoutlier),
      k = as.numeric(input$kselect),
      location = as.function(as.list.function(input$locationselect)),
      scale = as.function(as.list.function(input$scaleselect))
    )
  ))
  #------------------------Visual inspection--------------------------------
  
  output$plotoutliercandidates <- renderPlot({
    plot(
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) < input$numoutlier +1, ],
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) < input$numoutlier +1, ],
      ylim = c(min(range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)
      ), range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)
      )) - 5, max(range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)
      ), range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)
      )) + 5),
      xlim = c(min(range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)
      ), range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)
      )) - 5, max(range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)
      ), range(
        fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)
      )) + 5),
      ylab = "Projection 1",
      xlab = "Projection 2"
    )
    
    points(
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) > input$numoutlier, ],
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) > input$numoutlier, ],
      col = "red",
      pch = 16
    )
    
    text(
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun1)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) > input$numoutlier, ],
      fitted(dataSwitch()(input$chooseObjectoutlier), which = input$visualrun2)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) > input$numoutlier, ],
      pos = 2,
      label = row.names(X)[apply(
        EPPlabOutlier(
          dataSwitch()(input$chooseObjectoutlier),
          k = as.numeric(input$kselect),
          location = as.function(as.list.function(input$locationselect)),
          scale = as.function(as.list.function(input$scaleselect))
        )$outlier,
        1,
        sum
      ) > input$numoutlier]
    )
    
  })
  #-----------------------------------------Code Output------------------------------------------------------------
  
  output$codeoutput <- renderText({
    paste(
      "#---------- Epplab Object 1 ----------  \n#--Set Seed for Epplabobject1 \nset.seed(",
      input$epp1seed,
      ")",
      "\n #--Calculate Epplabobject1 \nEpplabobject1 <- EPPlab(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]",
      ", PPalg = \"",
      input$epp1ppalgselect,
      "\", PPindex = \"",
      input$epp1ppindexselect,
      "\", n.simu = ",
      toString(as.numeric(input$epp1simulationselect)),
      ", maxiter = ",
      toString(as.numeric(input$epp1maxiterationselect)),
      ", sphere = ",
      input$epp1sphereselect,
      ")",
      "\n#---------- Epplab Object 2 ----------  \n#--Set Seed for Epplabobect2 \nset.seed(",
      input$epp2seed,
      ")",
      "\n #--Calculate Epplabobject2 \nEpplabobject2 <- EPPlab(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]",
      ",PPalg = \"",
      input$epp2ppalgselect,
      "\", PPindex = \"",
      input$epp2ppindexselect,
      "\", n.simu = ",
      toString(as.numeric(input$epp2simulationselect)),
      ", maxiter = ",
      toString(as.numeric(input$epp2maxiterationselect)),
      ", sphere = ",
      input$epp1sphereselect,
      ")",
      "\n#---------- Epplab Object 3 ----------  \n#--Set Seed for Epplabobect3 \nset.seed(",
      input$epp3seed,
      ")",
      "\n#--Calculate Epplabobject3 \nEpplabobject3 <- EPPlab(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]",
      ",PPalg = \"",
      input$epp3ppalgselect,
      "\", PPindex = \"",
      input$epp3ppindexselect,
      "\", n.simu = ",
      toString(as.numeric(input$epp3simulationselect)),
      ", maxiter = ",
      toString(as.numeric(input$epp3maxiterationselect)),
      ", sphere = ",
      input$epp1sphereselect,
      ")",
      "\n#---------- Epplab Object 4 ----------  \n#--Set Seed for Epplabobect4 \nset.seed(",
      input$epp4seed,
      ")",
      "\n#--Calculate Epplabobject4 \nEpplabobject4 <- EPPlab(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]",
      ",PPalg = \"",
      input$epp4ppalgselect,
      "\", PPindex = \"",
      input$epp4ppindexselect,
      "\", n.simu = ",
      toString(as.numeric(input$epp4simulationselect)),
      ", maxiter = ",
      toString(as.numeric(input$epp4maxiterationselect)),
      ", sphere = ",
      input$epp1sphereselect,
      ")",
      "\n#---------- Epplab Object 5 ----------  \n#--Set Seed for Epplabobect5 \nset.seed(",
      input$epp5seed,
      ")",
      "\n#--Calculate Epplabobject5 \nEpplabobject5 <- EPPlab(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]",
      ",PPalg = \"",
      input$epp5ppalgselect,
      "\", PPindex = \"",
      input$epp5ppindexselect,
      "\", n.simu = ",
      toString(as.numeric(input$epp5simulationselect)),
      ", maxiter = ",
      toString(as.numeric(input$epp5maxiterationselect)),
      ", sphere = ",
      input$epp1sphereselect,
      ")",
      "\n#----- Plots -----",
      "\n#--- Screeplot ---",
      "\nscreeplot(",
      input$chooseObjectscree,
      ", which = 1:",
      input$sampleSize[2],
      ")",
      "\n#--- Angles ---",
      "\nplot(",
      input$chooseObjectangles,
      ", type = \"angles\", which = 1:",
      input$sampleSizeangles[2],
      ")",
      "\n#--- Runs ---",
      "\npairs(",
      input$chooseObjectruns,
      ",which = c(",
      input$run,
      "))",
      "\n#--- Col ---",
      "\nplot(fitted(",
      input$chooseObjectcol,
      ", which = ",
      "as.numeric(unlist(strsplit(\"",
      input$colruns2,
      "\", split = \",\"))),), col = rainbow(length(levels(factor(X[ , \"",
      input$choosecolor,
      "\"]))))[factor(X[ , \"",
      input$choosecolor,
      "\"])], pch = 16)",
      "\n#---------- Marginal Plots ----------",
      "\nplot(",
      input$chooseObjectmarg,
      ",which = as.numeric(unlist(strsplit(\"",
      input$marg,
      "\", split = \",\"))), layout = c(3, ceiling(length(strsplit(\"",
      input$marg,
      "\", split = \", \")[[1]]) / 3)), xlab = \"Runs\")",
      "\n#---------- Aggregation ----------",
      "\npairs(as.matrix(X[",
      input$eppobsselect,
      ", ",
      input$eppvarselect,
      "]) %*% EPPlabAgg(list(",
      paste(unlist(input$aggselect), collapse = ","),
      "), method = \"",
      input$chooseaggmethod,
      "\")$O, col = rainbow(length(levels(factor(X[ , \"",
      input$aggchoosecolor,
      "\"]))))[factor(X[ , \"",
      input$aggchoosecolor,
      "\"])], pch = 16)",
      "\n#---------- Epplab Outlier Object ----------",
      "\nEppout <- EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      as.numeric(input$kselect),
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")",
      "\n#----- Summary -----",
      "\nsummary(Eppout)",
      "\n#----- Plots -----",
      "\n#--- Boxplot ---",
      "\nboxplot(X)",
      "\n#--- Outlier ---",
      "\nplot(Eppout)",
      "\n#----------visual inspection---------",
      "\nplot(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) < ",input$numoutlier+1, ", ], fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) < ",input$numoutlier+1, ", ], ylim = c(min(range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")), range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      "))) - 5, max(range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")), range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      "))) + 5), xlim = c(min(range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")), range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      "))) - 5, max(range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")), range(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      "))) + 5), ylab = \"Projection 1\", xlab = \"Projection 2\")",
      "\npoints(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) > ",input$numoutlier, ", ], fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) > ",input$numoutlier, ", ], col = \"red\", pch = 16)",
      "\ntext(fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun1,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) > ",input$numoutlier, ", ], fitted(",input$chooseObjectoutlier,", which = ",
      input$visualrun2,
      ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) > ",input$numoutlier, ", ], pos = 2, label = row.names(X)[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
      input$kselect,
      ", location = ",
      input$locationselect,
      ", scale = ",
      input$scaleselect,
      ")$outlier, 1, sum) > ",input$numoutlier, "])",
      sep = ""
    )
  })
  
  output$downloadCode <- downloadHandler(
    filename = function() {
      paste("Epplabcode-", Sys.Date(), ".R", sep = "")
    },
    content = function(file) {
      write(
        paste(
          "#---------- Epplab Object 1 ----------  \n#--Set Seed for Epplabobject1 \nset.seed(",
          input$epp1seed,
          ")",
          "\n #--Calculate Epplabobject1 \nEpplabobject1 <- EPPlab(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]",
          ", PPalg = \"",
          input$epp1ppalgselect,
          "\", PPindex = \"",
          input$epp1ppindexselect,
          "\", n.simu = ",
          toString(as.numeric(input$epp1simulationselect)),
          ", maxiter = ",
          toString(as.numeric(input$epp1maxiterationselect)),
          ", sphere = ",
          input$epp1sphereselect,
          ")",
          "\n#---------- Epplab Object 2 ----------  \n#--Set Seed for Epplabobect2 \nset.seed(",
          input$epp2seed,
          ")",
          "\n #--Calculate Epplabobject2 \nEpplabobject2 <- EPPlab(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]",
          ",PPalg = \"",
          input$epp2ppalgselect,
          "\", PPindex = \"",
          input$epp2ppindexselect,
          "\", n.simu = ",
          toString(as.numeric(input$epp2simulationselect)),
          ", maxiter = ",
          toString(as.numeric(input$epp2maxiterationselect)),
          ", sphere = ",
          input$epp1sphereselect,
          ")",
          "\n#---------- Epplab Object 3 ----------  \n#--Set Seed for Epplabobect3 \nset.seed(",
          input$epp3seed,
          ")",
          "\n#--Calculate Epplabobject3 \nEpplabobject3 <- EPPlab(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]",
          ",PPalg = \"",
          input$epp3ppalgselect,
          "\", PPindex = \"",
          input$epp3ppindexselect,
          "\", n.simu = ",
          toString(as.numeric(input$epp3simulationselect)),
          ", maxiter = ",
          toString(as.numeric(input$epp3maxiterationselect)),
          ", sphere = ",
          input$epp1sphereselect,
          ")",
          "\n#---------- Epplab Object 4 ----------  \n#--Set Seed for Epplabobect4 \nset.seed(",
          input$epp4seed,
          ")",
          "\n#--Calculate Epplabobject4 \nEpplabobject4 <- EPPlab(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]",
          ",PPalg = \"",
          input$epp4ppalgselect,
          "\", PPindex = \"",
          input$epp4ppindexselect,
          "\", n.simu = ",
          toString(as.numeric(input$epp4simulationselect)),
          ", maxiter = ",
          toString(as.numeric(input$epp4maxiterationselect)),
          ", sphere = ",
          input$epp1sphereselect,
          ")",
          "\n#---------- Epplab Object 5 ----------  \n#--Set Seed for Epplabobect5 \nset.seed(",
          input$epp5seed,
          ")",
          "\n#--Calculate Epplabobject5 \nEpplabobject5 <- EPPlab(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]",
          ",PPalg = \"",
          input$epp5ppalgselect,
          "\", PPindex = \"",
          input$epp5ppindexselect,
          "\", n.simu = ",
          toString(as.numeric(input$epp5simulationselect)),
          ", maxiter = ",
          toString(as.numeric(input$epp5maxiterationselect)),
          ", sphere = ",
          input$epp1sphereselect,
          ")",
          "\n#----- Plots -----",
          "\n#--- Screeplot ---",
          "\nscreeplot(",
          input$chooseObjectscree,
          ", which = 1:",
          input$sampleSize[2],
          ")",
          "\n#--- Angles ---",
          "\nplot(",
          input$chooseObjectangles,
          ", type = \"angles\", which = 1:",
          input$sampleSizeangles[2],
          ")",
          "\n#--- Runs ---",
          "\npairs(",
          input$chooseObjectruns,
          ",which = c(",
          input$run,
          "))",
          "\n#--- Col ---",
          "\nplot(fitted(",
          input$chooseObjectcol,
          ", which = ",
          "as.numeric(unlist(strsplit(\"",
          input$colruns2,
          "\", split = \",\"))),), col = rainbow(length(levels(factor(X[ , \"",
          input$choosecolor,
          "\"]))))[factor(X[ , \"",
          input$choosecolor,
          "\"])], pch = 16)",
          "\n#---------- Marginal Plots ----------",
          "\nplot(",
          input$chooseObjectmarg,
          ",which = as.numeric(unlist(strsplit(\"",
          input$marg,
          "\", split = \",\"))), layout = c(3, ceiling(length(strsplit(\"",
          input$marg,
          "\", split = \", \")[[1]]) / 3)), xlab = \"Runs\")",
          "\n#---------- Aggregation ----------",
          "\npairs(as.matrix(X[",
          input$eppobsselect,
          ", ",
          input$eppvarselect,
          "]) %*% EPPlabAgg(list(",
          paste(unlist(input$aggselect), collapse = ","),
          "), method = \"",
          input$chooseaggmethod,
          "\")$O, col = rainbow(length(levels(factor(X[ , \"",
          input$aggchoosecolor,
          "\"]))))[factor(X[ , \"",
          input$aggchoosecolor,
          "\"])], pch = 16)",
          "\n#---------- Epplab Outlier Object ----------",
          "\nEppout <- EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          as.numeric(input$kselect),
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")",
          "\n#----- Summary -----",
          "\nsummary(Eppout)",
          "\n#----- Plots -----",
          "\n#--- Boxplot ---",
          "\nboxplot(X)",
          "\n#--- Outlier ---",
          "\nplot(Eppout)",
          "\n#----------visual inspection---------",
          "\nplot(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) < ",input$numoutlier+1, ", ], fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) < ",input$numoutlier+1, ", ], ylim = c(min(range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")), range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          "))) - 5, max(range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")), range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          "))) + 5), xlim = c(min(range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")), range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          "))) - 5, max(range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")), range(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          "))) + 5), ylab = \"Projection 1\", xlab = \"Projection 2\")",
          "\npoints(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) > ",input$numoutlier, ", ], fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) > ",input$numoutlier, ", ], col = \"red\", pch = 16)",
          "\ntext(fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun1,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) > ",input$numoutlier, ", ], fitted(",input$chooseObjectoutlier,", which = ",
          input$visualrun2,
          ")[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) > ",input$numoutlier, ", ], pos = 2, label = row.names(X)[apply(EPPlabOutlier(",input$chooseObjectoutlier,", k = ",
          input$kselect,
          ", location = ",
          input$locationselect,
          ", scale = ",
          input$scaleselect,
          ")$outlier, 1, sum) > ",input$numoutlier, "])",
          sep = ""
        ),
        file
      )
    }
  )

  observeEvent(input$close, {
    epp_list <-
      structure(
        list(
          "Epplabobject1" = ReactiveEpplab$epp1,
          "Epplabobject2" = ReactiveEpplab$epp2,
          "Epplabobject3" = ReactiveEpplab$epp3,
          "Epplabobject4" = ReactiveEpplab$epp4,
          "Epplabobject5" = ReactiveEpplab$epp5
        ),
        class = "epplabshiny"
      )
    epp_list <-
      list(
        "Epplabobject1" = ReactiveEpplab$epp1,
        "Epplabobject2" = ReactiveEpplab$epp2,
        "Epplabobject3" = ReactiveEpplab$epp3,
        "Epplabobject4" = ReactiveEpplab$epp4,
        "Epplabobject5" = ReactiveEpplab$epp5
      )
    class(epp_list) <- "epplabshiny"
    stopApp(returnValue = invisible(epp_list))
  })
  
  observeEvent(input$close1, {
    epp_list <-
      structure(
        list(
          "Epplabobject1" = ReactiveEpplab$epp1,
          "Epplabobject2" = ReactiveEpplab$epp2,
          "Epplabobject3" = ReactiveEpplab$epp3,
          "Epplabobject4" = ReactiveEpplab$epp4,
          "Epplabobject5" = ReactiveEpplab$epp5
        ),
        class = "epplabshiny"
      )
    epp_list <-
      list(
        "Epplabobject1" = ReactiveEpplab$epp1,
        "Epplabobject2" = ReactiveEpplab$epp2,
        "Epplabobject3" = ReactiveEpplab$epp3,
        "Epplabobject4" = ReactiveEpplab$epp4,
        "Epplabobject5" = ReactiveEpplab$epp5
      )
    class(epp_list) <- "epplabshiny"
    stopApp(returnValue = invisible(epp_list))
  })
})
