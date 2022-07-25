## ###########################################################
##
## dice/app.R
##
## Educational shiny app to teach students the ideas
## behind a chi-square distribution using the investigation
## of different dice making companies
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2021-Jan-24
##
## ###########################################################

library(shiny)
library(ggplot2)

# Returns string without leading white space
trim.leading <- function (x)
  sub("^\\s+", "", x)

# Returns string without trailing white space
trim.trailing <- function (x)
  sub("\\s+$", "", x)

# Returns string without leading or trailing white space
trim <- function (x)
  gsub("^\\s+|\\s+$", "", x)

## ###########################################################
## Return the list of company names as a vector
## ###########################################################
getDiceCompanyNamesAsVector <- function()
{
  company_ids = c()
  company_names = c()
  allCompanies = getDiceCompanyNames()
  for (row in 1:nrow(allCompanies))
  {
    entry = allCompanies[row, ]
    company_ids = c(company_ids, entry$CompanyId)
    company_names = c(company_names, entry$CompanyName)
  }
  
  # Assign the names to the IDs
  names(company_ids) <- company_names
  return(company_ids)
}

## ###########################################################
## Return the list of company names as a dataframe
## ###########################################################
getDiceCompanyNames <- function()
{
  # Our dice company names
  infile = paste("company_names.txt", sep = "")
  companyNames = read.csv(infile, header = T, stringsAsFactors = F)
  return(companyNames)
}

## ###########################################################
## Lookup a company name from it's ID value
## ###########################################################
getCompanyById <- function(companyId)
{
  allCompanies = getDiceCompanyNames()
  for (row in 1:nrow(allCompanies))
  {
    entry = allCompanies[row, ]
    if (entry$CompanyId == companyId)
    {
      return(entry$CompanyName)
    }
  }
  return("")
}


## ###########################################################
##
## Load the company weights from text file
##
## Note, this expects the weights to be specified
## in fractional notation in the CSV input file.
## ###########################################################
getAllCompanyWeights <- function()
{
  library(stringr)
  # How many companies are configured?
  totalCompanies = nrow(getDiceCompanyNames())
  
  # Our dice company names
  infile = paste("company_weights.csv", sep = "")
  companyWeights = read.csv(infile, header = T, stringsAsFactors = F)
  allWeights = list()
  index = 1
  for (row in 1:nrow(companyWeights))
  {
    entry = companyWeights[row,]
    entryWeights = c()
    for (colidx in 1:length(colnames(entry)))
    {
      value = trim(entry[, colidx])
      fraction = str_split(value, "/")
      numerator = as.numeric(fraction[[1]][1])
      denominator = as.numeric(fraction[[1]][2])
      decimal = (numerator * 1.0) / (denominator * 1.0)
      entryWeights = c(entryWeights, decimal)
    }
    
    allWeights[[index]] = entryWeights
    index = index + 1
  }
  
  return(allWeights)
}



## ###########################################################
## Compute the distributions of the company dice rolls
## ###########################################################
getMyDistributions <- function() {
  my_weights = getAllCompanyWeights()
  
  my_distributions = list()
  current_company_index = 1
  for (company_weights in my_weights)
  {
    max = 10000
    distribution = c()
    dice_value = 1
    for (dice_weight in company_weights)
    {
      repeat_roll = floor(max * dice_weight)
      repeated_roll = rep.int(dice_value, repeat_roll)
      distribution = c(distribution, repeated_roll)
      dice_value = dice_value + 1
    }
    
    # Adding vec to list
    my_distributions[[current_company_index]] <- distribution
    current_company_index = current_company_index + 1
  }
  return(my_distributions)
}


## ###########################################################
## Get the weights of a single company
## ###########################################################
getCompanyWeight <- function(companyId)
{
  current_company_index = 1
  for (weights in getAllCompanyWeights())
  {
    if (current_company_index == companyId)
      return(weights)
    current_company_index = current_company_index + 1
  }
  return(NULL)
}

## ###########################################################
## END DICE FUNCTIONS
## ###########################################################


##############################################################
# Load these to start
all_companies = getDiceCompanyNamesAsVector()

# store these once for all users
dice_distributions <- getMyDistributions()

# we want to keep text input but students
# can still type in a number than the max
# set in the input, this is a safety check
# to compare in our code and prevent the server
# from being overloaded
MAX_SIMULATIONS = 500
MAX_SAMPLES = 10000

ui <- navbarPage(
  title = "Dice Explorer",
  
  ## ###########################################################
  ## Panel 1
  ## ###########################################################
  tabPanel("Analysis",
           
           sidebarLayout(
             sidebarPanel(
               em("Select a company and sample size"),
               
               # input
               selectInput(
                 inputId = "panel1_input_company_id",
                 label = "Dice Company",
                 all_companies,
                 selected = 1
               ),
               
               # input
               numericInput(
                 inputId = "panel1_input_sample_size",
                 label = "Sample Size  (10,000 max)",
                 value = 10,
                 min = 1,
                 max = 10000
               ),
               
               actionButton(inputId = "panel1_btn_graph", label = "Graph Samples"),
               
               # text output
               htmlOutput(outputId = "panel1_output_dice_freq")
               
             ),
             
             mainPanel(
               HTML(
                 '<div style="float: right;"><img src="color-dice.png" style="width:150px;"></div>'
               ),
               includeHTML("www/task_01.html"),
               
               # output
               plotOutput(outputId = "panel1_histogram_plot"),
             )
           ),),
  
  ## ###########################################################
  ## panel 2 Test Statistic
  ## ###########################################################
  tabPanel("Test Statistic",
           
           
           
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               numericInput(
                 inputId = "samples",
                 label = "Number of rolls",
                 value = 20,
                 min = 0,
                 max = 100
               ),
               
               
               
               
               checkboxInput("chk_line", label = "Show expected value line", value = F),
               checkboxInput("chk_exp", label = "Show observed - expected values", value = F),
               checkboxInput("chk", label = "Show Observed and Expected Calculations", value = F),
               checkboxInput("chk2", label = "Show Summations", value = F),
               radioButtons(
                 "radio",
                 label = "",
                 choices = list("Show Weights" = 1, "Hide Weights" = 2),
                 selected = 2
               ),
               
               conditionalPanel(
                 condition = "input.radio == 1",
                 numericInput(
                   inputId = "weight1",
                   label = "Set weight of outcome 1",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                 ),
                 numericInput(
                   inputId = "weight2",
                   label = "Set weight of outcome 2",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                 ),
                 numericInput(
                   inputId = "weight3",
                   label = "Set weight of outcome 3",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                 ),
                 numericInput(
                   inputId = "weight4",
                   label = "Set weight of outcome 4",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                 ),
                 numericInput(
                   inputId = "weight5",
                   label = "Set weight of outcome 5",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                 ),
                 numericInput(
                   inputId = "weight6",
                   label = "Set weight of outcome 6",
                   value = round(1 / 6, 4),
                   min = 0,
                   max = 1
                   
                 )
               ),
               
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               includeHTML("www/task_02.html"),
               dataTableOutput("stat_table"),
               plotOutput("distPlot"),
               
               dataTableOutput("test_table"),
               
               
             )
             
           )),
  
  
  ## ###########################################################
  ## panel 3: Distribution
  ## ###########################################################
  tabPanel("Distribution",
           sidebarLayout(
             sidebarPanel(
               em("Pips and Dots Analysis"),
               # input
              
               
               # input
               numericInput(
                 inputId = "panel3_input_sample_size",
                 label = "Sample Size (min = 30, max = 10,000)",
                 value = 30,
                 min = 30,
                 max = 10000
               ),
               
               numericInput(
                 inputId = "panel3_input_simulations",
                 label = "Number of simulations (500 max)",
                 value = 1,
                 min = 1,
                 max = 500
               ),
               
               actionButton(inputId = "panel3_btn_graph", label = "Update Sample Size"),
               
               checkboxInput("slider", "Show cut-off slider", FALSE),
               
               conditionalPanel(
                 condition = "input.slider == 1",
                 sliderInput("panel3_slider_cutoff", "Choose cut-off Value",
                             min = 0, max = 30, value = 0
                 )
               ),
               
               
               
               hr(),
               # html - adds break line
               h4("True weight of dice"),
               # html header level 4
               plotOutput(
                 outputId = "panel3_graph_company_weights",
                 height = "150px",
                 width = "100%"
               )
               
             ),
             
             mainPanel(
               includeHTML("www/task_03.html"),
               # put the plots up top so easier to see
               plotOutput(outputId = "panel3_graph_chisquare"),
               
               # text output
               verbatimTextOutput(outputId = "panel3_output_test_statistic"),
               
               
               
             )
           )),
  ## ###########################################################
  ## panel 4 Chi Square Analysis
  ## ###########################################################
  tabPanel(
    "Chi Square Analysis",
    sidebarLayout(
      sidebarPanel(
        # input
        selectInput(
          inputId = "panel4_input_company_id",
          label = "Dice Company",
          all_companies,
          selected = 1
        ),
        
        # input
        numericInput(
          inputId = "panel4_input_sample_size",
          label = "Sample Size (10,000 max)",
          value = 10,
          min = 1,
          max = 10000
        ),
        
        actionButton(inputId = "panel4_btn_graph",
                     label = "Update Sample Size"),
        
        # text output
        htmlOutput(outputId = "panel4_output_dice_percents"),
        
        # text output
        verbatimTextOutput(outputId = "panel4_output_test_statistic"),
      ),
      
      mainPanel(includeHTML("www/task_04.html"),
                
                # output
                fluidRow(
                  column(5, plotOutput(outputId = "panel4_graph_histogram"), ),
                  column(5, plotOutput(outputId = "panel4_graph_chisquare"))
                ),)
    ),
  ),
  ## ###########################################################
  ## Panel 5: Power
  ## ###########################################################
  tabPanel("Power",
           sidebarLayout(
             sidebarPanel(
               # input
               selectInput(
                 inputId = "panel5_input_company_id",
                 label = "Dice Company",
                 all_companies,
                 selected = 1
               ),
               
               # input
               numericInput(
                 inputId = "panel5_input_sample_size",
                 label = "Sample Size (10,000 max)",
                 value = 10,
                 min = 1,
                 max = 10000
               ),
               
               numericInput(
                 inputId = "panel5_input_simulations",
                 label = "Number of simulations (500 max)",
                 value = 100,
                 min = 1,
                 max = 500
               ),
               
               actionButton(inputId = "panel5_btn_graph", label = "Update Sample Size"),
               
               hr(),
               # html - adds break line
               h4("True weight of dice"),
               # html header level 4
               plotOutput(
                 outputId = "panel5_graph_company_weights",
                 height = "150px",
                 width = "100%"
               )
               
             ),
             
             mainPanel(
               # put the plots up top so easier to see
               plotOutput(outputId = "panel5_graph_chisquare"),
               
               # text output
               verbatimTextOutput(outputId = "panel5_output_test_statistic"),
               
               includeHTML("www/task_05.html")
               
             )
           )),
  tabPanel("About",
           includeHTML("www/about.html"))
)


server <- function(input, output, session) {
  ## #################################
  ## Panel 1 input/outputs
  ## #################################
  # INPUT
  #   panel1_input_company_id
  #   panel1_input_sample_size
  #   panel1_btn_graph
  ## #################################
  # OUTPUT
  #   panel1_output_dice_freq
  ## #################################
  panel1_rv <- eventReactive(input$panel1_btn_graph, {
    total_samples = input$panel1_input_sample_size
    if ( total_samples > MAX_SAMPLES )
    {
      total_samples = MAX_SAMPLES
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel1_input_sample_size",
        value = MAX_SAMPLES
      )
    }

    companyId = as.numeric(input$panel1_input_company_id)
    companyWeights = dice_distributions[[5]]
    dice_roll = sample(companyWeights, total_samples, replace = T)
    tbl = table(dice_roll)
    dd = data.frame(tbl)
    
    # check for missing dice rolls
    all_rolls = c(1:6)
    dice_rolled = as.numeric(row.names(tbl))
    # need labels as a string to add to data frame
    missing_rolls = as.character(setdiff(all_rolls, dice_rolled))
    for (dice in missing_rolls)
    {
      newrow = data.frame("dice_roll" = dice, "Freq" = 0)
      dd = rbind(dd, newrow)
    }
    
    dd$percent <- dd$Freq / sum(dd$Freq)
    
    # dice are factors and need this to order empty dice rolls
    dd$dice_roll <-
      factor(dd$dice_roll, levels = as.character(c(1:6)))
    dd = dd[order(dd$dice_roll), ]
    row.names(dd) <- NULL
    results_dataframe = dd
    return(dd)
  })
  
  
  ######################################################
  # Panel 1
  ######################################################
  myTitle <- eventReactive(input$panel1_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel1_input_company_id))
    rolls = input$panel1_input_sample_size
    if ( rolls > MAX_SAMPLES )
    {
      # we need to reset the input to max and override
      rolls = MAX_SAMPLES
    }
    title = paste(rolls, " dice rolls from Company: ", companyName)
  })
  
  # ggplot our data frame
  output$panel1_histogram_plot <- renderPlot({
    ggplot(panel1_rv()) +
      geom_bar(aes(x = dice_roll, y = Freq, fill = dice_roll), stat =
                 'identity') +
      ggtitle(myTitle()) +
      theme(plot.title = element_text(
        lineheight = 0.8,
        size = 20,
        face = "bold"
      )) +
      labs(x = "Dice Roll", y = "Frequency")
  })
  
  output$panel1_output_dice_freq <- renderPrint({
    # build output as html table
    df = panel1_rv()
    
    output = "<table style='border: 1px solid black; padding: 15px; width: 100%; text-align: center'> <tr style='background-color:#FFE4C4'> <th> Dice Roll </th> <th> Freq </th> <th> Percent </th> </tr>"
    for (row in 1:nrow(df))
    {
      entry = df[row, ]
      output = paste(
        output,
        "<tr><td>",
        entry$dice_roll,
        "</td> <td>",
        entry$Freq,
        "</td><td>",
        entry$percent,
        "</td></tr>"
      )
    }
    output = paste(output, "</table>")
    
    # print the table
    HTML(output)
    
  })
  ######################################################
  # panel 2
  ######################################################
  sample_size <- reactive({
    input$samples
  })
  
  weights <- reactive({
    c(
      as.numeric(input$weight1),
      as.numeric(input$weight2),
      as.numeric(input$weight3),
      as.numeric(input$weight4),
      as.numeric(input$weight5),
      as.numeric(input$weight6)
    )
  })
  
  results <- reactive({
    sample(seq(1:6),
           size = sample_size(),
           replace = T,
           prob = weights())
    
  })
  
  
  
  output$distPlot <- renderPlot({
    data <- data.frame(table(results()))
    colnames(data) <- c("outcomes", "Freq")
    
    roll1 <- data$Freq[1]
    roll2 <- data$Freq[2]
    roll3 <- data$Freq[3]
    roll4 <- data$Freq[4]
    roll5 <- data$Freq[5]
    roll6 <- data$Freq[6]
    
    samples <- round(input$samples,2)
    expected <- input$samples / 6
    diff1 <- round((roll1 - input$samples / 6), 2)
    diff2 <- round((roll2 - input$samples / 6), 2)
    diff3 <- round((roll3 - input$samples / 6), 2)
    diff4 <- round((roll4 - input$samples / 6), 2)
    diff5 <- round((roll5 - input$samples / 6), 2)
    diff6 <- round((roll6 - input$samples / 6), 2)
    
    if(input$chk_line == 1 & input$chk_exp == 0){
      plot <- ggplot(data = data, aes(x = outcomes)) +
        geom_bar(aes(x = outcomes, y = Freq, fill = outcomes), stat =
                   'identity')+
        
        geom_hline(
          yintercept = input$samples / 6,
          color = "red",
          lty = 2,
          lwd = 3
        )+
        annotate(
          "text",
          x = .75 ,
          y = samples / 6 ,
          label = round(samples/6,2),
          color = "black",
          size = 6)
    }else if((input$chk_line == 1 & input$chk_exp == 1) |  (input$chk_line == 0 & input$chk_exp == 1)){
      plot <- ggplot(data = data, aes(x = outcomes)) +
        geom_bar(aes(x = outcomes, y = Freq, fill = outcomes), stat =
                   'identity')+
        
        geom_hline(
          yintercept = input$samples / 6,
          color = "red",
          lty = 2,
          lwd = 3
        )+
        annotate(
          "text",
          x = .75 ,
          y = samples / 6 ,
          label = round(samples/6,2),
          color = "black",
          size = 6)+
        
        
        geom_segment(
          aes(
            x = 1,
            y = roll1 ,
            xend = 1,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        geom_segment(
          aes(
            x = 2,
            y = roll2 ,
            xend = 2,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        geom_segment(
          aes(
            x = 3,
            y = roll3 ,
            xend = 3,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        geom_segment(
          aes(
            x = 4,
            y = roll4 ,
            xend = 4,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        geom_segment(
          aes(
            x = 5,
            y = roll5 ,
            xend = 5,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        geom_segment(
          aes(
            x = 6,
            y = roll6 ,
            xend = 6,
            yend = input$samples / 6
          ),
          lty = 1,
          lwd = 2,
          color = "blue"
        ) +
        annotate(
          "text",
          x = 1 + .25,
          y = (roll1 + samples / 6) / 2,
          label = diff1,
          color = "black",
          size = 5
        ) +
        annotate(
          "text",
          x = 2 + .25,
          y = (roll2 + samples / 6) / 2,
          label = diff2,
          color = "black",
          size = 5
        ) +
        annotate(
          "text",
          x = 3 + .25,
          y = (roll3 + samples / 6) / 2,
          label = diff3,
          color = "black",
          size = 5
        ) +
        annotate(
          "text",
          x = 4 + .25,
          y = (roll4 + samples / 6) / 2,
          label = diff4,
          color = "black",
          size = 5
        ) +
        annotate(
          "text",
          x = 5 + .25,
          y = (roll5 + samples / 6) / 2,
          label = diff5,
          color = "black",
          size = 5
        ) +
        annotate(
          "text",
          x = 6 + .25,
          y = (roll6 + samples / 6) / 2,
          label = diff6,
          color = "black",
          size = 5
        )
    }else{
      plot <- ggplot(data = data, aes(x = outcomes)) +
        geom_bar(aes(x = outcomes, y = Freq, fill = outcomes), stat =
                   'identity')
    }
    plot
  })
  
  
  test <- reactive({
    chisq.test(table(results()))
  })
  
  
  output$test_table <- renderDataTable(if (input$chk == 0) {
    df <- NULL
  } else{
    obs <- data.frame(test()$observed)
    exp <- test()$expected
    diff <- obs$Freq - exp
    diff_squared <- (obs$Freq - exp) ^ 2
    scaled_diff_squared <- (obs$Freq - exp) ^ 2 / exp
    
    data_table <-
      data.frame(obs$Var1,
                 obs$Freq,
                 exp,
                 diff,
                 diff_squared,
                 scaled_diff_squared)
    colnames(data_table) <-
      c("Outcome",
        "Observed",
        "Expected",
        "Obs - Exp",
        "(Obs - Exp)^2",
        "(Obs-Exp)^2/Exp")
    df <- data_table
  },
  options = list(paging = FALSE, searching = FALSE))
  
  
  
  output$stat_table <- renderDataTable({
    if (input$chk2 == 0) {
      df <- NULL
    } else{
      obs <- data.frame(test()$observed)
      exp <- test()$expected
      diff <- obs$Freq - exp
      diff_squared <- (obs$Freq - exp) ^ 2
      scaled_diff_squared <- (obs$Freq - exp) ^ 2 / exp
      
      sum_diff <- round(sum(diff), 4)
      sum_diff_squared <- round(sum(diff_squared), 4)
      sum_scaled_diff_squared <- round(sum(scaled_diff_squared), 4)
      
      data_table <-
        data.frame(sum_diff, sum_diff_squared, sum_scaled_diff_squared)
      colnames(data_table) <-
        c(
          "Sum of differences",
          "Sum of squared differences",
          "Sum of scaled squared differences"
        )
      data_table
    }
  }, options = list(paging = FALSE, searching = FALSE))


  ## #################################
  ## panel 3 input/outputs
  ## #################################
  # INPUT
  #   panel3_input_company_id
  #   panel3_input_sample_size
  #   panel3_input_simulations
  #   panel3_btn_graph
  ## #################################
  # OUTPUT
  #   panel3_graph_chisquare
  #   panel3_output_test_statistic
  ## #################################
  
  ######################################################
  # panel 3
  ######################################################
  
  # Compose the graph title.  This only updates when the button is clicked
  panel3_ggplot_title <- eventReactive(input$panel3_btn_graph, {
    companyName = getCompanyById(as.numeric('input$panel3_input_company_id'))
    rolls = input$panel3_input_sample_size
    if ( rolls > MAX_SAMPLES )
    {
      rolls = MAX_SAMPLES
    }
    
    sims = input$panel3_input_simulations
    if ( sims > MAX_SIMULATIONS ) {
      sims = MAX_SIMULATIONS
    }
    title = paste(companyName, ":", sims, "simulations of", rolls, "rolls")
  })
  
  panel3_rv <- eventReactive(input$panel3_btn_graph, {
    total_samples = input$panel3_input_sample_size
    if ( total_samples > MAX_SAMPLES )
    {
      # we need to reset the input to max and override
      total_samples = MAX_SAMPLES
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel3_input_sample_size",
        value = MAX_SAMPLES
      )
    }
    
    companyId = as.numeric(input$panel3_input_company_id)
    sample_dist = dice_distributions[[5]]
    
    simulations = input$panel3_input_simulations
    if ( simulations > MAX_SIMULATIONS )
    {
      # we need to reset the input to max and override
      simulations = MAX_SIMULATIONS
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel3_input_simulations",
        value = MAX_SIMULATIONS
      )
      
    }
    
    alltstats = data.frame()
    
    for (sim in 1:simulations) {
      dice_roll = sample(sample_dist, total_samples, replace = T)
      tbl = table(dice_roll)
      dd = data.frame(tbl)
      
      # check for missing dice rolls
      all_rolls = c(1:6)
      dice_rolled = as.numeric(row.names(tbl))
      # need labels as a string to add to data frame
      missing_rolls = as.character(setdiff(all_rolls, dice_rolled))
      for (dice in missing_rolls)
      {
        newrow = data.frame("dice_roll" = dice, "Freq" = 0)
        dd = rbind(dd, newrow)
      }
      
      dd$percent <- dd$Freq / sum(dd$Freq)
      tstats = ts(dd)
      alltstats = rbind(alltstats, tstats)
    }
    
    return(alltstats)
  })
  
  
  output$panel3_output_test_statistic <- renderPrint({
    df = panel3_rv()
    # fail_to_reject = nrow(df[df$test_statistic < 11.07, ]) / nrow(df)
    reject = nrow(df[df$test_statistic > input$panel3_slider_cutoff, ]) / nrow(df)
    reject = round(reject, 4)
    Unusual = reject
    Typical = 1 - reject
    Measure = c( "Percent of observations")
    # Round to 4 decimal places each
    output = data.frame(Measure, Typical, Unusual )
    print(output, row.names = F, col.names = F)
  })
  
  # ggplot our data frame
  output$panel3_graph_chisquare <- renderPlot({
    
    data = panel3_rv()
    x <- data$test_statistic
    y1 <- pchisq(seq(0, 40, length = 1), 5)
    x1 <- seq(0, 40, length = .5)
    y_limit <- if(input$panel3_input_simulations < 100){
      coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 0.2))
    } else if(input$panel3_input_simulations >= 100 & input$panel3_input_simulations <= 300 ){
      coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 1.4))
    } else{
      coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 5.4))
    }
    dot_size <- if(input$panel3_input_simulations <= 100){
      geom_dotplot( binwidth = 1)
    }else if(input$panel3_input_simulations >= 100 & input$panel3_input_simulations <= 300 ){
      geom_dotplot( binwidth = .75)
    }else {
      geom_dotplot(binwidth = .6)
    }
    if (input$slider == 1){
    ggplot(data, aes(x)) +
      ggtitle("Distribution of Test Statistics for Pips and Dots") +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) + coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 0.2)) + 
      
      
      dot_size +
      #geom_histogram(aes(x, y=..density..), binwidth = 1)
      geom_vline(
        xintercept = input$panel3_slider_cutoff,
        linetype = "dashed",
        color = "red",
        size = 2
      ) +
      annotate(
        "text",
        x = input$panel3_slider_cutoff + 5,
        y = .2,
        label = paste0("Unusual","\n","Values"),
        color = "red",
        size = 5
      ) +
      annotate(
        "text",
        x = input$panel3_slider_cutoff - 5,
        y = .2,
        label = paste0("Typical", "\n", "Values"),
        color = "red",
        size = 5
      ) +
      labs(x = "Test Statistics", y = "Frequency")
    }else{
      ggplot(data, aes(x)) +
        ggtitle("Distribution of Test Statistics for Pips and Dots") +
        theme(
          plot.title = element_text(
            lineheight = 0.8,
            size = 20,
            face = "bold"
          ),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
        ) + coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 0.2)) + 
        
        
        dot_size +
        
        labs(x = "Test Statistics", y = "Frequency")
    }
  })
  
  ## Output the true weights of a company
  output$panel3_graph_company_weights <- renderPlot({
    companyId = as.numeric(input$panel3_input_company_id)
    weights = getCompanyWeight(5)
    # convert to a dataframe
    df = data.frame()
    dice_roll = 1
    for (weight in weights)
    {
      row = data.frame("dice_roll" = dice_roll, "Weight" = weight)
      df = rbind(df, row)
      dice_roll = dice_roll + 1
    }
    df$dice_roll <-
      factor(df$dice_roll, levels = as.character(c(1:6)))
    ggplot(df) + geom_bar(aes(x = dice_roll, y = Weight, fill = dice_roll), stat =
                            'identity') +
      xlab("Roll") + ylab("Percent")
  })
  
  
  ## #################################
  ## panel 4 input/outputs
  ## #################################
  # INPUT
  #   panel4_input_company_id
  #   panel4_input_sample_size
  #   panel4_btn_graph
  ## #################################
  # OUTPUT
  #   panel4_output_dice_percents
  #   panel4_output_test_statistic
  #   panel4_graph_histogram
  #   panel4_graph_chisquare
  ## #################################
  
  #####################################################
  # panel 4
  ######################################################
  panel4_rv <- eventReactive(input$panel4_btn_graph, {
    
    companyId = as.numeric(input$panel4_input_company_id)
    sample_dist = dice_distributions[[companyId]]
    
    total_samples = input$panel4_input_sample_size
    if ( total_samples > MAX_SAMPLES )
    {
      # we need to reset the input to max and override
      total_samples = MAX_SAMPLES
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel4_input_sample_size",
        value = MAX_SAMPLES
      )
    }
    
    dice_roll = sample(sample_dist, total_samples, replace = T)
    tbl = table(dice_roll)
    dd = data.frame(tbl)
    
    # check for missing dice rolls
    all_rolls = c(1:6)
    dice_rolled = as.numeric(row.names(tbl))
    # need labels as a string to add to data frame
    missing_rolls = as.character(setdiff(all_rolls, dice_rolled))
    for (dice in missing_rolls)
    {
      newrow = data.frame("dice_roll" = dice, "Freq" = 0)
      dd = rbind(dd, newrow)
    }
    
    dd$percent <- dd$Freq / sum(dd$Freq)
    
    # dice are factors and need this to order empty dice rolls
    dd$dice_roll <-
      factor(dd$dice_roll, levels = as.character(c(1:6)))
    dd = dd[order(dd$dice_roll), ]
    row.names(dd) <- NULL
    results_dataframe = dd
    return(dd)
  })
  
  ts <- function(dd) {
    # company ID
    companyId = as.numeric(input$panel4_input_company_id)
    weights = getCompanyWeight(companyId)
    total_rolls = sum(dd$Freq)
    exp_counts = (1.0 / 6.0) * total_rolls
    test_statistic = sum((dd$Freq - exp_counts) ^ 2 / exp_counts)
    p_value = pchisq(test_statistic, df = 5, lower.tail = FALSE)
    results = data.frame("test_statistic" = test_statistic,
                         "p-value" = round(p_value, 4))
    return(results)
  }
  
  myTitle3 <- eventReactive(input$panel4_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel4_input_company_id))
    rolls = input$panel4_input_sample_size
    if ( rolls > MAX_SAMPLES )
    {
      rolls = MAX_SAMPLES
    }

    title = paste(companyName, ":", rolls, "dice rolls")
  })
  
  # ggplot our data frame
  output$panel4_graph_histogram <- renderPlot({
    
    sample_size = input$panel4_input_sample_size
    if ( sample_size > MAX_SAMPLES )
    {
      sample_size = MAX_SAMPLES
    }
    
    ggplot(panel4_rv()) +
      geom_bar(aes(x = dice_roll, y = Freq, fill = dice_roll), stat = 'identity') +
      ggtitle(myTitle3()) +
      theme(plot.title = element_text(
        lineheight = 0.8,
        size = 20,
        face = "bold"
      )) +
      geom_hline(
        yintercept = (sample_size / 6.0),
        linetype = "dashed",
        color = "red"
      ) +
      annotate(
        "text",
        x = 3.25,
        y = (sample_size / 5.8),
        label = "Expected Value",
        color = "red",
        size = 6
      ) +
      labs(x = "Dice Roll", y = "Frequency")
  })
  
  
  output$panel4_graph_chisquare <- renderPlot({
    #create density curve
    curve(
      dchisq(x, df = 5),
      from = 0,
      to = 40,
      main = 'Chi-Square Distribution (df = 5)',
      ylab = 'Density',
      lwd = 2
    )
    
    #create vector of x values
    x_vector <- seq(11.07, 40)
    
    #create vector of chi-square density values
    p_vector <- dchisq(x_vector, df = 5)
    
    #fill in portion of the density plot from 0 to 40
    polygon(
      c(x_vector, rev(x_vector)),
      c(p_vector, rep(0, length(p_vector))),
      col = adjustcolor('red', alpha = 0.5),
      border = NA
    )
    text(11.07, 0.1, "alpha =0.05", col = "red")
    text(15, 0.05, "Reject", col = "red")
    text(5, 0.05, "Fail to reject", col = "blue")
    abline(
      v = 11.07,
      lwd = 3,
      lty = 3,
      col = "blue"
    )
    
    # Extract our test statistic
    TS = ts(panel4_rv())$test_statistic
    t_vector <- seq(TS, 40)
    tsp_vector <- dchisq(t_vector, df = 5)
    polygon(
      c(t_vector, rev(t_vector)),
      c(tsp_vector, rep(0, length(tsp_vector))),
      col = adjustcolor('green', alpha = 0.5),
      border = NA
    )
    
    points(TS, 0.001, pch = 19, lwd = 5)
  })
  
  output$panel4_output_dice_percents <- renderPrint({
    # build output as html table
    df = panel4_rv()
    
    output = "<table style='border: 1px solid black; padding: 15px; width: 100%; text-align: center'> <tr style='background-color:#FFE4C4'> <th> Dice Roll </th> <th> Freq </th> <th> Percent </th> </tr>"
    for (row in 1:nrow(df))
    {
      entry = df[row, ]
      output = paste(
        output,
        "<tr><td>",
        entry$dice_roll,
        "</td> <td>",
        entry$Freq,
        "</td><td>",
        round(entry$percent, 2),
        "</td></tr>"
      )
    }
    output = paste(output, "</table>")
    
    # print the table
    HTML(output)
    
  })
  
  output$panel4_output_test_statistic <-
    renderPrint({
      print(ts(panel4_rv()), row.names = F)
    })
  
  ## #################################
  ## panel 5 input/outputs
  ## #################################
  # INPUT
  #   panel5_input_company_id
  #   panel5_input_sample_size
  #   panel5_input_simulations
  #   panel5_btn_graph
  ## #################################
  # OUTPUT
  #   panel5_graph_chisquare
  #   panel5_output_test_statistic
  ## #################################
  
  ######################################################
  # panel 5
  ######################################################
  
  # Compose the graph title.  This only updates when the button is clicked
  panel5_ggplot_title <- eventReactive(input$panel5_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel5_input_company_id))
    rolls = input$panel5_input_sample_size
    if ( rolls > MAX_SAMPLES )
    {
      rolls = MAX_SAMPLES
    }

    sims = input$panel5_input_simulations
    if ( sims > MAX_SIMULATIONS ) {
      sims = MAX_SIMULATIONS
    }
    title = paste(companyName, ":", sims, "simulations of", rolls, "rolls")
  })
  
  panel5_rv <- eventReactive(input$panel5_btn_graph, {
    total_samples = input$panel5_input_sample_size
    if ( total_samples > MAX_SAMPLES )
    {
      # we need to reset the input to max and override
      total_samples = MAX_SAMPLES
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel5_input_sample_size",
        value = MAX_SAMPLES
      )
    }
    
    companyId = as.numeric(input$panel5_input_company_id)
    sample_dist = dice_distributions[[companyId]]
    
    simulations = input$panel5_input_simulations
    if ( simulations > MAX_SIMULATIONS )
    {
      # we need to reset the input to max and override
      simulations = MAX_SIMULATIONS
      
      # Force update the UI component
      updateTextInput(
        session,
        "panel5_input_simulations",
        value = MAX_SIMULATIONS
      )
      
    }
    
    alltstats = data.frame()
    
    for (sim in 1:simulations) {
      dice_roll = sample(sample_dist, total_samples, replace = T)
      tbl = table(dice_roll)
      dd = data.frame(tbl)
      
      # check for missing dice rolls
      all_rolls = c(1:6)
      dice_rolled = as.numeric(row.names(tbl))
      # need labels as a string to add to data frame
      missing_rolls = as.character(setdiff(all_rolls, dice_rolled))
      for (dice in missing_rolls)
      {
        newrow = data.frame("dice_roll" = dice, "Freq" = 0)
        dd = rbind(dd, newrow)
      }
      
      dd$percent <- dd$Freq / sum(dd$Freq)
      tstats = ts(dd)
      alltstats = rbind(alltstats, tstats)
    }
    
    return(alltstats)
  })
  
  
  output$panel5_output_test_statistic <- renderPrint({
    df = panel5_rv()
    fail_to_reject = nrow(df[df$test_statistic < 11.07, ]) / nrow(df)
    reject = nrow(df[df$test_statistic > 11.07, ]) / nrow(df)
    Decision = c("Power", "Type 2 error")
    # Round to 4 decimal places each
    Rate = c(round(reject, 4), round(fail_to_reject, 4))
    output = data.frame(Decision, Rate)
    print(output, row.names = F)
  })
  
  # ggplot our data frame
  output$panel5_graph_chisquare <- renderPlot({
    
    data = panel5_rv()
    x <- data$test_statistic
    y1 <- pchisq(seq(0, 40, length = 1), 5)
    x1 <- seq(0, 40, length = .5)
    y_limit <- if(input$panel5_input_simulations < 100){
      coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 0.2))
    } else{
      coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 1.4))
    }
    dot_size <- if(input$panel5_input_simulations <= 100){
      geom_dotplot( binwidth = 1)
    }else{
      geom_dotplot( binwidth = .75)
    }
    ggplot(data, aes(x)) +
      ggtitle(panel5_ggplot_title()) +
      theme(
        plot.title = element_text(
          lineheight = 0.8,
          size = 20,
          face = "bold"
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) + coord_cartesian(xlim = c(0, 80), ylim = c(-.01, 0.2)) + 
      
    
      dot_size +
      #geom_histogram(aes(x, y=..density..), binwidth = 1)
      geom_vline(
        xintercept = 11.07,
        linetype = "dashed",
        color = "red",
        size = 2
      ) +
      annotate(
        "text",
        x = 22,
        y = .3,
        label = "Rejection region",
        color = "red",
        size = 6
      ) +
      annotate(
        "text",
        x = 22,
        y = .25,
        label = expression(chi ^ 2 ~ "> 11.07"),
        color = "red",
        size = 4
      ) +
      labs(x = "Test Statistics", y = "Frequency")
  })
  
  ## Output the true weights of a company
  output$panel5_graph_company_weights <- renderPlot({
    companyId = as.numeric(input$panel5_input_company_id)
    weights = getCompanyWeight(companyId)
    # convert to a dataframe
    df = data.frame()
    dice_roll = 1
    for (weight in weights)
    {
      row = data.frame("dice_roll" = dice_roll, "Weight" = weight)
      df = rbind(df, row)
      dice_roll = dice_roll + 1
    }
    df$dice_roll <-
      factor(df$dice_roll, levels = as.character(c(1:6)))
    ggplot(df) + geom_bar(aes(x = dice_roll, y = Weight, fill = dice_roll), stat =
                            'identity') +
      xlab("Roll") + ylab("Percent")
  })
  
}

shinyApp(ui = ui, server = server)
