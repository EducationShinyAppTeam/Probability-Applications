library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(boastUtils)

source("helpers.R")

APP_TITLE <<- "Probability Applications"
GAME_OVER <- FALSE

ui <- dashboardPage(
  skin = "blue",
  # Title
  dashboardHeader(
    title = "Probability Applications",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://shinyapps.science.psu.edu/",
        icon("home", lib = "font-awesome")
      )
    ),
    tags$li(
      class = "dropdown",
      actionLink("info", icon("info"),
        class =
          "myClass"
      )
    )
  ),
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Prerequisites",
        tabName = "concepts",
        icon = icon("book")
      ),
      menuItem("Overview", tabName = "information", icon = icon("dashboard")),
      menuItem("Game", tabName = "test", icon = icon("gamepad"))
    )
  ),

  # Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    tabItems(
      tabItem(
        tabName = "information",
        tags$a(
          href = "http://stat.psu.edu/",
          tags$img(src = "PS-HOR-RGB-2C.png", align = "left", width = 180)
        ),
        br(),
        br(),
        br(),

        h3("About: "),
        p(
          "This app quizzes your knowledge of turning probability applications  with context into mathematical expressions using a hangman game format."
        ),
        br(),
        h3("Instructions:"),
        tags$ul(
          tags$li(
            "You'll start this game with a little man on the top of a tree, and you are trying to prevent his fall to the ground. If you provide a wrong answer, he falls to a lower branch and eventually to the ground. If you get 10 questions correct before he falls to the ground, you have won the game and saved the little man!"
          ),
          tags$li(
            "Read the given text before you make your choice. Make sure you understand the scenario text provided."
          ),
          tags$li(
            "If you need some extra help, click the 'hint' button (shown as a question mark symbol)."
          ),
          tags$li("After you select the choice, click 'Submit' to check your answer."),
          tags$li(
            "Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on your challenge."
          )
        ),

        div(
          style = "text-align: center;",
          bsButton(
            inputId = "go",
            label = "GO!",
            size = "large",
            icon = icon("bolt"),
            class = "circle grow"
          )
        ),
        br(),
        h3("Acknowledgements:"),
        p("This app was developed and coded by Yiyang Wang.")
      ),

      #### Pre-requisites Page####
      tabItem(
        tabName = "concepts",
        withMathJax(),
        fluidRow(div(
          style = "text-align: center;",
          h2(
            "Here are some concepts you may want to review before doing the practice"
          )
        )),
        fluidRow(
          with = 12,
          box(
            title = strong("General expectation equations"),
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            background = NULL,
            collapsible = TRUE,
            column(
              width = 4,
              wellPanel(
                style = "background-color: #d3efff",
                fluidRow(
                  width = 12,
                  style = "text-align:center",
                  p(
                    "\\(\\text{E}\\left[\\sum_i{a_{i}X_{i}}\\right]=\\sum_i{a_{i}\\text{E}\\left[X_{i}\\right]}\\)"
                  ), br(),
                  p(
                    "\\(\\text{Var}\\left[aX+b\\right]=a^2\\text{Var}\\left[X\\right]\\)"
                  ),
                  p(
                    "\\(\\text{Var}\\left[X\\right]=\\text{E}\\left[X^2\\right]-\\left(\\text{E}\\left[X\\right]\\right)^2\\)"
                  ), br(),
                  p("\\(\\text{Cov}(aX,bY)=ab\\text{Cov}(X,Y)\\)"),
                  p(
                    "\\(\\text{Cov}(X,Y)=\\text{E}\\left[XY\\right]-\\text{E}\\left[X\\right]\\cdot\\text{E}\\left[Y\\right]\\)"
                  ), br()
                )
              )
            ),
            column(
              width = 8,
              wellPanel(
                style = "background-color: #d3efff",
                fluidRow(
                  width = 12,
                  style = "text-align:center",
                  p(
                    "\\(\\text{Var}\\left[X+Y\\right]=\\text{Var}\\left[X\\right]+\\text{Var}\\left[Y\\right]+2\\text{Cov}\\left(X,Y\\right)\\)"
                  ),
                  p(
                    "\\(\\text{Var}\\left[\\sum_{i}X_{i}\\right]=\\sum_{i}\\text{Var}\\left[X_{i}\\right]+2\\underset{i\\neq j}{\\sum_{i}\\sum_{j}}\\text{Cov}\\left(X_{i},X_{j}\\right)\\)"
                  )
                ),
                fluidRow(
                  width = 12,
                  style = "text-align:center",
                  column(
                    width = 4,
                    p(
                      "\\(M_X(t)=\\text{E}\\left[e^{tX}\\right]\\)"
                    )
                  ),
                  column(
                    width = 4,
                    p(
                      "\\(M'_X(0)=\\text{E}\\left[X\\right]\\)"
                    )
                  ),
                  column(
                    width = 4,
                    p(
                      "\\(M''_X(0)=\\text{E}\\left[X^2\\right]\\)"
                    )
                  )
                ),
                p(
                  "Transformations of Random Variablies using any function, \\(g\\)."
                ),
                p(
                  "Discrete Case: \\(\\text{E}\\left[g(X)\\right]=\\sum\\limits_{x\\in\\mathcal{X}}g(x)p(x)\\)"
                ),
                p(
                  "Continuous Case:  \\(\\text{E}\\left[g(X)\\right]=\\int\\limits_{\\mathcal{X}}g(x)f(x)dx\\)"
                )
              )
            )
          )
        ),
        box(
          title = strong("Discrete random variable"),
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          background = NULL,
          collapsible = TRUE,
          wellPanel(
            style = "background-color: #d3efff",
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p("$$ f(x)	=	P(X=x)$$")
              ),
              column(
                width = 6,
                p("$$ F(x) =	P[X≤x]$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Discrete Uniform distribution on the set\\(\\{x_{i},\\;i=1,2,\\ldots,k\\}\\)")
            ),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,

                p("$$P[X=x_{i}]=1/k$$"),
                p("$$Var(X)=(k^{2}-1)/12$$")
              ),
              column(
                width = 6,
                p("$$E(X)=\\frac{k+1}{2}$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Bernoulli random variable with parameter θ:")
            ),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,

                p("$$f(x) =\\theta^{x}(1-θ)^{1-x}$$"),
                p("$$Var(X)	=	n\\theta(1−\\theta)$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	n\\theta$$"),
                p("$$Mx(t)	=	(1-\\theta) + θe^{t}$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Binomial random variable with parameters n and θ:")
            ),
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p("$$f(x) =	[n!/x!(n-x)!]θ^{x}(1-θ)^{n-x}$$"),
                p("$$Var(X)	=	n^{θ}(1−θ)$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	n^θ$$"),
                p("$$Mx(t)	=	[(1-\\theta) + θe^{t}]^n$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Geometric random variable with parameter θ:")
            ),
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p("$$f(x) =θ(1-θ)^{x-1}$$"),
                p("$$Var(X)	=	\\frac{(1-θ)}{θ^{2}}$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	\\frac{1}{θ}$$"),
                p("$$Mx(t)	=	\\frac{θe^{t}}{[1-(1-θ)e^{t}]}$$")
              )
            )
          ),
          # Negative Bionomial
          wellPanel(
            style = "background-color: #d3efff",
            tags$style(
              type = "text/css",
              "#question {background-color: #EAF2F8;color: black;}",
              ".well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }"
            ),
            h5(strong("Negative Bionomial random variable with parameters r and θ:")),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,
                p("$$f(x) =\\frac{(x-1)!}{(r-1)!(x-r)!}$$"),
                p("$$Var(X)	=	\\frac{r(1-θ)}{θ^{2}}$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	\\frac{r}{θ}$$"),
                p("$$Mx(t)	=	(\\frac{θe^{t}}{1-(1-θ)e^{t}})^r$$")
              )
            )
          ),
          # Poisson
          wellPanel(
            style = "background-color: #d3efff",
            tags$style(
              type = "text/css",
              "#question {background-color: #EAF2F8;color: black;}",
              ".well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }"
            ),
            h5(strong("Poisson random variable with parameter λ:")),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,
                p("$$f(x) = (\\frac{λ^{x}}{x!})e^{-λ}$$"),
                p("$$Var(X)	=	λ$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	λ$$"),
                p("$$Mx(t)	=	exp{λ(e^{t}-1)}$$")
              )
            )
          )
        ),
        box(
          title = strong("Continuous random variable"),
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          background = NULL,
          collapsible = TRUE,
          wellPanel(
            style = "background-color: #d3efff",
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,
                p("$$ f(x)	=	P(X=x)$$")
              ),
              column(
                width = 6,
                p("$$ F(a) = P(X≤a) = {∫^{a}_{−∞}}f(x)dx$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Uniform random variable between A and B:")
            ),
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p("$$f(x) =	\\frac{1}{(B-A)}$$"),
                p("$$	Var(X)	=	\\frac{(B-A)^{2}}{12}	$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	\\frac{(A+B)}{2}$$"),
                p("$$Mx(t)	=\\frac{(e^{tB}- e^{tA})}{t(B-A)}$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Normal random variable 	with	mean µ and standard	deviation σ:")
            ),
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p(
                  "$$φ(x)=\\frac{1}{σ\\sqrt{2\\pi}}exp{[\\frac{-(x-µ)^2}{2σ^2}]}$$"
                ),
                p("$$	Var(X)	=	σ^2	$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	µ	$$"),
                p("$$Mx(t) = exp(µt + 0.5(σt)^2$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            h5(
              strong("Exponential random variable with parameter λ:")
            ),
            fluidRow(
              style = "text-align:center",
              width = 12,
              column(
                width = 6,
                p("$$f(x) = λe^{-λx}$$"),
                p("$$	Var(X)	=	\\frac{1}{λ^{2}}	$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	\\frac{1}{λ}$$"),
                p("$$Mx(t)	=\\frac{λ}{(λ-t)}$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            tags$style(
              type = "text/css",
              "#question {background-color: #EAF2F8;color: black;}",
              ".well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }"
            ),
            h5(strong("Gamma random variable with parameters λ and α:")),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,
                p("$$f(x) =	\\frac{λe^{-λx}(λx)^{α-1}}{Γ(α)}$$"),
                p("$$Var(X)	=	\\frac{α}{λ^{2}}$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	\\frac{α}{λ}$$"),
                p("$$Mx(t)	=	[\\frac{λ}{(λ-t)}]^{α}$$")
              )
            )
          ),
          wellPanel(
            style = "background-color: #d3efff",
            tags$style(
              type = "text/css",
              "#question {background-color: #EAF2F8;color: black;}",
              ".well { padding: 10px; margin-bottom: 15px; max-width: 2000px; }"
            ),
            h5(strong("Chi-square random	variable	with	k	degrees	of freedom:")),
            fluidRow(
              width = 12,
              style = "text-align:center",
              column(
                width = 6,
                p("$$f(x) =	\\frac{0.5e^{\\frac{-x}{2}}(0.5x)^{\\frac{k-2}{2}}}{Γ(k/2)}$$"),
                p("$$Var(X)	=	2k$$")
              ),
              column(
                width = 6,
                p("$$E(X)	=	k$$"),
                p("$$Mx(t)	=	{1-2t}^{\\frac{-k}{2}}$$")
              )
            )
          )
        ),
        br(),
        div(
          style = "text-align: center;",
          bsButton(
            inputId = "ready",
            label = "I'm ready!",
            size = "large",
            icon = icon("bolt"),
            class = "circle grow"
          )
        )
      ),

      ###### Game Page #######
      tabItem(
        tabName = "test",
        withMathJax(),
        sidebarLayout(
          sidebarPanel(
            h3("Challenge"),
            wellPanel(
              style = "background-color: #EAF2F8",
              uiOutput("question")
            ),
            p(
              "Which expression addresses the question?",
              tags$li(
                style = "display: inline-block;",
                circleButton(
                  "hint",
                  icon = icon("question"),
                  status = "myClass",
                  size = "xs"
                )
              )
            ),
            wellPanel(
              style = "background-color: #EAF2F8",
              width = 8,

              fluidRow(
                # style = "text-align:center",
                width = 12,
                withMathJax(),
                column(12,
                  offset = 0,
                  radioGroupButtons(
                    inputId = "mc1",
                    label = NULL,
                    status = "game",
                    direction = "vertical",
                    selected = NULL,
                    checkIcon = list(
                      yes = icon("check-square"),
                      no = icon("square-o")
                    ),
                    choices = list(
                      # "Pick the expression below that best addresses the question.",
                      "\\(\\frac{1}{4}\\)",
                      "\\(\\frac{2}{4}\\)",
                      "\\(\\frac{3}{4}\\)",
                      "\\(\\frac{4}{4}\\)"
                    ),
                    width = "100%",
                    justified = FALSE,
                    individual = FALSE
                  )
                )
              ),
              uiOutput("test1"),
              uiOutput("test2")
            ),
            fluidRow(
              width = 12,
              column(1, uiOutput("mark")),
              column(
                1,
                bsButton(
                  "submit",
                  "   Submit   ",
                  size = "large",
                  style = "warning",
                  disabled = FALSE
                ),
                offset = 0
              ),
              column(3,
                bsButton(
                  "nextq",
                  "Next Question",
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                offset = 2
              ),
              column(
                3,
                bsButton(
                  "restart",
                  "Restart",
                  size = "large",
                  style = "warning",
                  disabled = FALSE
                ),
                offset = 1
              ),
              br(), br(), br(),
              column(
                width = 12,
                h5(strong(("CAUTION: If you don't make a selection, the first answer is assumed to be your choice.")))
              )
            ),
            tags$head(
              tags$style(
                HTML("#result {font-size: 17px;background-color:#EAF2F8}")
              )
            ),
            width = 6
          ),
          mainPanel(
            br(),
            width = 6,
            fluidRow(uiOutput("correct", align = "center")),
            br(),
            br(),
            fluidRow(uiOutput("distPlot", align = "center"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ### Variables starting value###
  selected <<- c()
  correct_answer <<- c()

  # Learning Locker Statement Generation
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if (is.na(object)) {
      object <- paste0("#shiny-tab-", session$input$tabs)
    } else {
      object <- paste0("#", object)
    }

    stmt <- list(
      verb = verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )

    if (!is.na(value)) {
      stmt$result <- list(
        response = paste(value)
      )
    }

    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)
    
    return(response)
  }

  .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {
    statement <- rlocker::createStatement(list(
      verb = verb,
      object = list(
        id = paste0(getCurrentAddress(session), "#", object),
        name = paste0(APP_TITLE),
        description = paste0("Identify the distribution of given text: ", description),
        interactionType = interactionType
      ),
      result = list(
        success = success,
        response = response,
        completion = completion
      )
    ))

    return(rlocker::store(session, statement))
  }

  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app quizzes your knowledge of turning probability applications with context into mathematical expressions using a hangman game format.",
      type = "info"
    )
  })

  ### QUESTION BANK###
  bank <- read.csv("completeg.csv", stringsAsFactors = FALSE)
  Qs_array <- c(1:nrow(bank))
  value <- reactiveValues(
    index = 1,
    mistake = 0,
    correct = 0
  )
  hint <- as.matrix(bank[1:nrow(bank), 3])

  # Go button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "test")
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = FALSE)
  })

  # Ready button
  observeEvent(input$ready, {
    updateTabItems(session, "tabs", "information")
  })

  # Reset button
  observeEvent(input$restart, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = FALSE)
    updateButton(session, "restart", disabled = FALSE)
    Qs <<- nrow(bank)
    Qs_array <<- c(1:Qs)
    id <- 1
    
    GAME_OVER <<- FALSE
    .generateStatement(session, object = "restart", verb = "interacted", description = "Game has been restarted.")
    
    output$question <- renderUI({
      withMathJax()
      hint <<- withMathJax(bank[id, 3])
      return(bank[id, 2])
    })
    updateRadioGroupButtons(session, "mc1",
      choices = list(
        bank[id, "A"],
        bank[id, "B"],
        bank[id, "C"],
        bank[id, "D"]
      ),
      selected = NULL,
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
      status = "game"
    )
    output$test1 <- renderUI({
      withMathJax()
    })
    output$test2 <- renderUI({
      withMathJax()
    })
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    value[["mistake"]] <<- 0
    value$correct <<- 0
  })

  # Print out a question
  output$question <- renderUI({
    id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
    Qs_array <<- Qs_array[!Qs_array %in% id]
    updateRadioGroupButtons(session, "mc1",
      selected = NULL, choices = list(
        bank[id, "A"],
        bank[id, "B"],
        bank[id, "C"],
        bank[id, "D"]
      ),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      ),
      status = "game"
    )
    output$test1 <- renderUI({
      withMathJax()
    })
    output$test2 <- renderUI({
      withMathJax()
    })
    hint <<- withMathJax(bank[id, 3])
    return(withMathJax(bank[id, 2]))
  })

  ### NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    if (length(Qs_array) > 1) {
      id <<- sample(Qs_array, 1, replace = FALSE, prob = NULL)
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 3]
      withBusyIndicatorServer("nextq", {
        updateButton(session, "submit", disabled = FALSE)
        output$question <- renderUI({
          return(withMathJax(bank[id, 2]))
        })
        updateRadioGroupButtons(session, "mc1",
          selected = NULL, choices = list(
            bank[id, "A"],
            bank[id, "B"],
            bank[id, "C"],
            bank[id, "D"]
          ),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )
        output$test1 <- renderUI({
          withMathJax()
        })
        output$test2 <- renderUI({
          withMathJax()
        })
        output$mark <- renderUI({
          img(src = NULL, width = 30)
        })
      })
    }
    else if (length(Qs_array) == 1) {
      id <<- Qs_array[1]
      Qs_array <<- Qs_array[!Qs_array %in% id]
      hint <<- bank[id, 3]
      withBusyIndicatorServer("nextq", {
        output$question <- renderUI({
          return(withMathJax(bank[id, 2]))
        })
        updateButton(session, "submit", disabled = FALSE)
        updateRadioGroupButtons(session, "mc1",
          selected = NULL, choices = list(
            bank[id, "A"],
            bank[id, "B"],
            bank[id, "C"],
            bank[id, "D"]
          ),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "game"
        )
        output$test1 <- renderUI({
          withMathJax()
        })
        output$test2 <- renderUI({
          withMathJax()
        })
        output$mark <- renderUI({
          img(src = NULL, width = 30)
        })
      })
    }
    else {
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "restart", disabled = FALSE)
      sendSweetAlert(
        session = session,
        title = "Run out of question",
        type = "error",
        closeOnClickOutside = TRUE,
        h4("Run out of question. Please click Restart to start over")
      )
      output$question <- renderUI({
        return(NULL)
      })
      updateRadioGroupButtons(session, "mc1",
        selected = NULL, choices = list(
          bank[id, "A"],
          bank[id, "B"],
          bank[id, "C"],
          bank[id, "D"]
        ),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square-o")
        ),
        status = "game"
      )
      output$test1 <- renderUI({
        withMathJax()
      })
      output$test2 <- renderUI({
        withMathJax()
      })
    }
  })

  ### SUBMIT BUTTON###
  observeEvent(input$submit, {
    cAnswer <- bank[id, 10]
    WIN <- FALSE
    success <- input$mc1 == cAnswer
    
    if (success) {
      # print("correct")
      value$correct <- value$correct + 1
      if (value$correct == 10) {
        WIN <- TRUE
        GAME_OVER <<- TRUE
        sendSweetAlert(
          session = session,
          title = "Success:",
          type = "success",
          closeOnClickOutside = TRUE,
          h4("Congrats! You Win! Please click Restart to start over.")
        )
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = TRUE)
        updateButton(session, "restart", disabled = FALSE)
      }
      else {
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = FALSE)
      }
    } else {
      # print("wrong")
      value[["mistake"]] <<- value[["mistake"]] + 1
      if (value[["mistake"]] == 4) {
        WIN <- FALSE
        GAME_OVER <<- TRUE
        sendSweetAlert(
          session = session,
          title = "Lost:",
          type = "error",
          closeOnClickOutside = TRUE,
          h4("You lost. Please click Restart to start over")
        )
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = TRUE)
        updateButton(session, "restart", disabled = FALSE)
      } else {
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = FALSE)
      }
    }
    
    .generateAnsweredStatement(
      session,
      object = "submit",
      verb = "answered",
      description = bank[id, 2],
      response = input$mc1,
      interactionType = "choice",
      success = success,
      completion = GAME_OVER
    )
    
    if (GAME_OVER) {
      if (WIN) {
        .generateStatement(session, object = "game", verb = "completed", description = "Player has won the game.")
      } else {
        .generateStatement(session, object = "game", verb = "completed", description = "Player has lost the game.")
      }
    }
    
    output$mark <- renderUI({
      if (input$mc1 == cAnswer) {
        img(src = "check.png", width = 30)
      } else {
        img(src = "cross.png", width = 30)
      }
    })
  })

  #### PRINT NUMBER OF CORRECT ANSWERS####
  output$correct <- renderUI({
    h3("Number of correct answers:", "", value$correct)
  })

  ### PRINT HINTS###
  observeEvent(input$hint, {
    sendSweetAlert(
      session = session,
      title = "Hint:",
      type = NULL,
      closeOnClickOutside = TRUE,
      p(bank[id, 3])
    )
    .generateStatement(session, object = "hint", verb = "interacted", description = "Hint", value = bank[id, 3])
  })

  ### Cartoon ###
  output$distPlot <- renderUI({
    img(src = "Cell01.jpg")
    if (value[["mistake"]] == 0) {
      img(src = "Cell01.jpg")
    }

    else if (value[["mistake"]] == 1) {
      img(src = "Cell02.jpg")
    }

    else if (value[["mistake"]] == 2) {
      img(src = "Cell03.jpg")
    }

    else if (value[["mistake"]] == 3) {
      img(src = "Cell04.jpg")
    }

    else if (value[["mistake"]] == 4) {
      img(src = "Cell05.jpg")
    }
  })
}

boastApp(server = server, ui = ui)
