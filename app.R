library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)


## Data Files ----
questionBank <- read.csv("questionBank.csv", header = TRUE)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Probability Applications",
      titleWidth = 250,
      tags$li(
        class = "dropdown", actionLink("info", icon("info"))
      ),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Probability_Applications")
      ),
      tags$li(
        class = "dropdown", tags$a(
          href = "https://shinyapps.science.psu.edu/", icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ## Overview page ----
        tabItem(
          tabName = "overview",
          h1("Probability Applications"),
          p("This app quizzes your knowledge of turning probability applications
            with context into mathematical expressions using a hangman game format."),
          h2("Instructions"),
          tags$ul(
            tags$li("You will start this game with a little man on the top of a 
                  tree, and you will try to prevent his fall to the ground. If you
                  provide a wrong answer, he will fall to a lower branch and
                  eventually to the ground. If you get 10 questions correct
                  before he falls to the ground, you will win the game and save
                  the little man!"),
            tags$li("Read the given text before you make your choice. Make sure
                    you understand the scenario text provided."),
            tags$li("If you need some extra help, click the hint button."),
            tags$li("After you select the choice, click 'Submit' to check your
                    answer."),
            tags$li("Once you click 'Submit', you cannot revise your answer. You
                    can only click 'Next Question' to move on in your challenge.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToPrereq",
              label = "Prerequisites",
              size = "large",
              icon = icon("book")
            )
          ),
          br(),
          br(),
          h3("Acknowledgements"),
          p("This app was developed and coded by Yiyang Wang. The app was further
            updated by Shravani Samala and Sean Burke.",
          br(),
          br(),
          br(),
          "Cite this app as:",
          br(),
          citeApp(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/12/2023 by SB.")
          )
        ),
        ## Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Here are some concepts you may want to review before playing the game."),
          #### Gen Eqn's box ----
          box(
            width = 12,
            title = "General Equations",
            collapsible = TRUE,
            collapsed = FALSE,
            p("Expectation"),
            p(
              "\\(\\text{E}\\!\\left[\\sum_i{a_{i}X_{i}}\\right]=
              \\sum_i{a_{i}\\cdot\\text{E}\\!\\left[X_{i}\\right]}\\)"
            ),
            br(),
            p("Variance and Covariance"),
            p(
              "\\(\\text{Var}\\!\\left[aX+b\\right]=
                  a^2\\cdot\\text{Var}\\!\\left[X\\right]\\)",
              br(),
              "\\(\\text{Var}\\!\\left[X\\right]=
                  \\text{E}\\!\\left[X^2\\right]-
                  \\big(\\text{E}\\!\\left[X\\right]\\!\\big)^2\\)"
            ),
            br(),
            p("\\(\\text{Cov}\\!\\left(aX,bY\\right)=
                  ab\\cdot\\text{Cov}\\left(X,Y\\right)\\)",
              br(),
              "\\(\\text{Cov}\\!\\left(X,Y\\right)=
                  \\text{E}\\!\\left[XY\\right]-
                  \\text{E}\\!\\left[X\\right]\\cdot\\text{E}\\!\\left[Y\\right]\\)"
            ),
            br(),
            p(
              "\\(\\text{Var}\\!\\left[X+Y\\right]=
                  \\text{Var}\\!\\left[X\\right]+
                  \\text{Var}\\!\\left[Y\\right]+
                  2\\cdot\\text{Cov}\\!\\left(X,Y\\right)\\)",
              br(),
              "\\(\\text{Var}\\!\\left[\\sum_{i}X_{i}\\right]=
                  \\sum_{i}\\text{Var}\\!\\left[X_{i}\\right]+
                  2\\cdot\\underset{i<j}{\\sum_{i}\\sum_{j}}
                  \\text{Cov}\\!\\left(X_{i},X_{j}\\right)\\)"
            ),
            br(),
            p("Moment Generating Functions"),
            p(
              "\\(M_X(t)=\\text{E}\\!\\left[e^{tX}\\right]\\)",
              br(),
              "\\(M'_X(0)=\\text{E}\\!\\left[X\\right]\\)",
              br(),
              "\\(M''_X(0)=\\text{E}\\!\\left[X^2\\right]\\)"
            ),
            br(),
            p(
              "Transformations of Random Variablies using any function, \\(g\\)."
            ),
            p(
              "Discrete Case:
              \\(\\text{E}\\!\\left[g(X)\\right]=
              \\sum\\limits_{x\\in\\mathcal{X}}g(x)\\cdot p(x)\\)",
              br(),
              "Continuous Case:
              \\(\\text{E}\\!\\left[g(X)\\right]=
              \\int\\limits_{\\mathcal{X}}g(x)\\cdot f(x)dx\\)"
            )
          ),
          #### Discrete Distributions ----
          h3("Discrete Random Variables"),
          p("The probability mass function, \\(f\\), and the cumulative
          distribution function (CDF), \\(F\\), for discrete random variables
          are \\(f(x)=P\\left[X=x\\right]\\) and \\(F(x)=P\\left[X\\leq x\\right]\\)."
          ),
          p("Note: \\(exp\\left(x\\right)=e^{x}\\)"),
          fluidRow(
            box(
              title = "Discrete Uniform Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("On the set \\(\\{x_i,i = 1, 2, \\ldots, k\\}\\),"),
              p("\\(P[X=x_{i}]=1\\big/k\\)"),
              p("in the spacial case where \\(x_i=i\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=\\left(k+1\\right)\\!\\big/2\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=\\left(k^{2}-1\\right)\\!\\big/12\\)"),
              p("\\(M_{X}(t)=\\left(\\sum\\limits^{k}_{i=1}exp\\left(it\\right)\\right)
            \\!\\big/k\\)")
            ),
            box(
              title = "Poisson Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With the parameter \\(0\\leq\\lambda<\\infty\\),"),
              p("\\(P[X=x]=\\left(exp\\left(-\\lambda\\right)\\cdot\\lambda^{x}\\right)
            \\big/x!\\)"),
            p("\\(\\text{E}\\!\\left[X\\right]=\\lambda\\)"),
            p("\\(\\text{Var}\\!\\left[X\\right]=\\lambda\\)"),
            p("\\(M_{X}(t)=
            exp\\left(\\lambda\\left(exp\\left(t\\right)-1\\right)\\right)\\)"),
            br(),
            br(),
            br()
            )
          ),
          fluidRow(
            box(
              title = "Bernoulli Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(0\\leq\\theta\\leq1\\),"),
              p("\\(P[X=x]=\\theta^{x}\\left(1-\\theta\\right)^{1-x}\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=\\theta\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=\\theta\\left(1-\\theta\\right)\\)"),
              p("\\(M_{X}(t)=\\left(1-\\theta\\right)+\\theta\\cdot exp\\left(t\\right)\\)")
            ),
            box(
              title = "Binomial Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(0\\leq\\theta\\leq1\\) and \\(n\\),"),
              p("\\(P[X=x]={n\\choose x}\\theta^{x}\\left(1-\\theta\\right)^{n-x}\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=n\\theta\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=n\\theta\\left(1-\\theta\\right)\\)"),
              p("\\(M_{X}(t)=\\left(\\theta\\cdot exp\\left(t\\right)+
            \\left(1-\\theta\\right)\\right)^n\\)")
            )
          ),
          fluidRow(
            box(
              title = "Geometric Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(0\\leq\\theta\\leq 1\\),"),
              p("\\(P[X=x]=\\theta\\left(1-\\theta\\right)^{x-1}\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=1\\big/\\theta\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=
            \\left(1-\\theta\\right)\\!\\big/\\theta^2\\)"),
            p("\\(M_{X}(t)=\\left(\\theta\\cdot exp\\left(t\\right)\\right)
            \\!\\big/\\left(1-\\left(1-\\theta\\right)exp\\left(t\\right)\\right)\\)")
            ),
            box(
              title = "Negative Binomial Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(0\\leq\\theta\\leq1\\) and \\(r\\),"),
              p("\\(P[X=x]={{r+x-1}\\choose{x}}\\theta^r\\left(1-\\theta\\right)^x\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=\\left(r\\left(1-\\theta\\right)\\right)
            \\!\\big/\\theta\\)"),
            p("\\(\\text{Var}\\!\\left[X\\right]=\\left(r\\left(1-\\theta\\right)\\right)
            \\!\\big/\\theta^2\\)"),
            p("\\(M_{X}(t)=\\left[\\left(\\theta \\cdot exp\\left(t\\right)\\right)
            \\!\\big/\\left(1-\\left(1-\\theta\\right)\\cdot exp\\left(t\\right)
            \\right)\\right]^r\\)")
            )
          ),
          ### Continuous Distributions ----
          h3("Continuous Random Variables"),
          p("For a probability density function, \\(f\\), the cumulative
          distribution function (CDF), \\(F\\), is defined as
          \\(F(a)=P\\left[X\\leq a\\right]=\\int\\limits_{-\\infty}^af(x)dx\\)."),
          fluidRow(
            box(
              title = "Continuous Uniform Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameters \\(A\\) and \\(B\\) such that \\(A\\leq x\\leq B\\),"),
              p("\\(f(x)=1\\!\\big/\\left(B-A\\right)\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=\\left(A + B\\right)
              \\!\\big/2\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=\\left(B-A\\right)^2
            \\!\\big/12\\)"),
            p("\\(M_{X}(t)=\\frac{exp\\left(Bt\\right)-exp\\left(At\\right)}
              {\\left(B-A\\right)t}\\)")
            ),
            box(
              title = "Normal (Gaussian) Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameters \\(\\mu\\) and \\(\\sigma^2>0\\),"),
              p("\\(f(x)=\\frac{1}{\\sqrt[2]{2\\pi\\sigma^2}}\\cdot
              exp\\left(\\frac{-\\left(x-\\mu\\right)^2}{2\\sigma^2}\\right)\\)",
              br(),
              "(For a standard normal with \\(\\mu=0\\) and \\(\\sigma=1\\), the
              density is also sometimes called \\(\\phi\\) with the CDF called
              \\(\\Phi\\))",),
              p("\\(\\text{E}\\!\\left[X\\right]=\\mu\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=\\sigma^2\\)"),
              p("\\(M_{X}(t)=exp\\left(\\mu t + \\frac{\\sigma^2t^2}{2}\\right)\\)")
            )
          ),
          fluidRow(
            box(
              title = "Exponential Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(\\lambda >0\\),"),
              p("\\(f(x)=\\lambda\\cdot exp\\left(-\\lambda x\\right)\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=1\\!\\big/\\lambda\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=1\\!\\big/\\lambda^2\\)"),
              p("\\(M_{X}(t)=\\lambda\\big/\\left(\\lambda-t\\right)\\)"),
              footer = "Note: Can also be parameterized with \\(\\beta = 1/\\lambda\\)"
            ),
            box(
              title = "Gamma Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameters \\(\\alpha > 0\\) and \\(\\lambda > 0\\),"),
              p("\\(f(x)=\\left(\\lambda^{\\alpha}\\cdot x^{\\alpha-1}\\cdot
              exp\\left(-x\\lambda\\right)\\right)\\!\\big/{\\Gamma(\\alpha)}\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=\\alpha\\big/\\lambda\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=\\alpha\\big/\\lambda^2\\)"),
              p("\\(M_{X}(t)=\\left(\\lambda\\big/
              \\left(\\lambda-t\\right)\\right)^{\\alpha}\\)"),
              footer = "Note: Can also be parameterized with \\(\\beta = 1/\\lambda\\)"
            )
          ),
          fluidRow(
            box(
              title = "\\(\\chi^2\\) (Chi-squared) Distribution",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p("With parameter \\(k \\geq 1\\) and an integer,"),
              p("\\(f(x)=\\left(x^{k/2}\\cdot exp\\left(-x/2\\right)\\right)\\!\\big/
              \\left(\\Gamma(k/2)\\cdot 2^{k/2}\\right)\\)"),
              p("\\(\\text{E}\\!\\left[X\\right]=k\\)"),
              p("\\(\\text{Var}\\!\\left[X\\right]=2k\\)"),
              p("\\(M_{X}(t)=\\left(1\\!\\big/\\left(1-2t\\right)\\right)^{k/2}\\)")
            )
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToGame",
              label = "Game!",
              size = "large",
              icon = icon("bolt")
            )
          )
        ),
        ## Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Probability Application Game"),
          p("Examine the given context and then select the expression that addresses
          the context's question."),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Question"),
                uiOutput("question"),
                br(),
                radioGroupButtons(
                  inputId = "mc1",
                  label = "Which expression addresses the question?",
                  status = "game",
                  direction = "vertical",
                  selected = character(0),
                  checkIcon = list(
                    yes = icon("check-square"),
                    no = icon("square")
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
                ),
                bsButton(
                  inputId = "hint",
                  label = "Hint",
                  size = "large",
                  disabled = FALSE,
                  style = "default"
                ),
                br(),
                uiOutput("hintDisplay"),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default",
                      disabled = FALSE
                    )
                  ),
                  column(
                    width = 4,
                    uiOutput("mark")
                  )
                ),
                br(),
                bsButton(
                  inputId = "nextQuestion",
                  label = "Next Question",
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                bsButton(
                  "restart",
                  "Restart",
                  size = "large",
                  style = "danger",
                  disabled = FALSE
                )
              )
            ),
            column(
              width = 6,
              uiOutput(
                outputId = "correct",
                align = "center"
              ),
              uiOutput(
                outputId = "gameProgressTree",
                align = "center"
              )
            )
          ),
          uiOutput("math1"),
          uiOutput("math2")
        ),
        tabItem(
          ## References ----
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components
                      for Shiny. R package version 0.61. Available from
                      https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils:
                      BOAST Utilities. R package version 0.1.6.3. Available from
                      https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard:
                      Create Dashboards with 'Shiny'. R package version 0.7.1.
                      Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and
                      McPherson, J. (2020). shiny: Web Application Framework for
                      R. R package version 1.5.0. Available from
                      https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets:
                      Custom Inputs Widgets for Shiny. R package version 0.5.3.
                      Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)
# Define the server ---
server <- function(input, output, session) {
  ## Info Button ----
  observeEvent(
    eventExpr = input$info, 
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Information",
        text = "This app quizzes your knowledge of turning probability applications
              with context into mathematical expressions using a hangman game format.",
        type = "info"
      )
    }
  )

  ## Prerequisites Button ----
  observeEvent(
    eventExpr = input$goToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )

  # Game button ----
  observeEvent(
    eventExpr = input$goToGame,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    }
  )

  ## Set Up Game Variables ----
  shuffledProbIDs <- sample(
    x = seq_len(nrow(questionBank)),
    size = nrow(questionBank),
    replace = FALSE
  )
  scoring <- reactiveValues(
    correct = 0,
    mistakes = 0,
    id = 1,
    questionNum = shuffledProbIDs[1]
  )

  ansChoices <- eventReactive(
    eventExpr = scoring$id,
    valueExpr = {
      order <- sample(x = LETTERS[1:4], size = 4, replace = FALSE)
      shuffled = c(
        questionBank[scoring$questionNum, order[1]],
        questionBank[scoring$questionNum, order[2]],
        questionBank[scoring$questionNum, order[3]],
        questionBank[scoring$questionNum, order[4]]
      )
      if (scoring$id >= 1) {
        return(shuffled)
      } else {
        return("error in shuffling answer choices")
      }
    }
  )

  # Display a question ----
  output$question <- renderUI(
    expr = {
      updateRadioGroupButtons(
        session = session,
        inputId = "mc1",
        selected = character(0),
        choices = list(
          ansChoices()[1],
          ansChoices()[2],
          ansChoices()[3],
          ansChoices()[4]
        ),
        checkIcon = list(
          yes = icon("check-square"),
          no = icon("square")
        ),
        status = "game"
      )
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      withMathJax(questionBank[scoring$questionNum, "Problem.Description"])
    }
  )
  
  ## Display Hint ----
  observeEvent(
    eventExpr = input$hint,
    handlerExpr = {
      output$math1 <- renderUI({withMathJax()})
      output$math2 <- renderUI({withMathJax()})
      output$hintDisplay <- renderUI(
        expr = {
          withMathJax(
            p(tags$b("Hint:"), questionBank[scoring$questionNum, "Hint"])
          )
        }
      )
    }
  )

  ## Submit Button ----
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      ### Disable submit button
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = FALSE
      )
      ### Get correct answer
      cAnswer <- questionBank[scoring$questionNum, "ansValue"]
      ### Scoring mark
      output$mark <- renderIcon(
        icon = ifelse(
          test = is.null(input$mc1),
          yes = "incorrect",
          no = ifelse(
            test = input$mc1 == cAnswer,
            yes = "correct",
            no = "incorrect"
          )
        ),
        width = 50
      )
      ### Scoring
      if (is.null(input$mc1) || length(input$mc1) == 0 || input$mc1 != cAnswer) {
        scoring$mistakes <- scoring$mistakes + 1
      } else {
        scoring$correct <- scoring$correct + 1
      }
      ### Game Over Check
      if (scoring$correct >= 10) {
        sendSweetAlert(
          session = session,
          title = "You Win!",
          type = "success",
          text = "You have won the game! Congrats!"
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      } else if (scoring$mistakes  >= 4) {
        sendSweetAlert(
          session = session,
          title = "You Lost.",
          type = "error",
          text = "You have lost the game. Please try again.",
          closeOnClickOutside = FALSE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      }
    }
  )

  ## Next Question Button ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      if (scoring$id < nrow(questionBank)) {
        scoring$id <- scoring$id + 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      } else {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "error",
          text = "You've used all of the questions; shuffling the question bank."
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(questionBank)),
          size = nrow(questionBank),
          replace = FALSE
        )
        scoring$id <- 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      }
      output$hintDisplay <- renderUI({return(NULL)})
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      output$mark <- renderIcon()
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )
    }
  )

  ## Reset button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )
      shuffledProbIDs <- sample(
        x = seq_len(nrow(questionBank)),
        size = nrow(questionBank),
        replace = FALSE
      )
      scoring$id <- 1
      scoring$questionNum <- shuffledProbIDs[scoring$id]
      scoring$correct <- 0
      scoring$mistakes <- 0
      output$hintDisplay <- renderUI({return(NULL)})
      output$mark <- renderIcon()
    }
  )

  ## Display score ----
  output$correct <- renderUI(
    expr = {
      paste0(
        "Number of questions answered correctly: ",
        scoring$correct
      )
    }
  )
  
  ## Cartoon Display ----
  output$gameProgressTree <- renderUI(
    expr = {
      img(src = "Cell01.jpg")
      if (scoring$mistakes == 0) {
        img(
          src = "Cell01.jpg",
          width = "100%",
          alt = "The man is on the top branch"
        )
      } else if (scoring$mistakes == 1) {
        img(
          src = "Cell02.jpg",
          width = "100%",
          alt = "The man has fallen one branch"
        )
      } else if (scoring$mistakes == 2) {
        img(
          src = "Cell03.jpg",
          width = "100%",
          alt = "The man has fallen another branch, only one remaining"
        )
      } else if (scoring$mistakes == 3) {
        img(
          src = "Cell04.jpg",
          width = "100%",
          alt = "The man has fallen to the last branch"
        )
      } else if (scoring$mistakes == 4) {
        img(
          src = "Cell05.jpg",
          width = "100%",
          alt = "The man has fallen to the ground"
        )
      }
    }
  )
}

boastUtils::boastApp(ui = ui, server = server)
