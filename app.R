library(shiny)
library(tidyverse)
library(jsonlite)
library(here)

squads <- read_delim(here("results", "squads.txt"), 
                     delim = "\t",
                     locale = locale("fi"))
teams <- readLines(here("results", "teams.txt"),
                   encoding = "UTF-8")
matches <- read_delim(here("results", "group_matches.txt"),
                      delim = "\t", 
                      locale = locale("fi"))

matches_a <- matches %>%
  filter(group %in% "Group A")

matches_b <- matches %>%
  filter(group %in% "Group B")

matches_c <- matches %>%
  filter(group %in% "Group C")

matches_d <- matches %>%
  filter(group %in% "Group D")

matches_e <- matches %>%
  filter(group %in% "Group E")

matches_f <- matches %>%
  filter(group %in% "Group F")

ui <- navbarPage(
  'EM-skaba 2021',
  id = 'mainNav',
  tabPanel(
    'Selitteet',
    value = 'Selite',
    fluidRow(
      tags$p('EM-skaba 2021: Paras tietäjä voittakoon!'),
      tags$p(),
      tags$p('Täytä veikkaukset välilehdille, lataa tiedosto koneelle ja 
              lähetä Atelle sähköpostilla tai whatsapilla.
              Deadline 11.6.2021 klo 21:00'),
      tags$hr(),
      tags$strong('Säännöt:'),
      tags$li('Alkulohkot 1x2-veikkaukset - 1 piste / oikea merkki (max 36 pistettä)'),
      tags$li('Jatkoonmenijäveikkaukset - 1 piste / oikea jatkoonmenijä (max 30 pistettä)'),
      tags$li('Mestari (5 pistettä)'),
      tags$li('Maalintekijät 0,5 pistettä / maali'),
      tags$li('Maalikuningas 3 pistettä'),
      tags$hr()
    )
  ),
  tabPanel(
    'Alkulohkot',
    value = 'group_stage',
    fluidRow(
      tags$i(
        'Alkulohkoissa veikataan otteluiden lopputulos.
                    Veikkauksista 1 p per oikea vastaus.'
      ),
      # LOHKO A
      tags$hr(),
      tags$strong('Lohko A:'),
      lapply(matches_a$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr(),
      tags$strong('Lohko B:'),
      lapply(matches_b$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr(),
      tags$strong('Lohko C:'),
      lapply(matches_c$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr(),
      tags$strong('Lohko D:'),
      lapply(matches_d$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr(),
      tags$strong('Lohko E:'),
      lapply(matches_e$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr(),
      tags$strong('Lohko F:'),
      lapply(matches_f$no, function(x) {
        selected_match <- matches[which(matches$no == x),]
        home <- selected_match$home
        away <- selected_match$away
        radioButtons(
          x,
          label = paste(home, away, sep = " - "),
          choices = c('1', 'x', '2'),
          inline = TRUE
        )
      }),
      tags$hr()
    )
  ),
  tabPanel(
    'Pudotuspelit',
    value = 'finals',
    fluidRow(
      tags$i(
        'Pudotuspeleissä veikataan jatkoon menevät joukkueet.
                    Jatkoonmenijöistä 1 p per oikea vastaus.
                    Mestarista 5 pistettä.'
      ),
      tags$hr(),
      tags$strong('Neljännesvälierät:'),
      selectizeInput(
          'round16',
          label = NULL,
          choices = teams,
          selected = NULL,
          options = list(maxItems = 16,
                         placeholder = 'Valitse joukkueet (16 kpl)',
                         onInitialize = I('function() { this.setValue(""); }'))
          ),
      tags$hr(),
      tags$strong('Puolivälierät:'),
      selectizeInput(
        'round8',
        label = NULL,
        choices = teams,
        selected = NULL,
        options = list(maxItems = 8,
                       placeholder = 'Valitse joukkueet (8 kpl)',
                       onInitialize = I('function() { this.setValue(""); }'))
      ),
      tags$hr(),
      tags$strong('Välierät:'),
      selectizeInput(
        'round4',
        label = NULL,
        choices = teams,
        selected = NULL,
        options = list(maxItems = 4,
                       placeholder = 'Valitse joukkueet (4 kpl)',
                       onInitialize = I('function() { this.setValue(""); }'))
      ),
      tags$hr(),
      tags$strong('Finaali:'),
      selectizeInput(
        'final',
        label = NULL,
        choices = teams,
        selected = NULL,
        options = list(maxItems = 2,
                       placeholder = 'Valitse joukkueet (2 kpl)',
                       onInitialize = I('function() { this.setValue(""); }'))
      ),
      tags$hr(),
      tags$strong('Voittaja:'),
      selectInput(
        'winner',
        label = NULL,
        choices = teams,
        selected = ' N/A'
      ),
      tags$hr()
    ),
  ),
  tabPanel(
    'Maalintekijät',
    value = 'goalscorers',
    fluidRow(
      tags$i(
        'Veikataan viisi maalintekijää ja maalikuningas.
          Maalintekijöistä 1 p per maali.
          Kuninkaan oikeasta veikkauksesta 3 p.'
      ),
      tags$hr(),
      tags$strong('Maalintekijät:'),
      selectizeInput(
        'scorers',
        label = NULL,
        choices = squads$Player,
        selected = NULL,
        options = list(maxItems = 5,
                       placeholder = 'Valitse maalintekijät (5 kpl)',
                       onInitialize = I('function() { this.setValue(""); }'))
      ),
      tags$hr(),
      tags$strong('Maalikuningas:'),
      selectInput(
        'top_scorer',
        label = NULL,
        choices = squads$Player,
        selected = NULL
      ),
      tags$hr()
    ),
  ),
  
  tabPanel(
    'Tallenna',
    value = 'save',
    fluidRow(
      tags$i(
        'Tallenna vastaukset ja lähetä eteenpäin pe 11.6. klo 21:00 mennessä.'
      ),
      tags$hr(),
      tags$strong('Veikkaaja:'),
      textInput(
        'name',
        label = NULL,
        value = 'Nimi'),
      tags$hr(),
      tags$strong('Tallenna:'),
      downloadButton(outputId = 'saveJson', label = 'Tallenna koneelle'),
      ),
  ),
  
  tags$head(
    tags$style(
      'body{min-height: 600px; height: auto; max-width: 800px; margin: auto;}
      .bottomSpacer{height: 100px;}
      .btn{background-color: steelblue; color: white;}'
    )
  )
)


server <- function(input, output, session) {
  
  # List for data from user ----
  usrdata <- reactive({
    matchdata <- lapply(matches$no, function(x) {paste0(
      matches[which(matches$no == x), "home"], "-", 
      matches[which(matches$no == x), "away"], ":",
      input[[x]]
      )})
    toJSON(
      list("group_matches" = matchdata,
           "round16" = input$round16,
           "round8" = input$round8,
           "round4" = input$round4,
           "final" = input$final,
           "winner" = input$winner,
           "scorers" = input$scorers,
           "top_scorer" = input$top_scorer
           ),
      pretty = TRUE,
      auto_unbox = TRUE)
    })
  
  # JSON download ----
  output$saveJson <- downloadHandler(
    filename = function() {
      paste0(input$name, '_', Sys.Date(), '.txt')
    },
    content = function(con) {
      writeLines(usrdata(), con)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)