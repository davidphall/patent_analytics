




################################################################################
################################################################################
# Layout
  # fluidpage
  # navbarpage
  # shinydashboard


################################################################################
# fluidpage

# ui <- fluidPage(
# 
#   fluidRow(
#     h1('Page title')
#   ),
#   fluidRow(
#     column(width = 6,
#            wellPanel(
#              p('Hello world')
#            )
#            ),
#     column(width = 6,
#            wellPanel(
#              p('Panel 2')
#            )
#            )
#   ),
#   tabsetPanel(
#     tabPanel(title = 'Inputs',
#              wellPanel(
#             textInput(inputId = 'my_input',label = 'Input',width = '200px',placeholder = 'Enter text here')
#              )),
#     tabPanel(title = 'Outputs',
#              wellPanel(
#                plotlyOutput(outputId = 'my_output')
#              ))
#   )
# 
# )



################################################################################
# navbarpage

# ui <- navbarPage(title = 'Patent Analytics',
# 
#   tabPanel(title = 'Home',
#            titlePanel('Home'),
#            sidebarLayout(
#              sidebarPanel = sidebarPanel(width = 3,
#                                          h4('Side bar')
#                                          ),
#              mainPanel = mainPanel(
#                tags$h4('Main bar')
#              )
#            )
#            ),
# 
#   tabPanel(title = 'Competitive positioning',
#            fluidPage(
#              fluidRow()
#            )
# 
# 
#            ),
# 
#   tabPanel(title = 'Technology trends',
# 
# 
# 
#            )
# 
# )

################################################################################
# dashboard

# ui <- shinydashboard::dashboardPage(
# 
#   # header
#       dashboardHeader(title = 'Patent analytics'),
# 
#   # sidebar
#       dashboardSidebar(
#         sidebarMenu(
#           menuItem(text = 'Page 1',tabName = 'page1'),
#           menuItem(text = 'Page 2',tabName = 'page2',
#                    menuSubItem('Page 2.1',tabName = 'page2-1'),
#                    menuSubItem('Page 2.2',tabName = 'page2-2')
#                    )
#         )
# 
#       ),
#   # body
#       dashboardBody(
#         tabItems(
#           tabItem(tabName = 'page1',
#                   tags$h4('Hello world')
#           ),
#           tabItem(tabName = 'page2-1',
#                   HTML('<p style="font-size:18pt; color:blue;">Hello world</p>')
#           ),
#           tabItem(tabName = 'page2-2',
#                   HTML('<p style="font-size:24pt; color:red;">Hello world</p>'),
#                   sidebarPanel(
#                     h1('this is my sidebar')
#                   ),
#                   mainPanel(
# 
#                   )
#           )
#         )
# 
#       )
# 
# 
# )


################################################################################
################################################################################
# Look/feel
  # html
  # css
  # bslib
  # shiny themes


################################################################################
# html

  # customize look using html attributes 
  # ui <- navbarPage(title = 'Patent analytics',
  #                  tabPanel(title = 'Home',
  #                           wellPanel(
  #                             HTML('<p style="font-size:20px;font-weight:bold;background-color: #000;">Some text here</p>')
  #                           )
  #                  ),
  #                  tabPanel(title = 'Charts',
  #                  )
  # 
  # )




################################################################################
# css

  # adding css to html elements directly 
  # ui <- navbarPage(title = 'Patent analytics',
  #                  tabPanel(title = 'Home',
  # 
  #                           wellPanel(style = 'margin-top: 20px; margin-left: 25px; background-color: coral;',
  #                             p(style = 'background-color: #000; font-size: 20px;','Some text here')
  #                             # same as 
  #                             # HTML('<p style="font-size:20px;font-weight:bold;background-color: #000;">Some text here</p>')
  #                           )
  #                  ),
  #                  tabPanel(title = 'Charts',
  #                  )
  # 
  # )


  # adding a custom css file
  # ui <- navbarPage(title = 'Patent analytics', 
  #                  #theme = 'custom.css',
  #                  tabPanel(title = 'Home',
  #                           
  #                           wellPanel(
  #                             p('Some text here')
  #                           )
  #                  ),
  #                  tabPanel(title = 'Charts',
  #                  )
  #                  
  # )


################################################################################
# bslib (shinythemes library also offers bootswatch themes)

  # bootswatch_themes()
  # https://bootswatch.com/

  # mytheme <- bs_theme(version = 5,
  #                     bootswatch = 'cosmo',
  #                     bg = '#000',
  #                     fg = '#FFF')
  # 
  #   ui <- navbarPage(title = 'Patent analytics',
  #                    theme = mytheme,
  #                    tabPanel(title = 'Home',
  # 
  #                             wellPanel(
  #                               p('Some text here')
  #                             )
  #                    ),
  #                    tabPanel(title = 'Charts',
  #                    )
  # 
  #   )




################################################################################
################################################################################
# Inputs and outputs

  # https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
  # https://fontawesome.com/search?o=r&m=free

  # ui <- navbarPage(title = 'Patent analytics',
  #                  theme = 'custom.css',
  #                  tabPanel(title = 'Home',
  #                           column(width = 6,
  #                                  wellPanel(
  #                                    p('Inputs'),
  #                                    selectInput(inputId = 'input1',
  #                                                label = 'Select input',
  #                                                choices = c('Option 1','Option 2','Option 3'),
  #                                                multiple = T,
  #                                                width = '200px'),
  #                                    actionButton(inputId = 'button1',label = 'Github',icon = icon('github')
  #                                                 )
  #                                  )
  #                                  ),
  #                           column(width = 6,
  #                                  wellPanel(
  #                                    p('Outputs'),
  #                                    plotlyOutput(outputId = 'chart1')
  #                                  )
  #                                  )
  # 
  #                  ),
  #                  tabPanel(title = 'Charts',
  #                  )
  # 
  # )










