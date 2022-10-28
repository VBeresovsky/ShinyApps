ui <- fixedPage(
  h2('Virus Simulator'),
  
  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = 'popN',
        label = 'Total Population',
        choices = c(10:50),
        selected = 20
      )
    ),
    column(
      width = 3,
      pickerInput(
        inputId = 'infN',
        label = 'Infected Population',
        choices = c(1:50),
        selected = 4
      )
    ),
    column(
      width = 3,
      pickerInput(
        inputId = 'mskN',
        label = 'Masked Population',
        choices = c(0:50),
        selected = 1
      )
    ),
    column(
      width = 3,
      pickerInput(
        inputId = 'vacN',
        label = 'Vaccinated Population',
        choices = c(0:50),
        selected = 1
      )
    )
  ),
  
  fluidRow(
    align = 'center',
    column(
      width = 4,
      offset = 2,
      actionBttn(
        inputId = 'init',
        label = 'Initialize Network',
        style = 'gradient',
        color = 'success',
        block = TRUE
      )
    ),
    column(
      width = 4,
      actionBttn(
        inputId = 'move',
        label = 'Step Forward',
        style = 'gradient',
        color = 'primary',
        block = TRUE
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 6,
      offset = 3,
      uiOutput('vNetDisp')
    )
  ),
  
  wellPanel(
    visNetworkOutput('vNet', height = '550px', width = '100%')
  )
    
)