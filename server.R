server <- function(input, output, session) {
  
  # reactive values----
  nod <- reactiveValues(
    z = NULL,
    i = 0 # iteration
  )
  
  edg <- reactiveValues(
    z = NULL
  )
  
  # 1. Initialize placement of nodes----
  # space is 500x500 rectangle
  # everyone will be randomly assigned a initial position
  output$vNet <- renderVisNetwork(
    if (!is.null(nod$z)) {
      visNetwork(
        nodes = nod$z,
        edges = edg$z
      ) %>% 
        visNodes(
          # size = 5,
          shape = 'circle',
          physics = FALSE
        ) %>% 
        visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
          color = 'red',
          physics = FALSE
        )
        
    }
  )
  
  observeEvent(
    eventExpr = input$init,
    {
      n <- as.numeric(input$popN)
      nod$z <- tibble(
        id = c(1:n),
        x = sample(-250:250, n),
        y = sample(-250:250, n),
        msk = FALSE,
        vac = FALSE,
        inf = FALSE,
        color = 'green',
        size = 5,
        t = 0
      )
      mskN <- sample(1:n, as.numeric(input$mskN)) # ids of nodes wearing masks
      vacN <- sample(1:n, as.numeric(input$vacN)) # ids of nodes vaccinated
      infN <- sample(1:n, as.numeric(input$infN)) # ids of nodes infected
      nod$z$msk <- replace(nod$z$msk, nod$z$id %in% mskN, TRUE)
      nod$z$vac <- replace(nod$z$vac, nod$z$id %in% vacN, TRUE)
      nod$z$inf <- replace(nod$z$inf, nod$z$id %in% infN, TRUE)
      nod$z$color <- replace(nod$z$color, nod$z$id %in% infN, 'red')
    }
  )
  
  # 2. Move the nodes----
  observeEvent(
    eventExpr = input$move,
    {
      n <- as.numeric(input$popN)
      for (i in 1:n) {
        r <- runif(1) * ms # 10 is the max step distance
        t <- runif(1) * 2 * pi
        nod$z$x[i] <- max(-250, min(round(nod$z$x[i] + r * cos(t), 0), 250))
        nod$z$y[i] <- max(-250, min(round(nod$z$y[i] + r * sin(t), 0), 250))
      }
      ni <- NULL # id's of newly infected nodes within this iteration
      for (i in 1:n) { # for each node
        if (nod$z$inf[i]) {
          q <- prRec(nod$z$vac[i])
          if (q > runif(1)) {
            nod$z$inf[i] <- FALSE # the ith node has recovered
            nod$z$color[i] <- 'green'
          }
        }
        if (nod$z$inf[i]) { # if ith node is infected
          for (j in 1:n) { # for each node
            if (i != j & !nod$z$inf[j]) { # except for node where i = j and jth node infected
              if (dist(nod$z$x[i], 
                       nod$z$y[i], 
                       nod$z$x[j], 
                       nod$z$y[j]) < ir) { # if dist from i to j < infection radius
                p <- prInf(nod$z$msk[j], nod$z$vac[j])
                if (p > runif(1)) { # if jth node is infected
                  ni <- c(ni, j) # add j to the list of newly infected id's
                  edg$z <- bind_rows(
                    edg$z,
                    tibble(
                      from = i,
                      to = j
                    )
                  )
                }
              }
            }
          } 
        }
      }
      nod$z$inf <- replace(nod$z$inf, nod$z$id %in% ni, TRUE)
      nod$z$color <- replace(nod$z$color, nod$z$id %in% ni, 'red')
    }
  )
  
  # 3. Check for new infections----
  observeEvent(
    eventExpr = input$check,
    {
      n <- as.numeric(input$popN)
      ni <- NULL # id's of newly infected nodes within this iteration
      for (i in 1:n) { # for each node
        if (nod$z$inf[i]) { # if ith node is infected
          for (j in 1:n) { # for each node
            if (i != j | !nod$z$inf[j]) { # except for node where i = j and jth node infected
              if (dist(nod$z$x[i], 
                       nod$z$y[i], 
                       nod$z$x[j], 
                       nod$z$y[j]) < 50) { # if dist from i to j < infection radius
                p <- prInf(nod$z$msk[j], nod$z$vac[j])
                if (p > runif(1)) { # if jth node is infected
                  ni <- c(ni, j) # add j to the list of newly infected id's
                  edg$z <- edg$z %>% 
                    bind_rows(
                      tibble(
                        from = i,
                        to = j
                      )
                    )
                }
              }
            }
          } 
        }
      }
      nod$z$inf <- replace(nod$z$inf, nod$z$id %in% ni, TRUE)
      nod$z$color <- replace(nod$z$color, nod$z$id %in% ni, 'red')
      # sendSweetAlert(
      #   title = 'Done',
      #   text = ni,
      #   type = 'information'
      # )
    }
  )
  
  
  observeEvent(
    eventExpr = input$popN,
    {
      updatePickerInput(session, 'infN', choices = c(1:as.numeric(input$popN)))
      updatePickerInput(session, 'mskN', choices = c(0:as.numeric(input$popN)))
      updatePickerInput(session, 'vacN', choices = c(0:as.numeric(input$popN)))
    }
  )
  
  # network display----
  output$vNetDisp <- renderUI(
    if (!is.null(nod$z)) {
      fluidRow(
        column(
          width = 6,
          wellPanel(
            align = 'center',
            style = 'background-color:green; padding:3px;',
            h4(style = 'color:white;', sum(!nod$z$inf))
          )
        ),
        column(
          width = 6,
          wellPanel(
            align = 'center',
            style = 'background-color:red; padding:3px;',
            h4(style = 'color:white;', sum(nod$z$inf))
          )
        )
      )
    }
  )
  
  
  
  
}