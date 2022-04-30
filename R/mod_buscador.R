dados <- read_rds("R/dicio_base_app.rds") %>%  
  distinct(palavra_limpa, .keep_all = TRUE) # removendo algumas palavras repetidas desconsiderando acentos
    
baseletras <- data.frame(letras = LETTERS)


mod_buscador_UI <- function(id) {
  ns <- NS(id)
  tagList( #aqui colocar todas a ui para ser empacotada 
    sidebarPanel(width = 2,
                 tags$head(tags$script(src = "message-handler.js")),# faz parte do botao ajuda
                 actionButton(ns("help") , icon("fa-solid fa-info"), 
                              style="color: #fff; background-color: #AEF359; border-color: #AEF359"),
                 introBox(
                 selectInput(ns("idntem"), "Não Tem as Letras:", choices = LETTERS, multiple = TRUE),
                 data.step = 1,data.intro = "Preencha com as letras que a palavra não contém. Siga sempre a ordem de ações a cada novo chute para evitar erros de preenchimento!"),
                 introBox(
                 selectInput(ns("idtem"), "Tem as Letras:", choices = LETTERS, multiple = TRUE), #choices NULL e reativo
                 data.step = 2,data.intro = "Preencha com as letras que estão contidas na palavra mesmo sabendo ou não a posição."),
                 tags$strong("Sei a posição da letra?"),
                 
                 introBox(
                 selectInput(ns("idlocesta1"), "Posição 1:", choices = c("",LETTERS), multiple = FALSE),# selected =  character(0)
                 data.step = 3,data.intro = "Se você souber a posição coloque a letra na posição correta."),
                 selectInput(ns("idlocesta2"), "Posição 2:", choices = c("",LETTERS), multiple = FALSE),
                 selectInput(ns("idlocesta3"), "Posição 3:", choices = c("",LETTERS), multiple = FALSE),
                 selectInput(ns("idlocesta4"), "Posição 4:", choices = c("",LETTERS), multiple = FALSE),
                 selectInput(ns("idlocesta5"), "Posição 5:", choices = c("",LETTERS), multiple = FALSE),
                 tags$strong("Esta na palavra, mas fora da posição?"),
                 
                 introBox(
                 selectInput(ns("idlocnao1"), "Não é Posição 1:", choices = LETTERS, multiple = TRUE, selected =  character(0)),
                 data.step = 4,data.intro = "Você sabe que a palavra tem a letra, todavia não sebe a posição exata. Nos quadros abaixo você pode colocar onde a letra não está na palavra."),
                 selectInput(ns("idlocnao2"), "Não é Posição 2:", choices = LETTERS, multiple = TRUE, selected =  character(0)),
                 selectInput(ns("idlocnao3"), "Não é Posição 3:", choices = LETTERS, multiple = TRUE, selected =  character(0)),
                 selectInput(ns("idlocnao4"), "Não é Posição 4:", choices = LETTERS, multiple = TRUE, selected =  character(0)),
                 selectInput(ns("idlocnao5"), "Não é Posição 5:", choices = LETTERS, multiple = TRUE, selected =  character(0)),
                 useShinyjs(), #para btn automatico
                 introBox(
                 actionButton(ns("idchuta"), 
                               "Novo Chute!", icon("fa-solid fa-lightbulb"), 
                              style="color: #fff; background-color: #C37D0E; border-color: #C37D0E"),
                 data.step = 5,data.intro = "Após os passos acima você pode fazer seu próprio chute, consultar as palavras disponíveis na tabela ou clicar neste botão para um chute aleatório de uma das palavras restantes do banco de palavras.")
                 
    ),
             mainPanel(width = 10, 
               tabsetPanel(
                 fluidRow(
                   box(title = "Resumo das Palavras",
                       status = "success",
                       solidHeader = TRUE, 
                     width = 12,
                       infoBoxOutput(ns("nchute"), width = 3),
                      infoBoxOutput(ns("ninicial"), width = 3),
                      infoBoxOutput(ns("nremovidas"), width = 3),
                      infoBoxOutput(ns("nrestantes"), width = 3)
                       
                   )),
                          fluidRow(
                           box(title = "Palavras",
                               status = "success",
                               solidHeader = TRUE, 
                               width = 4,
                           fluidRow(
                           column(12,
                                  DT::dataTableOutput(ns("tabelanomes"))
                       
                           ))),
                          
                          box(title = "Frequência das Letras em Cada Posição",
                              status = "success",
                              solidHeader = TRUE, 
                              width = 8,
                              fluidRow(
                                column(12,
                                       plotlyOutput(ns("grafico1"), 
                                                   # height ="100%", 
                                                    height = "515px"),
                                      
                                       
                                       
                                )))
                          
                          
                          )
    )))
  }




# Server --------------------------------------------------------------------------------------

mod_buscador_sv <- function(id){
  moduleServer(
    id, 
    function(input, output, session) {
  

# Objetos Reativos ----------------------------------------------------------------------------
      observeEvent(input$help,
                   introjs(session, options = list("showBullets"="false", "showProgress"="true", 
                                                   "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip"))
      )
# contem 
      contem <- reactive({
        contem <- c("^", paste0(paste0("(?=.*", input$idtem),")"),".+")
        paste(contem, collapse = '')
        
      })
 
      local1 <- reactive({
        contem <- paste0(input$idtem)
        
      })
      
 
      dadosfill <- reactive({
        
        dadosfill <-  dados %>% 
          filter(!L1 %in%  input$idntem & !L2 %in%  input$idntem & !L3 %in%  input$idntem & !L4 %in%  input$idntem & !L5 %in% input$idntem) %>% 
           filter(!L1 %in%  input$idlocnao1 & !L2 %in%  input$idlocnao2 & !L3 %in%  input$idlocnao3 & !L4 %in%  input$idlocnao4 & !L5 %in% input$idlocnao5) %>%
           mutate(logico = str_detect(palavra_limpa, pattern = contem())) %>% 
           filter(logico == TRUE) %>% 
           select(-logico) 
        
        if(input$idlocesta1 == ""){
         dadosfill <- dadosfill
         
        }else{ dadosfill <- dadosfill %>% 
          filter(L1 == input$idlocesta1)}
        
        if(input$idlocesta2 == ""){
          dadosfill <- dadosfill
          
        }else{ dadosfill <- dadosfill %>% 
          filter(L2 == input$idlocesta2)}
        
        
        if(input$idlocesta3 == ""){
          dadosfill <- dadosfill
          
        }else{ dadosfill <- dadosfill %>% 
          filter(L3 == input$idlocesta3)}
        
        
        if(input$idlocesta4 == ""){
          dadosfill <- dadosfill
        }else{ dadosfill <- dadosfill %>% 
          filter(L4 == input$idlocesta4)}
        
        
        if(input$idlocesta5 == ""){
          dadosfill <- dadosfill
          
        }else{ dadosfill <- dadosfill %>% 
          filter(L5 == input$idlocesta5)}
      })
      
      novo_chute <- eventReactive(input$idchuta,{
        sample(dadosfill()$palavra_limpa,1)
      })
      
      # primeiro clique no botão de chute é automático
      
        primeiro <- observe({
          shinyjs::click("idchuta")
          primeiro$destroy() # remove o objeto do observador... 
        })
        
        
# saidas --------------------------------------------------------------------------------------

# Boxes  --------------------------------------------------------------------------------------
      
            output$ninicial <- renderInfoBox({
              infoBox(
                title = tags$p("Total",style = "font-size: 110%;"),
                value = tags$p(paste0(nrow(dados)), style = "font-size: 100%;"),
                icon= icon("fa-solid fa-database"), 
                color = "green", 
                fill = TRUE)
              })
      
      output$nchute <- renderInfoBox({
        infoBox(
          title = tags$p("Chute",style = "font-size: 110%;"),
          value = tags$p(paste0(novo_chute()), style = "font-size: 100%;"),
          icon= icon("fa-solid fa-lightbulb"), 
          color = "yellow", 
          fill = TRUE)
      })
      
      output$nremovidas <- renderInfoBox({
        infoBox(
          title = tags$p("Removidas",style = "font-size: 110%;"),
          value = tags$p(paste0(abs(nrow(dadosfill()) - nrow(dados))), style = "font-size: 100%;"),
          icon= icon("fa-solid fa-ban"), 
          color = "red", 
          fill = TRUE)
      })
      
      output$nrestantes <- renderInfoBox({
        infoBox(
          title = tags$p("Restantes",style = "font-size: 110%;"),
          value = tags$p(paste0(nrow(dadosfill())), style = "font-size: 100%;"),
          icon= icon("fa-solid fa-database"), 
          color = "aqua", 
          fill = TRUE)
      })

      # tabela 
      output$tabelanomes = DT::renderDataTable({
       dadosfill()[,1:2]
      }) 
      
      
      # saida grafico 
      output$grafico1 <- renderPlotly({
        
        conta_letra <- function(coluna, nomecon){
          dadosfill() %>% 
            group_by({{coluna}}) %>% 
            count() %>% 
            filter({{coluna}} %in% LETTERS) %>% 
            rename(Letra = {{coluna}}) %>% 
            ungroup() %>% 
            mutate(Total = sum(n), Frequencia = (n/Total)*100) %>% 
            select(-n,- Total)
        }
        
        
        base1 <- conta_letra(L1) %>% rename(`Posição 1` = Frequencia)
        base2 <- conta_letra(L2) %>% rename(`Posição 2` = Frequencia)
        base3 <- conta_letra(L3) %>% rename(`Posição 3` = Frequencia)
        base4 <- conta_letra(L4) %>% rename(`Posição 4` = Frequencia)
        base5 <- conta_letra(L5) %>% rename(`Posição 5` = Frequencia)
        
        baseplot <- base1 %>% 
          full_join(base2) %>% 
          full_join(base3) %>%
          full_join(base4) %>%
          full_join(base5) %>% 
          pivot_longer(cols = `Posição 1`:`Posição 5`, names_to = "Casa", values_to = "Frequência") %>% 
          mutate(Casa = as.factor(Casa)) %>% 
          ggplot(aes(x = Letra, y = `Frequência`, fill = `Frequência`))+
          geom_bar(stat='identity') +
          facet_wrap(~Casa, ncol = 5)+
          theme_minimal()+
          coord_flip()+
          ylab("Frquências nas Palavras Restantes")+
          scale_fill_gradient(low="lightgreen", high="darkgreen") 
        
        ggplotly(baseplot)
      })
  
    })
}



