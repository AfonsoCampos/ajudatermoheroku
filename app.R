
# Gerador de Mapas e Planilhas experimentos  --------------------------------------------------


ui =  tagList(
   # html/css
    tags$head(tags$style(HTML('.info-box {min-height: 78px;} 
                              .info-box-icon {height: 78px; line-height: 78px;}
                              .info-box-content {padding-top: 0px; padding-bottom: 0px;}
                              div.box-header {text-align: center;}
                              .tabbable > .nav > li > a {background-color: ligthgray; color:green}
                              .tabbable > .nav > li[class=active] > a {background-color: green; color:white}'))),
#
  setBackgroundColor(color = "white"),
  useShinydashboard(),
  
  # navebars 
  navbarPage(
    theme = "journal", 
    "AJUDA TERMO",
   
     tabPanel("Buscardor da Palavra",
              mod_buscador_UI("parte_tabelas"))
    )
  )
  
server = function(input, output) {

  #tabelas aba superior  
  mod_buscador_sv("parte_tabelas")

  
  
  }

shinyApp(ui, server)

