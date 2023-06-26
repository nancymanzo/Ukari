

library(googledrive)
library(googlesheets4)
library(shiny)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(shinyjs)
library(leaflet)
library(mxmaps)
library(shinyWidgets)
library(sf)
library(bs4Dash)
library(fresh)


font_add("Nutmeg-Light", "Nutmeg-Light.ttf")
font_families()

nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


nb.cols_10 <- 10
mycolors_10 <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols_10)


cebp <- read_excel("datos_ukari.xlsx", 
         sheet = "cebp", col_types = c("text", 
                                       "text", "numeric", "text", "text", 
                                       "date"))

setnames(cebp, tolower(names(cebp)))


cebp <- cebp %>%
  mutate(
    # sexo=case_when(
    #   sexo=="MUJER"~ "Mujer", T~"Otro"),
    `estado civil`=case_when(
      `estado civil`== "-- ESTADO CIVIL --"~ "No especificado",
      `estado civil`== "SOLTERO(A)"~"Soltera",
      `estado civil`== "UNIÓN LIBRE"~ "Unión libre",
      `estado civil`== "SEPARADO(A)"~ "Separada",
      `estado civil`== "CASADO(A)" ~ "Casada",
      `estado civil`== "VIUDO(A)" ~ "Viuda"),
    `lugar de nacimiento`=case_when(
      `lugar de nacimiento`=="NAYARIT" ~ "Nayarit",
       T ~ "Fuera de Nayarit"),
    estatus=case_when(
      estatus=="DESAPARECIDO" ~"Desaparecida",
      estatus=="LOCALIZADO SIN VIDA" ~"Localizada sin vida",
      estatus=="LOCALIZADO CON VIDA" ~"Localizada con vida",
      estatus=="NO LOCALIZADO" ~"No localizada"),

    edad=case_when(
      edad == 0 ~ "Menores de 1 año",
      edad >= 1 & edad <= 2 ~"1 a 2 años",
      edad >= 3 & edad <= 5 ~"3 a 5 años",
      edad >= 6 & edad <= 12 ~"6 a 12 años",
      edad >= 13 & edad <= 17 ~"13 a 17 años",
      edad >= 18 & edad <= 25 ~"18 a 25 años",
      edad >= 26 & edad <= 35 ~"26 a 35 años",
      edad >= 36 & edad <= 45 ~"36 a 45 años",
      edad >= 46 & edad <= 59 ~"46 a 59 años",
      edad >= 60 ~"60 en adelante"),
    edad=factor(edad, levels=c("Menores de 1 año", "1 a 2 años", "3 a 5 años", "6 a 12 años","13 a 17 años",
                              "18 a 25 años", "26 a 35 años", "36 a 45 años", "46 a 59 años", "60 en adelante")))

cebp$fecha   <-as.Date(cebp$fecha,format="%Y-%m-%d")
cebp$año <-format(as.Date(cebp$fecha,format="%Y-%m-%d"), "%Y")
cebp$periodo <-format(as.Date(cebp$fecha,format="%Y-%m-%d"), "%Y-%m")
cebp$mes     <-format(as.Date(cebp$fecha,format="%Y-%m-%d"), "%B")
cebp$month   <-format(as.Date(cebp$fecha,format="%Y-%m-%d"), "%m")



cebp <- cebp %>%
  mutate(
    mes=case_when(
      mes=="enero" ~ "Enero",
      mes=="febrero" ~ "Febrero",
      mes=="marzo" ~ "Marzo",
      mes=="abril" ~ "Abril",
      mes=="mayo" ~ "Mayo",
      mes=="junio" ~ "Junio",
      mes=="julio" ~ "Julio",
      mes=="agosto" ~ "Agosto",
      mes=="septiembre" ~ "Septiembre",
      mes=="octubre" ~ "Octubre",
      mes=="noviembre" ~ "Noviembre",
      mes=="diciembre" ~ "Diciembre"),
    mes=factor(
      mes,levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre")),
    periodo = ymd(paste0(año, "-", month, "-01"))) %>% 
  mutate(
    año=case_when(
       año=="2017"~ "2017",
       año=="2018"~ "2018",
       año=="2019"~ "2019",
       año=="2020"~ "2020",
       año=="2021"~ "2021",
       año=="2022"~ "2022",
       T~"Sin especificar"),
    año=factor(
      año,levels=c("2017", "2018", "2019", "2020", "2021", "2022", "Sin especificar")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ceav <-read_excel("datos_ukari.xlsx", sheet = "ceav"
                  #,col_types = c("text", "text", "text", "text", "numeric", "numeric")
                 ) %>%
  mutate(
  `ENTIDAD FEDERATIVA`=case_when(
      `ENTIDAD FEDERATIVA`==1 ~ "Aguascalientes",
      `ENTIDAD FEDERATIVA`==9 ~ "Ciudad de México",
      `ENTIDAD FEDERATIVA`==14 ~ "Jalisco",
      `ENTIDAD FEDERATIVA`==18 ~ "Nayarit",
      `ENTIDAD FEDERATIVA`==25 ~ "Sinaloa",
      `ENTIDAD FEDERATIVA`==32 ~ "Zacatecas",
      T~"Sin especificar"),

  `DELEGACION O MUNICIPIO`=case_when(
    `DELEGACION O MUNICIPIO` ==1~ "Acaponeta",
    `DELEGACION O MUNICIPIO` ==2~ "Ahuacatlán",
    `DELEGACION O MUNICIPIO` ==3~ "Amatlán de Cañas",
    `DELEGACION O MUNICIPIO` ==4~ "Compostela",
    `DELEGACION O MUNICIPIO` ==5~ "Huajicori",
    `DELEGACION O MUNICIPIO` ==6~ "Ixtlán del Río",
    `DELEGACION O MUNICIPIO` ==7~ "Jala",
    `DELEGACION O MUNICIPIO` ==8~ "Xalisco",
    `DELEGACION O MUNICIPIO` ==9~ "Del Nayar",
    `DELEGACION O MUNICIPIO` ==10~ "Rosamorada",
    `DELEGACION O MUNICIPIO` ==11~ "Ruíz",
    `DELEGACION O MUNICIPIO` ==12~ "San Blas",
    `DELEGACION O MUNICIPIO` ==13~ "San Pedro Lagunillas",
    `DELEGACION O MUNICIPIO` ==14~ "Santa María del Oro",
    `DELEGACION O MUNICIPIO` ==15~ "Santiago Ixcuintla",
    `DELEGACION O MUNICIPIO` ==16~ "Tecuala",
    `DELEGACION O MUNICIPIO` ==17~ "Tepic",
    `DELEGACION O MUNICIPIO` ==18~ "Tuxpan",
    `DELEGACION O MUNICIPIO` ==19~ "La Yesca",
    `DELEGACION O MUNICIPIO` ==20~ "Bahía de Banderas",

    `ENTIDAD FEDERATIVA`=="Aguascalientes" & `DELEGACION O MUNICIPIO` ==2~ "Ensenada",
    `ENTIDAD FEDERATIVA`=="Ciudad de México" &`DELEGACION O MUNICIPIO` ==7~ "Gustavo A. Madero",
    `ENTIDAD FEDERATIVA`=="Jalisco" &`DELEGACION O MUNICIPIO` ==67~ "Puerto Vallarta",
    `ENTIDAD FEDERATIVA`=="Jalisco" &`DELEGACION O MUNICIPIO` ==39~ "Guadalajara",
    `ENTIDAD FEDERATIVA`=="Jalisco" &`DELEGACION O MUNICIPIO` ==120~ "Zapopan",

  ))


setnames(ceav, tolower(names(ceav)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


fiscalia<-read_excel("datos_ukari.xlsx", sheet = "fiscalia") %>%
  pivot_longer(cols=3:22,
               names_to = "municipio",
                values_to = "total")

fiscalia %>%
  group_by(año, Violencia) %>%
  summarise(total_violencia=sum(total, na.rm=T)) ->delitos_nacional

fiscalia %>%
  group_by(municipio,año, Violencia) %>%
  summarise(total_violencia=sum(total, na.rm=T)) ->delitos_estatal

nacional<- c("Estado de Nayarit")
cbind(nacional, delitos_nacional)->delitos_nacional
names(delitos_nacional)[names(delitos_nacional) == "...1"] <- "municipio"

rbind(delitos_nacional, delitos_estatal)->fiscalia

# nayarit_shp <- st_read("Municipios.shp") %>%
#   filter(CVE_ENT==18)
#
# merge(nayarit_shp, fiscalia,
#       by.x="Municipios", by.y="municipio")->fiscalia
#
#
# fiscalia %>%
#   group_by(año, Violencia) %>%
#   summarise(total_violencia=sum(total_violencia, na.rm=T)) ->delitos_nacional
#
# fiscalia %>%
#   group_by(Municipios,año, Violencia) %>%
#   summarise(total_violencia=sum(total_violencia, na.rm=T)) ->delitos_estatal
#
# Municipios<- c("Estado de Nayarit")
# cbind(nacional, delitos_nacional)->delitos_nacional
# names(delitos_nacional)[names(delitos_nacional) == "nacional"] <- "Municipios"
#
# rbind(delitos_nacional, delitos_estatal)->fiscalia

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




aborto<-read_excel("datos_ukari.xlsx", sheet = "aborto"
                   ,
                   col_types = c("numeric",
                                                                       "text", "date", "text", "numeric",
                                                                       "text", "text", "text", "text", "text",
                                                                       "text", "text", "text", "text", "numeric",
                                                                       "text", "text", "text", "text", "text"))


aborto$fecha <-as.Date(aborto$`Fecha de Consulta`,format="%Y-%m-%d")
aborto$año <-format(as.Date(aborto$fecha,format="%Y-%m-%d"), "%Y")
aborto$periodo <-format(as.Date(aborto$fecha,format="%Y-%m-%d"), "%Y-%m")
aborto$mes     <-format(as.Date(aborto$fecha,format="%Y-%m-%d"), "%B")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

municipios_2<-read.csv("municipios_nay.csv")

# municipios_2<-read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   mutate(Subtipo.de.delito=case_when(
#     Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
#     Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
#     Subtipo.de.delito=="Violación equiparada"~"Violación",
#     Subtipo.de.delito=="Violación simple"~ "Violación",
#     Subtipo.de.delito=="Feminicidio"~"Feminicidio",
#     Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
#     Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>%
#   filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio",
#                                   "Hostigamiento sexual",
#                                   "Violación", "Violencia familiar"))

municipios_2<-municipios_2 %>%
  group_by(Año, Subtipo.de.delito, Municipio) %>%
  summarise(ene=sum(ene, na.rm = T),
            feb=sum(feb, na.rm = T),
            mar=sum(mar, na.rm = T),
            abr=sum(abr, na.rm = T),
            may=sum(may, na.rm = T),
            jun=sum(jun, na.rm = T),
            jul=sum(jul, na.rm = T),
            ago=sum(ago, na.rm = T),
            sep=sum(sep, na.rm = T),
            oct=sum(oct, na.rm = T),
            nov=sum(nov, na.rm = T),
            dic=sum(dic, na.rm = T),
            Total=sum(ene+ feb+ mar+ abr+
                        may+ jun + jul+ ago+
                        sep+ oct+ nov+ dic))




# read.csv("IDM_NM_oct22.csv",check.names = T, encoding = "latin1") %>%
#   filter(Entidad=="Jalisco",
#          Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
#                                   "Violación simple", "Violación equiparada", "Feminicidio",
#                                   "Violencia familiar",
#                                   "Violencia de género en todas sus modalidades distinta a la violencia familiar")) %>%
#   group_by(Año, Subtipo.de.delito) %>% summarise(ene=sum(Enero, na.rm = T),
#                                                  feb=sum(Febrero, na.rm = T),
#                                                  mar=sum(Marzo, na.rm = T),
#                                                  abr=sum(Abril, na.rm = T),
#                                                  may=sum(Mayo, na.rm = T),
#                                                  jun=sum(Junio, na.rm = T),
#                                                  jul=sum(Julio, na.rm = T),
#                                                  ago=sum(Agosto, na.rm = T),
#                                                  sep=sum(Septiembre, na.rm = T),
#                                                  oct=sum(Octubre, na.rm = T),
#                                                  nov=sum(Noviembre, na.rm = T),
#                                                  dic=sum(Diciembre, na.rm = T),
#                                                  Total=sum(ene+ feb+ mar+ abr+
#                                                              may+ jun + jul+ ago+
#                                                              sep+ oct+ nov+ dic))-> Estatal_total
# entidad<- c("Estado de Jalisco")
# cbind(entidad, Estatal_total)->Estatal_total
# names(Estatal_total)[names(Estatal_total) == "...1"] <- "Municipio"
# rbind(Estatal_total, municipios_2 )->municipios
# write.csv(municipios, "municipios.csv")

municipios<-read.csv("municipios_nay.csv",check.names = T, encoding = "latin-1") %>%
  # mutate(Subtipo.de.delito=case_when(
  #   Subtipo.de.delito=="Acoso sexual"~"Acoso sexual",
  #   Subtipo.de.delito=="Abuso sexual"~"Abuso sexual",
  #   Subtipo.de.delito=="Violación equiparada"~"Violación",
  #   Subtipo.de.delito=="Violación simple"~ "Violación",
  #   Subtipo.de.delito=="Feminicidio"~"Feminicidio",
  #   Subtipo.de.delito=="Violencia familiar" ~"Violencia familiar",
  #   Subtipo.de.delito=="Hostigamiento sexual" ~"Hostigamiento sexual")) %>%
  # filter(Subtipo.de.delito %in% c("Abuso sexual", "Acoso sexual","Feminicidio",
  #                                 "Hostigamiento sexual",
  #                                 "Violación", "Violencia familiar")) %>%
pivot_longer(cols = ene:dic,
             names_to = "mes",
             values_to = "total") %>%
  mutate(mes=case_when(
    mes=="ene"~1,
    mes=="feb"~2,
    mes=="mar"~3,
    mes=="abr"~4,
    mes=="may"~5,
    mes=="jun"~6,
    mes=="jul"~7,
    mes=="ago"~8,
    mes=="sep"~9,
    mes=="oct"~10,
    mes=="nov"~11,
    mes=="dic"~12),


    month=case_when(
      mes==1~"Enero",
      mes==2~"Febrero",
      mes==3~"Marzo",
      mes==4~"Abril",
      mes==5~"Mayo",
      mes==6~"Junio",
      mes==7~"Julio",
      mes==8~"Agosto",
      mes==9~"Septiembre",
      mes==10~"Octubre",
      mes==11~"Noviembre",
      mes==12~"Diciembre")
    ,
    month=factor(month,
                 levels=c("Enero", "Febrero", "Marzo",
                          "Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre",
                          "Octubre", "Noviembre", "Diciembre"))
  ) %>%
  mutate(Periodo = ymd(paste0(Año, "-", mes, "-01"))) %>%
  mutate(text = paste("Año: ", Año,
                      "\nPeriodo: ",  format(as_date(Periodo), "%B de %Y"),
                      "\nTotal de carpetas: ", scales::comma(total), sep="")) %>%
  filter(Periodo <= "2022-12-01") #actualizar cada mes







ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
        class = 'p-0',
        tags$head(
          tags$style(HTML("
        
        
      .p-0 {
       padding: 0px!important;
      }
      .small-box h3 {
    font-size: 38px;
    font-weight: 700;
    margin: 0 0 10px 0;
    white-space: normal!important;
    padding: 0;
    }
    @media (min-width: 768px) {
  .d-flex {
    display: flex;
  }
    }
    .small-box{
    border-radius: 2px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
    height: calc(100% - 20px);
    }
    # .html-widget{min-width: 300px;
    # }
    .mb-2{ 
    margin-bottom:20px;
    }
    .p-2{ 
    padding: 20px;     
    }x|
    #table_muertes{overflow: scroll; 
    }   
    
  .small-box.bg-fuchsia {
   background-color: #734C88 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-purple {
   background-color: #A54D9F !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon {
     background-color: #E9860C !important; 
   color: white !important;       
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-light-blue {
   background-color: #70AD47 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
   }
    
                        "))),
      shinythemes::themeSelector(),

      add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E9860C"),

      navbarPage(title = "Datos abiertos",
                 header=
                   busy_start_up(
                     loader = spin_epic("flower", color = "#E9860C"),
                     text = "Cargando",
                     timeout = 1500,
                     color = "#E9860C",
                     background = " white"),
                
                 useShinydashboard(),
                 navbarMenu(title = "Busqueda y localización", #icon = icon("dot-circle"),
                            tabPanel(title = "Comisión Estatal de Busqueda de Personas",
                                     #tabsetPanel(
                                     #icon = icon("dot-circle"),
                                     # tabPanel(title = "Municipal",
                                       #box(width=12,
                                       div(class="row d-flex", #Replicar
                                           
                                       
                                       valueBox(tags$p("565 casos de mujeres", style = "font-size: 200%; font-weight: bold"), "fueron iniciadas su busqueda y localización entre junio del 2015 a septiembre 2022.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                       valueBox(tags$p("61% han sido localizadas con vida", style = "font-size: 200%; font-weight: bold"), "siendo el mayor porcentaje, seguido de 26% no localizadas, 10% como desaparecidas y 3% localizadas sin vida.", icon=icon("equals"), color="purple", width = 4),
                                       valueBox(tags$p("Menores de 1 año representa 27.1%", style = "font-size: 200%; font-weight: bold"), "lo cual puede entenderse como que sí son menores de la edad señala o simplemente las edades no fueron especificaddas.", icon=icon("wave-square"), color="maroon", width = 4)),
                                    tabsetPanel(

                                    
                                     sidebarLayout(
                                       sidebarPanel("\nSeleccione algunas características",
                                                    selectInput(
                                                      inputId = "cebp_año",
                                                      label = "Año",
                                                      choices = sort(unique(cebp$año)),
                                                      multiple = T
                                                    ),
                                                    # selectInput(
                                                    #   inputId = "cebp_mes",
                                                    #   label = "Mes",
                                                    #   choices = sort(unique(cebp$mes)),
                                                    #   multiple = T#,
                                                    #   #selected = "Violencia familiar"
                                                    # ),
                                                    selectInput(
                                                      inputId = "cebp_lugar",
                                                      label = "Lugar de nacimiento",
                                                      choices = sort(unique(cebp$`lugar de nacimiento`)),
                                                      multiple = T
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),
                                                    selectInput(
                                                      inputId = "cebp_rango",
                                                      label = "Rango de edad",
                                                      choices = sort(unique(cebp$edad)),
                                                      multiple = T
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),
                                                    selectInput(
                                                      inputId = "cebp_estado_civil",
                                                      label = "Estado civil",
                                                      choices = sort(unique(cebp$`estado civil`)),
                                                      multiple = T
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),

                                                    downloadButton("downloadData_cebp", "\nDescarga (.xlsx)")
                                                    ),
                                       mainPanel(
                                         fluidRow(
                                           splitLayout(cellWidths = c("60%", "40%"),
                                                       plotlyOutput("grafico_cebp_1"),
                                                       dataTableOutput("table_1"),
                                                 
                                                 h6("Fuente: Datos de la Comisión Estatal de Busqueda de Personas."),
                                                 h6("Datos a octubre de 2022"),
                                    
                                    # - - - - - - - - - - - - - - - - - - - - - -
                                      
                                    
                                    
                                    
                                     )),#)),
                                    
                                    tabsetPanel(
                                      # sidebarLayout(
                                      #   sidebarPanel("\nSeleccione algunas características",
                                      #                selectInput(
                                      #                  inputId = "cebp_año_1",
                                      #                  label = "Año",
                                      #                  choices = sort(unique(cebp$año)),
                                      #                  multiple = T
                                      #                ),
                                      #                # selectInput(
                                      #                #   inputId = "cebp_mes",
                                      #                #   label = "Mes",
                                      #                #   choices = sort(unique(cebp$mes)),
                                      #                #   multiple = T#,
                                      #                #   #selected = "Violencia familiar"
                                      #                # ),
                                      #                selectInput(
                                      #                  inputId = "cebp_lugar_1",
                                      #                  label = "Zona",
                                      #                  choices = sort(unique(cebp$`lugar de nacimiento`)),
                                      #                  multiple = T
                                      #                  # options = list(
                                      #                  #   `actions-box` = TRUE,
                                      #                  #   `deselect-all-text` = "Sin selección filtro",
                                      #                  #   `select-all-text` = "Seleccionar todos",
                                      #                  #   `none-selected-text` = "Sólo los de la Región")
                                      #                ),
                                      #                
                                      #                #downloadButton("downloadData_cebp", "\nDescarga (.xlsx)")
                                      #   ),
                                        # mainPanel(
                                        #   fluidRow(
                                            #splitLayout(cellWidths = c("50%", "50%"),
                                                        plotlyOutput("grafico_cebp_5", height = "auto"),
                                                        plotlyOutput("grafico_cebp_3", height = "650px"),
                                                        
                                                        h6("Fuente: Datos de la Comisión Estatal de Busqueda de Personas."),
                                                        h6("Datos a octubre de 2022")
                                    
                                    ))))))#)#)
                                    ,
                                    
                 navbarMenu(title = "Incidencia delictiva", #icon = icon("dot-circle"),
                             tabPanel(title = "Fiscalía Nayarit",
                              #icon = icon("dot-circle"),
                               # tabPanel(title = "Municipal",
                                       #tabPanel("Total por municipio",
                                                #box(width=12,
                                                  div(class="row d-flex", #Replicar
                                                      
                                                  valueBox("2022 (ene-dic)", "se registran 4,588 carpetas iniciadas
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                                  valueBox("De 2018 a 2019", "se presenta la variación anual más grande del histórico con 88%, al pasar de 972 a 1,828", icon=icon("equals"), color="purple", width = 4),
                                                  valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 83%,
                                                seguido de violación con (15%) y abuso sexual con 1%.", icon=icon("wave-square"), color="maroon", width = 4)),

                                                sidebarLayout(
                                                  sidebarPanel("\nSeleccione algunas características",
                                                               selectInput(
                                                                 inputId = "fiscalia_año",
                                                                 label = "Año",
                                                                 choices = sort(unique(fiscalia$año)),
                                                                 multiple = T
                                                               ),
                                                               selectInput(
                                                                 inputId = "fiscalia_delito",
                                                                 label = "Delito",
                                                                 choices = sort(unique(fiscalia$Violencia)),
                                                                 multiple = T#,
                                                                 #selected = "Violencia familiar"
                                                               ),
                                                               selectInput(
                                                                 inputId = "fiscalia_municipio",
                                                                 label = "Municipio",
                                                                 choices = sort(unique(fiscalia$municipio)),
                                                                 multiple = F,
                                                                  selected = "Estado de Nayarit"
                                                                 # options = list(
                                                                 #   `actions-box` = TRUE,
                                                                 #   `deselect-all-text` = "Sin selección filtro",
                                                                 #   `select-all-text` = "Seleccionar todos",
                                                                 #   `none-selected-text` = "Sólo los de la Región")
                                                               ),

                                                               downloadButton("downloadData_fiscalia", "\nDescarga (.xlsx)")
                                                  ),
                                                  mainPanel(plotlyOutput("grafico_fiscalia"),
                                                            h6("Fuente: Datos de la Fiscalía del Estado de Nayarit."),
                                                            h6("Datos a octubre de 2022"),
                                                            h6("En este apartado sólo se muestran los delitos que se consideran: feminicidio, hostigamiento y acoso sexual, homicidio
                                                    sucidio, violación y violencia familiar")

                                                    #         plotlyOutput("maoa_fiscalia"),
                                                    #         h6("Fuente: Datos de la Fiscalía del Estado de Nayarit."),
                                                    #         h6("Datos a octubre de 2022"),
                                                    #         h6("En este apartado sólo se muestran los delitos que se consideran: feminicidio, hostigamiento y acoso sexual, homicidio
                                                    # sucidio, violación y violencia familiar.")
                                                )
                                                )
                              ),#),
                            tabPanel(title = "Secretariado Ejecutivo del Sistema Nacional Seguridad Pública (SESNSP)",
                                     #icon = icon("dot-circle"),
                                     # tabPanel(title = "Municipal",
                                     #tabPanel("Total por municipio",
                                     #box(width=12,
                                       div(class="row d-flex", #Replicar
                                           
                                       valueBox("2022 (ene-dic)", "se registran 4,588 carpetas iniciadas
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                       valueBox("De 2018 a 2019", "se presenta la variación anual más grande del histórico con 88%, al pasar de 972 a 1,828", icon=icon("equals"), color="purple", width = 4),
                                       valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 83%,
                                                seguido de violación con (15%) y abuso sexual con 1%.", icon=icon("wave-square"), color="maroon", width = 4)
                                     )
                                     ,

                                     sidebarLayout(
                                       sidebarPanel("\nSeleccione algunas características",
                                                    selectInput(
                                                      inputId = "municipal_año",
                                                      label = "Año",
                                                      choices = sort(unique(municipios$Año)),
                                                      multiple = T
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_mes",
                                                      label = "Mes",
                                                      choices = sort(unique(municipios$month)),
                                                      multiple = T,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_delito",
                                                      label = "Delito",
                                                      choices = sort(unique(municipios$Subtipo.de.delito)),
                                                      multiple = F,
                                                      selected = "Violencia familiar"
                                                    ),
                                                    selectInput(
                                                      inputId = "municipal_municipio",
                                                      label = "Municipio",
                                                      choices = sort(unique(municipios$Municipio)),
                                                      multiple = T,
                                                      selected = "Estado de Nayarit"
                                                      # options = list(
                                                      #   `actions-box` = TRUE,
                                                      #   `deselect-all-text` = "Sin selección filtro",
                                                      #   `select-all-text` = "Seleccionar todos",
                                                      #   `none-selected-text` = "Sólo los de la Región")
                                                    ),

                                                    downloadButton("downloadData_municipal", "\nDescarga (.csv)")
                                       ),
                                       mainPanel(plotlyOutput("grafico_municipal_periodo"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a diciembre de 2022"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."),

                                                 plotlyOutput("grafico_municipal"),
                                                 h6("Fuente: Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                                                 h6("Datos a diciembre de 2022"),
                                                 h6("En este apartado sólo se muestran los delitos que se consideran estar relacionados a una razón de género: acoso sexual,
                                                    abuso sexual, feminicidio, hostigamiento sexual, violación y violencia familiar."))
                                     ))
                            ),

                 navbarMenu(title = "Atención a víctimas", #icon = icon("dot-circle"),
                            tabPanel(title = "Comisión Ejecutiva de Atención a Víctimas",
                                     #box(width=12,
                                         div(class="row d-flex", #Replicar
                                             
                                     valueBox("2022 (ene-dic)", "se registran 4,588 carpetas iniciadas
                                                por incidencia delicitva por razón de género.",icon=icon("chart-area"),color="fuchsia", width = 4),
                                     valueBox("De 2018 a 2019", "se presenta la variación anual más grande del histórico con 88%, al pasar de 972 a 1,828", icon=icon("equals"), color="purple", width = 4),
                                     valueBox("Violencia familiar", "es el delito con mayor número de carpetas al concentrar el 83%,
                                                seguido de violación con (15%) y abuso sexual con 1%.", icon=icon("wave-square"), color="maroon", width = 4)),

                                                sidebarLayout(
                                                  sidebarPanel("\nSeleccione algunas características",
                                                               selectInput(
                                                                 inputId = "ceav_municipio",
                                                                 label = "Seleccione municipio",
                                                                 choices = sort(unique(ceav$`delegacion o municipio`)),
                                                                 multiple = T
                                                               ),

                                                               selectInput(
                                                                 inputId = "ceav_entidad",
                                                                 label = "Seleccione entidad",
                                                                 choices = sort(unique(ceav$`entidad federativa`)),
                                                                 multiple = T
                                                               ),
                                                               selectInput(
                                                                 inputId = "ceav_victima",
                                                                 label = "Tipo de víctima",
                                                                 choices = sort(unique(ceav$`tipo de victima`)),
                                                                 multiple = T
                                                               ),

                                                               downloadButton("downloadData_ceav", "\nDescarga (.xlsx)")
                                                  ),

                                                  mainPanel(plotlyOutput("grafico_ceav"),
                                                            h6("Fuente: Datos de la Comisión Estatal de Busqueda de Personas."),
                                                            h6("Datos a octubre de 2022")#,
                                                            # plotlyOutput("mapa_ceav"),
                                                            # h6("Fuente: Datos de la Comisión Estatal de Busqueda de Personas."),
                                                            # h6("Datos a octubre de 2022")
                                                            #

                                                  ))))
    ))))







server <- function(input, output) {


  #############################################################################
  #                  CEBP
  #############################################################################
  output$cebp_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(cebp$año)))

  })

  output$cebp_mes <- renderUI({
    selectInput("cebp_mes",
                label =  "Seleccione el mes",
                choices = sort(unique(cebp$mes)))
  })
  output$cebp_lugar <- renderUI({
    selectInput("cebp_lugar",
                label =  "Seleccione el lugar de nacimiento",
                choices = sort(unique(cebp$`lugar de nacimiento`)))

  })
  output$cebp_rango <- renderUI({
    selectInput("cebp_rango",
                label =  "Seleccione el rango de edad",
                choices = sort(unique(cebp$edad)))
    
  })
  
  output$cebp_estado_civil <- renderUI({
    selectInput("cebp_estado_civil",
                label =  "Seleccione el estado civil",
                choices = sort(unique(cebp$`estado civil`)))
    
  })






  output$cebp_año_1 <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(cebp$año)))

  })

  output$cebp_lugar_1 <- renderUI({
    selectInput("cebp_lugar",
                label =  "Seleccione el lugar de nacimiento",
                choices = sort(unique(cebp$`lugar de nacimiento`)))

  })

  
  
  #######################################################################33



  cebp_data <- reactive({

    cebp %>%
      filter(if(!is.null(input$cebp_año))                     año %in% input$cebp_año            else  año != "",
             if(!is.null(input$cebp_lugar)) `lugar de nacimiento` %in% input$cebp_lugar          else `lugar de nacimiento` != "",
             if(!is.null(input$cebp_rango))                  edad %in% input$cebp_rango          else  edad != "",
             if(!is.null(input$cebp_estado_civil)) `estado civil` %in% input$cebp_estado_civil   else `estado civil` != "",
             )

  })


  cebp_data_1 <- reactive({

    cebp %>%
      filter(if(!is.null(input$cebp_año_1))                     año %in% input$cebp_año_1     else año != "",
             if(!is.null(input$cebp_lugar_1)) `lugar de nacimiento` %in% input$cebp_lugar_1   else `lugar de nacimiento` != "")

  })



  output$downloadData_cebp <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(cebp_data(), file, row.names = FALSE)
    })

  output$grafico_cebp_1<- renderPlotly({

    cebp_data() %>%
      # cebp %>%
      # filter(! is.na(año)) %>%
      group_by(año) %>%
      summarise(total=n(), na.rm=T) %>%
      mutate(text = paste("Total de búsquedas: ", scales::comma(total),
                          "\nAño: ", año,
                          sep="")) %>%
      ggplot(aes(x = año, y = total,
                 group=1, text=text )) +
      geom_line(size=1, colour="#764a88") +
      geom_point(alpha=1, size=4,fill="#764a88", colour="#764a88")+

      labs(x="", y="", title = "", color=" ", fill=" " , size="") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
      
      theme_minimal()+
      theme(text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->gr_cebp_1

    ggplotly(gr_cebp_1, tooltip = "text") %>%
      layout(title = paste0("Total de investigaciones iniciadas \n por busqueda de mujeres \n" ),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=50))


  })
  
  
  output$table_1 <- renderDataTable ({
    
    
     cebp_data() %>%
      # cebp %>%
      # filter(! is.na(año)) %>%
      group_by(año) %>%
      summarise(total=n()) %>%
      mutate(text = paste("Total de búsquedas: ", scales::comma(total),
                          "\nAño: ", año)) %>%
      # arrange(-año)%>% 
      select(año, total) %>%
      adorn_totals() %>% 
      datatable(
        
        filter = 'top',
        colnames = c( 'Año','Total de búsquedas'),
        
        extensions = 'Buttons',
        options = list(
          #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          # language = list(
          #   info = ' ',
          #   paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#734C88', 'color': '#fff', align:'center'});","}"),
          # 
          dom = "tip",#'Blfrtip',
          buttons = c('copy', 'excel', 'print'),
          lengthMenu = list(c(8,1,5,10, "All"),
                            c(8,1,5,10, "All")),
          columnDefs = list(list(className = 'dt-center', targets = 1:2)))) %>% 
      
      formatCurrency('total',currency = "", interval = 4, mark = ",", digits = 0) %>% 
      
      #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
      #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        #fontSize = "13px",
        #color = '#008080',
        fontWeight = 'plain',
        #paddingRight = "0.5em",
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        #borderCollapse = "collapse",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word"#,
        #backgroundColor = '#e6e6e5'
      )
    
    
  })


# 
#   output$grafico_cebp_2<- renderPlotly({
# 
#     cebp_data() %>%
# 
#       group_by(año, `lugar de nacimiento`) %>%
#       filter(!is.na(año)) %>%
#       summarise(total=n()) %>%
#       mutate(text = paste("Total de búsquedas: ", scales::comma(total),
#                           "\nAño: ", año,
#                           "\nLugar de nacimiento: ", `lugar de nacimiento`, sep="")) %>%
#       ggplot(aes(x = año, y = total,
#                  colour = `lugar de nacimiento`, fill=`lugar de nacimiento`,
#                  group=`lugar de nacimiento`,
#                   text=text )) +
#       geom_line(size=1) +
#       geom_point(alpha=1, size=4)+
#       # geom_label(data=cebp %>% filter(!periodo=="NA") %>%
#       #              group_by(año) %>% summarise(total=n()),
#       #            aes(x=año, y=total, label=paste0(año, "\n Total anual: ", total)),
#       #            y=23, hjust=0, size=2, angle=0, fontface="bold")+
#       scale_fill_manual(
#         values = c(
#           `Nayarit` = "#A24895",
#           `Entidad del país` = "#F18822"))+
#       scale_color_manual(
#         values = c(
#           `Nayarit` = "#A24895",
#           `Entidad del país` = "#F18822"))+
#       labs(x="", y="", title = "", color=" ", fill=" " , size="") +
#       theme_minimal()+
#       theme(text=element_text(size=11,family="Century Gothic"),
#             plot.margin = margin(2, 2, 2, 2, "cm"),
#             strip.text.x = element_text(size = 12, face = "bold", angle=90),
#             plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
#             plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
#             plot.caption = element_text(size = 12L, hjust = 0.5),
#             axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->gr_cebp_2
# 
#     ggplotly(gr_cebp_2, tooltip = "text") %>%
#       layout(title = paste0("Total de investigaciones iniciadas por busqueda \n de mujeres por lugar de nacimiento \n" ),
#              legend = list(orientation = "h", x = 0.1, y = -0.4),
#              margin = list(b=0,t=30))
# 
# 
#   })
# 

  output$grafico_cebp_3<- renderPlotly({

    cebp_data() %>%
    # filter(!is.na(periodo)) %>%
    group_by(año, edad, `estado civil`) %>%
    summarise(total=n()) %>%
    ggplot()+
    aes(x=año, y=as.factor(edad), size=total,
        text = paste("\nAño: ", año,
                     "\nTotal: ", comma(total, accuracy = 1),
                     "\nRango de edad: ", edad , sep=""))+
    geom_point(mapping=aes(colour=edad), alpha=1)+
    #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
    geom_text(aes(label=total),#hjust=.5, vjust=-.8,
              size=3, color="white")+
    scale_y_discrete(limits = rev ,labels = function(x) str_wrap(x, width = 25)) +
    scale_size_continuous(range = c(3,9)) +
    scale_fill_manual(values = mycolors_10) +
    scale_color_manual(values = mycolors_10) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      
    facet_wrap(~`estado civil`, nrow = 2)+
    labs(y="", x="")+
    theme_minimal()+
      theme(legend.position = "none",
            text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 10, face = "bold", angle=0),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=.5, size=10))->gr_cebp_3

    ggplotly(gr_cebp_3, tooltip = "text") %>%
      layout(title = paste0("Busqueda de personas por rango de edad" ),
             #legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=60))


  })
  
  
  output$table_3 <- renderDataTable ({
    
    
    cebp_data_1() %>%
      # filter(!is.na(periodo)) %>%
      group_by(año, edad, `estado civil`) %>%
      summarise(total=n()) %>% 
      # arrange(-año)%>% 
      select(año, total) %>% 
      datatable(
        
        filter = 'top',
        colnames = c('Estado civil','Rango de edad','Año','Total'),
        
        extensions = 'Buttons',
        options = list(
          #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          # language = list(
          #   info = ' ',
          #   paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#734C88', 'color': '#fff', align:'center'});","}"),
          # 
          dom = "tip",#'Blfrtip',
          buttons = c('copy', 'excel', 'print'),
          lengthMenu = list(c(5,1,5,10, "All"),
                            c(5,1,5,10, "All")),
          columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
      
      formatCurrency('total',currency = "", interval = 4, mark = ",", digits = 0) %>% 
      
      #formatStyle('entidad', target = "row", fontWeight = "bold") %>% 
      #formatStyle("subtipo.de.delito", color = styleEqual(c("Homicidio doloso"), c("#5F55A0"))) %>% 
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        #fontSize = "13px",
        #color = '#008080',
        fontWeight = 'plain',
        #paddingRight = "0.5em",
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        #borderCollapse = "collapse",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word"#,
        #backgroundColor = '#e6e6e5'
      )
    
    
  })
  
 






  output$grafico_cebp_4<- renderPlotly({

    cebp_data_1() %>%
      #cebp %>%
      group_by(año, `estado civil`) %>%
      # filter(!is.na(año)) %>%
      summarise(total=n()) %>%
      ggplot(aes(x=año, y=fct_infreq(`estado civil`), size = total, fill = `estado civil`, color = `estado civil`,
                 text = paste("Mes: ", año,
                              "\nTotal: ", comma(total, accuracy = 1),
                              "\nEstado civil: ", `estado civil` , sep=""))) +
      geom_point(alpha=1) +
      geom_text(aes(x=año, y=fct_infreq(`estado civil`),label=total, size=1),colour="white")+
      scale_size(range = c(5, 19), name="Total") +
      # viridis::scale_color_viridis(discrete=TRUE, guide=FALSE) +
      scale_fill_manual(values =  mycolors_10) +
      scale_color_manual(values = mycolors_10) +
      labs(x="", y="", fill="", color="")+
      theme_minimal() +
      theme(legend.position = "none",
            text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=0),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->gr_cebp_4

    ggplotly(gr_cebp_4, tooltip = "text") %>%
      layout(title = paste0("Busqueda de personas por estado civil \n" ),
             #legend = list(orientation = "v", x = 0.1, y = -0.4),
             margin = list(b=0,t=150))


  })







  output$grafico_cebp_5<- renderPlotly({

    cebp_data() %>%
    # cebp %>%
      # filter(!is.na(año)) %>%
      group_by(año, estatus) %>%
      summarise(total=n()
                #Porcentaje= `Total de cias`/total_det
      ) %>%
      ggplot(aes(x=año, weight=total, fill=estatus ,
                 label=total,
                 text = paste("Año", año,
                              "\nTotal: ", total,
                              "\nEstatus: ", estatus , sep="")))+
      geom_bar(position = "fill",  alpha=1)+
      geom_text(aes(y=total, group=estatus), colour="white",
                 position = position_fill(vjust = 0.5), size = 3)+
      # scale_fill_viridis_d(option = "D", alpha = .8)+
      # scale_color_viridis_d(option = "D") +
      scale_color_brewer(palette = "Dark2")+
      scale_fill_brewer(palette = "Dark2")+
      coord_flip()+
      scale_y_continuous(labels = percent_format()) +
      labs(x="", y="", fill="", color="")+

      theme_minimal()+
      theme(
            text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 6L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->gr_cebp_5

    ggplotly(gr_cebp_5, tooltip = "text") %>%
      layout(title = paste0("Busqueda de personas por estatus de su busqueda" ),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=50))


  })









  #############################################################################
  #                  Fiscalia
  #############################################################################
  output$fiscalia_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(fiscalia$año)))

  })

  output$fiscalia_delito <- renderUI({
    selectInput("fiscalia_delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(fiscalia$Violencia)))
  })
  output$fiscalia_municipio <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione uno o varios municipios",
                choices = sort(unique(fiscalia$municipio)))

  })

  #######################################################################33



  fiscalia_data <- reactive({

    fiscalia %>%
      filter(if(!is.null(input$fiscalia_año))               año %in% input$municipal_año        else año != "",
             if(!is.null(input$fiscalia_delito))      Violencia %in% input$fiscalia_delito      else Violencia != "",
             if(!is.null(input$fiscalia_municipio))  municipio %in% input$fiscalia_municipio   else municipio != "")

  })


  output$downloadData_fiscalia <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(fiscalia_data(), file, row.names = FALSE)
    })

  output$grafico_fiscalia<- renderPlotly({

    fiscalia_data() %>%
      # fiscalia %>%
      group_by(año, Violencia, municipio) %>%
      summarise(total_violencia=sum(total_violencia, na.rm=T)) %>%
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(total_violencia),
                          "\nAño: ", año,
                          "\nMunicipio: ", municipio,
                          "\nDelito: ", Violencia, sep="")) %>%
      ggplot(aes(x=año, y=total_violencia, text=text
                 , fill=Violencia, color=Violencia, group=Violencia))+
      geom_line(size=2)+
      geom_point(size=4, alpha=1)+
      scale_color_brewer(palette = "Dark2")+
      scale_fill_brewer(palette = "Dark2")+
      scale_y_continuous(labels = comma_format()) +
      labs(x="", y="", fill="", color="")+
      theme_minimal()+
      theme(text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->gr_fiscalia

    ggplotly(gr_fiscalia, tooltip = "text") %>%
      layout(title = paste0("Total de carpetas por el delito de \n" , fiscalia_data()$municipio[1]),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=30))


  })



  output$mapa_fiscalia<- renderPlotly({

fiscalia_data() %>%
  ggplot(aes(fill=Violencia))+
    geom_sf()+
    # scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+
    # scale_fill_viridis_c(option="F", direction = -1) +
    # labs(title = "Grado de marginación",
    #      subtitle = "Por colonia en la Ciudad de México",
    #      caption = "Elaborado con base a los datos abiertos del IECM (2022) y Índice de Desarrollo Social (IDS) 2020.",
    # fill="Grado de marginación")+
  theme_minimal()+
  theme(legend.position = "bottom",
        text=element_text(size=10,  family="Nutmeg-Light"),
        plot.title = element_text(family="Nutmeg-Light"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg-Light"))


  })
  ##############################################################################
  #                     SESNSP
  ##############################################################################
  output$municipal_año <- renderUI({
    selectInput("Año",
                label =  "Seleccione uno o varios años",
                choices = sort(unique(municipios$Año)))

  })
  output$municipal_mes <- renderUI({
    selectInput("Mes",
                label =  "Seleccione uno o varios mes",
                choices = sort(unique(municipios$month)))

  })
  output$municipal_delito <- renderUI({
    selectInput("municipal_delito",
                label =  "Seleccione tipo de delito",
                choices = sort(unique(municipios$Tipo.de.delito)))
  })
  output$municipal_municipio <- renderUI({
    selectInput("Municipio",
                label =  "Seleccione uno o varios municipios",
                choices = sort(unique(municipios$Municipio)))

  })

  #######################################################################33



  municipios_data <- reactive({

    municipios %>%
      filter(if(!is.null(input$municipal_año))                    Año %in% input$municipal_año        else Año != "",
             if(!is.null(input$municipal_mes))                    month %in% input$municipal_mes        else month != "",
             if(!is.null(input$municipal_delito))   Subtipo.de.delito %in% input$municipal_delito     else Subtipo.de.delito != "",
             if(!is.null(input$municipal_municipio))         Municipio %in% input$municipal_municipio  else Municipio != "")

  })


  output$downloadData_municipal <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(municipios_data(), file, row.names = FALSE)
    })

  output$grafico_municipal<- renderPlotly({

    municipios_data() %>%
      mutate(text = paste("Total de carpetas del delito: ", scales::comma(Total),
                          "\nAño: ", Año,
                          "\nMunicipio: ", Municipio,
                          "\nDelito: ", Subtipo.de.delito, sep="")) %>%
      ggplot() +
      aes(x =as.factor(Año), y = Total, fill= Municipio, text=text) +
      geom_col(position = "dodge", alpha=1)+
      # geom_line(aes(x=Año, y=Total, colour=Municipio),size=1) +
      # geom_point(aes(x=Año, y=Total, colour=Municipio), size=3)+
      labs(x="Año", y="Total de carpetas")+
      #scale_color_brewer(palette = "mycolors")+
      #scale_fill_brewer(palette = "mycolors")+
      scale_fill_manual(values = mycolors) +
      #scale_color_manual(values = mycolors129)+
      scale_y_continuous(labels = scales::comma) +

      theme_minimal()+
      theme(text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->municipal

    ggplotly(municipal, tooltip = "text") %>%
      layout(title = paste0("Total de carpetas por el delito de \n" , municipios_data()$Subtipo.de.delito[1]),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=30))


  })



  output$grafico_municipal_periodo<- renderPlotly({

    municipios_data() %>%
      # municipios %>%
      ggplot()+
      aes(x=Periodo, y=total,
          fill=Municipio, colour = Municipio, group = Municipio,
          text=text)+
      geom_line(aes(x=Periodo, y=total),size=1) +
      geom_point(aes(x=Periodo, y=total), size=3, alpha=.1)+
      labs(x="", y="Total de carpetas"
           #title = paste0("Total de carpetas por el delito de \n"#, Regiones_data()$Subtipo.de.delito[1])
      ) +
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values = mycolors)+
      scale_y_continuous(labels = scales::comma) +
      # scale_x_discrete(breaks = c("2015-12","2015-03","2015-06","2015-09",
      #                             "2016-12","2016-03","2016-06","2016-09",
      #                             "2017-12","2017-03","2017-06","2017-09",
      #                             "2018-12","2018-03","2018-06","2018-09",
      #                             "2019-12","2019-03","2019-06","2019-09",
      #                             "2020-12","2020-03","2020-06","2020-09",
      #                             "2021-12","2021-03","2021-06","2021-09",
      #                             "2022-03","2022-06", "2022-09"))+
      labs(x="", y="Total de carpetas iniciadas")+
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=11))->municipal_periodo

    ggplotly(municipal_periodo, tooltip="text") %>%
      layout(legend = list(orientation = "h", x = 0.1, y = -0.8),
             margin = list(b=0,t=30),
             title = paste0("Total de carpetas por el delito de \n" , municipios_data()$Subtipo.de.delito[1])
      )



  })



  #############################################################################
  #                  CEAV
  #############################################################################
  output$ceav_entidad <- renderUI({
    selectInput("ceav_entidad",
                label =  "Seleccione entidad",
                choices = sort(unique(ceav$`entidad federativa`)))

  })

  output$ceav_municipio <- renderUI({
    selectInput("ceav_municipio",
                label =  "Seleccione el municipio",
                choices = sort(unique(ceav$`delegacion o municipio`)))
  })
  output$ceav_victima <- renderUI({
    selectInput("ceav_victima",
                label =  "Seleccione el tipo de victima",
                choices = sort(unique(ceav$`tipo de victima`)))

  })


  ceav_data <- reactive({

    ceav %>%
      filter(if(!is.null(input$ceav_entidad)) `entidad federativa` %in% input$ceav_entidad     else `entidad federativa` != "",
             if(!is.null(input$ceav_municipio)) `delegacion o municipio` %in% input$ceav_municipio   else `delegacion o municipio` != "",
             if(!is.null(input$ceav_victima)) `tipo de victima`%in% input$ceav_victima   else `tipo de victima` != "")

  })




  output$downloadData_ceav <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(ceav_data(), file, row.names = FALSE)
    })

  output$grafico_ceav<- renderPlotly({

    ceav_data() %>%
      # ceav %>%
      group_by(`tipo de victima`) %>%
      filter(!is.na(`tipo de victima`)) %>%
      summarise(total=n()) %>%
      ggplot(aes(x="", weight=total, fill=`tipo de victima` ,
                 label=total,
                 text = paste("\nTotal de cias: ", total,
                              "\nConducta: ", `tipo de victima` , sep="")))+
      geom_bar(position = "fill",  alpha=1)+
      coord_flip()+
      scale_y_continuous(labels = comma_format()) +
      scale_fill_manual(
        values = c(
          `Indirecta` = "#A24895",
          `Directa` = "#F18822"))+
      theme_minimal()+
      theme(text=element_text(size=11,family="Century Gothic"),
            plot.margin = margin(2, 2, 2, 2, "cm"),
            strip.text.x = element_text(size = 12, face = "bold", angle=90),
            plot.tag = element_text(size = 15L, hjust = 0, family="Century Gothic"),
            plot.title = element_text(size = 8L, hjust = 0.5, family="Century Gothic"),
            plot.caption = element_text(size = 12L, hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11))->gr_ceav

    ggplotly(gr_ceav, tooltip = "text") %>%
      layout(title = paste0("Total de atenciones a víctimas \n" ),
             legend = list(orientation = "h", x = 0.1, y = -0.4),
             margin = list(b=0,t=30))


  })



}


# Run the application
shinyApp(ui = ui, server = server)

# # library(shiny)
# # library(bs4Dash) 
# # library(fresh)
# # 
# # 
# # 
# # 
# # 
# # if(interactive()){
# # 
# #   
# #   shinyApp(
# #     ui = dashboardPage(
# #       header = dashboardHeader(),
# #       sidebar = dashboardSidebar(),
# #       body = dashboardBody(
# #         carousel(
# #           id = "mycarousel",
# #           carouselItem(
# #             caption = "Qué es",
# #             userBox(
# #               title = userDescription(
# #                 title = "¿Qué es?",
# #                 # subtitle = "",
# #                 type = 2,
# #                 image = "https://avgm.jalisco.gob.mx/wp-content/themes/avgm/images/lista-icono.png",
# #               ),
# #               headerBorder = F,
# #               status = "indigo",
# #               width=8,
# #               closable = TRUE,
# #               "La Alerta de Violencia de Género contra las Mujeres (AVGM) es un conjunto de medidas y acciones emergentes para enfrentar y erradicar la violencia feminicida en un territorio determinado (municipio y entidad federativa), ya sea ejercida por individuos o por la propia comunidad”.",
# #               footer = " ")          
# #           ),
# #           carouselItem(
# #             caption = "Objetivo",
# #             userBox(
# #               title = userDescription(
# #                 title = "¿Cuál es el objetivo?",
# #                 # subtitle = "",
# #                 type = 2,
# #                 image = "https://avgm.jalisco.gob.mx/wp-content/themes/avgm/images/escudo-icono.png",
# #               ),
# #               headerBorder = F,
# #               status = "indigo",
# #               width=8,
# #               closable = TRUE,
# #               "ARTÍCULO 23.- La Alerta de Violencia de Género contra las mujeres tendrá como objetivos: I. Garantizar la vida, la integridad, la libertad y la seguridad, así como el acceso a la justicia de las mujeres, adolescentes y niñas; II. Generar las condiciones y políticas públicas que contribuyan a la disminución y cese de la violencia feminicida en su contra, y III. Eliminar la desigualdad y discriminación producidas por ordenamientos jurídicos o políticas públicas que agravian los derechos humanos de las mujeres, adolescentes y niñas!",
# #               footer = " ")
# #             ),
# # 
# #           carouselItem(
# #             caption = "Item 4",
# #             #tags$img(src = "https://via.placeholder.com/500")
# #             userBox(
# #               id = "userbox",
# #               title = userDescription(
# #                 title = "Nadia Carmichael",
# #                 subtitle = "lead Developer",
# #                 type = 2,
# #                 image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
# #               ),
# #               status = "primary",
# #               gradient = TRUE,
# #               background = "primary",
# #               boxToolSize = "xl",
# #               "Some text here!",
# #               footer = "The footer here!")
# #           )
# #         )
# #       ),
# #       title = "Carousel"
# #     ),
# #     server = function(input, output) { }
# #   )
# # }
# # 
# 
# 
# # Change colors used in bs4Dash
# bs4dash_sidebar_light(
#   bg = "#D7DF01",
#   color = "#FF0000",
#   active_color = "#00FF00",
#   submenu_bg = "#00FFFF"
# )#> $`sidebar-light-bg`
# #> [1] "#D7DF01"
# #> 
# #> $`sidebar-light-color`
# #> [1] "#FF0000"
# #> 
# #> $`sidebar-light-active-color`
# #> [1] "#00FF00"
# #> 
# #> $`sidebar-light-submenu-bg`
# #> [1] "#00FFFF"
# #> 
# #> attr(,"class")
# #> [1] "fresh_sass_vars" "bs4dash_vars"    "list"           
# 
#   library(shiny)
#   library(bs4Dash)
#   
#   ui <- bs4DashPage(
#     title = "bs4Dash Custom Sidebar",
#     navbar = bs4DashNavbar(),
#     sidebar = bs4DashSidebar(
#       title = "bs4Dash Custom Sidebar",
#       skin = "light",
#       bs4SidebarHeader("Sidebar Title"),
#       bs4SidebarMenu(
#         bs4SidebarMenuItem(
#           tabName = "menu1",
#           text = "Menu 1",
#           icon = "home"
#         ),
#         bs4SidebarMenuItem(
#           tabName = "menu2",
#           text = "Menu 2",
#           icon = "th"
#         ),
#         bs4SidebarMenuItem(
#           text = "Item List",
#           icon = "bars",
#           startExpanded = TRUE,
#           bs4SidebarMenuSubItem(
#             text = "Item 1",
#             tabName = "item1",
#             icon = "circle-thin"
#           ),
#           bs4SidebarMenuSubItem(
#             text = "Item 2",
#             tabName = "item2",
#             icon = "circle-thin"
#           )
#         )
#       )
#     ),
#     body = bs4DashBody(
#       use_theme(create_theme(
#         bs4dash_sidebar_light(
#           bg = "#D7DF01",
#           color = "#FF0000",
#           active_color = "#00FF00",
#           submenu_bg = "#00FFFF"
#         )
#       ))
#     )
#   )
#   
#   server <- function(input, output) {
#     
#   }
#   
#   shinyApp(ui, server)
#   


