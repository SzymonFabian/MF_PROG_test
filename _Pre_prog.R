
##############################################
#Notatki
##############################################

#height uzależnić od ilości wykresów

##############################################
#Pakiety
##############################################
library(shiny)
library(shinythemes)

library(grid)
library(gridExtra)

library(xlsx)
library(stringr)
library(dplyr)

library(ggplot2)
library(ggridges)

theme_set(theme_ridges())

##############################################
#Ładowanie danych
##############################################


Prognoza <- read.csv('//win00001222/PM/PM1/Robocze/SzF/PROGNOZA.CSV', sep = '\t')
names(Prognoza) <- gsub('_0','',names(Prognoza))

stfct <- read.csv('//win00001222/PM/PM1/Robocze/SzF/DYN_KWART.CSV', sep = '\t')
names(stfct) <- c('OBS','dYER','dYE','dPCR','dITR','dMEITR','dGEITR','dGCR','dXTR','dMTR',
                  'dXTR_MTR','DIVR_YER','dYFD','dYED','dPCD','dMTD','dXTD','dITD',
                  'dCPI','dULC','dWRN','dWR','LN','LPROD','YGA','STN','UR','EUR',
                  'CAB_YE','dHDIR','dWHR','dYET')


##############################################
#Obrobka danych
##############################################



PKB <- Prognoza %>% select(OBS,YER,PCR,GCR,ITR,DIVR,XTR,MTR)
deflatory <- Prognoza %>% select(OBS,YFD, PCD, CPI,YED,GCD,XTD,MTD,ITD)

dPKB <- stfct %>% select(OBS, dYER, dPCR, dITR, dGCR, dXTR, dMTR, dXTR_MTR) %>%
  mutate(ROK = substr(OBS, 1, 4)) %>%
  group_by(ROK) %>%
  summarize(dYER = mean(dYER), dPCR = mean(dPCR), dITR=mean(dPCR),
            dGCR=mean(dGCR),dXTR=mean(dXTR),dMTR=mean(dMTR),dXTR_MTR = mean(dXTR_MTR)) %>%
  filter(ROK >=2018)
names(dPKB) <- gsub('d','',names(dPKB))

ddeflatory <- stfct %>% select(OBS,dYFD,dYED,dPCD,dCPI,dXTD,dMTD,dITD) %>%
  mutate(ROK = substr(OBS, 1, 4)) %>%
  group_by(ROK) %>%
  summarize(dYFD = mean(dYFD),dYED=mean(dYED),dPCD=mean(dPCD),dCPI=mean(dCPI),
            dXTD=mean(dXTD),dMTD=mean(dMTD),dITD=mean(dITD)) %>%
  filter(ROK >=2018)
names(ddeflatory) <- gsub('d','',names(ddeflatory))

##############################################
#Kategoria
##############################################

Kategorie <- c('Deflatory', 'PKB')


##############################################
#Próba
##############################################

start <- paste(str_sub(string = deflatory$OBS[1], start=1, end=4),'-0',str_sub(string = deflatory$OBS[1], start=6, end=6),'-01')
start <- gsub(" ", "", start, fixed = TRUE)

if((str_sub(string = tail(deflatory$OBS, n=1), start=6, end=6))=='4'){
  end <- paste(str_sub(string = tail(deflatory$OBS, n=1), start=1, end=4),'-10-01')
  if((str_sub(string = tail(deflatory$OBS, n=1), start=6, end=6))=='3'){
    end <- paste(str_sub(string = tail(deflatory$OBS, n=1), start=1, end=4),'-07-01')
    if((str_sub(string = tail(deflatory$OBS, n=1), start=6, end=6))=='2'){
      end <- paste(str_sub(string = tail(deflatory$OBS, n=1), start=1, end=4),'-04-01')
    } else{
      end <- paste(str_sub(string = tail(deflatory$OBS, n=1), start=1, end=4),'-01-01')
    }
  }
}
end <- gsub(" ", "", end, fixed = TRUE)

period <- seq(as.Date(start),as.Date(end),by="quarter")


##############################################
#Parametry
##############################################

roz_prog <- as.Date('2022-01-01')


####################################################################################################################################################
#SHINY
####################################################################################################################################################

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "eMPF",
                  tabPanel("Prognoza 2025",
                           sidebarPanel(
                             
                             selectInput("kategoria", "Kategoria:", 
                                         choices=Kategorie),
                             
                             h3("Legenda:"),
                             h5('YE - PKB'),
                             h5('PC - Spożycie gospodarstw domowych'),
                             h5('GC - Spożycie publiczne'),
                             h5('DIV - Zmiana stanu zapasów'),
                             h5('XT - Eksport'),
                             h5('MT - Import'),
                             h6('*R - w cenach stałych'),
                             h6('*D - deflator')
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Prognoza 2025"),
                             
                             h4("Dynamika roczna"),
                             
                               tableOutput('dynamiki'),
                               
                             
                             
                             h4("Wykres - prognoza"),
                             
                               plotOutput('plot1')
                               
                             
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Prognoza 2026", "This panel is intentionally left blank"),
                  tabPanel("Symulacja 06-2022", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  #####################
  #WYKRESY
  #####################
  
  ######PKB
  
  YER <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = YER), color = "darkred") +
    labs(title = "YER - PKB (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
    
  
  PCR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = PCR), color = "darkred") +
    labs(title = "PCR - Spożycie gospodarstw domowych (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  GCR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = GCR), color = "darkred") +
    labs(title = "GCR - Spożycie publiczne (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  ITR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = ITR), color = "darkred") +
    labs(title = "ITR - Nakłady brutto na środki trwałe (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  DIVR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = DIVR), color = "darkred") +
    labs(title = "DIVR - Zmiana stanu zapasów (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  XTR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = XTR), color = "darkred") +
    labs(title = "XTR - Eksport (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  MTR <- ggplot(
    PKB, aes(x=period)) + 
    geom_line(aes(y = MTR), color = "darkred") +
    labs(title = "MTR - Import (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  ##### Deflatory
  
  YFD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = YFD), color = "darkred")+
    labs(title = "YFD - Wartość dodana (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  YED <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = YED), color = "darkred")+
    labs(title = "YED - PKB (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  PCD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = PCD), color = "darkred")+
    labs(title = "PCD - Spożycie gospodarstw domowych (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  GCD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = GCD), color = "darkred")+
    labs(title = "GCD - Spożycie publiczne (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  XTD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = XTD), color = "darkred")+
    labs(title = "XTD - Eksport (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  MTD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = YFD), color = "darkred")+
    labs(title = "MTD - Import (deflator)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  ITD <- ggplot(
    deflatory, aes(x=period)) + 
    geom_line(aes(y = ITD), color = "darkred")+
    labs(title = "ITD - Nakłady brutto na środki trwałe -  (w cenach stałych)",y= "mln PLN", x ="")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
    geom_vline(xintercept=as.numeric(roz_prog), linetype=4)
  
  
 

  
  
  
  
#########################
#OUTPUT
#########################
  
  output$plot1 <- renderPlot(
    {
      if(input$kategoria == 'PKB'){
        
        k1<- grid.arrange(YER, PCR,GCR, ITR, DIVR, XTR, MTR, ncol = 1)
        
      } else {
        k1 <- grid.arrange(YFD, YED, PCD, GCD, XTD, MTD, ITD, ncol = 1)
      }
      grid.draw(k1)
    }, height = 3000)
  

  
  output$dynamiki <- renderTable({
    if(input$kategoria == 'PKB'){
      dynamiki <- dPKB
    } else {
      dynamiki <- ddeflatory
    }
    dynamiki
      })
  

  
  
} # server





# Create Shiny object

shinyApp(ui = ui, server = server)




































