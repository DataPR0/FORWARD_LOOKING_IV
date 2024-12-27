# -------------------------------------------------------------------------

# MODELO FL ---------------------------------------------------------------

# -------------------------------------------------------------------------
# LIBRERIAS ---------------------------------------------------------------

setwd("C:/Users/ivan.villalba/Documents/R Scripts and projects/FORWARD LOOKING/MODELO PRINCIPAL")

require(pacman)
p_load(tidyverse,lubridate,zoo,stringi,magrittr,readxl,openxlsx)
p_load(tseries,forecast,vars,seasonal,tempdisagg,urca,lmtest)
p_load(caret)

# ARCHIVOS ----------------------------------------------------------------
DATAOR <- read_excel('FL ICV VARIABLES.xlsx') %>% ts(start = c(2005,1),frequency = 12)
DATA_ICV <- read_excel('INDICADORES_CV.xlsx')

DATA_ICV %<>% sapply(function(x) x*100) %>% data.frame() %>% ts(start = c(2010,1),frequency = 12)

DATA_CONS <- merge.zoo(DATAOR,DATA_ICV) %>% data.frame() %>% 
                mutate(Fecha = seq(as.Date('2005-01-01'),as.Date('2024-12-01'),by='month')) %>%
                drop_na()

DATA <- DATA_CONS 

prueba_st <- DATA_ICV %>% sapply(function (x){return(diff(x))}) %>% 
                          data.frame %>% 
                          sapply(function (y){return(adf.test(y)$p.value)}) 
prueba_st
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

### VARIABLES INTEGRADAS DE ORDEN 1

DATA_PREP <- DATA %>% dplyr::select(ISE=Indicador.de.Seguimiento.a.la.Economía,
                                    M1=M1_D,
                                    TI,
                                    TES.5Y.1Y,
                                    CV60)  %>% 
                      mutate(ISE = log(ISE),
                             M1 = log(M1),
                             TI = log(TI)) %>%
                      ts(start = c(2010,1),frequency = 12) %>% diff()


### VARIABLES INTEGRADAS DE ORDEN 2
INFLACION <- DATA %>% dplyr::select(IPC=IPC_D)  %>%
                      mutate(IPC = log(IPC)) %>%
                      ts(start = c(2010,1),frequency = 12) %>% diff() %>% diff()

### UNION DE ARCHIVOS
DATA_MOD <- DATA_PREP %>% merge.zoo(INFLACION,all = F)


###
prueba_st <- DATA_MOD %>% sapply(function (x){return(adf.test(x)$p.value)})
prueba_st

# -------------------------------------------------------------------------
model_lag <- VARselect(DATA_MOD, lag.max = 15, type = 'const')

model <- VAR(DATA_MOD, p =3)
#summary(model)

# FIN PROCEDIMIENTO -------------------------------------------------------
# MODELO ------------------------------------------------------------------
# -------------------------------------------------------------------------

# VERIFICACION DE SUPUESTOS -----------------------------------------------

serial.test(model, lags.pt = 12)
roots(model)
normality.test(model)
arch.test(model, lags.multi = 12, multivariate.only = T)

###

stability1 <- stability(model, type = "OLS-CUSUM")
plot(stability1)
# -------------------------------------------------------------------------


#x <- vars::irf(model,impulse='M1', response = 'ICV60', lag.ahead = 13, ci=0.90,boot = T, boot.nrep=1000)
#plot(x)
#ggsave('M1IRF.png', dpi = 600, height = 5,width = 5, units = 'in', device = 'png')

### RESPUESTAS DE ICV60

IRF_DF <- names(DATA_MOD) %>% 
  lapply(function(var){vars::irf(model,impulse=var,response='ICV60',lag.ahead=13,ci=0.9,boot = T,boot.nrep=1000)$irf})
#openxlsx::write.xlsx(IRF_DF,'COEFICIENTES IRF.xlsx')

# -------------------------------------------------------------------------
VD <- fevd(model, n.ahead = 12)
VD <- data.frame(t(VD$ICV60)) %>% mutate(var = rownames(.)) %>% setNames(c(1:12,'Var')) %>% pivot_longer(names_to = 'Mes', values_to = 'VARDECOMP', cols = 1:12) 
# VD %>% filter(Var != 'ICV60') %>% mutate(Mes = as.numeric(Mes)) %>% 
#   ggplot(aes(x=Mes, y=VARDECOMP, group = Mes, fill = Var )) + geom_bar(stat = 'identity') + theme_minimal() +
#   ggtitle(label = 'Descomposición de Varianza ICV60',
#           subtitle = 'Variables explicativas') +
#   ylab('VARDECOMP (%)') + xlab('MES') + scale_color_continuous()

# FORECASTING -------------------------------------------------------------

PRONOSTICO <- predict(model,n.ahead = 12, ci=0.9)
 fanchart(PRONOSTICO, names = 'ICV60', col.y = '#33673B' ,cis = 0.00001, colors = '#9A6D38', 
          main = 'PRONÓSTICO ICV60', xlab='Tiempo',ylab='Diferencia (pp)')

# -------------------------------------------------------------------------
model_coefs <- model$varresult$ICV60 %>% coef() %>% data.frame()
DATA_MOD %>% View()
openxlsx::write.xlsx(list(COEFICIENTES=model_coefs,DATA=DATA_MOD %>% data.frame(),FORECAST=FORE$fcst$ICV60),
                     'INPUTS DE SENSIBILIDAD.xlsx',rowNames=T) 
# -------------------------------------------------------------------------

# GRAFICA DE PRONOSTICO ICV ------------------------------------------------
AAA <- PRONOSTICO$endog[,5] %>% data.frame() %>% 
                      setNames('fcst')%>% 
                      mutate(lower= fcst,
                             upper = fcst,
                             CI = fcst) %>% 
                      rbind(PRONOSTICO$fcst$ICV60 %>% data.frame()) %>%
        ts(start = c(2010,3),frequency = 12) %>% zoo()

options(digits = 3)

AAA %>% data.frame() %>% mutate(Date = seq.Date(as.Date('2010-03-01'),as.Date('2025-08-01'),by='month'),
                                original = ifelse(Date <='2024-08-01',fcst,NA))  %>%
  filter(Date >='2022-01-01')%>% 
  ggplot +
  geom_line(aes(x=Date,y=upper, colour = 'IC'),size=0.6,alpha=0.3) +
  geom_line(aes(x=Date,y=lower, colour = 'IC'),alpha=0.5,size=0.6,alpha=0.3) + 
  geom_line(aes(x=Date,y=fcst, colour = 'Pronóstico'),size=0.6) +
  geom_line(aes(x=Date,y=original, colour = 'Data'),size=0.6) + theme_classic() +
  scale_color_manual(values = c('#15616d','#15616d','#ffb627')) +
  labs(subtitle='Pronóstico 12 meses - cambios (pp)',
       col="") + xlab('') + ylab('') +
  scale_y_continuous(breaks = seq(-1, 2, by = 0.2)) +
  scale_x_date(breaks = '3 month', date_labels = '%Y-%m') +
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 14),
        legend.text =  element_text(size = 10) )
ggsave('PRONOSTICOICV.png', dpi = 600, height = 7.5,width = 14, units = 'in', device = 'png')

# -------------------------------------------------------------------------

# CALCULOS ADICIONALES ----------------------------------------------------


# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

DATA_graph <- DATA %>% dplyr::select(ISE=ISE.D,IPC,TI,M1=M1_D,TPM = tasa_intervencion,TES5y1y=TES.5Y.1Y_D)  %>% 
  mutate(ISE = log(ISE),
         M1 = log(M1),
         TI = log(TI),
         IPC = log(IPC)
  ) %>%
  ts(start = c(2010,1),frequency = 12) 
plot.zoo(DATA_graph)


DATA_graph <- DATA %>% dplyr::select(TPM = tasa_intervencion,TES5y1y=TES.5Y.1Y_D)  %>%
  ts(start = c(2010,1),frequency = 12)
  plot.zoo(DATA_graph, main= 'RELACIÓN TPM ~ TES5y1y',col = c(3,4))
cor(DATA_graph)


DATA_graph <- DATA %>% dplyr::select(TRM = Promedio_mensual,TI=TI)  %>%
  ts(start = c(2010,1),frequency = 12)
plot.zoo(DATA_graph, main= 'RELACION TRM ~ TI ')
cor(DATA_graph)


# GRAFICOS PRESENTACION ---------------------------------------------------

### ICV HIST
DATA_ICV <- read_excel('INDICADORES_CV.xlsx') 
p_load(scales)

DATA_ICV %>% mutate(Date=seq.Date(as.Date('2010-01-01'), as.Date('2024-10-01'), by = 'month')) %>% ggplot + 
  geom_line(aes(x=Date,y=ICV30, colour = 'ICV30'),size=0.7) +
  geom_line(aes(x=Date,y=ICV60, colour = 'ICV60'),size=0.7)  + 
  geom_line(aes(x=Date,y=ICV90, colour = 'ICV90'),size=0.7) + theme_classic() +
  labs(subtitle = 'Histórico desde 2010',
       col ="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.015)) + 
  scale_color_manual(values = c('#aec3b0','#15616d','#cce3de')) +
  xlab('') + ylab('') +
  theme(axis.text.x = element_text(size = 12),   # Change x-axis text (tick labels) size
        axis.text.y = element_text(size = 12))
  
ggsave('MARCOICV.png', dpi = 600, height = 7.5,width = 14, units = 'in', device = 'png')
