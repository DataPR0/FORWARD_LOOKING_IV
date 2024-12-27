
# -------------------------------------------------------------------------
# ACTUALIZACIÓN ICV FORECAST ----------------------------------------------
# -------------------------------------------------------------------------


require(pacman)
p_load(tidyverse,lubridate, tseries, zoo, magrittr,readxl)
p_load(vars,urca,forecast,mFilter,tempdisagg, seasonal)


# -------------------------------------------------------------------------
# AJUSTE DE VARIABLES MACROECONOMICAS -------------------------------------
# -------------------------------------------------------------------------

# TRATAMIENTO TRM ---------------------------------------------------------

setwd("C:/Users/ivan.villalba/Documents/R Scripts and projects/FORWARD LOOKING/MODELO PRINCIPAL") 


# ISE ---------------------------------------------------------------------

ISE <- read_excel(paste0(getwd(),'/FL OCT 2024.xlsx'), sheet = 'ISE') %>%
        dplyr::select(-c(1,2)) %>% ts(start = c(2005,1), frequency = 12)

BD_VAR <- merge.zoo(ISE)

# -------------------------------------------------------------------------

# IPC---------------------------------------------------------------------
IPC <- read_excel(paste0(getwd(),'/FL OCT 2024.xlsx'), sheet = 'IPC') %>% 
        pivot_longer(names_to = 'Año',values_to = 'IPC', cols = c(3:24)) %>%
      arrange(Año,Mes) %>% dplyr::select(-1) %>% ts(start = c(2003,1),frequency = 12)
IPC <- IPC[,3]
IPC_D <- seas(IPC)$data[,1]
BD_VAR <- merge.zoo(BD_VAR,IPC,IPC_D)

#View(BD_VAR)

# TES ---------------------------------------------------------------------

TES <- read_excel(paste0(getwd(),'/FL OCT 2024.xlsx'), sheet = 'TES') %>% 
      dplyr::select(-1) %>%
      ts(start = c(2003,1),frequency = 12)

#TES_D = TES %>% sapply(function(x) seas(x)$data[,1]) %>%
#          ts(start = c(2003,1),frequency = 12)
#colnames(TES_D) <- paste0(colnames(TES_D),'_D')

BD_VAR <- merge.zoo(BD_VAR,TES)



# AGREGADOS MONETARIOS ------------------------------------------------
M <- read_excel(paste0(getwd(),'/FL OCT 2024.xlsx'), sheet = 'MASA') %>% dplyr::select(-1) %>%
  ts(start = c(1984,1),frequency = 12)


Serie <- time(M)

M <- Serie %>% cbind(M) %>% data.frame() %>% setNames(c('Fecha','M1','M2','M3')) %>% filter(Serie >=2010)
M <- M %>% ts(start = c(2010,1),frequency = 12)

M1 <- seas(M[,2])
M1 <- M1$data[,1]
M2 <- seas(M[,3])
M2 <- M2$data[,1]
M3 <- seas(M[,4])
M3 <- M3$data[,1]

BD_VAR <- merge.zoo(BD_VAR,M1_D =M1,M2_D =M2, M1= M[,2], M2 = M[,3])

#View(BD_VAR)


# TI ----------------------------------------------------------------------
TI <- read_excel(paste0(getwd(),'/FL OCT 2024.xlsx'), sheet = 'TI') %>% dplyr::select(-1) %>%
  ts(start = c(1995,1),frequency = 12)

Serie_ti <- time(TI)

TI <- Serie_ti %>% cbind(TI) %>% data.frame() %>% filter(. >=2005) %>% ts(start = c(2005,1),frequency = 12)

BD_VAR <- merge.zoo(BD_VAR, TI = TI[,2],TI_D = seas(TI[,2],x11="")$data[,1])


# -------------------------------------------------------------------------
View(BD_VAR)
dim(BD_VAR)
BD_DF <- data.frame(BD_VAR) %>% mutate(Fecha = seq(from=as.Date('2003-01-01'),as.Date('2024-12-01'),by='month')) %>%
                                dplyr::select(14,everything()) %>%
                                filter(Fecha >= '2005-01-01')
openxlsx::write.xlsx(BD_DF,'FL ICV VARIABLES.xlsx')
