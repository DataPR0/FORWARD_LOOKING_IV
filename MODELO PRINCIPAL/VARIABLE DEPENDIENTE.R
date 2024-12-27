# -------------------------------------------------------------------------

# CONSTRUCCION VARIABLE DEPENDIENTE ---------------------------------------

# -------------------------------------------------------------------------

# LIBRERIAS Y DIRECTORIO --------------------------------------------------

setwd("C:/Users/ivan.villalba/Documents/R Scripts and projects/FORWARD LOOKING/MODELO PRINCIPAL")

require(pacman)
p_load(tidyverse, readxl, openxlsx, stringi, magrittr, DBI, odbc)
p_load(tseries,seasonal,seasonalview,zoo)

# CONEXION A DB Y QUERY ---------------------------------------------------
Conexion_comite <- DBI::dbConnect(odbc::odbc(),Driver = "SQL Server",Server = '192.168.50.38\\DW_FZ',  
                                  Trusted_Connection = "yes",timeout = 1000, database = "DW_FZ")

QUERY2 <- "select id_fecha,
                  credito,
                  valor_desembolso,
                  valor_garantia,
                  tipo_id_deudor,
                  numero_id_deudor,
                  tasa_nominal,
                  saldo_capital,
                  id_fecha_desembolso,
                  numero_dias_mora,plazo_total,
                  tipo_producto,
              	  datediff(MONTH,CAST(id_fecha_desembolso as date),CAST(id_fecha as date)) as altura,
              	  case when tipo_id_deudor in (1,3) then 'Natural' else'Jurídica' end as deudor,
              	  case when codigo_oficial in (319,348,407,442,443,453,481,482,521,522,523,525,537,542,510,512,513,444) or codigo_concesionario in (135,995,939) then 'Transformación'
              			   when codigo_oficial in (150,166,320,111) then 'RFS' else 'Concesionarios' end as Portafolio
          from [DW_FZ].[dbo].[Fact_Cartera_Mes] with(nolock) where id_fecha >= 20100000 and credito not in (220804,176986,935994)"

Tabla_comite <- DBI::dbGetQuery(conn = Conexion_comite, statement = QUERY2)

# -------------------------------------------------------------------------

cod_ocupaciones <- read_csv2("C:/Users/ivan.villalba/Documents/R Scripts and projects/FORWARD LOOKING/Ocupaciones/VISTA_UBICACION_DEMO.csv")

# MANIPULACION ------------------------------------------------------------

Cartera <- Tabla_comite %>% left_join(cod_ocupaciones, by = 'credito') %>%
                            filter(id_fecha >= 20100000 &
                                    Portafolio != 'RFS' & 
                                     !credito %in% c(176986, 220804) &
                                     tipo_producto %in% c(10,30,35,70)) %>%
                            mutate(Año = as.numeric(str_sub(id_fecha,1,4)), Mes = as.numeric(str_sub(id_fecha,5,6))) %>%
                            mutate(saldo_30 = ifelse(numero_dias_mora > 30,saldo_capital,0),
                                   saldo_60 = ifelse(numero_dias_mora > 60,saldo_capital,0),
                                   saldo_90 = ifelse(numero_dias_mora > 90,saldo_capital,0))

# CARTERA P NATURALES ------------------------------------------------------
# ### Distribución ocupaciones
# 
# din_ocu <- Cartera %>% group_by(Año,Mes,descripcion_ocu) %>% summarise(n=n()) %>%
#                        mutate(descripcion_ocu = ifelse(is.na(descripcion_ocu),'N/R',descripcion_ocu),
#                               Fecha = as.numeric(paste0(Año,stri_pad_left(as.character(Mes),width=2,pad='0'))))
# 
# din_ocu %>% ggplot(aes(x= Fecha, y = n, fill = descripcion_ocu), group = Fecha) + geom_area(position = 'fill') +
#             ggtitle(label= 'Distribución por ocupaciones') + theme_minimal()
#             

# } -----------------------------------------------------------------------


ICVS <- Cartera %>% group_by(id_fecha) %>% 
                      summarise(ICV30 = sum(saldo_30,na.rm = T)/sum(saldo_capital),
                                ICV60 = sum(saldo_60,na.rm = T)/sum(saldo_capital),
                                ICV90 = sum(saldo_90,na.rm = T)/sum(saldo_capital)) %>%
                      ts(start = c(2010,1),frequency = 12)

ICCS <- Cartera  %>% group_by(id_fecha) %>% 
                        summarise(ICC30 = sum(saldo_30,na.rm = T)/sum(valor_desembolso),
                                  ICC60 = sum(saldo_60,na.rm = T)/sum(valor_desembolso),
                                  ICC90 = sum(saldo_90,na.rm = T)/sum(valor_desembolso)) %>%
                        ts(start = c(2010,1),frequency = 12)


# -------------------------------------------------------------------------


Cartera_vencida <- merge.zoo(ICVS,ICCS)

### CUIDADO! AJUSTAR JUSTO DESPUES DE CORRER LAS LINEAS ANTERIORES
Cartera_vencida2 <- data.frame(Cartera_vencida)
Cartera_vencida2 %<>% dplyr::select(-c(id_fecha.ICVS,id_fecha.ICCS))
#Cartera_vencida2 %<>% slice(1:(nrow(.)-2))
Cartera_vencida2 %<>% ts(start = c(2010,1), frequency = 12 ) %>% 
                        sapply(function(y) return(seas(y, x11="")$data[,1])) %>% 
                      data.frame() %>% 
                      ts(start = c(2010,1), frequency = 12 )

colnames(Cartera_vencida2) <- paste0(colnames(Cartera_vencida2),'_D')

Cartera_BD <- Cartera_vencida %>% merge.zoo(Cartera_vencida2) %>% data.frame()

openxlsx::write.xlsx(Cartera_BD,'INDICADORES_CV.xlsx')


