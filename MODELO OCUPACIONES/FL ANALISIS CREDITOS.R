# -------------------------------------------------------------------------

# FL COMPONENTE 2 ---------------------------------------------------------
# ANALISIS CREDITOS INDIVIDUALES ------------------------------------------
setwd("//192.168.40.9/garaje$/BackUps_Analitica/BACKUP IVAN/ORGANIGRAMA/FORWARD LOOKING")
# -------------------------------------------------------------------------

require(pacman)
p_load(tidyverse,stringi,magrittr,openxlsx)
p_load(lmtest, sandwich)
p_load(plm)

# CONEXION A DB Y QUERY ---------------------------------------------------
Conexion_comite <- DBI::dbConnect(odbc::odbc(),Driver = "SQL Server",Server = '192.168.50.38\\DW_FZ',  
                                  Trusted_Connection = "yes",timeout = 1000, database = "DW_FZ")

QUERY_cartera <- "SELECT db1.*, db2.valor_garantia_final, db2.conteo_garantias, db2.es_vehiculo, db2.LTV, db2.cobertura_garantia FROM (SELECT *,
                	  case when tipo_id_deudor in (1,3) then 'Natural' else'Jurídica' end as deudor,
                	  case when codigo_oficial in (319,348,407,442,443,453,481,482,521,522,523,525,537,542,510,512,513,444) or codigo_concesionario in (135,995,939) then 'Transformación'
                			   when codigo_oficial in (150,166,320,111) then 'RFS' else 'Concesionarios' end as Portafolio
                    from [DW_FZ].[dbo].[Fact_Cartera_Mes] with(nolock) 
                    WHERE (id_fecha > 20220000 and id_fecha < 20240701)) as db1
                  LEFT JOIN (SELECT credito,id_fecha, valor_garantia_final, conteo_garantias, es_vehiculo, LTV, cobertura_garantia FROM [Cartera175].[dbo].[LTV]) as db2
                  ON db1.credito=db2.credito and db1.id_fecha=db2.id_fecha"

Tabla_comite <- DBI::dbGetQuery(conn = Conexion_comite, statement = QUERY_cartera)


# Tabla_comite %>% dplyr::select(id_fecha, credito, saldo_capital,valor_garantia, valor_garantia_final, LTV,conteo_garantias) %>% 
#                   arrange(credito,id_fecha)%>% View()


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#Tabla_comite <- read_csv2('TABLA COMITE - PANEL DESDE 2022.csv')

cod_ocupaciones <- read_csv2("//192.168.40.9/garaje$/BackUps_Analitica/BACKUP IVAN/ORGANIGRAMA/FORWARD LOOKING/MODELO OCUPACIONES/Ocupaciones/VISTA_UBICACION_DEMO.csv")

# MANIPULACION ------------------------------------------------------------

Cartera <- Tabla_comite %>% left_join(cod_ocupaciones, by = 'credito') %>%
                            filter(!credito %in% c(176986, 220804) &
                                    tipo_producto %in% c(10,30,35,70)) %>%
                             mutate(Año = as.numeric(str_sub(id_fecha,1,4)), Mes = as.numeric(str_sub(id_fecha,5,6))) %>%
                             mutate(saldo_30 = ifelse(numero_dias_mora > 30,saldo_capital,0),
                                    saldo_60 = ifelse(numero_dias_mora > 60,saldo_capital,0),
                                    saldo_90 = ifelse(numero_dias_mora > 90,saldo_capital,0))

Cartera <- Cartera %>% mutate(icvs = case_when(numero_dias_mora <= 30 ~ 'ICV0',
                                               numero_dias_mora >= 31 & numero_dias_mora <=60 ~ 'ICV30',
                                               numero_dias_mora >= 61 & numero_dias_mora <=90 ~ 'ICV60',
                                               numero_dias_mora >= 91 & numero_dias_mora <=120 ~ 'ICV90',
                                               numero_dias_mora >= 121 & numero_dias_mora <=150 ~ 'ICV120',
                                               numero_dias_mora >= 151 & numero_dias_mora <=180 ~ 'ICV150',
                                               numero_dias_mora >= 181 & numero_dias_mora <=210 ~ 'ICV180',
                                               numero_dias_mora >= 211 & numero_dias_mora <=240 ~ 'ICV210',
                                               numero_dias_mora >= 241 & numero_dias_mora <=270 ~ 'ICV240',
                                               numero_dias_mora >= 271 & numero_dias_mora <=300 ~ 'ICV270',
                                               numero_dias_mora >= 301 & numero_dias_mora <=330 ~ 'ICV300',
                                               numero_dias_mora >= 331 & numero_dias_mora <=360 ~ 'ICV330',
                                               numero_dias_mora >= 361 & numero_dias_mora <=10000 ~ 'ICV360')) 

# -------------------------------------------------------------------------

# CATEGORIZACION AGRUPADA ----------------------------------------------------------

### OCUPACION Y VARIABLES CUANTITATIVAS

ICV_OCUPACIONES <- Cartera %>%  group_by(id_fecha, descripcion_ocu) %>% 
                                summarise(ICV_60 = sum(saldo_60,na.rm = T)/sum(saldo_capital,na.rm=T),
                                          'int_cap' = sum(saldo_interes,na.rm = T)/sum(saldo_capital,na.rm=T),
                                          'T_Garantia' = sum(valor_garantia,na.rm=T)/sum(total_deuda, na.rm=T),
                                          iqr_dm = IQR(numero_dias_mora))

### ICV 60 t-1

tabla_lag <- ICV_OCUPACIONES %>% group_by(descripcion_ocu,id_fecha) %>% summarise(ICV_60) %>%
  mutate(diff_60 = dplyr::lag(ICV_60,1)) %>% dplyr::select(-ICV_60)

### PROPORCION PERSONERIAS
tabla_personeria <- Cartera %>% group_by(id_fecha, descripcion_ocu, tipo_id_deudor) %>%
                                summarise(n = n()) %>% 
                                pivot_wider(names_from = tipo_id_deudor, values_from = n) %>% 
                                setNames(c('id_fecha','descripcion_ocu','Tipo_1','Tipo_2','Tipo_3')) %>%
                                mutate(p_tipo2 = sum(Tipo_2, na.rm = T)/sum(Tipo_1,Tipo_2,Tipo_3, na.rm=T))


### PROPORCION TIPO_PRODUCTO
tabla_proporcion <- Cartera %>% group_by(id_fecha, descripcion_ocu,tipo_producto) %>%
                                summarise(n= n()) %>% ungroup(tipo_producto) %>%
                                pivot_wider(names_from = tipo_producto, values_from = n) %>%
                                setNames(c('id_fecha','descripcion_ocu','n_10','n_30','n_35','n_70')) %>%
                                mutate(p_3070 = sum(n_30,n_70,na.rm = T)/sum(n_10,na.rm = T),
                                       p_35 = n_35/ sum(n_10,n_30,n_70,na.rm = T))
### PROPORCION DIAS MORA


### PROPORCION PLACA
tabla_placa <- Cartera %>% group_by(id_fecha, descripcion_ocu, licencia_vehiculo) %>%
                                summarise(n = n()) %>% filter(licencia_vehiculo !='') %>%
                                pivot_wider(names_from = licencia_vehiculo, values_from = n) %>% 
                                mutate(p_placa = sum(PUB, na.rm = T)/sum(PAR, na.rm=T))

### PROPORCION MORA SIMPLE
tabla_mora <- Cartera %>% group_by(id_fecha, descripcion_ocu, icvs) %>%
                          summarise(n = n()) %>% ungroup(icvs) %>%
                          mutate(total = sum(n)) %>%
                          pivot_wider(names_from = icvs, values_from = n) %>% 
                          mutate(p_mora = sum(ICV0, na.rm=T)/sum(total,na.rm=T)) %>%
                          dplyr::select(id_fecha,descripcion_ocu,p_mora)
                          
### PROPORCION RFS
tabla_rfs <- Cartera %>% group_by(id_fecha, descripcion_ocu, Portafolio) %>%
                        summarise(n=n()) %>% ungroup(Portafolio) %>%
                        mutate(total= sum(n)) %>%
                        pivot_wider(names_from = Portafolio, values_from = n) %>% 
                        mutate(p_rfs = sum(RFS, na.rm=T)/sum(total,na.rm=T)) %>%
                        dplyr::select(id_fecha,descripcion_ocu,p_rfs)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# PANEL -------------------------------------------------------------------

PANEL_ICV_BRUTO <- ICV_OCUPACIONES %>% 
                          left_join(tabla_proporcion %>% dplyr::select(id_fecha,descripcion_ocu,p_3070,p_35),
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          left_join(tabla_personeria %>% dplyr::select(id_fecha,descripcion_ocu,p_tipo2),
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          left_join(tabla_placa %>% dplyr::select(id_fecha,descripcion_ocu,p_placa),
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          left_join(tabla_mora,
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          left_join(tabla_rfs,
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          left_join(tabla_lag,
                                    by = c('id_fecha'='id_fecha','descripcion_ocu'='descripcion_ocu')) %>%
                          mutate(ICV_60 = (ICV_60*100), diff_60= (diff_60*100), int_cap = (int_cap *100), p_tipo2 = (p_tipo2*100+1), 
                                 p_3070 = (p_3070*100), p_35 = (p_35*100), T_Garantia = (T_Garantia*100),
                                 p_placa = (p_placa*100), p_mora = (p_mora*100), p_rfs=(p_rfs*100+1),
                                 iqr_dm = (iqr_dm*100)) %>%
                          filter(id_fecha >=20220000)
  

PANEL_ICV <- PANEL_ICV_BRUTO  %>% drop_na() %>% 
                    filter(!is.na(descripcion_ocu) & !descripcion_ocu %in% c('Empleado','Independiente'))

PANEL_ICV <- PANEL_ICV_BRUTO  %>% mutate(descripcion_ocu = ifelse(is.na(descripcion_ocu),'Otros',descripcion_ocu))  %>% 
                                  filter(!descripcion_ocu %in% c('Empleado','Independiente'))
# -------------------------------------------------------------------------

PANEL_ICV %>% ggplot(aes(x=as.factor(id_fecha), y=int_cap, col=descripcion_ocu, group = descripcion_ocu)) + 
              geom_line() + theme_classic() + 
              ggtitle(label = 'Dinámica ICV60 por grupos de ocupación',
                      subtitle = 'Sin Empleados, Independientes y NA') +
              labs(col = "Actividad") + ylab(label = 'ICV 60') + xlab(label='Fecha') +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
              
data.frame(efectos_ind) %>% mutate(Ocupación = rownames(.),
                                   efectos_ind = as.numeric(efectos_ind))  %>% 
                            openxlsx::write.xlsx('COMPONENTE DE EFECTO FIJO.xlsx', rownames=T)

# -------------------------------------------------------------------------

PANEL_ICV %<>% pdata.frame(index = c('descripcion_ocu','id_fecha'))

# MODELOS -----------------------------------------------------------------

cor(PANEL_ICV[,3:12])

# EFECTOS FIJOS


FEPANEL_MODEL <- plm(ICV_60 ~ diff_60 +p_35 + p_3070 + T_Garantia + p_35:T_Garantia 
                     + p_tipo2 + p_tipo2:int_cap + p_rfs:int_cap,
                     model = 'within',data = PANEL_ICV , effect = 'twoways')


efectos_ind <- fixef(FEPANEL_MODEL)
print(efectos_ind)

# EFECTOS ALEATORIOS

REPANEL_MODEL <- plm(ICV_60 ~  diff_60 +p_35 + p_3070 + T_Garantia + p_35:T_Garantia 
                     + p_tipo2 + p_tipo2:int_cap + p_rfs:int_cap,
                     model = 'random',data = PANEL_ICV)

# MODELO OLS
OLS_MODEL <- plm(ICV_60 ~ diff_60 +p_35 + p_3070 + T_Garantia + p_35:T_Garantia 
                 + p_tipo2 + p_tipo2:int_cap + p_rfs:int_cap,
                 model='pooling',data = PANEL_ICV)

# -------------------------------------------------------------------------
summary(FEPANEL_MODEL)

# VALIDACION SUPUESTOS ----------------------------------------------------

## PREBA ELECCION EFECTOS FIJOS SOBRE POOLED
pFtest(FEPANEL_MODEL,OLS_MODEL)

## PRUEBA ELECCION EFECTOS FIJOS SOBRE RANDOM
phtest(FEPANEL_MODEL,REPANEL_MODEL)

## PRUEBA DE DEPENDENCIA CRUZADA
pcdtest(FEPANEL_MODEL, test = c("cd"))

## PRUEBA AUTOCORRELACION IDIOSINCRASICA
pbgtest(FEPANEL_MODEL)

## COEFICIENTES ROBUSTOS
clustered_se <- vcovHC(FEPANEL_MODEL)
coeftest(FEPANEL_MODEL,clustered_se)

# EFECTOS FIJOS -----------------------------------------------------------
efectos_ind <- fixef(FEPANEL_MODEL)
print(efectos_ind)


# ANALISIS DEFECTOS -------------------------------------------------------

PANEL_RESTO <- PANEL_ICV_BRUTO %>% filter(is.na(descripcion_ocu) | descripcion_ocu %in% c('Empleado','Independiente'))

tablero_creditos_ocu <- Cartera %>% group_by(id_fecha,descripcion_ocu) %>% summarise(n=n())

PANEL_RESTO %>% ggplot(aes(x=as.factor(id_fecha),y= ICV_60,colour = descripcion_ocu, group = descripcion_ocu)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ggtitle(label = 'ICV60 histórico por grupo de ocupaciones con defectos')


tablero_creditos_ocu %>% ggplot(aes(x=as.factor(id_fecha), y=n,fill = descripcion_ocu, group = descripcion_ocu)) + geom_area(stat = "identity", position = "fill")

