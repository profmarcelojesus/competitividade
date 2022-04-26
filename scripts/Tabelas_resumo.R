quadro1 <- data.frame(c("Animais vivos (exceto pescados)","Bebidas", "Cacau e seus produtos", 
                        "Café", "Carnes", "Cereais, farinhas e preparações", 
                        "Chá, mate e especiarias", "Complexo soja", "Complexo sucroalcooleiro", 
                        "Couros, produtos de couro e peleteria", "Demais produtos de origem animal", 
                        "Demais produtos de origem vegetal", "Fibras e produtos têxteis"), 
                      c("Frutas (inclui nozes e castanhas)", "Fumo e seus produtos", 
                        "Lácteos", "Pescados", "Plantas vivas e produtos de floricultura", 
                        "Produtos alimentícios diversos", "Produtos apícolas", "Produtos florestais", 
                        "Produtos hortícolas, leguminosas, raízes e tubérculos", 
                        "Produtos oleaginosos (exclui soja)", "Rações para animais", "Sucos", ""))
quadro2 <- data.frame(c("SA - Sustentabilidade ambiental", "CH - Capital humano", 
                        "ED - Educação", "EP - Eficiência da máquina pública", 
                        "IF - Infraestrutura"), 
                      c("IN - Inovação", "PM - Potencial de mercado", "SF - Solidez fiscal", 
                        "SP - Segurança pública", "SS - Sustentabilidade social"))

# Tabela Conab - Dados de área plantada, produtividade e produção agropecuária por UF

tabela1 <- bind_rows(SerieHistoricaGraos, SerieHistoricaCana)
tabela1 <- filter(SerieHistoricaGraos, ano_agricola %in% c("2020", "2020/21"))
tabela1$area_plantada_mil_ha <- as.numeric(tabela1$area_plantada_mil_ha)
tabela1$producao_mil_t <- as.numeric(tabela1$producao_mil_t)
tabela1$produtividade_mil_ha_mil_t <- as.numeric(tabela1$produtividade_mil_ha_mil_t)
tabela1 <- tabela1 %>% group_by(uf) %>% 
  summarise(area = sum(area_plantada_mil_ha, na.rm = T), 
            produtividade = mean(produtividade_mil_ha_mil_t, na.rm = T), 
            producao = mean(producao_mil_t, na.rm = T))

t <- foreach(i = 1:nrow(tabela1), .combine = "rbind") %do% {
  linha  <- as.character(comma(tabela1[i,2:4], digits = 2,  big.mark = ".", decimal.mark = ","))
  return(linha)
}
tabela1[,2:4] <- t

# Principais grupos de produtos exportados por UF

tabela3 <- group_by(dbSetores[,1:4], SETOR, SG_UF_NCM, CO_ANO) %>% 
  pivot_wider(names_from = CO_ANO, values_from = V_EXP, values_fn = sum, values_fill = 0)
tabela3 <- tabela3[order(tabela3$SG_UF_NCM),]

groups <- table(tabela3$SETOR)

colmax <- foreach(i = 1:nrow(tabela3), .combine = "rbind") %do% {
  temp <- filter(tabela3, SETOR == tabela3$SETOR[i])
  ismax <- tabela3[i,"2015"] ==  max(temp[,"2015"]) | tabela3[i,"2016"] ==  max(temp[,"2016"]) |
    tabela3[i,"2017"] ==  max(temp[,"2017"]) | tabela3[i,"2018"] ==  max(temp[,"2018"]) |
    tabela3[i,"2019"] ==  max(temp[,"2019"]) | tabela3[i,"2020"] ==  max(temp[,"2020"])
  return(ismax)
}
tabela3$MAX <- colmax
tabela3 <- filter(tabela3, MAX == TRUE)

t <- foreach(i = 1:nrow(tabela3), .combine = "rbind") %do% {
  linha  <- as.character(comma(tabela3[i,3:8], digits = 0,  big.mark = ".", decimal.mark = ","))
  return(linha)
}

tabela3[,3:8] <- t



#### Tabela com cores ####

# capt <- "Matriz de Indicadores \\label{tabela4}"
# 
# pal.fnc = colorRamp(c("#d8b365", "#c7eae5", "#01665e"))
# 
# max.val = max(tabela4[,3:13][ , sapply(tabela4[,3:13], is.numeric)], na.rm=TRUE)
# 
# t_rce <- tabela4[,c(1,3:13)] %>% mutate_if(is.numeric, function(x) {
#   cell_spec(round(x,0), "latex", bold = F,
#             background = rgb(pal.fnc(x/max.val) %>% replace(., is.na(.), 200), maxColorValue=255))
# }) 
# 
# max.val = max(tabela4[,14:17][ , sapply(tabela4[,14:17], is.numeric)], na.rm=TRUE)
# 
# t_ica <- tabela4[,c(1,14:17)] %>% mutate_if(is.numeric, function(x) {
#   cell_spec(round(x,2), "latex", bold = F, 
#             background = rgb(pal.fnc(x/max.val) %>% replace(., is.na(.), 200), maxColorValue=255))
# }) 
# 
# pal.fnc = colorRamp(c("#01665e", "#c7eae5", "#d8b365"))
# 
# max.val = max(tabela4[,18:19][ , sapply(tabela4[,18:19], is.numeric)], na.rm=TRUE)
# 
# t_ica2 <- tabela4[,c(1,18:19)] %>% mutate_if(is.numeric, function(x) {
#   cell_spec(round(x,2), "latex", bold = F, color=grey(0),
#             background = rgb(pal.fnc(x/max.val) %>% replace(., is.na(.), 200), maxColorValue=255))
# }) 
# 
# t <- full_join(t_rce, t_ica)
# t <- full_join(t, t_ica2)
# t[order(t$RG),] %>%
#   kbl("latex", caption = capt,
#       escape = F, booktabs = F, align = "c")%>%
#   kable_styling(full_width = F, font_size = 10,
#                 latex_options = c("hold_position", "scale_down"))


# nota <- gt::html('<span style="background-color: #A50026; opacity: 0.5;"> &emsp;&emsp;&nbsp;</span>
#                        <span style="background-color: #D73027; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #F46D43; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #FDAE61; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #FEE090; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #FFFFBF; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #E0F3F8; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #ABD9E9; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #74ADD1; opacity: 0.5;"> &emsp;&emsp;</span>
#                        <span style="background-color: #4575B4; opacity: 0.5;"> &emsp;&emsp;&nbsp;&nbsp;</span>
#                        <br>
#                        <span style="color: black;">      10%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;20%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;30%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;40%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;50%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;60%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;70%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;80%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;90%&nbsp;</span>
#                        <span style="color: black;"> &nbsp;100%&nbsp;</span>')



# 
# t <- t %>% mutate_if(is.numeric, function(x) {
#     l <- 1
#     while (limites[[l]] < x) {
#       l <- l + 1
#       if(l==10){break}
#     }
#     cell_spec(round(x, 2), "latex", bold = F,
#               background = cores(10)[l])
# }) 
# 
# kbl(t, format = "latex", caption = "Matriz de indicadores \\label{tabela4}", digits = 0)

# for(x in 17:18) {
#   t <- t %>% column_spec(x, color = "white",
#               background = spec_color(tabela4[[x+1]], option = "F", 
#                                       begin = 0.1, end = 0.9, direction = 1))
# }

#### Tabela cores DT ####

# cores <- colorRampPalette(c("#C0392B","white", "#1F618D"))
# intervalo <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9)
# t <- tabela4[,-2]
# t <- datatable(t, rownames = FALSE, caption = "Matriz de indicadores \\label{tabela4}", 
#                options = list(pageLength = 27, dom = 't', ordering=F, 
#                               columnDefs = list(list(className = 'dt-right', targets = 1:17)),
#                               order = list(1, "desc"))) %>% 
#   formatRound(digits = 0, columns = 2:12) %>% 
#   formatRound(digits = 2, columns = 13:18)
# 
# for(x in 2:16) {
#   t <- t %>% formatStyle(x, 
#                          color = styleInterval(quantile(t$x$data[,x], c(.1,.9)), 
#                                                c('white', 'black', 'white')),
#                          backgroundColor = styleInterval(quantile(t$x$data[,x], 
#                                                                   c(intervalo)),cores(10))) 
# }
# for(x in 17:18) {
#   t <- t %>% formatStyle(x, 
#                          color = styleInterval(quantile(t$x$data[,x], c(.1,.9)), 
#                                                c('white', 'black', 'white')),
#                          backgroundColor = styleInterval(quantile(t$x$data[,x],
#                                                                   c(intervalo)),rev(cores(10)))) 
# }
# t

#### Tabela4 ####

tabela4 <- RCE[rev(order(RCE$RG)),] %>% group_by(UF) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  relocate(RG, .after = ANO) %>% relocate(.after = SS, VCRS = vcrs, VRE = vre, 
                                          CR = cr, GL = gl, ICP = icp, ICD = icd)

cores <- colorRampPalette(c("#C0392B","white", "#1F618D"))

intervalo <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9)

t <- as.data.frame(tabela4[,-2])
t <- t[rev(order(t$RG)),]

for(x in 2:18) {
  limites <- quantile(t[[x]], c(intervalo))
  lcores <- quantile(t[[x]], c(.1,.9))
  for(i in 1:27){
    l <- 1
    while (limites[[l]] < as.numeric(t[i,x])) {
      l <- l + 1
      if(l==10){break}
    }
    cor <- "black"
    if(as.numeric(t[i,x])<lcores[1] | as.numeric(t[i,x])>lcores[2]){cor <- "white"}
    if(x<13){
      t[i, x] <- cell_spec(round(as.numeric(t[i,x]),0), "latex", 
                           color = cor, background = cores(10)[l])
    }else{
      if(x<17){
        t[i, x] <- cell_spec(round(as.numeric(t[i,x]),2), "latex", 
                             color = cor, background = cores(10)[l])
      }else{
        t[i, x] <- cell_spec(round(as.numeric(t[i,x]),2), "latex", 
                             color = cor, background = cores(10)[11-l])
      }
    }
  }
}

tabela4 <- t

#### Tabela 5 - Matriz de correlações cruzadas entre as variáveis dos grupos #### 

tabela5 <- correl$XYcor

colnames(tabela5) <-  c("SA", "CH", "ED", "EP", "IF", "IN", "PM", "SF", "SP", 
                    "SS", "VCRS", "VRE", "CR", "GL", "ICP", "ICD")
rownames(tabela5) <-  c("SA", "CH", "ED", "EP", "IF", "IN", "PM", "SF", "SP", 
                    "SS", "VCRS", "VRE", "CR", "GL", "ICP", "ICD")
tabela5[,1:16] <- round(as.numeric(tabela5[,1:16]),2) 
for(i in 1:(nrow(tabela5))){
  if(i < 16) {tabela5[i, c((i+1):16)] <- NA}
  for(x in 1:16){
    if(!is.na(tabela5[i,x])){
      tabela5[i,x] <- cell_spec(tabela5[i,x], "latex",
                                bold = ifelse(!between(tabela5[i,x], -0.5, 0.50), T, F))
    }
  }
}

#### Tabela 6 - Correlações Canônicas entre as Variáveis Estatísticas ####

tabela8 <- data.frame("u1v1" = v.ca[1], "u2v2" = v.ca[2], "u3v3" = v.ca[3], 
                      "u4v4" = v.ca[4], "u5v5" = v.ca[5], "u6v6" = v.ca[6])

#### Tabela 9 

tabela9 <- p.asym(rho, n, p, q, tstat = "Wilks")

tabela9 <- data.frame(tabela9$stat, tabela9$approx, tabela9$df1, tabela9$df2, tabela9$p.value)

colnames(tabela9) <- c("stat", "approx", "df1", "df2", "p.value")

row.names(tabela9) <- c("1 a 6", "2 a 6", "3 a 6", "4 a 6", "5 a 6", "6 a 6")

tabela9[,5] <- cell_spec(round(tabela9[,5], 3))

tabela9[1,5]  <- paste0(tabela9[1,5], footnote_marker_symbol(1))


#### Tabela 10

tabela10 <- rbind(loadings$corr.Y.xscores, loadings$corr.X.xscores)
colnames(tabela10) <- c("v1", "v2", "v3", "v4", "v5", "v6")
row.names(tabela10) <- str_replace(row.names(tabela10), "z", "")
