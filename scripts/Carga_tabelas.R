# Importando a tabelas do Ranking de Competitividade dos Estados preparada a partir 
# de dados obtidos em https://clp.rankingdecompetitividade.org.br/

RCE <- read_csv2("dados/Ranking dos Estados.csv")

# Importando a tabela de agrupamentos para identificar produtos do agronegócio 
# a partir de dados obtidos em https://indicadores.agricultura.gov.br/agrostat/index.htm

AG <- read_csv2("dados/AGRUPAMENTOS.csv")

# Importando tabelas auxiliares de https://balanca.economia.gov.br/balanca/bd/tabelas

site <- "https://balanca.economia.gov.br/balanca/bd"
NCM <- read_csv2(paste(site,"/tabelas/NCM.csv", sep = ""), 
                 col_types = cols(CO_NCM = col_character()))
NCM_SH <- read_csv2(paste(site,"/tabelas/NCM_SH.csv", sep = ""), 
                    col_names = TRUE, col_types = cols(CO_SH6 = col_character()))
PAIS <- read_csv2(paste(site,"/tabelas/PAIS.csv", sep = ""), 
                  col_types = cols(CO_PAIS = col_character()))
UF <- read_csv2(paste(site,"/tabelas/UF.csv", sep = ""), 
                col_types = cols(CO_UF = col_character()))

# Importando e consolidando dados de exportações de 2015 a 2020 disponíveis em  
# https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_20XX.csv

EXP <- read_csv2(paste(site,"/comexstat-bd/ncm/EXP_2015.csv", sep = ""), 
                 col_types = cols(CO_NCM = col_character(), CO_PAIS = col_character()))
for(ano in 2016:2020){
  EXP <- bind_rows(
    EXP, read_csv2(paste(site,"/comexstat-bd/ncm/EXP_",ano,".csv", sep = ""), 
                   col_types = cols(CO_NCM = col_character(), CO_PAIS = col_character())))
}
EXP <- rename(EXP, V_EXP = VL_FOB)
EXP <- left_join(EXP, AG)

# Importando e consolidando dados de importações de 2015 a 2020 disponíveis em  
# https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_20XX.csv

IMP <- read_csv2(paste(site,"/comexstat-bd/ncm/IMP_2015.csv", sep = ""),
                 col_types = cols(CO_NCM = col_character(), CO_PAIS = col_character()))
for(ano in 2016:2020){
  IMP <- bind_rows(
    IMP, read_csv2(paste(site,"/comexstat-bd/ncm/IMP_",ano,".csv", sep = ""), 
                   col_types = cols(CO_NCM = col_character(), CO_PAIS = col_character())))
}
IMP <- rename(IMP, V_IMP = VL_FOB)
IMP <- left_join(IMP, AG)

# Criando tabelas agrupadas por ano, uf e setor e por ano, uf e destino

dbSetores <- full_join(EXP, IMP)%>%
  filter(SG_UF_NCM != "CB" & SG_UF_NCM != "EX" & SG_UF_NCM != "MN" & 
           SG_UF_NCM != "RE" & SG_UF_NCM != "ED" & SG_UF_NCM != "ND" & 
           SG_UF_NCM != "ZN" & !is.na(SETOR))%>%
  group_by(CO_ANO, SG_UF_NCM, SETOR)%>%
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE), 
            V_IMP = sum(V_IMP, na.rm = TRUE), .groups = "keep")

dbDestinos <- full_join(EXP, IMP)%>%
  filter(SG_UF_NCM != "CB" & SG_UF_NCM != "EX" & SG_UF_NCM != "MN" & 
           SG_UF_NCM != "RE" & SG_UF_NCM != "ED" & SG_UF_NCM != "ND" & 
           SG_UF_NCM != "ZN" & !is.na(SETOR))%>%
  group_by(CO_ANO, SG_UF_NCM, CO_PAIS)%>%
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE), 
            V_IMP = sum(V_IMP, na.rm = TRUE), .groups = "keep" )

# Calculando percentual de exportações dos setores do agronegócio por UF

dbSetoresRes <- group_by(dbSetores, SG_UF_NCM, SETOR)%>%
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE), .groups = "keep")%>%
  pivot_wider(names_from = SG_UF_NCM, values_from = V_EXP)

totais <- rowSums(dbSetoresRes[,2:28], na.rm = T)
dbSetoresRes$TOTAL <- totais

foreach(i = 1:nrow(dbSetoresRes)) %do% {
  foreach(j = 2:28) %do% {
    dbSetoresRes[i,j] <- dbSetoresRes[i,j] / dbSetoresRes$TOTAL[i]  * 100
  }
}
totais <- rowSums(dbSetoresRes[,2:28], na.rm = T)
dbSetoresRes$TOTAL <- totais

# Importando dados da Conab

portal_conab <- "https://portaldeinformacoes.conab.gov.br/downloads/arquivos"
SerieHistoricaGraos <- read_csv2(paste(portal_conab,"/SerieHistoricaGraos.txt", sep = ""))
SerieHistoricaCana <- read_csv2(paste(portal_conab,"/SerieHistoricaCana.txt", sep = ""))

Setores <- as.data.frame(unique(dbSetores$SETOR))
colnames(Setores) <- "SETOR"
