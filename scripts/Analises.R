cores=detectCores()
#### Calculando variáveis X e M

#### indicadores1, cache=TRUE}

registerDoParallel(cl <- makeCluster(4))
indicadores1 <- foreach(j = 1:nrow(dbSetores), .combine = rbind) %dopar% {
  # Xij = valor das exportações do produto j pela UF i
  Xij <- dbSetores$V_EXP[j]
  # Xi = valor das exportações da UF i
  Xi  <- sum(dbSetores$V_EXP[dbSetores$CO_ANO == dbSetores$CO_ANO[j] & 
                               dbSetores$SG_UF_NCM == dbSetores$SG_UF_NCM[j]], na.rm = TRUE)
  # Xwj = total das exportações do produto j
  Xwj <- sum(dbSetores$V_EXP[dbSetores$CO_ANO == dbSetores$CO_ANO[j] & 
                               dbSetores$SETOR == dbSetores$SETOR[j]], na.rm = TRUE)
  # Xw = total das exportações
  Xw <- sum(dbSetores$V_EXP[dbSetores$CO_ANO == dbSetores$CO_ANO[j]], na.rm = TRUE)
  # Mij = valor das importações do produto j pela UF i
  Mij <- dbSetores$V_IMP[j]
  # Mi = valor das importações da UF i
  Mi <- sum(dbSetores$V_IMP[dbSetores$CO_ANO == dbSetores$CO_ANO[j] & 
                              dbSetores$SG_UF_NCM == dbSetores$SG_UF_NCM[j]], na.rm = TRUE)
  # Mwj = total das importações do produto j
  Mwj <- sum(dbSetores$V_IMP[dbSetores$CO_ANO == dbSetores$CO_ANO[j] & 
                               dbSetores$SETOR == dbSetores$SETOR[j]], na.rm = TRUE)
  # Mw = total das importações
  Mw <- sum(dbSetores$V_IMP[dbSetores$CO_ANO == dbSetores$CO_ANO[j]], na.rm = TRUE)
  return(data.frame(xij = Xij, xi = Xi, xwj = Xwj, xw = Xw, 
                    mij = Mij, mi = Mi, mwj = Mwj, mw = Mw))
}
stopCluster(cl)

#### Calculando variáveis X e M com exclusões

#### indicadores2, cache=TRUE}

registerDoParallel(cl <- makeCluster(4))
indicadores2 <- foreach(j = 1:nrow(indicadores1), .combine = rbind) %dopar% {
  # Xjr = total das exportações do produto j excluindo a UF i
  Xjr <-  indicadores1$xwj[j] - indicadores1$xij[j]
  # Xir = valor das exportações da UF i excluindo o produto j
  Xir <- indicadores1$xi[j] - indicadores1$xij[j]
  # Xwr = total das exportações nacionais excluindo o produto j
  Xwr <- indicadores1$xw[j] - indicadores1$xwj[j]
  # Mjr = total das importações do produto j excluindo a UF i
  Mjr <- indicadores1$mwj[j] - indicadores1$mij[j]
  # Mir = valor das importações da UF i excluindo o produto j
  Mir <- indicadores1$mi[j] - indicadores1$mij[j]
  # Mwr = total das importações nacionais excluindo o produto j
  Mwr <- indicadores1$mw[j] - indicadores1$mwj[j]
  return(data.frame(xjr = Xjr, xir = Xir, xwr = Xwr, 
                    mjr = Mjr, mir = Mir, mwr = Mwr))
}
stopCluster(cl)

#### Calculando valores j para ICA

#### indicadores3, cache=TRUE}
registerDoParallel(cl <- makeCluster(4))
indicadores3 <- foreach(j = 1:nrow(indicadores1), .combine = rbind) %dopar% {
  # VANTAGEM COMPARATIVA REVELA SIMÉTRICA (VCRSij)
  IVCRij <- (indicadores1$xij[j] / indicadores1$xi[j]) / 
    (indicadores1$xwj[j] / indicadores1$xw[j])
  VCRSij <- (IVCRij - 1) / (IVCRij + 1)
  # VANTAGEM RELATIVA DAS EXPORTAÇÕES (VREij)
  VREij <- log((indicadores1$xij[j] / indicadores2$xjr[j]) / 
                 (indicadores2$xir[j] / indicadores2$xwr[j]))
  # COMPETITIVIDADE REVELADA (CRij)
  CRij <- log(((indicadores1$xij[j] / indicadores2$xjr[j]) / 
                 (indicadores2$xir[j] / indicadores2$xwr[j])) 
              / ((indicadores1$mij[j] / indicadores2$mjr[j]) / 
                   (indicadores2$mir[j] / indicadores2$mwr[j])))
  # ÍNDICE DE GRUBEL-LLOYD DE COMÉRCIO INTRAINDÚSTRIA (GLij)
  GLijn <- ((indicadores1$xij[j] + indicadores1$mij[j]) - 
              abs(indicadores1$xij[j] - indicadores1$mij[j]))
  GLijd <- (indicadores1$xij[j] + indicadores1$mij[j])
  # ÍNDICE DE CONCENTRAÇÃO POR PRODUTO (ICPij)
  ICPij <- (indicadores1$xij[j] / indicadores1$xi[j])^2
  return(data.frame(vcrs = VCRSij, vre = VREij, cr = CRij, 
                    gln = GLijn, gld = GLijd, icp = ICPij))
}
stopCluster(cl)
dbSetores[,6:11] <- NULL
dbSetores <- cbind(dbSetores, indicadores3)

#### Substituindo valores não númericos ou infinitos por missing values

#### set_na, cache=TRUE}
dbSetores$cr[is.nan(dbSetores$cr)] <- NA
dbSetores$cr[is.infinite(dbSetores$cr)] <- NA
dbSetores$gln[is.nan(dbSetores$gln)] <- NA
dbSetores$gln[is.infinite(dbSetores$gln)] <- NA
dbSetores$gld[is.nan(dbSetores$gld)] <- NA
dbSetores$gld[is.infinite(dbSetores$gld)] <- NA
dbSetores$vre[is.nan(dbSetores$vre)] <- NA
dbSetores$vre[is.infinite(dbSetores$vre)] <- NA

#### Calculando ICD

#### icd, cache=TRUE}

registerDoParallel(cl <- makeCluster(4))
icd <- foreach(k = 1:nrow(dbDestinos), .combine = 'c')  %dopar% {
  # Xi = valor das exportações da UF i
  Xi <- sum(dbDestinos$V_EXP[dbDestinos$CO_ANO == dbDestinos$CO_ANO[k] & 
                               dbDestinos$SG_UF_NCM == dbDestinos$SG_UF_NCM[k]], na.rm = TRUE)
  # Xik = valor das exportações da UF i para o destino k
  Xik <- sum(dbDestinos$V_EXP[dbDestinos$CO_ANO == dbDestinos$CO_ANO[k] & 
                                dbDestinos$SG_UF_NCM == dbDestinos$SG_UF_NCM[k] & 
                                dbDestinos$CO_PAIS == dbDestinos$CO_PAIS[k]], na.rm = TRUE)
  ICDik <- (Xik / Xi)^2
  return(ICDik)
}
stopCluster(cl)
dbDestinos$icd <- icd

#### Calculando valores i para ICA e adicionando à tabela RCE

#### ica, cache=TRUE}

registerDoParallel(cl <- makeCluster(4))
ica <- foreach(i = 1:nrow(RCE), .combine = rbind)  %dopar% {
  # VANTAGEM COMPARATIVA REVELA SIMÉTRICA (VCRSi)
  VCRSi <- mean(dbSetores$vcrs[dbSetores$CO_ANO == RCE$ANO[i] & 
                                 dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE)
  # VANTAGEM RELATIVA DAS EXPORTAÇÕES (VREi)
  VREi <- mean(dbSetores$vre[dbSetores$CO_ANO == RCE$ANO[i] & 
                               dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE)
  # COMPETITIVIDADE REVELADA (CRi)
  CRi <- mean(dbSetores$cr[dbSetores$CO_ANO == RCE$ANO[i] & 
                             dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE)
  # ÍNDICE DE GRUBEL-LLOYD DE COMÉRCIO INTRAINDÚSTRIA (GLi)
  GLin <- sum(dbSetores$gln[dbSetores$CO_ANO == RCE$ANO[i] & 
                              dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE)
  GLid <- sum(dbSetores$gld[dbSetores$CO_ANO == RCE$ANO[i] & 
                              dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE)
  GLi <- GLin / GLid
  # ÍNDICE DE CONCENTRAÇÃO POR PRODUTO (ICPi)
  ICPi <- sqrt(sum(dbSetores$icp[dbSetores$CO_ANO == RCE$ANO[i] & 
                                   dbSetores$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE))
  # ÍNDICE DE CONCENTRAÇÃO POR DESTINO (ICDi)
  ICDi <- sqrt(sum(dbDestinos$icd[dbDestinos$CO_ANO == RCE$ANO[i] & 
                                    dbDestinos$SG_UF_NCM == RCE$UF[i]], na.rm = TRUE))
  return(data.frame(vcrs = VCRSi, vre = VREi, cr = CRi, 
                    gl = GLi, icp = ICPi, icd = ICDi))
}
stopCluster(cl)
RCE[,14:19] = NULL
RCE <- cbind(RCE, ica)

#### Criando tabela de indicadores médios

#### dbTotais, cache=TRUE}

dbTotais <-group_by(RCE, UF)%>%
  summarise(SA = mean(SA, na.rm = TRUE), CH = mean(CH, na.rm = TRUE),
            ED = mean(ED, na.rm = TRUE), EP = mean(EP, na.rm = TRUE),
            IF = mean(IF, na.rm = TRUE), IN = mean(IN, na.rm = TRUE),
            PM = mean(PM, na.rm = TRUE), SF = mean(SF, na.rm = TRUE),
            SP = mean(SP, na.rm = TRUE), SS = mean(SS, na.rm = TRUE),
            RG = mean(RG, na.rm = TRUE), VCRS = mean(vcrs, na.rm = TRUE),
            VRE = mean(vre, na.rm = TRUE), CR = mean(cr, na.rm = TRUE),
            GL = mean(gl, na.rm = TRUE), ICP = mean(icp, na.rm = TRUE),
            ICD = mean(icd, na.rm = TRUE), .groups = "keep")

#### Obtendo valores padronizados para indicadores (scores z)

#### Variáveis Padronizadas, cache=TRUE}

dbTotais <- dbTotais %>% 
  mutate(zSA = (SA - mean(dbTotais$SA, na.rm = T)) / sd(dbTotais$SA, na.rm = T), 
         zCH = (CH - mean(dbTotais$CH, na.rm = T)) / sd(dbTotais$CH, na.rm = T), 
         zED = (ED - mean(dbTotais$ED, na.rm = T)) / sd(dbTotais$ED, na.rm = T),
         zEP = (EP - mean(dbTotais$EP, na.rm = T)) / sd(dbTotais$EP, na.rm = T),
         zIF = (IF - mean(dbTotais$IF, na.rm = T)) / sd(dbTotais$IF, na.rm = T),
         zIN = (IN - mean(dbTotais$IN, na.rm = T)) / sd(dbTotais$IN, na.rm = T),
         zPM = (PM - mean(dbTotais$PM, na.rm = T)) / sd(dbTotais$PM, na.rm = T),
         zSF = (SF - mean(dbTotais$SF, na.rm = T)) / sd(dbTotais$SF, na.rm = T),
         zSP = (SP - mean(dbTotais$SP, na.rm = T)) / sd(dbTotais$SP, na.rm = T),
         zSS = (SS - mean(dbTotais$SS, na.rm = T)) / sd(dbTotais$SS, na.rm = T),
         zVCRS = (VCRS - mean(dbTotais$VCRS, na.rm = T)) / sd(dbTotais$VCRS, na.rm = T),
         zVRE = (VRE - mean(dbTotais$VRE, na.rm = T)) / sd(dbTotais$VRE, na.rm = T),
         zCR = (CR - mean(dbTotais$CR, na.rm = T)) / sd(dbTotais$CR, na.rm = T),
         zGL = (GL - mean(dbTotais$GL, na.rm = T)) / sd(dbTotais$GL, na.rm = T),
         zICP = (ICP - mean(dbTotais$ICP, na.rm = T)) / sd(dbTotais$ICP, na.rm = T),
         zICD = (ICD - mean(dbTotais$ICD, na.rm = T)) / sd(dbTotais$ICD, na.rm = T))

#### Imprimindo tabela de indicadores padronizados

#### dbTotaisZ, cache=TRUE}

kbl(dbTotais[,c(1,19:34)], caption = "Indicadores padronizados (scores z)",
    digits = 2, linesep = "")%>%
  kable_styling(full_width = F, font_size = 7, bootstrap_options = "condensed", 
                latex_options = c("striped", "hold_position"))

#### Carregando conjuntos de variáveis X e Y
# x - variáveis independentes (RCE)
# y - variáveis dependentes (ICA)
#### Variáveis X e Y, cache=TRUE}
db <- dbTotais
X <- as.matrix(db[,19:28])
Y <- as.matrix(db[,29:34])

#### Analisando a matriz de correlação entre X e Y quanto à correlação dentro e entre os grupos. 

#### Calculando a matriz e salvando em "correl"

#### Correl, cache=TRUE}
correl <- matcor(X, Y)

#### Obtermos a correlação canonica

# X - matriz numérica (n * p), contendo as coodenadas X
# Y - matriz numérica (n * q), contendo as coodenadas Y

#### ccyx, cache=TRUE}

ccyx <- cc(X,Y)

#### Obtendo a correlação canônica e salvando no objeto v.ca
#### ca, cache=TRUE}

v.ca <- ccyx$cor

#### Testes multivariados de significância para ambas funções canônicas

#### rho, cache=TRUE}
rho <- ccyx$cor

#### Definindo o número de observações "n", o número de variáveis no primeiro
# conjunto de dados "p" e o número de variáveis no segundo conjunto "q".
#### npq, cache=TRUE}

n <- dim(db)[1]
p <- dim(X)[2]
q <- dim(Y)[2]

#### Lambda de Wilks
#### Wilks, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Wilks")

#### Traço de Pillai
#### Pillai, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Pillai")

#### Traço de Hoteling
#### Hoteling, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Hotelling")

#### Traço de gcr de Roy
#### Roy, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Roy")

#### Proporção da variância total explicada
#### Comput loadings, cache=TRUE}
loadings <- comput(X,Y,ccyx)

#### PVTE Uk: Variância das variáveis independentes explicada pelas variáveis canônicas
#### pvte_u}
pvte.u <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*100

#### PVTE Vk: Variância das variáveis dependentes explicada pelas variáveis canônicas
#### pvte_v}
pvte.v <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*100

#### Índice de redundância (IR)

# Sintetiza a PVTE e o R² canônico em um único indicador
# R² canônico: indica o quanto da variancia da variável canônica 
# dependente é explicada pela variável canônica independente. 
# É o quadrado da correlação canônica.
#### IR}
r2.c <-ccyx$cor^2

#### Calculando IR para as variáveis canônicas Uk
#### IRx}
ir.x <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*(ccyx$cor^2)*100

#### Calculando IR para as variáveis canônicas Vk
#### IRy}
ir.y <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*(ccyx$cor^2)*100

#### Coeficientes padronizados (pesos canônicos)

# A magnitude dos pesos canônicos (coeficientes padronizados) representa 
# a contribuição relativa de cada variável para com a respectiva variável 
# estatística latente (variável canônica).

#### Variáveis dependentes;
#### sy}
sy <- diag(sqrt(diag(cov(Y))))

#### Variáveis independentes;
#### sx}
sx <- diag(sqrt(diag(cov(X))))
