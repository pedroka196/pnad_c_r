library(PNADcIBGE)
library(survey)
library(hexbin)
library(ggplot2)

# Importação Online
help("get_pnadc")

# importarei os dados do ano de 2017
# Design importa os dados para usar no pacote survey
# vars permite que importemos apenas variáveis específicas.

 #podemos importar dados anuais com o parâmetro interview = 1
#Usamos o pacote survey para lidar com dados amostrais complexos.
# usaremos as seguintes variaveis da PNAD
# UF - Unidade da Federação
# V2007 - Sexo
# V2009 - Idade do morador na data de referência
# V2010 -	Cor ou raça
# V3007 -	Já concluiu algum outro curso de graduação?
# VD3001 - Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade)
# VD4001 - Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade
# VD4002 - Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade
# VD4020 - Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
# VD4035 - Horas efetivamente trabalhadas na semana de referência em todos os trabalhos para pessoas de 14 anos ou mais de idade

variaveis_selecionadas <- c("UF", "V2007", "V2009", "V2010", "V3007", "VD3001", "VD4001", "VD4002", "VD4020", "VD4035")
# Baixa os dados 
dadosPNADc_anual <- get_pnadc(year = 2017, labels = T, interview = 1, vars = variaveis_selecionadas,design = F)
dadosPNADc_anual <- pnadc_labeller(dadosPNADc_anual, dictionary.file = "dados/dicionario_PNAD_CONTINUA_MICRODADOS_1_visita_2017_20181108.xls")
dadosPNADc_anual <- pnadc_design(dadosPNADc_anual)
# Para dados offline
# dadosPNADc_anual <- read_pnadc(microdata = "dados/PNADC_2017_visita1_20190108.txt",
#                                input_txt = "dados/PNADC_2017_visita1_20190108.txt",
#                                vars = variaveis_selecionadas)
# dadosPNADc_anual <- pnadc_labeller(dadosPNADc_anual, dictionary.file = "dados/dicionario_PNAD_CONTINUA_MICRODADOS_1_visita_2017_20181108.xls") 
# dadosPNADc_anual <- pnadc_design(dadosPNADc_anual)

# Estimando totais
# usamos a função svytotal.
# a variável que desejamos calcular deve ser precedida por ~

renda.total <- svytotal(~VD4020, dadosPNADc_anual, na.rm = T)
renda.total
# coeficientes de variação
cv(renda.total)

# Intervalos de confiança. Podemos configurar o néi
confint(renda.total,level =.99)
# variáveis categoricas
totalsexo <- svytotal(~V2007, dadosPNADc_anual, na.rm = T)
totalsexo

# também é possível estimar o total de mais de uma variável categórica no mesmo código
totalsexoraca <- svytotal(~V2007 + V2010, dadosPNADc_anual, na.rm = T)
totalsexoraca

# Permite fazer o cruzamento de duas variáveis com o parâmetro interaction
totalsexoEraca <- svytotal(~interaction(V2007, V2010), dadosPNADc_anual, na.rm = T)
ftable(totalsexoEraca)
confint(totalsexoEraca)

# testar interação entre renta por sexo, raça e conclusao de curso de graduacao
total.sexo.raca.graduacao<- svytotal(~interaction(V2007,V2010,V3007), dadosPNADc_anual, na.rm = T)
total.sexo.raca.graduacao


# Estimativa de média usa a função svymean. usa os mesmos critérios
renda.media <- svymean(~VD4020, dadosPNADc_anual, na.rm = T)
renda.media
cv(renda.media) # desvio padrão relativo - mede a dispersão - mostra a extensão da variabilidade em relação à média da população

confint(renda.media)


##### Estimando proporções ##### 
# Utilizando variáveis categóricas, é possível estimar a frequência relativa de cada categoria. Isso pode ser feito também através da função svymean
propsexo <- svymean(~V2007, dadosPNADc_anual, na.rm = T)
propsexo

#Para mais de uma variável, temos:
propsexoraca <- svymean(~V2007 + V2010, dadosPNADc_anual, na.rm = T)
propsexoraca

# Interação entre duas variáveis
prop.sexo.raca <- svymean(~interaction(V2007,V2010), dadosPNADc_anual, na.rm = T)
ftable(prop.sexo.raca)

##### Estimando Razões #####
# usamos a svyratio para estimar a razão entre duas variáveis
txdesocup <- svyratio(~VD4002  == "Pessoas desocupadas", ~VD4001  == "Pessoas na força de trabalho", dadosPNADc_anual, na.rm = T)
txdesocup
# Não funcionou

# Estimando Medianas e Quantis
# Usamos a svyquantile
renda.mediana <- svyquantile(~VD4020, dadosPNADc_anual, quantiles = .5, na.rm = T, ci=T)
renda.mediana
SE(renda.mediana)

# Dá para calcular diversas medianas de uma única vez
renda.quantis <- svyquantile(~VD4020, dadosPNADc_anual, quantiles = c(.1,.25,.5,.75,.9), na.rm = T, ci = T)
renda.quantis

#####  Estimação para um Domínio ##### 
# O domínio pode ser selecionado utilizando a função subset no objeto do plano amostral, aplicando a condição que define aquele domínio
renda.media.mulher <- svymean(~VD4020, subset(dadosPNADc_anual, V2007 == "Mulher"), na.rm = T)
renda.media.mulher

# Isso também vale para condições de desigualdade
txdesocup25 <- svyratio(~VD4002 == "Pessoas desocupadas",~VD4001 == "Pessoas na força de trabalho", subset(dadosPNADc_anual, V2009>=25) , na.rm = T)
ftable(txdesocup25)

# Multiplas condições lógicas também
# Homens pardos com mais de 30 anos
nivelinstrHP30 <- svymean(~VD3001, subset(dadosPNADc_anual, V2007 == "Homem" & V2010 == "Parda" & V2009 > 30), na.rm = T)
nivelinstrHP30


##### Estimação para Vários Domínios #####
# Para vários domínios, podemos usar a função svyby
# argumentos
## A variável da qual se deseja calcular a quantidade
## A variável que define os domínios
## O objeto do plano amostral
## A função utilizada para calcular a quantidade de interesse (svytotal,svymean,svyratio,svyquantile,…)

# Frequência relativa de homens e mulheres em cada nível de instrução
frequencia.Sexo.Instr <- svyby(~V2007, ~VD3001, dadosPNADc_anual, svymean, na.rm = T)
frequencia.Sexo.Instr

# Se quisermos estimar nível de instrução por sexo, fazemos o seguinte
frequencia.Instr.Sexo <- svyby(~VD3001, ~V2007, dadosPNADc_anual, svymean, na.rm = T)
frequencia.Instr.Sexo

renda.media.cor <- svyby(~VD4020, ~V2010, dadosPNADc_anual, svymean, na.rm = T)
renda.media.cor
# Podemos pegar também a renda média efetiva por estado
renda.media.UF <- svyby(~VD4020, ~UF, dadosPNADc_anual, svymean, na.rm = T)
ftable(renda.media.UF)

ftable(confint(renda.media.UF))

# É possível definir domínios que sejam cruzamentos de variáveis categóricas com a função interaction
# É possível utilizar vartype="cv" caso desejemos que no output apareça o coeficiente de variação ao invés do erro padrão da estimativa
# Pessoas desocupadas por sexo e cor
txdesocupSexoRaca <- svyby(~VD4002 == "Pessoas desocupadas", ~interaction(V2007,V2010), dadosPNADc_anual, svyratio, denominator = ~VD4001 == "Pessoas na força de trabalho", na.rm = T, vartype = "cv")
ftable(txdesocupSexoRaca)

##### Gráficos para Dados Amostrais #####
#### Histograma ####
# A função svyhist permite a criação de histogramas para variáveis numéricas que consideram os pesos amostrais para computar as frequências ou densidades das barras.
svyhist(~ as.numeric(VD4035), dadosPNADc_anual, main = "Histograma", xlab = "Número de Horas Trabalhadas")
svyhist(~ as.numeric(VD4035), dadosPNADc_anual, freq = TRUE, main = "Histograma", xlab = "Número de Horas Trabalhadas")

#### Boxplot ####
# Para a construção de boxplots que considerem os pesos amostrais, a função é svyboxplot.
# É necessário declarar a variável numérica, sucedida por um ~ e a variável de agrupamento do boxplot.
svyboxplot(VD4035 ~ 1, dadosPNADc_anual, main = "Boxplot do Número de Horas Trabalhadas")
svyboxplot(VD4035 ~ V2007, dadosPNADc_anual, main = "Boxplot do Número de Horas Trabalhadas por Sexo")
svyboxplot(VD4035 ~ V2007, dadosPNADc_anual, main = "Boxplot do Número de Horas Trabalhadas por Sexo", all.outliers = TRUE)

#### Gráfico de Dispersão
#  gráficos de dispersão usuais não são muito úteis para dados provindo de amostras complexas, pois não conseguem diferenciar o peso de cada ponto representado no gráfico
# a função svyplot tem algumas opções de construção de gráficos de dispersão que representam esses pesos através do argumento style
svyplot(VD4020 ~ VD4035, dadosPNADc_anual, style = "grayhex", xlab = "Horas efetivamente trabalhadas", ylab = "Rendimento efetivo")


##### Modelagem com pacote survey #####
#### Testes de Hipóteses ####
# Os testes de hipóteses incluídos no pacote survey incluem teste-t para médias, teste qui-quadrado e teste de postos
# Abaixo um exemplo do teste t para diferenças de médias de rendimentos efetivos entre sexos:
svyttest(VD4020 ~ V2007, dadosPNADc_anual)
# Outro exemplo para testar diferenças entre médias de horas trabalhadas, entre concluintes e não concluintes de graduação
svyttest(as.numeric(VD4035) ~ V3007, dadosPNADc_anual)
# é possível estimar modelos lineares generalizados para dados amostrais complexos através da função svyglm.
# um modelo de regressão com rendimento efetivo como variável dependente e escolaridade, cor/raça e idade como variáveis explicativas
modeloLin <- svyglm(VD4020 ~ VD3001 + V2010 + V2009, dadosPNADc_anual)
summary(modeloLin)
confint(modeloLin)


# Para modelo logístico, usamos o parâmetro family="binomial"
modeloLog <- svyglm(V3007 ~ V2007 + V2010 + V2009, dadosPNADc_anual, family = "binomial")
summary(modeloLog)
confint(modeloLog)

##### Dados de concentração de renda com o convey ####
dadosPNADc_anual_convey <- convey_prep(dadosPNADc_anual)

# Índice de GINI para a renda efetiva
giniHab <- svygini(~VD4020, dadosPNADc_anual_convey, na.rm  =  TRUE)
giniHab
cv(giniHab)

## Usando svyby, podemos estimar GINI por UF ou município
giniUF <- svyby(~VD4020, by = ~UF, dadosPNADc_anual_convey, svygini, na.rm  =  TRUE)
barplot(giniUF,beside = giniUF$UF)

giniCor <- svyby(~VD4020, by = ~V2010, dadosPNADc_anual_convey, svygini, na.rm  =  TRUE)

giniCor.plot <- ggplot(data=giniCor, group_by = V2010, color = V2010) +
  geom_col(aes(y=VD4020,x=V2010,fill = V2010))+
  geom_label(aes(label = round(VD4020,digits = 3),y=VD4020, x=V2010),nudge_y = 0.1)+
  geom_errorbar(aes(ymin=VD4020-se,ymax=VD4020+se,x=V2010))+
  ggtitle("Índice de Gini por cor ou raça",subtitle = 'PNAD anual 2017')+
  xlab("Cor ou raça")+ylab("Índice de Gini")+
  theme(legend.title = element_blank())+
  ggthemes::theme_hc()

renda.media.cor.plot <- ggplot(data=renda.media.cor, group_by = V2010, color = V2010) +
  geom_col(aes(y=VD4020,x=V2010,fill = V2010))+
  geom_label(aes(label = round(VD4020,digits = 1),y=VD4020, x=V2010),nudge_y = 1000)+
  geom_errorbar(aes(ymin=VD4020-se,ymax=VD4020+se,x=V2010))+
  ggtitle("Renda média efetiva por cor ou raça",subtitle = 'PNAD anual 2017')+
  xlab("")+ylab("Renda média efetiva")+
  theme(legend.title = element_blank())+
  ggthemes::theme_hc()

renda.media.cor.plot
giniCor.plot
###  Curva de Lorenz da renda efetiva:
curvaLorenz <- svylorenz(~VD4020, dadosPNADc_anual_convey, quantiles = seq(0, 1, .05), na.rm  =  TRUE)

