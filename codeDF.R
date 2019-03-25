library(PNADcIBGE)
library(survey)
library(hexbin)
library(ggplot2)
library(data.table)
library(readxl)
library(Hmisc)
library(tidyverse)
library(srvyr)
library(ggthemes)

PDAD2015 <- fread("dados/Base_Moradores_PDAD_2015-1.csv", dec = ",")
PDAD2015.DICT <- read_excel("dados/Dicionário-Base-Moradores-PDAD-2.xlsx")

var.labels <- PDAD2015.DICT$`RÓTULO DA VARIÁVEL`
names(var.labels) <- PDAD2015.DICT$`NOME DA VARIÁVEL`

#### Dar nome aos dados 
PDAD2015 <- upData(PDAD2015, labels = var.labels)

RA <- PDAD2015 %>%
  dplyr::transmute(RA=factor(A01_DOM_RA,
                             levels=1:31,
                             labels=c('Plano Piloto',      
                                      'Gama',
                                      'Taguatinga',
                                      'Brazlândia',
                                      'Sobradinho',
                                      'Planaltina',
                                      'Paranoá',
                                      'Núcleo Bandeirante',
                                      'Ceilândia',
                                      'Guará',
                                      'Cruzeiro',
                                      'Samambaia',
                                      'Santa Maria',
                                      'São Sebastião',
                                      'Recanto das Emas',
                                      'Lago Sul',
                                      'Riacho Fundo',
                                      'Lago Norte',
                                      'Candangolândia',
                                      'Águas Claras',
                                      'Riacho Fundo II',
                                      'Sudoeste/Octogonal',
                                      'Varjão',
                                      'Park Way',
                                      'SCIA-Estrutural',
                                      'Sobradinho II',
                                      'Jardim Botânico',
                                      'Itapoã',
                                      'SIA',
                                      'Vicente Pires',
                                      'Fercal')))

### Mudar cor 
PDAD2015 <- PDAD2015 %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==1,"BRANCA",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==2,"PRETA",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==3,"AMARELA",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==4,"PARDA/MULATA",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==5,"INDÍGENA",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = ifelse(D05_MOR_COR_RACA==8,"NÃO SABE",D05_MOR_COR_RACA)) %>%
  mutate(D05_MOR_COR_RACA = as.factor(D05_MOR_COR_RACA))

#### Mudar sexo
PDAD2015$D04_MOR_SEXO <- ifelse(PDAD2015$D04_MOR_SEXO==1,"MASCULINO","FEMININO")
PDAD2015$D04_MOR_SEXO <- as.factor(PDAD2015$D04_MOR_SEXO)

## Aqui atribuímos o nome da RA ao código
PDAD2015$A01_DOM_RA <- RA$RA

## Ajustar variaveis
PDAD2015$E14_MOR_PRINC_REND_BRUTO <- ifelse(PDAD2015$E14_MOR_PRINC_REND_BRUTO==77777 | PDAD2015$E14_MOR_PRINC_REND_BRUTO==88888 | PDAD2015$E14_MOR_PRINC_REND_BRUTO==99999,NA,PDAD2015$E14_MOR_PRINC_REND_BRUTO)
PDAD2015$E15_MOR_OUTROS_REND_BRUTO <- ifelse(PDAD2015$E15_MOR_OUTROS_REND_BRUTO==77777 | PDAD2015$E15_MOR_OUTROS_REND_BRUTO==88888 | PDAD2015$E15_MOR_OUTROS_REND_BRUTO==99999,NA,PDAD2015$E15_MOR_OUTROS_REND_BRUTO)



sample.pdad <- PDAD2015 %>%
  srvyr::as_survey_design(id=CD_SEQ_DOM,  # Identificador único da unidade amostrada
                    strata=ESTRATO, # Identificação do estrato
                    weights=PESO_PRE, # Probabilidade da unidade ser sorteada
                    nest=T) # Parâmetro de tratamento para dos IDs dos estratos

#Crie um objeto para pós estrato
post.pop <- unique(subset(PDAD2015,select=c(POP_AJUSTADA)))
# Criar a variável de frequência (a mesma variável de pós-estrato, 
#para funcionar como id e peso)
post.pop$Freq <- post.pop$POP_AJUSTADA

# Declarar o objeto de pós-estrato
sample.pdad <- survey::postStratify(sample.pdad,~POP_AJUSTADA,post.pop)
PDAD2015$E14_MOR_PRINC_REND_BRUTO
renda.media <- svymean(~E14_MOR_PRINC_REND_BRUTO, sample.pdad,na.rm=T)
ra.total <- svytotal(~A01_DOM_RA, sample.pdad,na.rm=T)
raca.total <- svytotal(~D05_MOR_COR_RACA, sample.pdad,na.rm=T)
renda.media.cor <- svyby(~E14_MOR_PRINC_REND_BRUTO, ~D05_MOR_COR_RACA, sample.pdad, svymean, na.rm = T)
rendas.outras.media.RA <- svyby(~E15_MOR_OUTROS_REND_BRUTO, ~A01_DOM_RA, sample.pdad, svymean, na.rm = T)
salario.medio.RA <- svyby(~E14_MOR_PRINC_REND_BRUTO, ~A01_DOM_RA, sample.pdad, svymean, na.rm = T)

rendas.outras.media.RA$E15_MOR_OUTROS_REND_BRUTO+salario.medio.RA$E14_MOR_PRINC_REND_BRUTO
salario.medio.RA
renda.media.cor

renda.media.RA.grafico <- renda.media.RA %>%
  mutate(A01_DOM_RA = fct_reorder(A01_DOM_RA,E14_MOR_PRINC_REND_BRUTO)) %>%
  ggplot( 
       aes(x=A01_DOM_RA, 
           y=E14_MOR_PRINC_REND_BRUTO,
           fill = A01_DOM_RA)
       ) +
  geom_col() +
  geom_text(aes(label=round(E14_MOR_PRINC_REND_BRUTO,digits = 2)),nudge_y = -250)+
  ggtitle("Renda bruta média Região Administrativa do DF em 2015",
        sub = "Dados da PDAD 2015") +
  theme_hc() + 
  coord_flip()+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

renda.media.RA.grafico
