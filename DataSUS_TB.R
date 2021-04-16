library(tidyverse)
library(varhandle)

setwd("~/R/DataSUS/Dados")

dados <- dir(full.names = TRUE) %>%
     map_df(read.dbc)

# pegando somente as colunas de regiao e codigo do IBGE
municipios <- read.csv("C:/Users/Luquinhas/Documents/R/DataSUS/Municipio_sp.csv",
                       sep = ";",
                       encoding = "UTF-8")

municipios <- municipios[1:4]

# trocando nome das colunas
colnames(municipios) <- c("UF", "Nome_UF", "Nome_Microrregiao", "ID_MUNICIP")

# selecionando somente as colunas que iremos utilizar

colunas <- c("TP_NOT", "ID_AGRAVO", "DT_NOTIFIC", "NU_ANO", "DT_DIAG",
             "ID_MUNICIP", "DT_DIAG", "NU_IDADE_N", "CS_SEXO", "CS_GESTANT",
             "CS_RACA", "CS_ESCOL_N", "TRATAMENTO", "RAIOX_TORA", "FORMA", "HIV",
             "AGRAVAIDS", "AGRAVALCOO", "AGRAVDIABE", "AGRAVDOENC", "AGRAVOUTRA",
             "AGRAVDROGA", "AGRAVTABAC", "DT_INIC_TR", "DT_NOTI_AT",
             "SITUA_ENCE", "DT_ENCERRA", "POP_LIBER", "POP_RUA", "POP_SAUDE",
             "POP_IMIG", "TEST_MOLEC", "TEST_SENSI", "TRANSF")

dados2 <- select(dados, colunas)

# transformando colunas de niveis

fatores <- c("CS_SEXO", "CS_GESTANT", "CS_RACA", "CS_ESCOL_N", "TRATAMENTO", 
             "RAIOX_TORA", "FORMA", "HIV", "AGRAVAIDS", "AGRAVALCOO",
             "AGRAVDIABE", "AGRAVDOENC", "AGRAVOUTRA", "AGRAVDROGA", "AGRAVTABAC",
             "SITUA_ENCE", "POP_LIBER", "POP_RUA", "POP_SAUDE", "POP_IMIG",
             "TEST_MOLEC", "TEST_SENSI", "TRANSF")

dados2[fatores] <- lapply(dados[fatores], factor)

#transformando ID_MUNICIP em numeric

dados2$ID_MUNICIP <- unfactor(dados2$ID_MUNICIP)

dados_teste <- dados2

# como os valores de CS_GESTANT todos indicavam que não havia gestacao
# Na proxima filtragem, irei retirar a Coluna

dados_teste <- dados_teste %>% 
     mutate(CS_GESTANT = recode(CS_GESTANT,
                                '1' = 'Sim',
                                '2' = 'Sim',
                                '3' = 'Sim',
                                '4' = 'Ignorado',
                                '5' = 'Nao',
                                '6' = 'Nao',
                                '9' = 'Ignorado'))

# Atribuindo cada raca ao fator designado pelo dicionario do SUS

dados_teste <- dados_teste %>% 
     mutate(CS_RACA = recode(CS_RACA,
                             '1' = 'Branca',
                             '2' = 'Preta',
                             '3' = 'Amarela',
                             '4' = 'Parda',
                             '5' = 'Indigena',
                             '9' = 'Ignorado',
                             '0' = 'Ignorado'))

# Atribuindo niveis de escolaridade SUS

dados_teste <- dados_teste %>% 
     mutate(CS_ESCOL_N = recode(CS_ESCOL_N,
                                '0' = 'Analfabeto',
                                '1' = 'EF_Incompleto',
                                '2' = 'EF_Incompleto',
                                '3' = 'EF_Incompleto',
                                '4' = 'EF_Completo',
                                '5' = 'EM_Incompleto',
                                '6' = 'EM_Completo',
                                '7' = 'ES_Incompleto',
                                '8' = 'ES_Completo',
                                '9' = 'Ignorado',
                                '10' = 'N_aplicavel'))

# Tipo de tratamento inicial

dados_teste <- dados_teste %>% 
     mutate(TRATAMENTO = recode(TRATAMENTO,
                                '1' = 'Caso_Novo',
                                '2' = 'Recidiva',
                                '3' = 'Reingresso_abandono',
                                '4' = 'Nao_sabe',
                                '5' = 'Transf',
                                '6' = 'Pos_obito'))


# Resultados Raio X Torax

dados_teste <- dados_teste %>% 
     mutate(RAIOX_TORA = recode(RAIOX_TORA,
                                '1' = 'Suspeito',
                                '2' = 'Normal',
                                '3' = 'Outra_patologia',
                                '4' = 'Nao_realizado'))

# Forma Clinica - TB

dados_teste <- dados_teste %>% 
     mutate(FORMA = recode(FORMA,
                           '1' = 'Pulmonar',
                           '2' = 'Extra_pulmonar',
                           '3' = 'Pulmonar_EP'))

# Possiveis Agravantes (aids, Alcoolismo, Diabetes, Doenca Mental, Drogas, Tabagismo)

dados_teste <- dados_teste %>% 
     mutate(AGRAVAIDS = recode(AGRAVAIDS,
                               '1' = 'Sim',
                               '2' = 'Nao',
                               '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(AGRAVALCOO = recode(AGRAVALCOO,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(AGRAVDIABE = recode(AGRAVDIABE,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(AGRAVDOENC = recode(AGRAVDIABE,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(AGRAVDROGA = recode(AGRAVDROGA,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(AGRAVTABAC = recode(AGRAVTABAC,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))
dados_teste <- dados_teste %>%
     mutate(AGRAVOUTRA = recode(AGRAVOUTRA,
                                '1' = 'Sim',
                                '2' = 'Nao',
                                '9' = 'Ignorado'))

# categorias Teste HIV

dados_teste <- dados_teste %>% 
     mutate(HIV = recode(HIV,
                         '1' = 'Positivo',
                         '2' = 'Negativo',
                         '3' = 'Andamento',
                         '4' = 'Nao_realizado'))

# Categorias encerramentos

dados_teste <- dados_teste %>% 
     mutate(SITUA_ENCE = recode(SITUA_ENCE,
                                '1' = 'Cura',
                                '2' = 'Abandono',
                                '3' = 'Obito_Tuberculose',
                                '4' = 'Obito_outras',
                                '5' = 'Transferencia',
                                '6' = 'Mud_Diagnostico',
                                '7' = 'TB_DR',
                                '8' = 'Mud_Esquema',
                                '9' = 'Falencia',
                                '10' = 'Abandono_primario'))

# Categorias Tipo Populacional
# Privados de Liberdade, Situacao de Rua, Profissional Saude, Imigrantes

dados_teste <- dados_teste %>% 
     mutate(POP_LIBER = recode(POP_LIBER,
                               '1' = 'Sim',
                               '2' = 'Nao',
                               '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_RUA = recode(POP_RUA,
                             '1' = 'Sim',
                             '2' = 'Nao',
                             '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_SAUDE = recode(POP_SAUDE,
                               '1' = 'Sim',
                               '2' = 'Nao',
                               '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_IMIG = recode(POP_IMIG,
                              '1' = 'Sim',
                              '2' = 'Nao',
                              '9' = 'Ignorado'))

# Categoria sexo

dados_teste <- dados_teste %>% 
     mutate(CS_SEXO = recode(CS_SEXO,
                             'M' = 'Masculino',
                             'F' = 'Feminino',
                             'I' = 'Ignorado'))

#categoria transf

dados_teste <- dados_teste %>% 
     mutate(TRANSF = recode(TRANSF,
                            '1' = 'Mesmo_municipio',
                            '2' = 'Municipio_diferente',
                            '3' = 'UF_diferente',
                            '4' = 'Pais_diferente',
                            '9' = 'Ignorado'))

# tirando algumas outras colunas que julgava que seriam uteis
# no fim não terão uso nenhum

colunas_finais = c("DT_NOTIFIC", "NU_ANO", "ID_MUNICIP", "DT_DIAG", "NU_IDADE_N",
                   "CS_SEXO", "CS_RACA", "CS_ESCOL_N", "TRATAMENTO",
                   "RAIOX_TORA", "FORMA", "HIV", "AGRAVAIDS", "AGRAVALCOO", 
                   "AGRAVDIABE", "AGRAVDOENC", "AGRAVOUTRA", "AGRAVDROGA", 
                   "AGRAVTABAC", "DT_INIC_TR", "DT_NOTI_AT", "SITUA_ENCE",
                   "DT_ENCERRA", "POP_LIBER", "POP_RUA", "POP_SAUDE",
                   "POP_IMIG", "TRANSF")

dados_final <- select(dados_teste, colunas_finais)

# Juntando tabela do DataSUS com código do IBGE

dados_final <- dados_final %>% left_join(municipios, by = 'ID_MUNICIP')

