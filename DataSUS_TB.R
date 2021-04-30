library(tidyverse)
library(varhandle)
library(read.dbc)
library(lubridate)
library(ggmap)
library(treemap)
library(networkD3)

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
                             '0' = 'Ignorado',
                             'A' = 'Amarela',
                             'N' = 'Preta',
                             'B' = 'Branca'))

# Atribuindo niveis de escolaridade SUS

dados_teste <- dados_teste %>% 
     mutate(CS_ESCOL_N = recode(CS_ESCOL_N,
                                '0' = 'Analfabeto',
                                '1' = 'EF_Incompleto',
                                '01' = 'EF_Incompleto',
                                '2' = 'EF_Incompleto',
                                '02' = 'EF_Incompleto',
                                '3' = 'EF_Incompleto',
                                '03' = 'EF_Incompleto',
                                '4' = 'EF_Completo',
                                '04' = 'EF_Completo',
                                '5' = 'EM_Incompleto',
                                '05' = 'EM_Incompleto',
                                '6' = 'EM_Completo',
                                '06' = 'EM_Completo',
                                '7' = 'ES_Incompleto',
                                '07' = 'ES_Incompleto',
                                '8' = 'ES_Completo',
                                '08' = 'ES_Completo',
                                '9' = 'Ignorado',
                                '09' = 'Ignorado',
                                '10' = 'N_aplicavel',
                                '00' = 'Ignorado'))

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
                                '01' = 'Cura',
                                '2' = 'Abandono',
                                '02' = 'Abandono',
                                '3' = 'Obito_Tuberculose',
                                '03' = 'Obito_Tuberculose',
                                '4' = 'Obito_outras',
                                '04' = 'Obito_outras',
                                '5' = 'Transferencia',
                                '05' = 'Transferencia',
                                '6' = 'Mud_Diagnostico',
                                '06' = 'Mud_Diagnostico',
                                '7' = 'TB_DR',
                                '07' = 'TB_DR',
                                '8' = 'Mud_Esquema',
                                '08' = 'Mud_Esquema',
                                '9' = 'Falencia',
                                '09' = 'Falencia',
                                '10' = 'Abandono_primario'))

# Categorias Tipo Populacional
# Privados de Liberdade, Situacao de Rua, Profissional Saude, Imigrantes

dados_teste <- dados_teste %>% 
     mutate(POP_LIBER = recode(POP_LIBER,
                               '1' = 'Sim',
                               '2' = 'Nao',
                               '3' = 'Ignorado',
                               '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_RUA = recode(POP_RUA,
                             '1' = 'Sim',
                             '2' = 'Nao',
                             '3' = 'Ignorado',
                             '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_SAUDE = recode(POP_SAUDE,
                               '1' = 'Sim',
                               '2' = 'Nao',
                               '3' = 'Ignorado',
                               '9' = 'Ignorado'))

dados_teste <- dados_teste %>%
     mutate(POP_IMIG = recode(POP_IMIG,
                              '1' = 'Sim',
                              '2' = 'Nao',
                              '3' = 'Ignorado',
                              '9' = 'Ignorado'))

# Categoria sexo

dados_teste <- dados_teste %>% 
     mutate(CS_SEXO = recode(CS_SEXO,
                             'M' = 'Masculino',
                             'F' = 'Feminino',
                             'I' = 'Ignorado'))

# categoria transf

dados_teste <- dados_teste %>% 
     mutate(TRANSF = recode(TRANSF,
                            '1' = 'Mesmo_municipio',
                            '2' = 'Municipio_diferente',
                            '3' = 'UF_diferente',
                            '4' = 'Pais_diferente',
                            '9' = 'Ignorado'))

# ajustando a coluna de idade

# filtrando colunas que são maior que 4000
dados_teste <- dados_teste %>% 
        filter(NU_IDADE_N > 4000)

# subtraindo 4000 do vetor para ter a idade real do paciente
# ex 4022 - 4000 = 22 anos

dados_teste$NU_IDADE_N <- dados_teste$NU_IDADE_N - 4000

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

# visualização de dados

# visualizando a quantitadade de tratamentos por ano

p1 <- dados_final %>% 
        select(DT_INIC_TR) %>% 
        na.omit() %>% 
        group_by(DT_INIC_TR) %>%
        summarise(frequencia = n()) %>% 
        filter(DT_INIC_TR >= as.Date("2001-01-01") & DT_INIC_TR < as.Date("2017-01-01")) %>% 
        mutate(ano = year(DT_INIC_TR),
               mes = month(DT_INIC_TR),
               mes_nome = as.factor(recode(mes,
                                 '1' = 'Jan',
                                 '2' = 'Fev',
                                 '3' = 'Mar',
                                 '4' = 'Abr',
                                 '5' = 'Mai',
                                 '6' = 'Jun',
                                 '7' = 'Jul',
                                 '8' = 'Ago',
                                 '9' = 'Set',
                                 '10' = 'Out',
                                 '11' = 'Nov',
                                 '12' = 'Dez')))

p1_mes <- p1 %>% 
        group_by(ano, mes, mes_nome) %>% 
        summarise(total_casos = sum(frequencia))

p1_mes$mes_nome <- fct_relevel(p1_mes$mes_nome, "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                    "Jul", "Ago", "Set", "Out", "Nov", "Dez")

g1 <- ggplot(data = p1_mes, aes(x = mes, y = total_casos, fill = mes_nome)) +
        geom_bar(stat = "identity") +
        facet_wrap( ~ ano, ncol = 4) +
        labs(title = "Total de casos de Tuberculose por Ano",
             subtitle = "Estado de São Paulo",
             y = 'quantidade de casos',
             x = "Mês") + theme_bw(base_size = 15) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        guides(fill = guide_legend(title = "Meses"))

g1 

# Infectados - Sexo

p2 <-  dados_final %>% 
        select(NU_ANO, CS_SEXO) %>%
        filter(CS_SEXO != "Ignorado") %>%
        #filter(NU_ANO >= 2001 & NU_ANO <=2016) %>% 
        group_by(NU_ANO, CS_SEXO) %>% 
        summarise(freq = n())

g2 <- ggplot(data = p2, aes(x = CS_SEXO, y = freq)) +
        geom_bar(fill = 'steelblue', stat = 'identity') +
        labs(title = "Total de casos por Sexo",
             subtitle = "Estado de São Paulo",
             y = 'quantidade de casos',
             x = "")

g2

# boxplot - Idade

p3 <- dados_final %>% 
        select(NU_IDADE_N, CS_SEXO) %>% 
        filter(CS_SEXO != "Ignorado")

g3 <- ggplot(data = p3, aes(x = CS_SEXO, y = NU_IDADE_N, fill = CS_SEXO)) + 
        geom_boxplot() +
        labs(title = "Distribuição de idade por Sexo",
             subtitle = "Estado de São Paulo",
             y = 'Idade',
             x = "") +
        guides(fill = guide_legend(title = "Sexo"))

g3

# boxplot - raca

p4 <- dados_final %>%
        select(CS_RACA, NU_IDADE_N, CS_SEXO) %>% 
        na.omit()

g4 <- ggplot(data = p4, aes(x = CS_RACA, y = NU_IDADE_N, fill = CS_SEXO)) + 
        geom_boxplot() +
        labs(title = "Distribuição de idade por Raça e Sexo",
             subtitle = "Estado de São Paulo",
             y = 'Idade',
             x = "")

g4

# boxplot - nivel escolar

p5 <- dados_final %>%
        select(CS_ESCOL_N, NU_IDADE_N, CS_SEXO) %>% 
        na.omit()

p5$CS_ESCOL_N <- fct_relevel(p5$CS_ESCOL_N,
                             "Ignorado", "N_aplicavel", "Analfabeto",
                             "EF_Incompleto", "EF_Completo", "EM_Incompleto",
                             "EM_Completo", "ES_Incompleto", "ES_Completo")

g5 <- ggplot(data = p5, aes(x = CS_ESCOL_N, y = NU_IDADE_N)) + 
        geom_boxplot(fill = 'deeppink3') +
        labs(title = "Distribuição de idade por Escolaridade",
             subtitle = "Estado de São Paulo",
             y = 'Idade',
             x = "")

g5

# tratamento

p6 <- dados_final %>% 
        select(TRATAMENTO, CS_SEXO) %>% 
        group_by(TRATAMENTO, CS_SEXO) %>% 
        filter(CS_SEXO != "Ignorado") %>% 
        summarise(freq = length(TRATAMENTO)) %>% 
        na.omit()

g6 <- ggplot(data = p6) +
        geom_bar(mapping = aes(x = TRATAMENTO, y = freq, fill = CS_SEXO),
                 stat = 'identity', position = position_dodge()) +
        labs(title = "Total de casos novos por Sexo",
             subtitle = "Estado de São Paulo",
             y = 'quantidade de casos',
             x = "")
        
g6

# forma da tuberculose

p7 <- dados_final %>% 
        select(FORMA, CS_SEXO) %>% 
        group_by(FORMA, CS_SEXO) %>% 
        filter(CS_SEXO != "Ignorado") %>% 
        summarise(freq = length(FORMA)) %>% 
        na.omit()

g7 <- ggplot(data = p7) +
        geom_bar(mapping = aes(x = FORMA, y = freq, fill = CS_SEXO),
                 stat = 'identity', position = position_dodge()) +
        labs(title = "Tipo de tuberculose por Sexo",
             subtitle = "Estado de São Paulo",
             y = 'quantidade de casos',
             x = "")


g7

# situacao encerramento

p8 <- dados_final %>% 
        select(SITUA_ENCE, CS_SEXO) %>% 
        group_by(SITUA_ENCE, CS_SEXO) %>% 
        filter(CS_SEXO != "Ignorado") %>%
        summarise(freq = length(SITUA_ENCE)) %>% 
        na.omit()

g8 <- ggplot(data = p8) +
        geom_bar(mapping = aes(x = SITUA_ENCE, y = freq, fill = CS_SEXO),
                 stat = 'identity', position = position_dodge()) +
        labs(title = "Situação de encerramento por Sexo",
             subtitle = "Estado de São Paulo",
             y = 'quantidade de casos',
             x = "")


g8

# concentracao de casos de TB por microrregiao de SP

p9 <- dados_final %>% 
        filter(UF == 35 & TRATAMENTO == "Caso_Novo") %>% 
        select(Nome_Microrregiao, TRATAMENTO) %>% 
        group_by(Nome_Microrregiao, TRATAMENTO) %>%
        summarise(freq = length(TRATAMENTO)) %>% 
        na.omit()

g9 <- treemap(p9,
        index = "Nome_Microrregiao",
        vSize = "freq",
        type = "index",
        fontsize.labels = 12,
        title = "Concentração de novos casos de Tuberculose por microrregião de São Paulo")

# para aonde vao as transferencias - Diagrama de Sankey

p10 <- dados_final %>% 
        select(Nome_UF, UF) %>% 
        mutate(Origem = "São Paulo") %>% 
        filter(UF != 35) %>% 
        group_by(Nome_UF, UF, Origem) %>% 
        summarise(freq = length(Nome_UF))

# A connection data frame is a list of flows with intensity for each flow

links <- data.frame(
        source = p10$Origem,
        target = p10$Nome_UF,
        value = p10$freq)

# From these flows we need to create a node data frame: it lists every entities involved in the flow

nodes <- data.frame(
        name=c(as.character(links$source),
               as.character(links$target)) %>% 
                unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network

g10 <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE, fontSize = 15)

g10

g1
g2
g3
g4
g5
g6
g7
g8
g9
g10
