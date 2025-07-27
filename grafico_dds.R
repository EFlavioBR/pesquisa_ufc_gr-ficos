library(tidyverse)
library(readxl)
library(janitor)
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("ricardo-bion/ggradar")
file.choose()
planilha <- ("C:\\Users\\Emanuel\\Desktop\\faculdade\\1 semestre\\análiseexplodedados\\planilha_pesquisa.csv") 

dados_pesquisa <- read_csv(planilha)

#limpando os dados
dados_limpos <- dados_pesquisa %>%
  clean_names()

#questão 6 separada respostas

dados_violencia_separado <- dados_limpos %>%
  separate_rows(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sep=";")

contagem_violencia <- dados_violencia_separado %>%
  count(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sort = TRUE)
print(contagem_violencia)

#ideia de gráfico violencia predominante em x campus
contagem_campus_violencia <- dados_limpos %>%
  separate_rows(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sep=";") %>%
  mutate(tipo_violencia_limpo = str_trim(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar)) %>%
  filter(tipo_violencia_limpo != "",
         tipo_violencia_limpo != "Prefiro não informar",
         !is.na(x1_em_qual_campus_voce_estuda)) %>%
  count(x1_em_qual_campus_voce_estuda, tipo_violencia_limpo, sort = TRUE, name = "total_relatos")
print(contagem_campus_violencia)

#modelando
dados_porangabucu <- contagem_campus_violencia %>%
  filter(x1_em_qual_campus_voce_estuda == "Porangabuçu") %>%
  mutate(porcentagem = total_relatos / sum(total_relatos))

ggplot(dados_porangabucu, aes(x = "", y = total_relatos, fill = tipo_violencia_limpo)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(porcentagem, accuracy = 1)), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Violências no campus do Porangabuçu",
    fill = "Tipo de Violência",
    x = NULL,
    y = NULL
  ) +
  theme_void()


#modelando
dados_benfica <- contagem_campus_violencia %>%
  filter(x1_em_qual_campus_voce_estuda == "Benfica") %>%
  mutate(porcentagem = total_relatos / sum(total_relatos))

ggplot(dados_benfica, aes(x = "", y = total_relatos, fill = tipo_violencia_limpo)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(porcentagem, accuracy = 1)), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Violências no campus do Benfica",
    fill = "Tipo de Violência",
    x = NULL,
    y = NULL
  ) +
  theme_void()


#modelando
dados_labomar <- contagem_campus_violencia %>%
  filter(x1_em_qual_campus_voce_estuda == "Labomar") %>%
  mutate(porcentagem = total_relatos / sum(total_relatos))

ggplot(dados_labomar, aes(x = "", y = total_relatos, fill = tipo_violencia_limpo)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(porcentagem, accuracy = 1)), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Violências no campus do Labomar",
    fill = "Tipo de Violência",
    x = NULL,
    y = NULL
  ) +
  theme_void()


#gráfico spider chart





#------------------------------------------------------------------------------------
# CÓDIGO FINAL CORRIGIDO PARA O RADAR DE PERFIL DE IMPACTO
#------------------------------------------------------------------------------------

# Carregue as bibliotecas, se ainda não o fez
library(tidyverse)
library(ggradar)

# PASSO 1: PREPARAÇÃO E CÁLCULO DOS DADOS (COM A LÓGICA CORRETA)

# Identifica as 3 ações mais citadas na P14
top_3_acoes <- dados_limpos %>%
  separate_rows(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente, sep = ";") %>%
  mutate(acao = str_trim(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente)) %>%
  filter(acao != "" & acao != "Prefiro não responder") %>%
  count(acao, sort = TRUE) %>%
  top_n(3) %>%
  pull(acao)

# Cálculo principal
dados_perfil <- dados_limpos %>%
  
  # --- AQUI ESTÁ A GRANDE MUDANÇA ---
  # Cria a nossa própria coluna de agrupamento "Sim" vs "Não"
  mutate(group = case_when(
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Concordo totalmente", "Concordo parcialmente") ~ "Sim, Afetado",
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Discordo totalmente", "Discordo parcialmente") ~ "Não, Não Afetado",
    TRUE ~ "Outro" # Agrupa o resto
  )) %>%
  
  # Filtra para manter apenas os grupos que queremos comparar
  filter(group != "Outro") %>%
  
  # Agrupa pela nossa nova coluna
  group_by(group) %>%
  
  # Calcula todas as métricas
  summarise(
    `Confiança p/ Conversar` = mean(str_detect(x5_voce_sente_que_tem_alguem_em_quem_pode_confiar_para_conversar_caso_passe_por_uma_situacao_dificil_como_um_assedio, "Sim"), na.rm = TRUE) * 100,
    `Eventos (Média)` = mean(as.numeric(str_replace(x10_quantos_eventos_palestras_voce_participou_ou_ouviu_falar_que_foram_ofertados_pela_ufc_sobre_o_assunto, "4 ou mais", "4")), na.rm = TRUE),
    `Pensou em Desistir` = mean(str_detect(x16_ja_pensou_em_desistir_da_sua_permanencia_na_universidade_por_inseguranca_em_relacao_a_violencia_de_genero, "Concordo"), na.rm = TRUE) * 100,
    `Identificação c/ Minorias` = mean(x4_voce_se_identifica_com_alguma_minoria_social != "Não", na.rm = TRUE) * 100,
    `Crê no Silêncio Inst.` = mean(x13_o_silencio_institucional_contribui_para_a_continuidade_da_violencia_contra_a_mulher_no_ambiente_academico == "Sim", na.rm = TRUE) * 100,
    `Crê na Proteção da Reput.` = mean(x12_voce_acredita_que_a_universidade_protege_mais_a_reputacao_da_instituicao_do_que_as_vitimas_de_violencia == "Sim", na.rm = TRUE) * 100,
    `Insegurança (Locais Médio)` = mean(str_count(x9_onde_voce_se_sente_insegura_na_universidade_com_respeito_a_violencia_de_genero, ";") + 1, na.rm = TRUE),
    `Urgente: Ação 1` = mean(str_detect(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente, top_3_acoes[1]), na.rm = TRUE) * 100,
    `Urgente: Ação 2` = mean(str_detect(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente, top_3_acoes[2]), na.rm = TRUE) * 100,
    `Urgente: Ação 3` = mean(str_detect(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente, top_3_acoes[3]), na.rm = TRUE) * 100
  ) %>%
  rename(!!paste("Urgente:", top_3_acoes[1]) := `Urgente: Ação 1`,
         !!paste("Urgente:", top_3_acoes[2]) := `Urgente: Ação 2`,
         !!paste("Urgente:", top_3_acoes[3]) := `Urgente: Ação 3`) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# PASSO 2: NORMALIZAÇÃO DOS DADOS
dados_perfil_normalizado <- dados_perfil %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 100))))

# PASSO 3: CRIAÇÃO DO GRÁFICO
ggradar(
  dados_perfil_normalizado,
  grid.min = 0, 
  grid.mid = 50, 
  grid.max = 100,
  group.line.width = 1,
  group.point.size = 3,
  legend.title = "Desempenho Acadêmico Afetado?",
  legend.position = "bottom"
) +
  labs(title = "Perfil Comparativo: Impacto da Violência no Desempenho Acadêmico")



#----------------------------------------------------------------------------------
# ANÁLISE: IMPACTO NO DESEMPENHO (P15) POR CURSO
#----------------------------------------------------------------------------------
library(tidyverse)

# Supondo que seu dataframe limpo se chame 'dados_limpos'
dados_limpos %>%
  # 1. Filtra os campi de interesse
  filter(x1_em_qual_campus_voce_estuda %in% c("Pici", "Benfica", "Labomar")) %>%
  
  # 2. Remove respostas neutras ou vazias da P15 para focar na análise
  filter(!x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Prefiro não responder", "Não concordo e nem discordo")) %>%
  
  # Usamos 'mutate' e 'fct_lump_n' para agrupar os 15 cursos com mais respostas
  # e chamar todos os outros de "Outros". Isso limpa o gráfico.
  mutate(
    curso_agrupado = fct_lump_n(x3_qual_curso_voce_esta_realizando, n = 15),
    
    # Reordena os níveis da P15 para que apareçam em uma ordem lógica na legenda do gráfico
    resposta_p15_ordenada = fct_relevel(
      x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada,
      "Concordo totalmente", "Concordo parcialmente", "Discordo parcialmente", "Discordo totalmente"
    )
  ) %>%
  
  # Cria o gráfico
  ggplot(aes(y = fct_rev(curso_agrupado), fill = resposta_p15_ordenada)) +
  
  # geom_bar com position = "fill" cria o gráfico de 100%
  geom_bar(position = "fill") +
  
  # Formata o eixo X para mostrar porcentagens
  scale_x_continuous(labels = scales::percent_format()) +
  
  # Usa uma paleta de cores divergente (Vermelho-Amarelo-Azul) que é ótima para escalas de concordância
  scale_fill_brewer(palette = "RdYlBu") +
  
  labs(
    title = "Impacto no Desempenho Acadêmico por Curso",
    subtitle = "Análise para os campi Pici, Benfica e Labomar",
    x = "Proporção de Respostas",
    y = "Curso",
    fill = "Desempenho Afetado?"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8) # Ajusta o tamanho da fonte dos cursos
  ) + facet_wrap(~ x1_em_qual_campus_voce_estuda)








library(tidyverse)

dados_limpos %>%
  filter(x1_em_qual_campus_voce_estuda == "Benfica") %>%
  filter(!x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Prefiro não responder", "Não concordo e nem discordo")) %>%
  mutate(curso_agrupado = fct_lump_n(x3_qual_curso_voce_esta_realizando, n = 15)) %>%
  mutate(
    resposta_p15_ordenada = fct_relevel(
      x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada,
      "Concordo totalmente", "Concordo parcialmente", "Discordo parcialmente", "Discordo totalmente"
    )
  ) %>%
  ggplot(aes(y = fct_rev(curso_agrupado), fill = resposta_p15_ordenada)) +
  geom_bar(position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(
    title = "Impacto no Desempenho Acadêmico por Curso no Campus do Benfica",
    x = "Proporção de Respostas",
    y = "Curso",
    fill = "Desempenho Afetado?"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8)
  )
