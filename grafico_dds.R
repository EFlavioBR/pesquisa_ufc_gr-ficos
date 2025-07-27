library(tidyverse)
library(readxl)
library(janitor)
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
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


top_3_acoes <- dados_limpos %>%
  separate_rows(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente, sep = ";") %>%
  mutate(acao = str_trim(x14_pensando_em_aumentar_sua_sensacao_de_seguranca_nos_diferentes_espacos_da_universidade_salas_corredores_areas_externas_etc_qual_das_acoes_abaixo_voce_considera_mais_urgente)) %>%
  filter(acao != "" & acao != "Prefiro não responder") %>%
  count(acao, sort = TRUE) %>%
  top_n(3) %>%
  pull(acao)

dados_perfil <- dados_limpos %>%
  
  mutate(group = case_when(
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Concordo totalmente", "Concordo parcialmente") ~ "Sim, Afetado",
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Discordo totalmente", "Discordo parcialmente") ~ "Não, Não Afetado",
    TRUE ~ "Outro" 
  )) %>%
  
  filter(group != "Outro") %>%
  
  group_by(group) %>%
  
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

dados_perfil_normalizado <- dados_perfil %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 100))))

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



dados_limpos %>%
  filter(x1_em_qual_campus_voce_estuda %in% c("Pici", "Benfica", "Labomar")) %>%
  
  filter(!x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Prefiro não responder", "Não concordo e nem discordo")) %>%
  
  mutate(
    curso_agrupado = fct_lump_n(x3_qual_curso_voce_esta_realizando, n = 15),
    
    resposta_p15_ordenada = fct_relevel(
      x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada,
      "Concordo totalmente", "Concordo parcialmente", "Discordo parcialmente", "Discordo totalmente"
    )
  ) %>%
  
  # Cria o gráfico
  ggplot(aes(y = fct_rev(curso_agrupado), fill = resposta_p15_ordenada)) +
  geom_bar(position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
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
    axis.text.y = element_text(size = 8) 
  ) + facet_wrap(~ x1_em_qual_campus_voce_estuda)


dados_limpos %>%
  filter(x1_em_qual_campus_voce_estuda == "Labomar") %>%
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
    title = "Impacto no Desempenho Acadêmico por Curso no Campus do Labomar",
    x = "Proporção de Respostas",
    y = "Curso",
    fill = "Desempenho Afetado?"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8)
  )


dados_limpos %>%
  mutate(impacto_academico = case_when(
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Concordo totalmente", "Concordo parcialmente") ~ "Afetado",
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Discordo totalmente", "Discordo parcialmente") ~ "Não Afetado",
    TRUE ~ "Outro"
  )) %>%
  filter(impacto_academico != "Outro") %>%
  separate_rows(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sep = ";") %>%
  mutate(violencia_limpa = str_trim(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar)) %>%
  filter(violencia_limpa != "" & violencia_limpa != "Prefiro não informar") %>%
  ggplot(aes(y = violencia_limpa, fill = impacto_academico)) +
  geom_bar(position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Afetado" = "#d9480f", "Não Afetado" = "#555555")) +
  labs(
    title = "Relação entre Tipo de Violência e Impacto no Desempenho Acadêmico",
    subtitle = "Qual tipo de violência tem maior probabilidade de afetar os estudos?",
    x = "Proporção de Respondentes",
    y = "Tipo de Violência",
    fill = "Desempenho Acadêmico"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



dados_limpos %>%
  mutate(impacto_academico = case_when(
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Concordo totalmente", "Concordo parcialmente") ~ "Desempenho Afetado",
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Discordo totalmente", "Discordo parcialmente") ~ "Desempenho Não Afetado",
    TRUE ~ "Outro" 
  )) %>%
  
  mutate(grupo_minoria = case_when(
    x4_voce_se_identifica_com_alguma_minoria_social == "Não" ~ "Não Pertence a Minoria",
    x4_voce_se_identifica_com_alguma_minoria_social == "Prefiro não responder" ~ "Outro",
    TRUE ~ "Pertence a Minoria" # Agrupa todas as outras respostas ("Parda/preta", "LGBTQIA+", etc.)
  )) %>%
  
  filter(impacto_academico != "Outro",
         grupo_minoria != "Outro") %>%
  
  separate_rows(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sep = ";") %>%
  
  mutate(violencia_limpa = str_trim(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar)) %>%
  filter(violencia_limpa != "" & violencia_limpa != "Prefiro não informar") %>%
  
  ggplot(aes(y = violencia_limpa, fill = impacto_academico)) +
  
  geom_bar(position = "fill") +
  
  # Cria um painel separado para cada grupo de minoria
  facet_wrap(~ grupo_minoria) +
  
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Desempenho Afetado" = "#d9480f", "Desempenho Não Afetado" = "#555555")) +
  
  labs(
    title = "Impacto Acadêmico da Violência por Tipo e Identificação com Minoria",
    subtitle = "Comparando a proporção de estudantes com desempenho afetado",
    x = "Proporção de Respondentes",
    y = "Tipo de Violência",
    fill = "Impacto no Desempenho"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10)
  )


top_5_minorias <- dados_limpos %>%
  filter(!x4_voce_se_identifica_com_alguma_minoria_social %in% c("Não", "Prefiro não responder")) %>%
  count(x4_voce_se_identifica_com_alguma_minoria_social, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(x4_voce_se_identifica_com_alguma_minoria_social)

dados_radar_final <- dados_limpos %>%
  filter(x4_voce_se_identifica_com_alguma_minoria_social %in% top_5_minorias) %>%
  separate_rows(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar, sep = ";") %>%
  mutate(
    violencia = str_trim(x6_quais_tipos_de_violencia_voce_ja_vivenciou_ou_viu_outra_pessoa_vivenciar),
    violencia = str_remove(violencia, "Violência ")
  ) %>%
  filter(violencia != "" & violencia != "Prefiro não informar") %>%
  count(group = x4_voce_se_identifica_com_alguma_minoria_social, violencia) %>%
  pivot_wider(names_from = violencia, values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0)))

ggradar(
  dados_radar_final,
  grid.min = 0,
  grid.mid = 15,
  grid.max = 30,
  group.line.width = 1,
  group.point.size = 3,
  legend.title = "Top 5 Grupos de Minoria",
  legend.position = "bottom"
) +
  labs(
    title = "Perfil de Violência Sofrida pelas Top 5 Minorias"
  ) +
  theme(
    axis.text.x = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm')
  )



top_5_minorias <- dados_limpos %>%
  filter(!x4_voce_se_identifica_com_alguma_minoria_social %in% c("Não", "Prefiro não responder")) %>%
  count(x4_voce_se_identifica_com_alguma_minoria_social, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(x4_voce_se_identifica_com_alguma_minoria_social)


impacto_por_minoria <- dados_limpos %>%
  filter(x4_voce_se_identifica_com_alguma_minoria_social %in% top_5_minorias) %>%
  
  mutate(impacto_academico = case_when(
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Concordo totalmente", "Concordo parcialmente") ~ "Afetado",
    x15_voce_ja_teve_seu_desempenho_academico_afetado_por_uma_situacao_que_voce_se_sentiu_violentada %in% c("Discordo totalmente", "Discordo parcialmente") ~ "Não Afetado",
    TRUE ~ "Outro"
  )) %>%
  filter(impacto_academico != "Outro") %>%
  
  count(group = x4_voce_se_identifica_com_alguma_minoria_social, impacto_academico) %>%
  
  group_by(group) %>%
  mutate(porcentagem = n / sum(n)) %>%
  
  filter(impacto_academico == "Afetado")


ggplot(impacto_por_minoria, aes(x = porcentagem, y = reorder(group, porcentagem))) +
  geom_col(fill = "#d9480f") +
  geom_text(aes(label = scales::percent(porcentagem, accuracy = 1)), hjust = -0.2) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.6), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Impacto no Desempenho Acadêmico por Grupo de Minoria",
    subtitle = "Proporção de estudantes que relataram ter o desempenho afetado",
    x = "% com Desempenho Afetado",
    y = "Grupo de Minoria"
  ) +
  theme_minimal()



