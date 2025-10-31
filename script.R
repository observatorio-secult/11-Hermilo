library(openxlsx)
library(tidyr)
library(ggplot2)
library(here)
library(dplyr)
library(lubridate)
library(usethis)

setwd(here("dados/"))

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 12),
                  legend.position = "top",
                  panel.grid = element_blank(),
                  axis.title = element_text(face = "bold")))


a <- read.xlsx("Hermilo Inscrições.xlsx")

banco <- a %>%
  select(1, 4, 17,19, 25, 35, 36, 37, 38, 39, 40, 41, 42, 43,
         44, 45, 46, 47, 48, 49) %>% 
  rename(
    id = "Número.da.inscrição",
    status = "Status",
    cpf = "1.5.Número.do.CPF.-",
    nascimento = "1.2.Data.de.Nascimento.-",
    escolaridade = "3.8.Qual.seu.grau.de.escolaridade?-",
    macrorreg = "2.2.Em.qual.macrorregião.do.estado.de.Pernambuco.você.reside?.-",
    atuacao = "3.1.Qual.seu.tempo.de.atuação.na.área.artístico-cultural?.-",
    genero = "3.3.Qual.seu.gênero?.-",
    lgbt = "3.4.O.participante.é.membro.da.comunidade.LGBTQIAPN+?.-",
    raca = "3.5.Qual.sua.identidade.étnico-racial?.-",
    comunidade = "3.6.Você.pertence.a.alguma.comunidade.listada.abaixo?.-",
    pcd = "3.7.O/A.proponente.ou.representante.legal.é.uma.Pessoa.com.Deficiência.-.PcD?.-",
    pcd_tipo = "3.7.1.Se.Sim,.qual.o.tipo.de.deficiência?.-",
    renda = "3.10.Qual.sua.renda.individual?.-",
    rec_5 = "3.12.Você.acessou.recursos.públicos.do.fomento.à.cultura.nos.últimos.5.anos?.-",
    edicao = "3.11.Você.participou.em.alguma.edição.anterior.do.prêmio?.-",
    edicao_cont = "3.11.1.Se.contemplado,.qual(is).edição(ções)?.-",
    linguagem = "3.2.Função/Profissão.Cultural.na.área.de.Literatura.-",
    linguagem_outra = "3.2.1.Se.outra.função,.qual?.-"
  )

banco <- banco %>% filter(status != "Rascunho") %>% mutate(idade = round(time_length(difftime(today(),nascimento), unit = "years"),0)) %>%
  distinct(cpf, .keep_all = TRUE) %>% filter(idade >= 18) %>% 
  mutate(atuacao = gsub(" anos| ano", "", atuacao))

banco <- banco %>% mutate(atuacao = case_when(
  atuacao == "Menos de 1" ~ "0",
  TRUE ~ atuacao
)) %>% mutate(atuacao = as.numeric(atuacao))


idade_plot <- ggplot(banco, aes(idade)) +
  geom_histogram(breaks = c(seq(from = 18, to = 81, by = 3)), color = "black", fill = "#90BF97") + 
  scale_x_continuous(breaks = c(seq(from = 18, to = 81, by = 3))) +
  labs(x = "Idade",
       y = "Intervalos de Inscritos")

idade_plot

ggsave("graf idade.png", width = 8.38, height = 4.48)

atuacao_plot <- ggplot(banco, aes(atuacao)) +
  geom_histogram(breaks = c(seq(from = 0, to = 52, by = 2)), color = "black", fill = "#5E92BC") + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 52, by = 2))) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5)) +
  labs(x = "Tempo de Atuação",
       y = "Intervalos de Inscritos")

atuacao_plot

ggsave("graf atuacao.png", width = 8.38, height = 4.48)

library(scales)

macrorreg <- banco %>% group_by(macrorreg) %>% summarise(qtd = n()) %>% 
  mutate(prop = percent(qtd/sum(qtd))) %>% mutate(
    macrorreg = case_when(
      macrorreg == "Região Metropolitana do Recife (RMR)"~ "RMR",
TRUE ~ macrorreg)
  )

macrorreg_plot <- ggplot(macrorreg, aes(reorder(x = macrorreg, -qtd), y = qtd)) +
  geom_bar(stat = "identity", width = .6, fill = "#5e92bc", color = "black") +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, vjust = -.3) +
  labs(x = "",
       y = "Qtd. Inscritos") +
  scale_y_continuous(expand = expansion(mult = c(0, .2)))

macrorreg_plot

ggsave("graf macrorreg.png", width = 8.38, height = 4.48)

escolaridade <- banco %>% group_by(escolaridade) %>% summarise(qtd = n()) %>% 
  mutate(prop = percent(qtd/sum(qtd)))


escolaridade_plot <- ggplot(escolaridade, aes(reorder(x = escolaridade, qtd), y = qtd)) +
  geom_bar(stat = "identity", width = .6, fill = "#bfbcd7", color = "black") +
  geom_text(aes(label = paste0(qtd, " (", prop, ")")),
            size = 3, hjust = -.05) +
  labs(x = "",
       y = "Qtd. Inscritos") +
  coord_flip()+
  scale_y_continuous(expand = expansion(mult = c(0, .2)))

escolaridade_plot

ggsave("graf escolaridade.png", width = 8.38, height = 4.48)


funcao <- banco %>% group_by(linguagem) %>% summarise(qtd = n()) %>% 
  mutate(prop = percent(qtd/sum(qtd))) %>% arrange(desc(qtd))



