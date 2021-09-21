library(readr)
library(ggplot2)
library(reshape2)
library(tidyr)

elections_df = readr::read_tsv("data/elections_meduza_final.tsv")
elections_fdf = elections_df %>%
  dplyr::mutate(elections_candidate_short=gsub("([^ ]+).*", "\\1", elections_candidate)) %>%
  dplyr::group_by(elections_aik) %>%
  dplyr::arrange(dplyr::desc(elections_total_percent)) %>%
  dplyr::mutate(elections_aik_total_count=sum(elections_electronic_count) + sum(elections_inperson_count)) %>%
  dplyr::mutate(elections_aik_electronic_count=sum(elections_electronic_count)) %>%
  dplyr::mutate(elections_aik_inperson_count=sum(elections_inperson_count)) %>%
  dplyr::slice(1:2) %>%
  dplyr::mutate(elections_aik_subtotal_count=sum(elections_electronic_count) + sum(elections_inperson_count)) %>%
  dplyr::group_by(elections_aik, elections_aik_total_count, elections_aik_subtotal_count, elections_aik_electronic_count, elections_aik_inperson_count) %>%
  dplyr::summarize(
    elections_aik_desc=paste0(elections_aik[1], ": ", elections_candidate_short[1], " / ", elections_candidate_short[2]),
    elections_electronic_prcdiff=elections_electronic_percent[1]-elections_electronic_percent[2],
    elections_inperson_prcdiff=elections_inperson_percent[1]-elections_inperson_percent[2],
    elections_electronic_percent1=elections_electronic_percent[1],
    elections_electronic_percent2=elections_electronic_percent[2],
    elections_electronic_count1=elections_electronic_count[1],
    elections_electronic_count2=elections_electronic_count[2],
    elections_inperson_percent1=elections_inperson_percent[1],
    elections_inperson_percent2=elections_inperson_percent[2],
    elections_inperson_count1=elections_inperson_count[1],
    elections_inperson_count2=elections_inperson_count[2],
    elections_electronic_diff=elections_electronic_count[1]-elections_electronic_count[2],
    elections_inperson_diff=elections_inperson_count[1]-elections_inperson_count[2],
  ) %>%
  dplyr::mutate(elections_decided_by=dplyr::case_when(
    elections_inperson_diff>=0 & elections_electronic_diff>=0 ~ "Оба",
    elections_inperson_diff>=0 & elections_electronic_diff<=0 ~ "На участках",
    elections_electronic_diff>=0 & elections_inperson_diff<=0 ~ "Електронно",
    T<=0 ~ "What?",
  ))

f = lm(elections_aik_electronic_count ~ elections_electronic_count1, data=elections_fdf)
predict(f, data.frame(elections_electronic_count1=0))
x = elections_fdf %>% dplyr::filter(elections_decided_by== "Електронно")
sum(x$elections_inperson_count2) - sum(x$elections_inperson_count1)


ggplot(elections_fdf, aes(x=elections_aik_electronic_count, y=elections_electronic_count1)) +
  geom_point(aes(color=elections_decided_by, size=elections_aik_electronic_count/elections_aik_inperson_count)) +
  geom_smooth(method="lm") +
  geom_text(aes(x))
  ggrepel::geom_text_repel(aes(label=elections_aik_desc)) +
  labs(x="Количество голосов поданных электронно", y="Количество электронных голосов у кандидата")


ggplot(elections_fdf, aes(x=elections_electronic_prcdiff, y=elections_inperson_prcdiff)) +
  geom_point(aes(color=elections_decided_by, size=elections_aik_electronic_count/elections_aik_inperson_count)) +
  ggrepel::geom_text_repel(aes(label=elections_aik_desc)) +
  scale_size_continuous(range=c(2, 10)) +
  labs(x="Приимущество попедившего кандидата в\nэлектронном голосовании\n(процентные пункты)", y="Приимущество победившего кандидата в\nголосовании на участках\n(процентные пункты)", color="Перевес засчет...", size="Отношение явки\nэлектронно/на участках") +
  coord_fixed(xlim=c(10, 40), ylim=c(-10, 20)) +
  theme_gray(base_size=18)

ggplot(elections_fdf %>% reshape2::melt(id.vars=c("elections_aik", "elections_aik_desc"), measure.vars=c("elections_aik_electronic_count", "elections_aik_inperson_count")) %>% dplyr::mutate(variable=ifelse(grepl("inperson", variable), "На участке", "Электронно"))) +
  geom_bar(aes(x=factor(elections_aik), y=value, fill=variable), stat="identity", alpha=0.5, position=position_nudge()) +
  labs(x="Изберательный округ", y="Количество проголосовавших", fill="Способ голосования") +
  theme_classic(base_size=24)

  # Если предположить что явка на электронных участках
  ggplot(elections_fdf) +
    geom_density(aes(x=elections_aik_inperson_count/elections_aik_electronic_count))
ggplot(elections_fdf, aes(x=elections_electronic_prcdiff, y=elections_inperson_prcdiff)) +
  geom_point(aes(color=elections_decided_by), size=5) +
  ggrepel::geom_text_repel(aes(label=elections_aik_desc)) +
  labs(x="Приимущество попедившего кандидата в\nэлектронном голосовании\n(процентные пункты)", y="Приимущество попедившего кандидата в\nголосовании на участках\n(процентные пункты)", color="Перевес засчет...") +
  theme_classic(base_size=24)





ggplot(elections_fdf, aes(x=elections_electronic_diff/elections_aik_subtotal_count, y=elections_inperson_diff/elections_aik_subtotal_count)) +
  geom_point(aes(color=elections_decided_by, size=elections_inperson_prcdiff)) +
  ggrepel::geom_text_repel(aes(label=elections_aik_desc, color=elections_decided_by))


ggplot(elections_fdf, aes(x=elections_inperson_percent1, y=elections_electronic_percent2)) +
  geom_point(aes(color=elections_decided_by, size=elections_inperson_prcdiff)) +
  ggrepel::geom_text_repel(aes(label=elections_aik_desc, color=elections_decided_by)) +
  coord_fixed(xlim=c(15,43), ylim=c(15,43))
