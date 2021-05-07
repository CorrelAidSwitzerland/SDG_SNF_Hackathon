require(tidyverse)
require(detectSDG)
source("2_code/helpers.R")

# NOTE
# .csv are ignored because of github size limitations
# download files from http://p3.snf.ch/P3Export/P3_GrantExport_with_abstracts.csv (only place in 1_data)
# install detect from https://github.com/dwulff/detectSDG

# HANDLE PROJECTS ---------

projects = read_csv2('1_data/P3_GrantExport_with_abstracts.csv') %>%
  mutate(id = 1:n()) %>%
  filter(!is.na(Abstract))

# is_eng = is_english(projects$Abstract)
# saveRDS(tibble(id = projects$id, is_eng = is_eng), "1_data/eng_abstracts.RDS")

projects = projects %>% 
  left_join(readRDS('1_data/eng_abstracts.RDS'))

projects_eng = projects %>% filter(is_eng)

# corpus = detectSDG::make_corpus(projects_eng$`Project Title`)
# sdg_title = detectSDG::detect_elsevier(corpus)
# saveRDS(sdg_title, "1_data/sdg_title.RDS")
# corpus = detectSDG::make_corpus(projects_eng$Abstract)
# sdg_abstract = detectSDG::detect_elsevier(corpus)
# saveRDS(sdg_abstract, "1_data/sdg_abstract.RDS")
# keys = sapply(str_extract_all(projects_eng$Keywords, "[:alpha:]+"), function(x) paste0(x,collapse=' '))
# corpus = detectSDG::make_corpus(keys)
# sdg_keywords = detectSDG::detect_elsevier(corpus)
# saveRDS(sdg_keywords, "1_data/sdg_keys.RDS")

sdgs = as_tibble(readRDS("1_data/sdg_title.RDS")) %>% 
  bind_rows(as_tibble(readRDS("1_data/sdg_abstract.RDS"))) %>% 
  bind_rows(as_tibble(readRDS("1_data/sdg_keys.RDS"))) %>% 
  select(doc_id, code, feature) %>% 
  unique()

sdgs_uni = sdgs %>% select(1:2) %>% unique()

sdg_props = sdgs_uni %>% 
  count(code) %>% 
  mutate(no = str_extract(code, '[:digit:]+'),
         name = names(sdg_names)[as.numeric(no)],
         prop = n / sum(n)) %>% 
  select(name, prop) %>% 
  arrange(name) %>% 
  mutate(sdg_nam = as_factor(sdg_names[name]))
  
# HANDLE PUBLICATIONS ---------

publications = read_csv2('1_data/P3_PublicationExport.csv') %>%
  mutate(id = 1:n()) %>%
  filter(!is.na(Abstract))

# is_eng = is_english(publications$Abstract)
# saveRDS(tibble(id = publications$id, is_eng = is_eng), "1_data/eng_abstracts_pub.RDS")

publications = publications %>% 
  left_join(readRDS('1_data/eng_abstracts_pub.RDS'))

publications_eng = publications %>% filter(is_eng)

# corpus = detectSDG::make_corpus(publications_eng$`Title of Publication`)
# sdg_title = detectSDG::detect_elsevier(corpus)
# saveRDS(sdg_title, "1_data/sdg_title_pub.RDS")
# corpus = detectSDG::make_corpus(publications_eng$Abstract)
# sdg_abstract = detectSDG::detect_elsevier(corpus)
# saveRDS(sdg_abstract, "1_data/sdg_abstract_pub.RDS")

sdgs_pub = as_tibble(readRDS("1_data/sdg_title_pub.RDS")) %>% 
  bind_rows(as_tibble(readRDS("1_data/sdg_abstract_pub.RDS"))) %>% 
  select(doc_id, code, feature) %>% 
  unique()

sdgs_pub_uni = sdgs_pub %>% select(1:2) %>% unique()

sdg_pub_props = sdgs_pub_uni %>% 
  count(code) %>% 
  mutate(no = str_extract(code, '[:digit:]+'),
         name = names(sdg_names)[as.numeric(no)],
         prop = n / sum(n)) %>% 
  select(name, prop) %>% 
  arrange(name) %>% 
  mutate(sdg_nam = as_factor(sdg_names[name]))


# COMBINE ---------

sgds_props_combined = sdg_props %>% mutate(Item="projects") %>% 
  bind_rows(sdg_pub_props %>% mutate(Item="publications"))

p = ggplot(sgds_props_combined, aes(x = sdg_nam, y = prop)) + 
  geom_bar(stat = 'identity', position = "dodge", mapping = aes(fill = Item)) +
  scale_fill_manual(values = unibas_cols[c(5,1)]) +
  geom_segment(data = elsevier %>% mutate(x = (1:16)-.45, xend = (1:16)+.45), 
               mapping = aes(x = x, xend=xend, y = prop, yend=prop), col = unibas_cols[3], size = .65) +
  theme_minimal() + 
  theme(legend.position = c(.89, 1.07),legend.direction = "horizontal",legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = "", y = "Relative frequency", title = "SDG-matches in SNF research projects",
       subtitle = "Based on 26,811 projects and 69,326 publications in P3 database",
       caption = "Note: Red line shows relative frequency of SDG-matches in the global academic output (Elsevier, 2020)") +
  geom_text(data = tibble(x = 3.6, y = 0.682),
            mapping = aes(x = x, y = y, label = "Elsevier (2020)"),
            col = unibas_cols[3], hjust = 0, size=3.4) + ylim(c(0,.8))

ggsave(filename = '3_figures/sdgs.png',dpi = 500, device = 'png',plot = p, width=10,height=6)
ggsave(filename = '3_figures/sdgs.pdf', device = 'pdf',plot = p)

a = read_csv2('1_data/P3_PublicationExport.csv')

mean(is.na(a$Abstract))

