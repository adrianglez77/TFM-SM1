library(shiny)
library(shinydashboard)
library(dplyr)
library(bsicons)
library(DT)
library(tidyverse)
library(magrittr)
library(shiny)
library(openxlsx)
library(dplyr)
library(datavolley)
library(DataEditR)
library(ovscout2)
library(datavolleyXtra)
library(ggplot2)
library(httr)
library(ggimage)
library(gt)
library(gtExtras)
library(purrr)
library(tibble)
library(kableExtra)
library(knitr)
library(gt)
library(webshot)
library(htmltools)
library(gridExtra)
library(grid)
library(png)
library(curl)
library(stringr)
library(ggrepel)
library(shinyjs)
library(shinythemes)

ruta_archivo <- "dvws"

if (file.exists(ruta_archivo)) {
  df <- dv_readXtra(ruta_archivo)
} else {
  print("El archivo no existe en la ruta especificada.")
}


df <- df %>%
  mutate_all(tolower) %>%  # Todo a minúsculas
  mutate(
    team = gsub("-", " ", team),
    team = gsub("\\s+", " ", team),
    team = case_when(
      team == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      team == "melilla sport capital" ~ "cv melilla",
      team == "arenal emevac" ~ "arenal emevé",
      team == "club va3ley palma" ~ "club vóley palma",
      team == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ team
    ),
    point_won_by = case_when(
      point_won_by == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      point_won_by == "melilla sport capital" ~ "cv melilla",
      point_won_by == "arenal emevac" ~ "arenal emevé",
      point_won_by == "club va3ley palma" ~ "club vóley palma",
      point_won_by == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ point_won_by
    ),
    serving_team = case_when(
      serving_team == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      serving_team == "melilla sport capital" ~ "cv melilla",
      serving_team == "arenal emevac" ~ "arenal emevé",
      serving_team == "club va3ley palma" ~ "club vóley palma",
      serving_team == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ serving_team
    ),
    home_team = case_when(
      home_team == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      home_team == "melilla sport capital" ~ "cv melilla",
      home_team == "arenal emevac" ~ "arenal emevé",
      home_team == "club va3ley palma" ~ "club vóley palma",
      home_team == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ home_team
    ),
    visiting_team = case_when(
      visiting_team == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      visiting_team == "melilla sport capital" ~ "cv melilla",
      visiting_team == "arenal emevac" ~ "arenal emevé",
      visiting_team == "club va3ley palma" ~ "club vóley palma",
      visiting_team == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ visiting_team
    ),
    match_won_by = case_when(
      match_won_by == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      match_won_by == "melilla sport capital" ~ "cv melilla",
      match_won_by == "arenal emevac" ~ "arenal emevé",
      match_won_by == "club va3ley palma" ~ "club vóley palma",
      match_won_by == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ match_won_by
    ),
    set_won_by = case_when(
      set_won_by == "upv leleman conqueridor" ~ "leleman conqueridor upv",
      set_won_by == "melilla sport capital" ~ "cv melilla",
      set_won_by == "arenal emevac" ~ "arenal emevé",
      set_won_by == "club va3ley palma" ~ "club vóley palma",
      set_won_by == "unicaja costa de almera-a" ~ "unicaja costa de almería",
      TRUE ~ set_won_by
    ),
  )

teams_info <- df %>%
  filter(!is.na(team)) %>%
  group_by(team) %>%
  summarise(
    sets_played = n_distinct(paste(match_id, set_number)),
    matches_played = n_distinct(match_id),
    matches_won = sum(match_won_by_id == team_id & !duplicated(match_id), na.rm = TRUE),
    sets_won = sum(set_won_by_id == team_id & !duplicated(paste(match_id, set_number)), na.rm = TRUE)
  ) %>%
  mutate(
    matches_lost = matches_played - matches_won,
    sets_lost = sets_played - sets_won
  ) %>%
  arrange(desc(matches_won))

logo_df <- data.frame(
  team = c("cv san roque", "club vóley palma", "leleman conqueridor upv", 
           "cisneros alter", "grupo herce soria", "arenal emevé", 
           "pamesa teruel voleibol", "conectabalear cv manacor", 
           "cv guaguas", "volei villena petrer", "cv melilla", 
           "unicaja costa de almería"),
  logo = c(
    "http://intranet.rfevb.com/clubes/logos/web/cl01054.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl00300.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl01313.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl01140.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl01268.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl00734.jpg",
    "http://intranet.rfevb.com/clubes/logos/web/cl01279.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl00236.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl01946.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl00125.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl01445.png",
    "http://intranet.rfevb.com/clubes/logos/web/cl00135.png"
  )
)

# Realizar la unión con teams_info
teams_info <- left_join(teams_info, logo_df, by = "team")



mejores_lib <- mejores_lib %>%
  arrange(desc(Rec_AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played, 
    'Nº Recepciones' = Rec_Tot,
    'Rec AVG' = Rec_AVG,
    '%Eff flot' = `% Rec Eficiencia_flot`,
    'AVG flot' = Rec_AVG_flot,
    '%Eff salto' = `% Rec Eficiencia_jump`,
    'AVG salto' = Rec_AVG_jump)


mejores_oh <- mejores_oh %>%
  arrange(desc(AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Valoración' = AVG,
    'Nº Ataque' = Ataque_Tot,
    '%Eficacia' = Ataque_Eficacia,
    '%Eficiencia' = Ataque_Eficiencia,
    'Nº Recepciones' = Rec_Tot,
    'Rec AVG' = Rec_AVG,
    'Nº Saques' = Saque_Tot,
    'Saque AVG' = Saque_AVG,
    'Nº Bloqueos' = Bloqueo_Punto,
    'Bloq/set' = Bloqueos_set)

mejores_opp <- mejores_opp %>%
  arrange(desc(AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Valoración' = AVG,
    'Nº Ataque' = Ataque_Tot,
    '%Eficacia' = Ataque_Eficacia,
    '%Eficiencia' = Ataque_Eficiencia,
    'Nº Saques' = Saque_Tot,
    'Saque AVG' = Saque_AVG,
    'Nº Bloqueos' = Bloqueo_Punto,
    'Bloq/set' = Bloqueos_set)

mejores_mb <- mejores_mb %>%
  arrange(desc(AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Valoración' = AVG,
    'Nº Ataque' = Ataque_Tot,
    '%Eficacia' = Ataque_Eficacia,
    '%Eficiencia' = Ataque_Eficiencia,
    'Nº Saques' = Saque_Tot,
    'Saque AVG' = Saque_AVG,
    'Nº Bloqueos' = Bloqueo_Punto,
    'Bloq/set' = Bloqueos_set)


top_sacadores <- top_sacadores %>%
  arrange(desc(Saque_AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Saque' = Saque_Tot,
    'Saque AVG' = Saque_AVG,
    
    '% #' = '% Ace',
    '% /' = '% Barra',
    '% =' = '% Error',
  )

top_sacadores_flot <- top_sacadores_flot %>%
  arrange(desc(Saque_AVG_flot))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Saque' = Saque_Tot_flot,
    'Saque AVG' = Saque_AVG_flot,
    
    '% #' = '% ACE_flot',
    '% /' = '% Barra_flot',
    '% =' = '% Error_flot',
  )

top_sacadores_jump <- top_sacadores_jump %>%
  arrange(desc(Saque_AVG_jump))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Saque' = Saque_Tot_jump,
    'Saque AVG' = Saque_AVG_jump,
    
    '% #' = '% ACE_jump',
    '% /' = '% Barra_jump',
    '% =' = '% Error_jump',
  )

top_receptores <- top_receptores %>%
  arrange(desc(Rec_AVG))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Recepciones' = Rec_Tot,
    'Rec AVG' = Rec_AVG,
    
    '% Eficacia' = '% Rec Eficacia',
    '% Eficiencia' = '% Rec Eficiencia',
    '% #' = '% Rec Perf',
    '% +' = '% Rec Pos',
    '% /' = '% Rec Barra',
    '% =' = '% Rec Error',
  )

top_receptores_flot <- top_receptores_flot %>%
  arrange(desc(Rec_AVG_flot))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Recepciones' = Rec_Tot_flot,
    'Rec AVG' = Rec_AVG_flot,
    
    '% Eficacia' = '% Rec Eficacia_flot',
    '% Eficiencia' = '% Rec Eficiencia_flot',
    '% #' = '% Rec Perf_flot',
    '% +' = '% Rec Pos_flot',
    '% /' = '% Rec Barra_flot',
    '% =' = '% Rec Error_flot',
  )

top_receptores_jump <- top_receptores_jump %>%
  arrange(desc(Rec_AVG_jump))%>%
  mutate(Rank = row_number())%>% 
  select(
    Rank = Rank,
    Jugador = player_name, 
    Sets = sets_played,
    'Nº Recepciones' = Rec_Tot_jump,
    'Rec AVG' = Rec_AVG_jump,
    
    '% Eficacia' = '% Rec Eficacia_jump',
    '% Eficiencia' = '% Rec Eficiencia_jump',
    '% #' = '% Rec Perf_jump',
    '% +' = '% Rec Pos_jump',
    '% /' = '% Rec Barra_jump',
    '% =' = '% Rec Error_jump',
  )


top_bloqueadores <- players_info %>%
  filter(sets_played > 10)%>%
  mutate(
    Bloqueos_set = round(ifelse(Bloqueo_Punto > 0, round(Bloqueo_Punto / sets_played, 4), 0),2))%>% 
  select(
    Jugador = player_name, 
    Sets = sets_played,
    "Bloqueos #" = Bloqueo_Punto,
    "Bloqueos/set" = Bloqueos_set
  )

top_bloqueadores <- top_bloqueadores %>%
  arrange(desc(Bloqueos/set)) %>%
  head(15) %>%
  mutate(Rank = row_number())%>%
  select(
    Rank = Rank,
    Jugador,
    Sets,
    "Bloqueos #",
    "Bloqueos/set"
  )

teams_info <- teams_info %>%
  left_join(df_filtrado %>% filter(skill == "attack") %>%
              group_by(team) %>%
              summarise(
                Ataque_Tot = n(),
                Ataque_Perf = sum(evaluation_code == "#"),
                Ataque_Pos = sum(evaluation_code == "+"),
                Ataque_Exc = sum(evaluation_code == "!"),
                Ataque_Neg = sum(evaluation_code == "-"),
                Ataque_Barra = sum(evaluation_code == "/"),
                Ataque_Error = sum(evaluation_code == "=")
                
              ),
            by = "team")%>%
  mutate(
    `% Ataque Perf` = round((Ataque_Perf / Ataque_Tot) * 100, 2),
    `% Ataque Barra` = round((Ataque_Barra / Ataque_Tot) * 100, 2),
    `% Ataque Error` = round((Ataque_Error / Ataque_Tot) * 100, 2),
    `% Ataque Eficacia` = ifelse(Ataque_Tot > 0, round(Ataque_Perf  / Ataque_Tot * 100, 2), 0),
    `% Ataque Eficiencia` = ifelse(Ataque_Tot > 0, round((Ataque_Perf - (Ataque_Error + Ataque_Barra)) / Ataque_Tot, 2), 0)
  )


teams_info <- teams_info %>%
  left_join(df_filtrado %>% filter(skill == "attack" & attack_code %in% c("v5", "v3", "v6", "v8", "vp")) %>%
              group_by(team) %>%
              summarise(
                Ataque_Tot_high = n(),
                Ataque_Perf_high = sum(evaluation_code == "#"),
                Ataque_Barra_high = sum(evaluation_code == "/"),
                Ataque_Error_high = sum(evaluation_code == "=")
                
              ),
            by = "team")%>%
  mutate(
    `% Ataque Perf_high` = round((Ataque_Perf_high / Ataque_Tot_high) * 100, 2),
    `% Ataque Barra_high` = round((Ataque_Barra_high / Ataque_Tot_high) * 100, 2),
    `% Ataque Error_high` = round((Ataque_Error_high / Ataque_Tot_high) * 100, 2),
    `% Ataque Eficacia_high` = ifelse(Ataque_Tot_high > 0, round(Ataque_Perf_high  / Ataque_Tot_high * 100, 2), 0),
    `% Ataque Eficiencia_high` = ifelse(Ataque_Tot_high > 0, round((Ataque_Perf_high - (Ataque_Error_high + Ataque_Barra_high)) / Ataque_Tot_high, 2), 0)
  )


teams_info <- teams_info %>%
  left_join(df_filtrado %>% filter(skill == "attack" & attack_code %in% c("x5" , "x6", "x4", "x9", "x8", "xb", "xp", "c5", "c6", "c8")) %>%
              group_by(team) %>%
              summarise(
                Ataque_Tot_fast = n(),
                Ataque_Perf_fast = sum(evaluation_code == "#"),
                Ataque_Barra_fast = sum(evaluation_code == "/"),
                Ataque_Error_fast = sum(evaluation_code == "=")
                
              ),
            by = "team")%>%
  mutate(
    `% Ataque Perf_fast` = round((Ataque_Perf_fast / Ataque_Tot_fast) * 100, 2),
    `% Ataque Barra_fast` = round((Ataque_Barra_fast / Ataque_Tot_fast) * 100, 2),
    `% Ataque Error_fast` = round((Ataque_Error_fast / Ataque_Tot_fast) * 100, 2),
    `% Ataque Eficacia_fast` = ifelse(Ataque_Tot_fast > 0, round(Ataque_Perf_fast  / Ataque_Tot_fast * 100, 2), 0),
    `% Ataque Eficiencia_fast` = ifelse(Ataque_Tot_fast > 0, round((Ataque_Perf_fast - (Ataque_Error_fast + Ataque_Barra_fast)) / Ataque_Tot_fast, 2), 0)
  )

teams_info <- teams_info %>%
  left_join(df_filtrado %>% filter(skill == "attack" & attack_code %in% c("x1", "xc", "xm", "xd", "x2", "x7")) %>%
              group_by(team) %>%
              summarise(
                Ataque_Tot_3 = n(),
                Ataque_Perf_3 = sum(evaluation_code == "#"),
                Ataque_Pos_3 = sum(evaluation_code == "+"),
                Ataque_Exc_3 = sum(evaluation_code == "!"),
                Ataque_Neg_3 = sum(evaluation_code == "-"),
                Ataque_Barra_3 = sum(evaluation_code == "/"),
                Ataque_Error_3 = sum(evaluation_code == "=")
                
              ),
            by = "team")%>%
  mutate(
    `% Ataque Perf_3` = round((Ataque_Perf_3 / Ataque_Tot_3) * 100, 2),
    `% Ataque Barra_3` = round((Ataque_Barra_3 / Ataque_Tot_3) * 100, 2),
    `% Ataque Error_3` = round((Ataque_Error_3 / Ataque_Tot_3) * 100, 2),
    `% Ataque Eficacia_3` = ifelse(Ataque_Tot_3 > 0, round(Ataque_Perf_3  / Ataque_Tot_3 * 100, 2), 0),
    `% Ataque Eficiencia_3` = ifelse(Ataque_Tot_3 > 0, round((Ataque_Perf_3 - (Ataque_Error_3 + Ataque_Barra_3)) / Ataque_Tot_3, 2), 0)
  )


teams_info <- teams_info %>%
  left_join(df_filtrado %>% filter(skill == "attack" & attack_code %in% c("xp", "xb", "vp")) %>%
              group_by(team) %>%
              summarise(
                Ataque_Tot_pipe = n(),
                Ataque_Perf_pipe = sum(evaluation_code == "#"),
                Ataque_Barra_pipe = sum(evaluation_code == "/"),
                Ataque_Error_pipe = sum(evaluation_code == "=")
                
              ),
            by = "team")%>%
  mutate(
    `% Ataque Perf_pipe` = round((Ataque_Perf_pipe / Ataque_Tot_pipe) * 100, 2),
    `% Ataque Barra_pipe` = round((Ataque_Barra_pipe / Ataque_Tot_pipe) * 100, 2),
    `% Ataque Error_pipe` = round((Ataque_Error_pipe / Ataque_Tot_pipe) * 100, 2),
    `% Ataque Eficacia_pipe` = ifelse(Ataque_Tot_pipe > 0, round(Ataque_Perf_pipe  / Ataque_Tot_pipe * 100, 2), 0),
    `% Ataque Eficiencia_pipe` = ifelse(Ataque_Tot_pipe > 0, round((Ataque_Perf_pipe - (Ataque_Error_pipe + Ataque_Barra_pipe)) / Ataque_Tot_pipe, 2), 0)
  )


top_ataque_equipo <- teams_info %>%
  arrange(desc(`% Ataque Eficacia`)) %>%
               mutate(Rank = row_number()) %>% 
                 select(
                   Rank = Rank,
                   Equipo = team, 
                   Sets = sets_played,
                   'Nº Ataques' = Ataque_Tot,
                   '% #' = '% Ataque Eficacia',
                   '% Eficiencia' = '% Ataque Eficiencia',
                 )
               
top_ataque_high_equipo <- teams_info %>%
 arrange(desc(`% Ataque Eficacia_high`)) %>%
              mutate(Rank = row_number()) %>% 
                select(
                  Rank = Rank,
                  Equipo = team, 
                  Sets = sets_played,
                  'Nº Ataques' = Ataque_Tot_high,
                  '% #' = '% Ataque Eficacia_high',
                  '% Eficiencia' = '% Ataque Eficiencia_high',
                )
              
top_ataque_fast_equipo <- teams_info %>%
arrange(desc(`% Ataque Eficacia_fast`))%>%
mutate(Rank = row_number())%>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Ataques' = Ataque_Tot_fast,
   '% #' = '% Ataque Eficacia_fast',
   '% Eficiencia' = '% Ataque Eficiencia_fast',
 )

top_ataque_quick_equipo <- teams_info %>%
 arrange(desc(`% Ataque Eficacia_3`))%>%
              mutate(Rank = row_number())%>% 
                select(
                  Rank = Rank,
                  Equipo = team, 
                  Sets = sets_played,
                  'Nº Ataques' = Ataque_Tot_3,
                  '% #' = '% Ataque Eficacia_3',
                  '% Eficiencia' = '% Ataque Eficiencia_3',
                )
              
top_ataque_pipe_equipo <- teams_info %>%
  arrange(desc(`% Ataque Eficacia_pipe`))%>%
               mutate(Rank = row_number())%>% 
                 select(
                   Rank = Rank,
                   Equipo = team, 
                   Sets = sets_played,
                   'Nº Ataques' = Ataque_Tot_pipe,
                   '% #' = '% Ataque Eficacia_pipe',
                   '% Eficiencia' = '% Ataque Eficiencia_pipe',
                 ) 
               
               
               
top_saque_equipo <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "serve") %>%
             group_by(team) %>%
             summarise(
               Saque_Tot = n(),
               Saque_Punto = sum(evaluation_code == "#"),
               Saque_Error = sum(evaluation_code == "="),
               Saque_Pos = sum(evaluation_code == "+"),
               Saque_Neg = sum(evaluation_code == "-"),
               Saque_Exc = sum(evaluation_code == "!"),
               Saque_Barra = sum(evaluation_code == "/")
             ),
           by = "team")%>%
 mutate(
   `% Error` = round((Saque_Error / Saque_Tot) * 100, 2),
    `% Barra` = round((Saque_Barra / Saque_Tot) * 100, 2),
    `% Ace` = round((Saque_Punto / Saque_Tot) * 100, 2),
    Saque_AVG = ifelse(Saque_Tot > 0, round((Saque_Punto * 5 + Saque_Barra * 3 + Saque_Pos * 2 + Saque_Exc * 1  +
                                               Saque_Neg * -1 + Saque_Error * -2) / Saque_Tot, 2), 0)
 )

top_saque_equipo <- top_saque_equipo %>%
 arrange(desc(Saque_AVG))%>%
 mutate(Rank = row_number())%>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Saques' = Saque_Tot,
   'Saque AVG' = Saque_AVG,
   '% #' = '% Ace',
   '% /' = '% Barra',
   '% =' = '% Error'
 )  

top_saque_flot_equipo <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "serve" & skill_type == "jump-float serve") %>%
             group_by(team) %>%
             summarise(
               Saque_Tot_flot = n(),
               Saque_Punto_flot = sum(evaluation_code == "#"),
               Saque_Error_flot = sum(evaluation_code == "="),
               Saque_Pos_flot = sum(evaluation_code == "+"),
               Saque_Neg_flot = sum(evaluation_code == "-"),
               Saque_Exc_flot = sum(evaluation_code == "!"),
               Saque_Barra_flot = sum(evaluation_code == "/")
             ),
           by = "team")%>%
 mutate(
   `% Saque Error_flot` = round((Saque_Error_flot / Saque_Tot_flot) * 100, 2),
    `% Saque ACE_flot` = round((Saque_Punto_flot / Saque_Tot_flot) * 100, 2),
    `% Saque Barra_flot` = round((Saque_Barra_flot / Saque_Tot_flot) * 100, 2),
    Saque_AVG_flot = ifelse(Saque_Tot_flot > 0, round((Saque_Punto_flot * 5 + Saque_Barra_flot * 3 + Saque_Pos_flot *                           2 + Saque_Exc_flot * 1  + Saque_Neg_flot * -1 + Saque_Error_flot * -2) / Saque_Tot_flot, 2), 0)
 )

top_saque_flot_equipo <- top_saque_flot_equipo %>%
 arrange(desc(Saque_AVG_flot))%>%
 mutate(Rank = row_number())%>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Saques' = Saque_Tot_flot,
   'Saque AVG' = Saque_AVG_flot,
   '% #' = '% Saque ACE_flot',
   '% /' = '% Saque Barra_flot',
   '% =' = '% Saque Error_flot'
 )  


top_saque_jump_equipo <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "serve" & skill_type == "jump serve") %>%
             group_by(team) %>%
             summarise(
               Saque_Tot_jump = n(),
               Saque_Punto_jump = sum(evaluation_code == "#"),
               Saque_Error_jump = sum(evaluation_code == "="),
               Saque_Pos_jump = sum(evaluation_code == "+"),
               Saque_Neg_jump = sum(evaluation_code == "-"),
               Saque_Exc_jump = sum(evaluation_code == "!"),
               Saque_Barra_jump = sum(evaluation_code == "/")
             ),
           by = "team")%>%
 mutate(
   `% Saque Error_jump` = round((Saque_Error_jump / Saque_Tot_jump) * 100, 2),
    `% Saque ACE_jump` = round((Saque_Punto_jump / Saque_Tot_jump) * 100, 2),
    `% Saque Barra_jump` = round((Saque_Barra_jump / Saque_Tot_jump) * 100, 2),
    Saque_AVG_jump = ifelse(Saque_Tot_jump > 0, round((Saque_Punto_jump * 5 + Saque_Barra_jump * 3 + Saque_Pos_jump * 2 +
                                                         Saque_Exc_jump * 1  + Saque_Neg_jump * -1 + Saque_Error_jump * -2) /
                                                        Saque_Tot_jump, 2), 0)
 )

top_saque_jump_equipo <- top_saque_jump_equipo %>%
 arrange(desc(Saque_AVG_jump))%>%
 mutate(Rank = row_number())%>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Saques' = Saque_Tot_jump,
   'Saque AVG' = Saque_AVG_jump,
   '% #' = '% Saque ACE_jump',
   '% /' = '% Saque Barra_jump',
   '% =' = '% Saque Error_jump'
 )  


teams_info <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "reception") %>%
             group_by(team) %>%
             summarise(
               Rec_Tot = n(),
               Rec_Perf = sum(evaluation_code == "#"),
               Rec_Pos = sum(evaluation_code == "+"),
               Rec_Exc = sum(evaluation_code == "!"),
               Rec_Neg = sum(evaluation_code == "-"),
               Rec_Barra = sum(evaluation_code == "/"),
               Rec_Error = sum(evaluation_code == "=")
               
             ),
           by = "team")%>%
 mutate(
   `% Rec Barra` = round((Rec_Barra / Rec_Tot) * 100, 2),
    `% Rec Error` = round((Rec_Error / Rec_Tot) * 100, 2),
    `% Rec Eficacia` = ifelse(Rec_Tot > 0, round((Rec_Perf + Rec_Pos) / Rec_Tot * 100, 2), 0),
    `% Rec Eficiencia` = ifelse(Rec_Tot > 0, round((Rec_Perf + Rec_Pos - (Rec_Error + Rec_Barra)) / Rec_Tot, 2), 0),
    Rec_AVG = ifelse(Rec_Tot > 0, round((Rec_Perf * 5 + Rec_Pos * 3 + Rec_Exc * 2 + Rec_Neg * 1  + Rec_Barra *                          -1 + Rec_Error * -2) / Rec_Tot, 2), 0),
 )


teams_info <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "reception" & skill_type == "jump-float serve reception") %>%
             group_by(team) %>%
             summarise(
               Rec_Tot_flot = n(),
               Rec_Perf_flot = sum(evaluation_code == "#"),
               Rec_Pos_flot = sum(evaluation_code == "+"),
               Rec_Exc_flot = sum(evaluation_code == "!"),
               Rec_Neg_flot = sum(evaluation_code == "-"),
               Rec_Barra_flot = sum(evaluation_code == "/"),
               Rec_Error_flot = sum(evaluation_code == "=")
               
             ),
           by = "team")%>%
 mutate(
   `% Rec Barra_flot` = round((Rec_Barra_flot / Rec_Tot_flot) * 100, 2),
    `% Rec Error_flot` = round((Rec_Error_flot / Rec_Tot_flot) * 100, 2),
    `% Rec Eficacia_flot` = ifelse(Rec_Tot_flot > 0, round((Rec_Perf_flot + Rec_Pos_flot) / Rec_Tot_flot * 100, 2), 0),
    `% Rec Eficiencia_flot` = ifelse(Rec_Tot_flot > 0, round((Rec_Perf_flot + Rec_Pos_flot - (Rec_Error_flot + Rec_Barra_flot)) / Rec_Tot_flot, 2), 0),
    Rec_AVG_flot = ifelse(Rec_Tot_flot > 0, round((Rec_Perf_flot * 5 + Rec_Pos_flot * 3 + Rec_Exc_flot * 2 + Rec_Neg_flot * 1  + Rec_Barra_flot *                          -1 + Rec_Error_flot * -2) / Rec_Tot_flot, 2), 0),
 )

teams_info <- teams_info %>%
 left_join(df_filtrado %>% filter(skill == "reception" & skill_type == "jump serve reception") %>%
             group_by(team) %>%
             summarise(
               Rec_Tot_jump = n(),
               Rec_Perf_jump = sum(evaluation_code == "#"),
               Rec_Pos_jump = sum(evaluation_code == "+"),
               Rec_Exc_jump = sum(evaluation_code == "!"),
               Rec_Neg_jump = sum(evaluation_code == "-"),
               Rec_Barra_jump = sum(evaluation_code == "/"),
               Rec_Error_jump = sum(evaluation_code == "=")
               
             ),
           by = "team")%>%
 mutate(
   `% Rec Barra_jump` = round((Rec_Barra_jump / Rec_Tot_jump) * 100, 2),
    `% Rec Error_jump` = round((Rec_Error_jump / Rec_Tot_jump) * 100, 2),
    `% Rec Eficacia_jump` = ifelse(Rec_Tot_jump > 0, round((Rec_Perf_jump + Rec_Pos_jump) / Rec_Tot_jump * 100, 2), 0),
    `% Rec Eficiencia_jump` = ifelse(Rec_Tot_jump > 0, round((Rec_Perf_jump + Rec_Pos_jump - (Rec_Error_jump + Rec_Barra_jump)) / Rec_Tot_jump, 2), 0),
    Rec_AVG_jump = ifelse(Rec_Tot_jump > 0, round((Rec_Perf_jump * 5 + Rec_Pos_jump * 3 + Rec_Exc_jump * 2 + Rec_Neg_jump * 1  + Rec_Barra_jump *                          -1 + Rec_Error_jump * -2) / Rec_Tot_jump, 2), 0),
 )


top_rece_equipo <- teams_info %>%
 arrange(desc(Rec_AVG)) %>%
 mutate(Rank = row_number()) %>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Recepciones' = Rec_Tot,
   'Recepción AVG' = Rec_AVG,
   '% Eficacia (#+)' = '% Rec Eficacia',
   '% /' = '% Rec Barra',
   '% =' = '% Rec Error',
 )

top_rece_flot_equipo <- teams_info %>%
 arrange(desc(Rec_AVG_flot)) %>%
 mutate(Rank = row_number()) %>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Recepciones' = Rec_Tot_flot,
   'Recepción AVG' = Rec_AVG_flot,
   '% Eficacia (#+)' = '% Rec Eficacia_flot',
   '% /' = '% Rec Barra_flot',
   '% =' = '% Rec Error_flot',
 )

top_rece_jump_equipo <- teams_info %>%
 arrange(desc(Rec_AVG_jump)) %>%
 mutate(Rank = row_number()) %>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Recepciones' = Rec_Tot_jump,
   'Recepción AVG' = Rec_AVG_flot,
   '% Eficacia (#+)' = '% Rec Eficacia_jump',
   '% /' = '% Rec Barra_jump',
   '% =' = '% Rec Error_jump',
 )

teams_info <- teams_info %>%
 left_join(
   df %>%
     filter( skill == "block") %>% 
     group_by(team) %>%                         
     summarise(
       Block_Tot = n(),
       Block_Punto = sum(evaluation_code == "#"),
       Block_Pos = sum(evaluation_code == "+"),
       Block_Exc = sum(evaluation_code == "!"),
       Block_Neg = sum(evaluation_code == "-"),
       Block_Barra = sum(evaluation_code == "/"),
       Block_Error = sum(evaluation_code == "=")
       
     ),
   by = "team")%>%
 mutate(
   Bloqueos_set = round(ifelse(Block_Tot > 0, round(Block_Punto / sets_played, 4), 0),2),
   `% Block Neg` = round((Block_Neg / Block_Tot) * 100, 2),
   `% Block Error` = round((Block_Error / Block_Tot) * 100, 2),
   `% Block Exc` = round((Block_Exc / Block_Tot) * 100, 2),
   `% Block Pos` = round((Block_Pos / Block_Tot) * 100, 2),
   `% Block Barra` = round((Block_Barra / Block_Tot) * 100, 2),
   `% Block Punto` = round((Block_Punto / Block_Tot) * 100, 2)
 )

top_bloqueo_equipo <- teams_info %>%
 arrange(desc(Bloqueos_set)) %>%
 mutate(Rank = row_number()) %>% 
 select(
   Rank = Rank,
   Equipo = team, 
   Sets = sets_played,
   'Nº Bloqueos (todo)' = Block_Tot,
   'Bloqueos # / set' = Bloqueos_set,
   'Bloqueos #' = Block_Punto
 )


total_equipos <- df %>%
 filter(!is.na(team)) %>% 
 summarise(
   total_equipos = n_distinct(team)
 )

total_jugadores <- df %>%
 filter(!is.na(player_id)) %>% 
 summarise(
   total_jugadores= n_distinct(player_id)
 )

total_partidos <- df %>%
 filter(!is.na(team)) %>% 
 summarise(
   total_partidos= n_distinct(match_id)
 )


# Convertir las columnas a tipo numérico
df <- df %>%
 mutate(home_sets_won = as.numeric(home_sets_won),
        visiting_sets_won = as.numeric(visiting_sets_won))
                             

                            
                             
                             
                             
                             
                             
                             

ui <- dashboardPage(
 skin = "red",
 dashboardHeader(title = "SM1 23-24"), 
 dashboardSidebar(
   sidebarMenu(
     menuItem("Ranking equipos", tabName = "dashboard", icon = icon("dashboard")),
     menuItem("Ranking equipos/fundamento", tabName = "equipo_fundamento", icon = icon("th")),
     menuItem("Ranking jugadores/posición", tabName = "jugadores", icon = icon("th")),
     menuItem("Top15 jugadores/fundamento", tabName = "fundamento", icon = icon("th")),
     menuItem("Ataque", tabName = "Ataque", icon = icon("chart-bar")),
     menuItem("Recepcion", tabName = "Recepcion", icon = icon("chart-bar")),
     menuItem("partido", tabName = "match_analysis", icon = icon("chart-line")),
     menuItem("Predicción de sets", tabName = "prediccion", icon = icon("chart-line"))
   )
 ),
 dashboardBody(
   fluidRow(
     valueBoxOutput("value1"),
     valueBoxOutput("value2"),
     valueBoxOutput("value3")
   ),
   tabItems(
     tabItem(tabName = "dashboard",
             fluidRow(
               valueBoxOutput("value1"),
               valueBoxOutput("value2"),
               valueBoxOutput("value3")
             ),
             fluidRow( 
               box(
                 title = "Victorias-Derrotas",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput("teams_matches_win_lost", height = "800px")
               ) ,
               box(
                 title = "% Sets ganados",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput("teams_win_percent", height = "800px")
               ) 
             )
     ),
     tabItem(tabName = "jugadores",
             fluidRow(
               selectInput("jugador_type", "Posición", choices = c("Receptores", "Líberos", "Opuestos", "Centrales")),
               tableOutput("ranking_table")
             )
     ),
     tabItem(tabName = "fundamento",
             fluidRow(
               selectInput("fundamento_type", "Fundamento", choices = c("Saque", "Saque flot", "Saque salto","Recepción" ,"Recepción flot",
                                                                        "Recepción salto", "Bloqueo")),
               tableOutput("fundamento_table")
             )
     ),
     tabItem(tabName = "equipo_fundamento",
             fluidRow(
               selectInput("equipo_fundamento_type", "Fundamento", choices = c("Saque", "Recepción", "Ataque","Bloqueo")),
               uiOutput("equipo_ataque_subtype"),
               uiOutput("equipo_saque_subtype"),
               uiOutput("equipo_rece_subtype"),
               tableOutput("equipo_fundamento_table")
             )
     ),
     tabItem(tabName = "Ataque",
             fluidRow(
               checkboxGroupInput("equipos_seleccionados", "Seleccionar equipos", choices = unique(teams_info$team)),
               plotOutput("ataque_grafica")
             )
     ),
     tabItem(tabName = "Recepcion",
             fluidRow(
               checkboxGroupInput("equipos_seleccionados_rece", "Seleccionar equipos", choices = unique(teams_info$team)),
               plotOutput("recepcion_grafica")
             )
     ),
     tabItem(tabName = "match_analysis",
             fluidRow(
               selectInput("equipo_select", "Seleccionar equipo", choices = unique(df$home_team)),
               radioButtons("ida_vuelta_select", "Seleccionar tipo de partido:",
                            choices = c("Ida", "Vuelta"), inline = TRUE),
               uiOutput("partidos_select"),
               h2("Match analysis content"),
               textOutput("selected_match_id")
             )
     ),
     tabItem(tabName = "prediccion",
             fluidPage(
               selectInput("equipo_local", "Equipo local:", choices = unique(df$home_team)),
               selectInput("equipo_visitante", "Equipo visitante:", choices = unique(df$visiting_team)),
               numericInput("saques_local", "Saques equipo local:", value = NULL),
               numericInput("ataques_local", "Ataques equipo local:", value = NULL),
               numericInput("bloqueos_local", "Bloqueos equipo local:", value = NULL),
               numericInput("saques_visitante", "Saques equipo visitante:", value = NULL),
               numericInput("ataques_visitante", "Ataques equipo visitante:", value = NULL),
               numericInput("bloqueos_visitante", "Bloqueos equipo visitante:", value = NULL),
               verbatimTextOutput("resultado_prediccion")
             )
     )
     
   )
 )
)

server <- function(input, output, session) {
 
 output$value1 <- renderValueBox({
   valueBox(
     total_partidos
     ,"Total partidos analizados"
     ,icon = icon("stats",lib='glyphicon')
     ,color = "red"
   )  
 })
 output$value2 <- renderValueBox({ 
   valueBox(
     total_equipos
     ,'Total equipos analizados'
     ,icon = icon("signal", lib="glyphicon")
     ,color = "red"
   )  
 })
 
 output$value3 <- renderValueBox({ 
   valueBox(
     total_jugadores
     ,'Total jugadores analizados'
     ,icon = icon("user", lib="glyphicon")
     ,color = "red"
   )  
 })
 
 asp_ratio <- 1.618
 
 output$teams_matches_win_lost <- renderPlot({
   ggplot(teams_info, aes(x = matches_lost, y = matches_won, label = team, fill = team)) +
     scale_x_continuous(limits = c(0, 22), 
                        breaks = seq(0, 22, 1))+
     scale_y_continuous(limits = c(0, 22), 
                        breaks = seq(0, 22, 1))+
     geom_image(aes(image = logo), size = 0.1, nudge_y = 0.01) +  # Ajusta el tamaño aquí
     scale_fill_manual(values = rainbow(length(unique(teams_info$team)))) +
     labs(x = "Partidos perdidos",
          y = "Partidos ganados",
          title = expression(paste(bold("Victorias-Derrotas por equipo"))))+
     theme_minimal() +
     theme(
       aspect.ratio = 1/asp_ratio,
       panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
       panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
       text = element_text(color = "black", size = 12),     # Color y tamaño del texto
       axis.text = element_text(size = 12),                 # Tamaño del texto del eje
       axis.title = element_text(size = 14),                # Tamaño del título del eje
       axis.line = element_line(color = "beige"),           # Color de la línea del eje
       plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
       plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
       plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
       axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
       axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
       axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
       axis.text.y = element_text(vjust = -0.05),
       axis.text.x = element_text(hjust = +0.05)
     )+
     geom_point(aes(x = matches_lost, y = matches_won), color = "black", size = 1.5) +
     guides(fill = "none")+
     theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   
 })
 
 output$teams_win_percent <- renderPlot({
   ggplot(teams_info, aes(x = sets_played, y = (sets_won / sets_played) * 100, label = team, fill = team)) +
     scale_x_continuous(limits = c(70, 100), 
                        breaks = seq(70, 100, 5), 
                        labels = scales::number_format(accuracy = 0.1))+
     scale_y_continuous(limits = c(10, 90), 
                        breaks = seq(10, 90, 10), 
                        labels = scales::number_format(accuracy = 0.1))+
     geom_image(aes(image = logo), size = 0.1, nudge_y = 0.01) +  # Ajusta el tamaño aquí
     scale_fill_manual(values = rainbow(length(unique(teams_info$team)))) +
     labs(x = "Sets jugados",
          y = "% Sets ganados",
          title = expression(paste(bold("%Sets ganados por equipo"))))+
     theme_minimal() +
     theme(
       aspect.ratio = 1/asp_ratio,
       panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
       panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
       text = element_text(color = "black", size = 12),     # Color y tamaño del texto
       axis.text = element_text(size = 12),                 # Tamaño del texto del eje
       axis.title = element_text(size = 14),                # Tamaño del título del eje
       axis.line = element_line(color = "beige"),           # Color de la línea del eje
       plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
       plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
       plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
       axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
       axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
       axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
       axis.text.y = element_text(vjust = -0.05),
       axis.text.x = element_text(hjust = +0.05)
     )+
     geom_point(aes(x = sets_played, y = (sets_won / sets_played) * 100), color = "black", size = 1.5) +
     guides(fill = "none")+
     theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   
 })
 
 output$ranking_table <- renderTable({
   if (input$jugador_type == "Receptores") {
     mejores_oh
   } else if (input$jugador_type == "Líberos") {
     mejores_lib
   } else if (input$jugador_type == "Opuestos") {
     mejores_opp
   } else if (input$jugador_type == "Centrales") {
     mejores_mb
   }
 })
 
 output$fundamento_table <- renderTable({
   if (input$fundamento_type == "Saque") {
     top_sacadores
   } else if (input$fundamento_type == "Saque flot") {
     top_sacadores_flot
   } else if (input$fundamento_type == "Saque salto") {
     top_sacadores_jump
   } else if (input$fundamento_type == "Recepción") {
     top_receptores
   } else if (input$fundamento_type == "Recepción flot") {
     top_receptores_flot
   } else if (input$fundamento_type == "Recepción salto") {
     top_receptores_jump
   } else if (input$fundamento_type == "Bloqueo") {
     top_bloqueadores
   }
 })
 
 observeEvent(input$equipo_fundamento_type, {
   if (input$equipo_fundamento_type == "Ataque") {
     subtype_choices <- c("Todo","Bola rápida", "Bola alta", "1er tiempo", "Pipe")
   } else {
     subtype_choices <- NULL
   }
   output$equipo_ataque_subtype <- renderUI({
     if (!is.null(subtype_choices)) {
       selectInput("equipo_ataque_subtype_input", "Tipo de ataque", choices = subtype_choices)
     } else {
       NULL
     }
   })
 })
 
 observeEvent(input$equipo_ataque_subtype_input, {
   if (input$equipo_ataque_subtype_input == "Bola rápida") {
     output$equipo_fundamento_table <- renderTable({
       top_ataque_fast_equipo
     })
   } else if (input$equipo_ataque_subtype_input == "Bola alta") {
     output$equipo_fundamento_table <- renderTable({
       top_ataque_high_equipo
     })
   } else if (input$equipo_ataque_subtype_input == "Todo") {
     output$equipo_fundamento_table <- renderTable({
       top_ataque_equipo
     })
   } else if (input$equipo_ataque_subtype_input == "1er tiempo") {
     output$equipo_fundamento_table <- renderTable({
       top_ataque_quick_equipo
     })
   } else if (input$equipo_ataque_subtype_input == "Pipe") {
     output$equipo_fundamento_table <- renderTable({
       top_ataque_pipe_equipo
     })
   }
 })
 
 observeEvent(input$equipo_fundamento_type, {
   if (input$equipo_fundamento_type == "Saque") {
     subtype_choices <- c("Todo", "Saque flot", "Saque salto")
   } else {
     subtype_choices <- NULL
   }
   output$equipo_saque_subtype <- renderUI({
     if (!is.null(subtype_choices)) {
       selectInput("equipo_saque_subtype_input", "Tipo de saque", choices = subtype_choices)
     } else {
       NULL
     }
   })
 })
 
 observeEvent(input$equipo_saque_subtype_input, {
   if (input$equipo_saque_subtype_input == "Todo") {
     output$equipo_fundamento_table <- renderTable({
       top_saque_equipo
     })
   } else if (input$equipo_saque_subtype_input == "Saque flot") {
     output$equipo_fundamento_table <- renderTable({
       top_saque_flot_equipo
     })
   } else if (input$equipo_saque_subtype_input == "Saque salto") {
     output$equipo_fundamento_table <- renderTable({
       top_saque_jump_equipo
     })
   }
 })
 
 
 observeEvent(input$equipo_fundamento_type, {
   if (input$equipo_fundamento_type == "Recepción") {
     subtype_choices <- c("Todo", "Recepción flot", "Recepción salto")
   } else {
     subtype_choices <- NULL
   }
   output$equipo_rece_subtype <- renderUI({
     if (!is.null(subtype_choices)) {
       selectInput("equipo_rece_subtype_input", "Tipo de recepción", choices = subtype_choices)
     } else {
       NULL
     }
   })
 })
 
 observeEvent(input$equipo_rece_subtype_input, {
   if (input$equipo_rece_subtype_input == "Todo") {
     output$equipo_fundamento_table <- renderTable({
       top_rece_equipo
     })
   } else if (input$equipo_rece_subtype_input == "Recepción flot") {
     output$equipo_fundamento_table <- renderTable({
       top_rece_flot_equipo
     })
   } else if (input$equipo_rece_subtype_input == "Recepción salto") {
     output$equipo_fundamento_table <- renderTable({
       top_rece_jump_equipo
     })
   }
 })
 
 observeEvent(input$equipo_fundamento_type, {
   if (input$equipo_fundamento_type == "Bloqueo") {
     output$equipo_fundamento_table <- renderTable({
       top_bloqueo_equipo
     })
   }
 })
 
 #### ATAQUE GRÁFICO CON SELECCION DE EQUIPOS
 output$ataque_grafica <- renderPlot({
   ataque_seleccionado <- input$equipos_seleccionados
   if (length(ataque_seleccionado) > 0) {
     ataque_data <- teams_info[teams_info$team %in% ataque_seleccionado, ]
     
     ggplot(ataque_data, aes(x = Ataque_Tot, y = `% Ataque Eficiencia`, label = team)) +
       geom_point() +
       geom_text() +
       labs(x = "Ataques totales", y = "% Eficiencia Ataque", title = "%Eficiencia de ataque") +
       geom_image(aes(image = logo), size = 0.1, nudge_y = 0.01) + 
       scale_fill_manual(values = rainbow(length(unique(teams_info$team)))) +
       theme_minimal() +
       theme(
         aspect.ratio = 1/asp_ratio,
         panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
         panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
         text = element_text(color = "black", size = 12),     # Color y tamaño del texto
         axis.text = element_text(size = 12),                 # Tamaño del texto del eje
         axis.title = element_text(size = 14),                # Tamaño del título del eje
         axis.line = element_line(color = "beige"),           # Color de la línea del eje
         plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
         plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
         plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
         axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
         axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
         axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
         axis.text.y = element_text(vjust = -0.05),
         axis.text.x = element_text(hjust = +0.05)
       )+
       geom_point(aes(x = Ataque_Tot, y = `% Ataque Eficiencia`), color = "black", size = 1.5) +
       guides(fill = "none")+
       theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   } else {
     ggplot() +
       annotate("text", x = 0.5, y = 0.5, label = "Seleccione al menos un equipo para ver el gráfico",
                size = 5, color = "red", fontface = "bold", hjust = 0.5, vjust = 0.5)+
       labs(x = "Ataques totales", y = "% Eficiencia Ataque", title = "%Eficiencia de ataque") +
       theme_minimal() +
       theme(
         aspect.ratio = 1/asp_ratio,
         panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
         panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
         text = element_text(color = "black", size = 12),     # Color y tamaño del texto
         axis.text = element_text(size = 12),                 # Tamaño del texto del eje
         axis.title = element_text(size = 14),                # Tamaño del título del eje
         axis.line = element_line(color = "beige"),           # Color de la línea del eje
         plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
         plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
         plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
         axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
         axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
         axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
         axis.text.y = element_text(vjust = -0.05),
         axis.text.x = element_text(hjust = +0.05)
       )+theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   }
 })
 
 #### RECEPCION GRÁFICO CON SELECCION DE EQUIPOS
 output$recepcion_grafica <- renderPlot({
   recepcion_seleccionado <- input$equipos_seleccionados_rece
   if (length(recepcion_seleccionado) > 0) {
     recepcion_data <- teams_info[teams_info$team %in% recepcion_seleccionado, ]
     
     ggplot(recepcion_data, aes(x = `% Rec Error`, y = Rec_AVG, label = team)) +
       geom_point() +
       geom_text() +
       labs(x = "% Error", y = "AVG", title = "% Error vs AVG (Recepción)") +
       geom_image(aes(image = logo), size = 0.1, nudge_y = 0.01) + 
       scale_fill_manual(values = rainbow(length(unique(teams_info$team)))) +
       theme_minimal() +
       theme(
         aspect.ratio = 1/asp_ratio,
         panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
         panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
         text = element_text(color = "black", size = 12),     # Color y tamaño del texto
         axis.text = element_text(size = 12),                 # Tamaño del texto del eje
         axis.title = element_text(size = 14),                # Tamaño del título del eje
         axis.line = element_line(color = "beige"),           # Color de la línea del eje
         plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
         plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
         plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
         axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
         axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
         axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
         axis.text.y = element_text(vjust = -0.05),
         axis.text.x = element_text(hjust = +0.05)
       )+
       geom_point(aes(x = `% Rec Error`, y = Rec_AVG), color = "black", size = 1.5) +
       guides(fill = "none")+
       theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   } else {
     ggplot() +
       annotate("text", x = 0.5, y = 0.5, label = "Seleccione al menos un equipo para ver el gráfico",
                size = 5, color = "red", fontface = "bold", hjust = 0.5, vjust = 0.5)+
       labs(x = "% Error", y = "AVG", title = "% Error vs AVG (Recepción)") +
       theme_minimal() +
       theme(
         aspect.ratio = 1/asp_ratio,
         panel.background = element_rect(fill = "cornsilk"),  # Color de fondo claro
         panel.grid = element_line(color = "beige"),         # Líneas de la cuadrícula claras
         text = element_text(color = "black", size = 12),     # Color y tamaño del texto
         axis.text = element_text(size = 12),                 # Tamaño del texto del eje
         axis.title = element_text(size = 14),                # Tamaño del título del eje
         axis.line = element_line(color = "beige"),           # Color de la línea del eje
         plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Tamaño y estilo del título del gráfico
         plot.subtitle = element_text(size = 14, hjust = 0.5),             # Tamaño del subtítulo del gráfico
         plot.caption = element_text(size = 10),              # Tamaño de la leyenda del gráfico
         axis.ticks = element_blank(),                        # Elimina las marcas de los ejes
         axis.ticks.length = unit(0, "mm"),                   # Establece la longitud de las marcas de los ejes
         axis.ticks.margin = unit(0, "mm"),                    # Establece el margen de las marcas de los ejes
         axis.text.y = element_text(vjust = -0.05),
         axis.text.x = element_text(hjust = +0.05)
       )+theme(aspect.ratio = 1/asp_ratio, plot.background = element_rect(fill = "cornsilk",color = "cornsilk"))
   }
 })
 
 
 # Generar la seleccion de equipos
 output$partidos_select <- renderUI({
   equipo_seleccionado <- input$equipo_select
   partidos_equipo <- unique(c(df$match_id[df$home_team == equipo_seleccionado], df$match_id[df$visiting_team == equipo_seleccionado]))
   
   oponentes <- ifelse(df$home_team %in% equipo_seleccionado, df$visiting_team, df$home_team)
   oponentes_partidos <- oponentes[df$match_id %in% partidos_equipo]
   
   oponentes_unicos <- unique(oponentes_partidos)
   
   choices <- setNames(oponentes_unicos, oponentes_unicos)
   
   selectInput("partido_select", "Seleccionar oponente", choices = choices)
 })
 
 # Mostrar el filename del partido seleccionado
 output$selected_match_id <- renderText({
   oponente_seleccionado <- input$partido_select
   equipo_seleccionado <- input$equipo_select
   
   # Obtener el tipo de partido seleccionado
   tipo_partido <- input$ida_vuelta_select
   
   # Buscar el partido correspondiente entre los dos equipos seleccionados
   if (tipo_partido == "Ida") {
     partido_seleccionado <- df$match_id[(df$home_team == equipo_seleccionado & df$visiting_team == oponente_seleccionado) | (df$home_team == oponente_seleccionado & df$visiting_team == equipo_seleccionado)][1]
   } else {
     partido_seleccionado <- df$match_id[(df$home_team == equipo_seleccionado & df$visiting_team == oponente_seleccionado) | (df$home_team == oponente_seleccionado & df$visiting_team == equipo_seleccionado)][2]
   }
   
   filename_seleccionado <- df$filename[df$match_id == partido_seleccionado][1]
   paste("El filename del partido seleccionado es:", filename_seleccionado)
 })
 
 
 #PREDICCION DE SETS
 calcular_prediccion <- function(equipo_local, equipo_visitante, 
                                 saques_local, ataques_local, bloqueos_local,
                                 saques_visitante, ataques_visitante, bloqueos_visitante) {
   model_local <- glm(home_sets_won ~ home_team + visiting_team + home_attack_points 
                      + home_serve_points + home_block_points, 
                      family = poisson(link = "log"), data = df_aux)
   av_home_sets <- predict(model_local, 
                           data.frame(home_team = equipo_local, visiting_team = equipo_visitante, 
                                      home_attack_points = ataques_local,
                                      home_serve_points = saques_local,
                                      home_block_points = bloqueos_local), 
                           type = "response")
   

   model_visitante <- glm(visiting_sets_won ~ home_team + visiting_team + visiting_attack_points
                          + visiting_serve_points + visiting_block_points, 
                          family = poisson(link = "log"), data = df_aux)
   av_visiting_sets <- predict(model_visitante, 
                               data.frame(home_team = equipo_local, visiting_team = equipo_visitante, 
                                          visiting_attack_points = ataques_visitante,
                                          visiting_serve_points = saques_visitante,
                                          visiting_block_points = bloqueos_visitante), 
                               type = "response")
   
   return(list(sets_local = av_home_sets, sets_visitante = av_visiting_sets))
 }
 
 output$resultado_prediccion <- renderPrint({
   prediccion <- calcular_prediccion(input$equipo_local, input$equipo_visitante,
                                     input$saques_local, input$ataques_local, input$bloqueos_local,
                                     input$saques_visitante, input$ataques_visitante, input$bloqueos_visitante)
   cat("El equipo local", input$equipo_local, "realizará", prediccion$sets_local, "sets.\n")
   cat("El equipo visitante", input$equipo_visitante, "realizará", prediccion$sets_visitante, "sets.")
 })
 
 
}

shinyApp(ui = ui, server = server)
                                                           