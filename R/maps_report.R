#' ---
#' title: Maps and tables WW
#' output:
#'   html_document:
#'     toc: yes
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#'     number_sections: yes
#'     fig_caption: no
#'     fig_width: 10
#'     fig_height: 5.5
#'     mathjax: null
#'     theme: cerulean
#'     table_width: 10
#'     df_print: kable
#'     dev: png
#' ---

#+ setup, echo = FALSE
knitr::opts_chunk$set(
  echo = FALSE
)

#' # SARS-CoV-2 niveau {.tabset}
#'
#' Beregningen er lavet ved at skalere værdierne til at løbe mellem 0 og 100, 
#' hvor 0 repræsenterer laveste måling nogensinde, samt 100 den højeste måling nogensinde. 
#' 
#' Denne funktion benytter følgende udregning:
#' - **$(Pågældende_uge - (Højeste_ever - laveste_ever))(Højeste_ever - laveste_ever)*100$**
#' 
#' Alle værdier derimellem udgør så en given procentsats - dermed er den mere robust mod ændringer i limits end ved f.eks.
#' brug af percentiler. Der er valgt at benytte log transformeret data af flere årsager:
#' - Data bliver tilnærmelsesvis normalfordelt
#' - Mere gradvis stigning/fald som følge af der er et mindre spænd fra min til max værdi i log værdierne sammenlignet med de normale værdier
#' - Bør teoretisk set gardere mod potentielle "ekstreme" værdier - igen fordi forskellen vil være mindre. 
#'
#' Landsdele og regioner benytter udelukkende renseanlæg og ikke decentrale. 
#' Derudover er det baseret på det ugentlige vægtede gennemsnit af målingerne (vægtet med log population af opland).
#' 
#'
#' Resultaterne er gemt i: `S:/Spildevand/outputs/maps/`.
#'
#'
#'
#'

#' ## Regioner
#'

#+ region_graph
#fp_latest(here("S:/Spildevand/outputs/maps/week_30"), "_map_region\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_map_region.png",
                               week =  week(today()-days(7))))

#' ## Landsdele

#+ landsdele_graph
#fp_latest("S:/Spildevand/outputs/maps/week_30", "_map_landsdel\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_map_landsdel.png",
                               week =  week(today()-days(7))))


#' ## Alle anlæg
#'

#+ all anlaeg
#fp_latest("S:/Spildevand/outputs/maps/week_30", "_map_all_anlaeg\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_map_all_anlaeg.png",
                               week =  week(today()-days(7))))

#' ## Alle anlæg (interaktivt)
#' 

#+ interactive all
#fp_latest("S:/Spildevand/outputs/maps/week_30", "_kort-all_anlaeg\\.html") %>%
  knitr::include_url(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_kort-all_anlaeg.html",
                          week =  week(today()-days(7))),     
                     height = "600px")

#' # Kategori ændringer siden sidste uge {.tabset}
#'Det samlede antal renseanlæg/decentrale inkluderet i denne uge er: **`r as.character(sum(cat_change$n))`**
#'
#'
#'

#' ## Regioner

suppressMessages(cat_perc_change(map_region_final))

#' ## Landsdele
#'

suppressMessages(cat_perc_change(map_landsdel_final))


#' ## Alle anlæg

suppressMessages(cat_perc_change(map_all_anlaeg)) 

#' # Udvikling i niveau over tid {.tabset}
#'


#' ## Regioner (seneste 8 uger)
#+ udvikling_region_8
#fp_latest("S:/Spildevand/outputs/maps/week_30", "8_weeks_map_region\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_8_weeks_maps_region.png",
                          week =  week(today()-days(7))))

#' ## Landsdele (seneste 8 uger)
#+ udvikling_landsdel_
#fp_latest("S:/Spildevand/outputs/maps/week_30", "8_weeks_maps_landsdel\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_8_weeks_maps_landsdel.png",
                          week =  week(today()-days(7))))



#' # Procentvis ændring over 3 uger (21 dage) {.tabset}
#' Dette tager udgangspunkt i en lineær regression over 3 uger på log tranformeret data. 
#' Hældningen af regressionen benyttes ved følgende formel til procent vis ændring i +/- : **$(10^(slope) - 1) *100$**. 
#' Følgende kriterier skal være opfyldt før et givent renseanlæg bliver inkluderet:
#'  
#'  - 4 prøver eller derover
#'  - Hvoraf der er mindst 1 prøve1 i hver uge
#'  
#'  **`r as.character(sum(cat_change$n) - nrow(lin_reg))`** anlæg opfylder ikke disse kriterier og er derfor undladt af de enkelte anlæg. Dog vil disse stadig være inkluderet
#' i beregningerne af landsdele samt regioner.
#' 
#' 
#' 
#' ## Regioner
#+ regioner_procent
#fp_latest("S:/Spildevand/outputs/maps/week_30", "-change_regioner\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_percent-change_regioner.png",
                               week =  week(today()-days(7))))

#' ## Landsdele
#+ landsdele_procent
#fp_latest("S:/Spildevand/outputs/maps/week_30", "-change_landsdel\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_percent-change_landsdel.png",
                               week =  week(today()-days(7))))


#' ## Alle anlæg
#+ anlaeg_procent
#fp_latest("S:/Spildevand/outputs/maps/week_30", "-change_all_anlaeg\\.png") %>%
  knitr::include_graphics(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_percent-change_all_anlaeg.png",
                                week =  week(today()-days(7))))


#' ## Alle anlæg (interaktivt)

#+ interactive_lin_reg
#fp_latest("S:/Spildevand/outputs/maps/week_30", "_kort-lin_reg\\.html") %>%
  knitr::include_url(glue("S:/Spildevand/outputs/maps/week_{week}/week_{week}_kort-lin_reg.html",
                          week =  week(today()-days(7))),
                     height = "600px")

#' # Kategori ændringer siden sidste uge {.tabset}
#' 
#' 
#' 
#' 

#' ## Regioner

#+ change_region
lin_reg_two_weeks_region


#' ## Landsdele

#+ change_landsdel
lin_reg_two_weeks_landsdel


#' ## Alle anlæg

#+ change_all_lin
lin_reg_two_weeks_anlaeg



#' <style type="text/css">
#' ::selection {
#'   background: #c9deff;
#' }
#'
#' pre {
#'   display: inline-block
#' }
#'
#' iframe {
#'   border-style: none;
#' }
#'
#' .table {
#'   width: 10;
#'   min-width: 100%
#' }
#'
#' </style>
