library(data.table)
library(ggplot2)
library(magrittr)
library(viridis)
library(cowplot)
library(xgboost)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(dplyr)


themeTF <- function(base_size = 16) { 
  theme_minimal_hgrid(font_size = base_size) +
    theme(
      legend.position="bottom",
      legend.justification = "center",
      plot.margin=unit(c(0.5,1,0.5,0.5), "cm"),
      plot.title = element_text(hjust = 0.5),
      legend.key.width = grid::unit(1.5, "cm"),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

theme_set(themeTF())

produire_graphique <- function(
    data,
    variable = NULL,
    label_variable = "Predicted value",
    title = NULL
) {
  ggplot(data) + 
    geom_point(aes(x = x, y = y, color = {{variable}})) +
    scale_x_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
    scale_y_continuous(expand = c(0,0), breaks = NULL, labels = NULL) +
    scale_color_viridis_c(limits = c(-1, 1)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size=14)
    ) +
    labs(title = title, x = NULL, y = NULL)
}


#############################################
# Générer des données
#############################################

dimension_x <- 150 / 1
dimension_y <- 100 / 1

temp <- data.table(x = -dimension_x:dimension_x, un = 1) %>% 
  merge(
    data.table(y = -dimension_y:dimension_y, un = 1), 
    by = "un",
    allow.cartesian = TRUE
  )
temp[, un := NULL]
temp[, valeur := (1 - (x/50)/2 + (x/50)**5 + (y/50)**3) * exp(-(x/50)**2 - (y/50)**2)
][, valeur := (valeur) * 2 / (max(valeur) - min(valeur))
][, valeur := valeur - (max(valeur) + min(valeur)) / 2
][, valeur2 := valeur + 0.1 * rnorm(.N)
][valeur2 >  1, valeur2 :=  1
][valeur2 < -1, valeur2 := -1
]

# Graphique de la valeur observée
p_observe <- ggplot(temp) + 
  geom_point(aes(x = x, y = y, color = valeur)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_viridis_c(
    "Observed value",
    limits = c(-1, 1)
  ) + 
  labs(title = "Observed value")

ggsave(
  plot = p_observe,
  "p_observe.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)


#############################################
# Entraîner un modèle de GBRT
#############################################

dtrain <- xgb.DMatrix(data = temp[, .(x, y)] %>% as.data.frame() %>% as.matrix(), label = temp$valeur)

modele_xgb <- xgboost(
  data = dtrain, 
  # label = temp$valeur, 
  max.depth = 3, 
  eta     = 1,
  nthread = 2, 
  nrounds = 100, 
  objective = "reg:squarederror"
)

#############################################
# Construire la table d'exploitation
#############################################

temp2 <- copy(temp)
temp2[, valeur_predite := predict(modele_xgb, dtrain)]
for (i in 2:101) {
  temp2[, c(paste0("valeur_predite_arbres1_", i-1)) := predict(modele_xgb, dtrain, iterationrange = c(1, i))]
}
for (i in 2:101) {
  temp2[, c(paste0("valeur_predite_arbre", i-1)) := predict(modele_xgb, dtrain, iterationrange = c(i-1, i))]
}

temp2 <- temp2[, `:=`(
  valeur_predite_arbres1_25   = predict(modele_xgb, dtrain, iterationrange = c( 1, 26)),
  valeur_predite_arbres26_50  = predict(modele_xgb, dtrain, iterationrange = c(26, 51)),
  valeur_predite_arbres51_75  = predict(modele_xgb, dtrain, iterationrange = c(51, 76)),
  valeur_predite_arbres11_20 = predict(modele_xgb, dtrain, iterationrange = c(11, 21)),
  valeur_predite_arbres21_30 = predict(modele_xgb, dtrain, iterationrange = c(21, 31)),
  valeur_predite_arbres31_40 = predict(modele_xgb, dtrain, iterationrange = c(31, 41))
)]

#############################################
# Produire le graphique et le diagramme du premier arbre
#############################################

# Graphique de la valeur observée
p_arbre1 <- ggplot(temp2) + 
  geom_point(aes(x = x, y = y, color = valeur_predite_arbre1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_viridis_c(
    "Predicted value",
    limits = c(-1, 1)
  ) + 
  labs(title = "Prediction of tree 1")

ggsave(
  plot = p_arbre1,
  "p_arbre1.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)

# Extraire le premier arbre du modèle
arbre1 <- xgb.plot.tree(
  model = modele_xgb, 
  trees = 0,
  show_node_id = TRUE, 
  render = FALSE
)

# Modifier l'apparence de l'arbre
arbre1$nodes_df <- arbre1$nodes_df %>% 
  dplyr::mutate(
    fontsize = 20,
    label = label %>% stringr::str_replace(
      label %>% stringr::str_extract("[\\-\\.\\d]+$"),
      label %>% stringr::str_extract("[\\-\\.\\d]+$") %>% as.numeric() %>% round(3) %>% as.character()
    ) %>%
      stringr::str_remove("^Tree 0\\n") %>%
      stringr::str_remove("^0\\-") %>%
      stringr::str_remove("(\\n)?Gain.+$") %>%
      stringr::str_remove(paste0("\\:\\s", ifelse(data == "Leaf", "xxxxxxx", data))),
    # shape = ifelse(shape == "rectangle", "circle", shape)
  )
arbre1$edges_df <- arbre1$edges_df %>% 
  left_join(
    arbre1$nodes_df %>% select(id, data),
    by = c("from" = "id")
  ) %>%
  dplyr::mutate(
    color = "black",
    fontsize = 20, 
    arrowsize = 1,
    penwidth = 1,
    label = ifelse(label == "", "", paste0(data, " ", label))
  )

arbre1 %>% render_graph()

arbre1 %>% 
  export_graph(
    file_name = "diag_arbre1.svg", 
    file_type = "svg",
    # title = "First tree of the model",
  )



#############################################
# Produire tous les graphiques
#############################################

p_pred_arbre1 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbre1, 
    title = "Tree 1"
  )

p_pred_arbres1_1 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_1, 
    title = "Trees 1 to 1"
  )

p_pred_arbre2 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbre2, 
    title = "Tree 2"
  )

p_pred_arbres1_2 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_2, 
    title = "Trees 1 to 2"
  )


p_pred_arbre3 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbre3, 
    title = "Tree 3"
  )

p_pred_arbres1_3 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_3, 
    title = "Trees 1 to 3"
  )

p_pred_arbre4 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbre4, 
    title = "Tree 4"
  )

p_pred_arbres1_4 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_4, 
    title = "Trees 1 to 4"
  )

plot_grid(
  p_pred_arbre1,
  p_pred_arbre2,
  p_pred_arbre3,
  p_pred_arbre4,
  p_pred_arbres1_1,
  p_pred_arbres1_2,
  p_pred_arbres1_3,
  p_pred_arbres1_4,
  nrow = 2,
  ncol = 4,
  align = "v"
) + 
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "p_debut.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)

p_pred_arbres1_10 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_10, 
    title = "Trees 1 to 10"
  )

p_pred_arbres11_20 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres11_20, 
    title = "Trees 11 to 20"
  )

p_pred_arbres1_20 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_20, 
    title = "Trees 1 to 20"
  )

p_pred_arbres21_30 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres21_30, 
    title = "Trees 21 to 30"
  )

p_pred_arbres1_30 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_30, 
    title = "Trees 1 to 30"
  )

p_pred_arbres31_40 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres31_40, 
    title = "Trees 31 to 40"
  )

p_pred_arbres1_40 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_40, 
    title = "Trees 1 to 40"
  )

p_debut <- plot_grid(
  p_pred_arbres1_10, 
  p_pred_arbres11_20,
  p_pred_arbres21_30,
  p_pred_arbres31_40,
  p_pred_arbres1_10,
  p_pred_arbres1_20,
  p_pred_arbres1_30,
  p_pred_arbres1_40,
  nrow = 2,
  ncol = 4,
  align = "v"
) + 
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "p_complet.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)



p_pred_arbres1_25 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_25, 
    title = "Trees 1 to 25"
  )

p_pred_arbres26_50 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres26_50, 
    title = "Trees 26 to 50"
  )

p_pred_arbres1_50 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_50, 
    title = "Trees 1 to 50"
  )

p_pred_arbres51_75 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres51_75, 
    title = "Trees 51 to 75"
  )

p_pred_arbres1_75 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_75, 
    title = "Trees 1 to 75"
  )

p_pred_arbres76_100 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres76_100, 
    title = "Trees 76 to 100"
  )

p_pred_arbres1_100 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_100, 
    title = "Trees 1 to 100"
  )

p_debut <- plot_grid(
  p_pred_arbres1_25,
  p_pred_arbres26_50,
  p_pred_arbres51_75,
  p_pred_arbres76_100,
  p_pred_arbres1_25,
  p_pred_arbres1_50,
  p_pred_arbres1_75,
  p_pred_arbres1_100,
  nrow = 2,
  ncol = 4,
  align = "v"
) + 
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave(
  "p_complet2.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)

p_pred_arbres1_100 <- temp2 %>% 
  produire_graphique(
    variable = valeur_predite_arbres1_100, 
    title = "Trees 1 to 100"
  )

ggsave(
  plot = p_pred_arbres1_100,
  "p_final100.png", 
  dpi = 100,
  width  = 300,
  height = 140,
  units  = "mm"
)

