#' ------------------------- P'HairF-Performance Capillaires
#'
#'
#' @project User-friendly Application for hair care products performance evaluation
#'
#' @author Thi Thanh Hien NGUYEN - Antonin PUGET
#'
#' @title PHairF.R
#'
#' @description
#' Evaluate Repetability, Reproductivity, Agreement
#' Using Two One-Sided Tests(TOST)
#' ref: https://doi.org/10.1021/ac053390m
#'
#' @details
#' Version 1 - 15/11/2024: by: Thi Thanh Hien NGUYEN
#' Version 2 - 05/12/2024: Add dynamic marge and visualization of TOST results ; by: Thi Thanh Hien NGUYEN
#'                         UI and visual modifications ; by: Antonin PUGET
#' Version 3 - 05/03/2025: Use of t_TOST instead of tsum_TOST 
#'                         Use of "Paired" arguments in Agreement calculation 
#'                         Suppression of "number of non-repetability/reproductivity" graph ; by: Antonin PUGET
#' Version 4 - 27/03/2025: Add Global Agreement calculation and datatable visualisation
#'                         Add Agreement per descriptors/hair-dresser and datatable visualisation ; by: Antonin PUGET
#' Version 5 - 16/04/2025: Add agreement line graph
#'                         Agreement panel structure
#'                         Correct evolutivity dataframe "donnee_repro" 
#'                         Add standard deviation in global agreement df (mean_sd_prod) ; by: Antonin PUGET
#' Version 6 - 24/04/2025: Overall PCA graphs for ind & var added
#'                         PCA graph for each product added
#'                         Replacement of “r_get_donnee_repet” and “r_get_donnee_repro” EventReactive by Reactive ; by: Antonin PUGET
#'                       

list.of.packages <- c("dplyr", "tidyr", "ggplot2", "kableExtra", "knitr", "lme4", "irr", "plotly", "DT", "FactoMineR", "factoextra", "RColorBrewer", "TOSTER", "ggrepel", "viridis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(lme4)
library(irr)
library(plotly)
library(DT)
library(viridis)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(TOSTER)
library(tibble)
library(reshape2)

# Charger les données
file_path <- "J:/Fizz/FizzData/CAPILLAIRES_FIZZ3/2024/Perfs 3 coiffeuses/donnees_perfs_capi_2024_3coiffeuses.csv"
data <- read.csv2(file_path, sep = ";")

# Convertir les colonnes en types appropriés
data <- data %>%
  mutate(across(starts_with("OPACITE0"):starts_with("MISEENPLACE2"), as.numeric))
lst_des <- colnames(data)[9:ncol(data)]

# Calculer la moyenne pour chaque combinaison de CJ, ProductName par descriptor
mean_data <- data %>%
  pivot_longer(cols = OPACITE0:MISEENPLACE2, names_to = "Descriptor", values_to = "Value") %>%
  group_by(CJ, ProductName, Descriptor) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()

mean_data_pca<-mean_data

mean_data_pivot <- mean_data %>% pivot_wider(names_from = "CJ", values_from = "Mean")
mean_data_pivot$ProductDescriptor <- apply(mean_data_pivot,1,function(x){paste(x[1],x[2],sep="-")})

mean_data <- mean_data %>%
  unite(ProductDescriptor, ProductName, Descriptor, sep = "-") %>%
  select(CJ, ProductDescriptor, Mean)

mean_data <- mean_data %>% pivot_wider(names_from = "CJ", values_from = "Mean")

# df with mean & sd
mean_sd_data <- data %>%
  pivot_longer(cols = OPACITE0:MISEENPLACE2, names_to = "Descriptor", values_to = "Value") %>%
  group_by(CJ, ProductName, Descriptor) %>%
  summarise(
    Mean = round(mean(Value, na.rm = TRUE), 4), 
    Sd = round(sd(Value, na.rm = TRUE), 4),
    .groups = 'drop'
  ) %>%
  ungroup()
mean_sd_data <- mean_sd_data %>%
  pivot_wider(
    names_from = CJ,
    values_from = c(Mean, Sd),
    names_glue = "{CJ}_{.value}"
  )
mean_sd_data <- mean_sd_data %>%
  select(ProductName, Descriptor, 
         order(names(mean_sd_data)))

# df for PCA
mean_data_pca$Product_Judge <- apply(mean_data_pca,1,function(x){paste(x[1],x[2],sep="-")})
mean_data_pca_pivot <- pivot_wider(mean_data_pca, names_from = "Descriptor", values_from = "Mean")
mean_data_pca_pivot <- column_to_rownames(mean_data_pca_pivot, var = "Product_Judge")


# Partie UI
ui <- fluidPage(
  titlePanel("P'HairF"),
  sidebarLayout(
    sidebarPanel(
      numericInput("equivalence_margin","Equivalence margin",10), # True difference between the group means lies within [-eqm,eqm]
      checkboxInput("dynamic_eqb","Dynamic margin:", FALSE), #min(input$equivalence_margin,max(sd(group1),sd(group2)))
      selectInput("coif", "Sélection Juge :", choices = unique(data$CJ),multiple = FALSE),
      selectInput("choix_des", "Selection Descripteur:", choices = lst_des,multiple = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Global",
                 radioButtons("seuil_nr", "Seuil du nombre de répétition vérifiées", choices = list("1 Répétition" = 1, "2 Répétitions" = 2),
                              selected = 2),
                 radioButtons("seuil_prod", "Seuil du nombre de produits vérifiés pour la reproductivité", choices = list("2 Produits" = 2, "3 Produits" = 3, "4 Produits" = 4),
                              selected = 3),
                 plotlyOutput("repro_repet"),
                 tags$br(),
                 tags$h3("Accord :"),
                 tags$h4("Accord global entre les coiffeuses ; EM/1.5"),
                 DT::dataTableOutput("rslt_acc_global"),
                 tags$br(),
                 tags$h4("Accord par descripteur/produit entre les coiffeuses ; EM*1.5"),
                 DT::dataTableOutput("rslt_acc_des"),
                 tags$br(),
                 tags$h3("Reproductivité : Nombre de produits ayant été évalués de manière reproductible (max = 4)"),
                 DT::dataTableOutput("sum_repro"),
                 tags$br(),
                 tags$h3("Répétabilité : Nombre de produits ayant été évalués de manière répétable (max = 2)"),
                 DT::dataTableOutput("sum_repet")
                 
        ),
        tabPanel("Répétabilité",
                 selectInput("nr", "Selection de la répétition", choices = unique(data$NR), selected = unique(data$NR)[1], multiple = FALSE),
                 tags$h4("Notes de la répétition C-Cbis pour coiffeuse/descripteur/produit"),
                 DT::dataTableOutput("donnee_repet"),
                 tags$h4("t-TOST sur les 6 notations sur la coiffeuse/descripteur/produit selectionné pour les 2 répétitions"),
                 plotOutput("res_TOST_repet"),
                 tags$br(),
                 tags$h4("Résultats t-TOST sur  coiffeuse/répétition, pour chaque descripteur"),
                 DT::dataTableOutput("results_repet"),
                 tags$h3("Resumé"),
                 verbatimTextOutput("res_repet")
        ),
        tabPanel("Reproductivité",
                 selectInput("prod", "Selection du produit :", choices = unique(data$ProductName), selected = unique(data$ProductName), multiple = FALSE),
                 tags$h4("Notes de la reproductibilité R1-R2 pour coiffeuse/descripteur/produit"),
                 DT::dataTableOutput("donnee_repro"),
                 tags$h4("t-TOST sur les 6 notations sur la coiffeuse/descripteur/produit selectionné pour les 2 reproductions"),
                 plotOutput("res_TOST_repro"),
                 tags$br(),
                 tags$h4("Résultats t-TOST sur  coiffeuse/produit, pour chaque descripteur"),
                 DT::dataTableOutput("results_repro"),
                 tags$h3("Resumé"),
                 verbatimTextOutput("res_repro")
        ),
        tabPanel("Accord",
                 tags$h3("Comparaison globale entre 2 coiffeuses ; EM/1.5"),
                 tags$br(),
                 DT::dataTableOutput("rslt_acc_global_2"),
                 selectInput("coif_com", "Selection de la coiffeuse à comparer", choices = unique(data$CJ), selected = "Marina"),
                 plotOutput("res_TOST_agree"),
                 selectInput("product", "Selection du produit", choices = unique(data$ProductName)),
                 plotOutput("line_graph_acc"),
                 tags$h4("Accord par descripteur/produit entre les coiffeuses ; EM*1.5"),
                 DT::dataTableOutput("rslt_acc_des_2"),
                 tags$br(),
                 tags$h3("Note moyenne et ecart-type pour chaque produit, sur chaque descripteur pour chacune des coiffeuses"),
                 DT::dataTableOutput("mean_sd_prod"),
                 tags$br(),
                 
                 tags$h3("PCA"),
                 tags$h4("Global"),
                 selectInput("habillage","Selection d'un habillage :",choices = c("ProductName", "CJ"), selected = "CJ"),
                 plotOutput("pca_plot_ind"),
                 plotOutput("pca_plot_var"),
                 tags$h4("By Descriptors"),
                 selectInput("prod_pca","Sélection d'un produit :", choices = unique(data$ProductName)),
                 plotOutput("pca_prod_plot")
        )
      )
    )
  )
)

# Server part
server <- function(input, output, session) {
  # Fonction réactive pour la répétabilité ####
  r_repet <- reactive({
    lst_coif <- unique(data$CJ)
    res_repetibility <- list()
    res_repet_par_des <- as.data.frame(matrix(ncol=3,nrow=length(lst_des)))
    colnames(res_repet_par_des) <- lst_coif
    row.names(res_repet_par_des) <- lst_des
    
    for (coif in lst_coif){
      res_nr <- list()
      res_repet_par_des[[coif]] <- rep(0, length(lst_des))
      for (nr in unique(data$NR)){
        tab <- as.data.frame(matrix(nrow = length(lst_des), ncol = 3))
        row.names(tab) <- lst_des
        colnames(tab) <- c("t-test", "TOST Lower", "TOST Upper")
        for (des in lst_des){
          data_ind <- data %>%
            filter(CJ == coif, NR==nr) %>%
            select(ProductName, all_of(des))
          
          group1 <- data_ind[data_ind$ProductName == "C_SH_SNR", des]
          group2 <- data_ind[data_ind$ProductName == "Cbis_SH_SNR", des]
          
          if (input$dynamic_eqb){
            equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
          }else{
            equivalence_margin_ <- input$equivalence_margin
          }
          
          if (var(group1) == 0 & var(group2) == 0) {
            cat("RPT: All values are equal in one or both groups. Jitter added to perform t_TOST.\n")
            group1 <- group1 + rnorm(length(group1), mean = 0, sd = 1e-8)
            group2 <- group2 + rnorm(length(group2), mean = 0, sd = 1e-8)
            
            tost_result <- t_TOST(x= group1, y= group2, 
                                  eqb = equivalence_margin_,
                                  alpha = 0.05)
            tab[des,] <- tost_result[["TOST"]][["p.value"]]
            
          } else {
            tost_result <- t_TOST(x= group1, y= group2, 
                                  eqb = equivalence_margin_,
                                  alpha = 0.05)
            tab[des,] <- tost_result[["TOST"]][["p.value"]]
          }
        }
        res_nr[[nr]] <- tab
        res_repet_par_des[coif] <- res_repet_par_des[coif]+as.numeric(tab$`TOST Lower` < 0.05 & tab$`TOST Upper` < 0.05)
      }
      
      res_repetibility[[coif]] <- res_nr
    }
    return(list(res_repetibility,res_repet_par_des))
  })
  
  
  # Fonction réactive pour la reproductibilité
  
  r_repro <- reactive({
    lst_coif <- unique(data$CJ)
    lst_prod <- unique(data$ProductName)
    
    res_reproductivity <- list()
    res_repro_par_des <- as.data.frame(matrix(ncol=3,nrow=length(lst_des)))
    colnames(res_repro_par_des) <- lst_coif
    row.names(res_repro_par_des) <- lst_des
    
    for (coif in lst_coif){
      res_prod <- list()
      #create a list to stock the number of product is equivalent by descriptor
      res_repro_par_des[[coif]] <- rep(0, length(lst_des))
      names(res_repro_par_des[[coif]]) <- lst_des
      for (prod in lst_prod){
        tab <- as.data.frame(matrix(nrow = length(lst_des), ncol = 3))
        row.names(tab) <- lst_des
        colnames(tab) <- c("t-test", "TOST Lower", "TOST Upper")
        for (des in lst_des){
          data_ind <- data %>%
            filter(CJ == coif, ProductName==prod) %>%
            select(ProductName, all_of(des),NR)
          
          group1 <- data_ind[data_ind$NR==1, des]
          group2 <- data_ind[data_ind$NR==2, des]
          if (input$dynamic_eqb){
            equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
          }else{
            equivalence_margin_ <- input$equivalence_margin
          }
          
          if (var(group1) == 0 & var(group2) == 0) {
            cat("RPR: All values are equal in one or both groups. Jitter added to perform t_TOST.\n")
            group1 <- group1 + rnorm(length(group1), mean = 0, sd = 1e-8)
            group2 <- group2 + rnorm(length(group2), mean = 0, sd = 1e-8)
            
            tost_result <- t_TOST(x= group1, y= group2, 
                                  eqb = equivalence_margin_,
                                  alpha = 0.05)
            tab[des,] <- tost_result[["TOST"]][["p.value"]]
          } else {
            
            tost_result <- t_TOST(x=group1, y=group2,
                                  eqb = equivalence_margin_,
                                  alpha = 0.05)
            tab[des,] <- tost_result[["TOST"]][["p.value"]]
            
          }
        }
        #Add to res if the product is equivalent after 2 NR
        res_repro_par_des[coif] <- res_repro_par_des[coif]+as.numeric(tab$`TOST Lower` < 0.05 & tab$`TOST Upper` < 0.05)
        
        res_prod[[prod]] <- tab
      }
      res_reproductivity[[coif]] <- res_prod
    }
    
    return(list(res_reproductivity,res_repro_par_des))
  })
  
  
  # Fonction réactive pour l'accord par descripteur
  
  r_des_acc <- reactive({
    # Identifier les coiffeuses et produits uniques
    coiffeuses <- unique(data$CJ)
    produits <- unique(data$ProductName)
    descripteurs <- colnames(data)[9:ncol(data)]  # Colonnes des descripteurs sensoriels
    
    # Initialiser le data frame pour stocker les résultats
    df_agr_desc <- data.frame(
      Coiffeuse1 = character(),
      Coiffeuse2 = character(),
      Produit = character(),
      Descripteur = character(),
      Max_P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Boucles pour chaque paire de coiffeuses, chaque produit et chaque descripteur
    for (produit in produits) {
      for (i in 1:(length(coiffeuses) - 1)) {
        for (j in (i + 1):length(coiffeuses)) {
          coif1 <- coiffeuses[i]
          coif2 <- coiffeuses[j]
          
          for (descripteur in descripteurs) {
            group1 <- data[data$CJ == coif1 & data$ProductName == produit, descripteur]
            group2 <- data[data$CJ == coif2 & data$ProductName == produit, descripteur]
            
            if (input$dynamic_eqb){
              equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
            }else{
              equivalence_margin_ <- input$equivalence_margin
            }
            
            if (var(group1, na.rm = TRUE) == 0 & var(group2, na.rm = TRUE) == 0) {
              #df_agr_desc <- rbind(df_agr_desc, data.frame(Coiffeuse1 = coif1, Coiffeuse2 = coif2, Produit = produit, Descripteur = descripteur, Max_P_Value = -1))
              cat("ACC_D: All values are equal in one or both groups. Jitter added to perform t_TOST.\n")
              group1 <- group1 + rnorm(length(group1), mean = 0, sd = 1e-8)
              group2 <- group2 + rnorm(length(group2), mean = 0, sd = 1e-8)
              
              tost_result <- t_TOST(x= group1, y= group2, 
                                    eqb = equivalence_margin_,
                                    alpha = 0.05)
            } else {
              #cor_group1_group2 <- cor(group1, group2, use = "complete.obs")
              
              tost_result <- t_TOST(x = group1, y = group2,
                                    eqb = equivalence_margin_*1.5,
                                    #paired = TRUE,
                                    # r12 = cor_group1_group2,
                                    alpha = 0.05)
            }
            p_value_upper <- tost_result$TOST$p.value[3]
            p_value_lower <- tost_result$TOST$p.value[2]
            df_agr_desc <- rbind(df_agr_desc, data.frame(Coiffeuse1 = coif1, Coiffeuse2 = coif2, Produit = produit, Descripteur = descripteur,
                                                         Max_P_Value = round(max(p_value_upper, p_value_lower), 8)))
          }
        }
      }
    }
    df_agr_desc_wide <- dcast(df_agr_desc, Coiffeuse1 + Coiffeuse2 + Descripteur ~ Produit, value.var = "Max_P_Value")
    return(df_agr_desc_wide)
  })
  
  
  # Fonction réactive pour l'accord global:
  r_acc_global <- reactive({
    
    results_df <- data.frame(
      Coiffeuse1 = character(),
      Coiffeuse2 = character(),
      Max_p_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    col_names <- colnames(mean_data[-1,])
    col_names <- colnames(mean_data[,-1])
    
    for (i in 1:(length(col_names) - 1)) {
      for (j in (i + 1):length(col_names)) {
        coif1 <- col_names[i]
        coif2 <- col_names[j]
        
        # Extraire les données des deux colonnes
        group1 <- mean_data[[coif1]]
        group2 <- mean_data[[coif2]]
        
        # Vérifier que les variances ne sont pas nulles
        if (var(group1) == 0 & var(group2) == 0) {
          results[[paste(coif1, coif2, sep = "_vs_")]] <- NA
        } else {
          cor_group1_group2 <- cor(group1, group2)
          if (input$dynamic_eqb){
            equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
          }else{
            equivalence_margin_ <- input$equivalence_margin
          }
          
          # Effectuer le test TOST
          tost_result <- t_TOST(x = group1, y = group2,
                                eqb = equivalence_margin_/1.5,
                                paired = TRUE,
                                alpha = 0.05,
                                r12 = cor_group1_group2)
          
          p_value_upper <- tost_result$TOST$p.value[3]
          p_value_lower <- tost_result$TOST$p.value[2]
          
          # Stocker les p-values dans le data frame
          results_df <- rbind(results_df, data.frame(Coiffeuse1 = coif1, Coiffeuse2 = coif2,
                                                     Max_p_Value = round(max(p_value_upper, p_value_lower), 5)
                                                     )
                              )
        }
      }
    }
    return(results_df)
  })
  
  
  # 1. Table recap ####
  
  # Affichage des données de reproductibilité
  
  output$donnee_repro <- DT::renderDataTable({
    lst_coif <- unique(data$CJ)
    lst_prod <- unique(data$ProductName)
    
    res_reproductivity <- list()
    res_repro_par_des <- as.data.frame(matrix(ncol=3,nrow=length(lst_des)))
    colnames(res_repro_par_des) <- lst_coif
    row.names(res_repro_par_des) <- lst_des
    
    for (coif in lst_coif){
      res_prod <- list()
      #create a list to stock the number of product is equivalent by descriptor
      res_repro_par_des[[coif]] <- rep(0, length(lst_des))
      names(res_repro_par_des[[coif]]) <- lst_des
      for ( prod in lst_prod){
        tab <- as.data.frame(matrix(nrow = length(lst_des), ncol = 3))
        row.names(tab) <- lst_des
        colnames(tab) <- c("t-test", "TOST Lower", "TOST Upper")
        for (des in lst_des){
          data_ind <- data %>%
            filter(CJ == coif, ProductName==prod) %>%
            select(ProductName, all_of(des),NR)
          
          group1 <- data_ind[data_ind$NR==1, des]
          group2 <- data_ind[data_ind$NR==2, des]
          if (var(group1) == 0 & var(group2) == 0) {
            cat("All values are equal in one or both groups. Statistical tests cannot be performed.\n")
            tab[des,] <- rep(0, 3)
          } else {
            if (input$dynamic_eqb){
              equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
            }else{
              equivalence_margin_ <- input$equivalence_margin
            }
            tost_result <- t_TOST(x=group1, y=group2,
                                  eqb = equivalence_margin_,
                                  alpha = 0.05)
            tab[des,] <- tost_result[["TOST"]][["p.value"]]
            
          }
        }
        #Add to res if the product is equivalent after 2 NR
        res_repro_par_des[coif] <- res_repro_par_des[coif]+as.numeric(tab$`TOST Lower` < 0.05 & tab$`TOST Upper` < 0.05)
        
        res_prod[[prod]] <- tab
      }
      res_reproductivity[[coif]] <- res_prod
    }
  })
  
  # Résumé de la reproductibilité
  output$sum_repro <- DT::renderDataTable({
    datatable(r_repro()[[2]]) %>%
      formatStyle(
        columns = unique(data$CJ),
        color = styleInterval(c(0,2), c('red', "#a55e00", 'green')),
        backgroundColor = styleInterval(c(0,2), c('lightpink', "#ffdcb0", 'lightgreen'))
        )
  })
  
  # Résumé de la répétibilité
  output$sum_repet <- DT::renderDataTable({
    datatable(r_repet()[[2]]) %>%
      formatStyle(
        columns = unique(data$CJ),
        color = styleInterval(c(0,1), c('red', "#a55e00", 'green')),
        backgroundColor = styleInterval(c(0,1), c('lightpink', "#ffdcb0", 'lightgreen'))
        )
  })
  
  # Résumé de l'accord
  output$rslt_acc_global <- DT::renderDataTable({
    df <- r_acc_global()
    datatable(df, options = list(pageLength = 10)) %>%
      formatStyle('Max_p_Value',
                  color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
                  backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffdcb0", 'lightpink'))
      )
  })
  
  output$rslt_acc_des <- DT::renderDataTable({
    df <- r_des_acc()
    datatable(df, options = list(pageLength = 10)) %>%
      formatStyle(
        columns = names(df)[4:ncol(df)],  
        color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffdcb0", 'lightpink'))
      )
  })
  
  # Graphique de répétibilité et reproductibilité
  
  output$repro_repet <- renderPlotly({
    repet <- r_repet()[[2]]
    repro <- r_repro()[[2]]
    count_repet <- apply(repet,2,function(x){sum(x>=input$seuil_nr)})
    count_repro <- apply(repro,2,function(x){sum(x>=input$seuil_prod)})
    # Combine the results into a data frame
    combined_df <- data.frame(
      Individual = names(count_repet),
      Sum_Repet = count_repet,
      Sum_Repro = count_repro
    )
    
    # Create the plot
    p <- ggplot(combined_df, aes(x = Sum_Repro, y = Sum_Repet, color = Individual, label = Individual)) +
      geom_point(size = 3) +
      scale_color_viridis(discrete = TRUE) +
      geom_hline(yintercept = length(lst_des), size = 0.3, color = "green") + 
      geom_vline(xintercept = length(lst_des), size = 0.3, color = "green") + 
      labs(title = "Nombre de descripteurs Répétables/Reproductibles",
           x = "Descripteurs Reproductibles",
           y = "Descripteurs Répétables") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)
      )+
      xlim(0, length(lst_des)+1) +
      ylim(0, length(lst_des)+1)
    
    ggplotly(p)%>%
      layout(
        annotations = list(
          list(
            x = length(lst_des) + 0.5,
            y = 11,
            text = "Seuil maximum de reproductibilité",
            showarrow = FALSE,
            font = list(color = "green",size = 15, family = "Arial"),
            textangle = -90
          ),
          list(
            x = 3.5,
            y = length(lst_des) + 1,
            text = "Seuil maximum de répétabilité",
            showarrow = FALSE,
            font = list(color = "green", size = 15, family = "Arial"),
            textangle = 0
          )
        )
      )
  })
  
  # 2. Repetability####
  # Affichage des résultats de répétibilité
  
  output$results_repet <- DT::renderDataTable({
    datatable(r_repet()[[1]][[input$coif]][[as.numeric(input$nr)]]) %>%
      formatStyle(
        columns = c("TOST Lower", "TOST Upper"),
        valueColumns = c("TOST Lower", "TOST Upper"),
        color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffdcb0", 'lightpink'))
      )%>%
      formatStyle(
        columns = c("t-test"),
        valueColumns = c("t-test"),
        color = styleInterval(c(0.05,0.1), c('red', "#a55e00", 'green')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightpink', "#ffdcb0", 'lightgreen'))
      )
  })
  
  output$res_repet <- renderText({
    res <- r_repet()[[1]]
    selected_tab <- res[[input$coif]][[as.numeric(input$nr)]]
    equivalent_count <- sum(selected_tab$`TOST Lower` < 0.05 & selected_tab$`TOST Upper` < 0.05)
    non_discriminant_count <- sum(selected_tab$`t-test` < 0.05)
    
    print(paste("Equivalent:",equivalent_count, "\n Non Discriminant:", non_discriminant_count))
  })
  
  r_get_donnee_repet <- reactive({
    coiffeuse <- input$coif
    des <- input$choix_des
    nr <- input$nr
    data_ind <- data%>%
      filter(CJ==coiffeuse, NR%in%nr) %>%
      select(ProductName,all_of(des))
    
    group1 <- data_ind[data_ind$ProductName=="C_SH_SNR",des]
    group2 <- data_ind[data_ind$ProductName=="Cbis_SH_SNR",des]
    if (var(group1) == 0 & var(group2) == 0) {
      cat("RPT_2: All values are equal in one or both groups. Jitter added to perform t_TOST.\n")
      group1 <- group1 + rnorm(length(group1), mean = 0, sd = 1e-8)
      group2 <- group2 + rnorm(length(group2), mean = 0, sd = 1e-8)
    }
    return(list(group1,group2))
  })
  
  output$donnee_repet <- DT::renderDataTable({
    
    group1 <- r_get_donnee_repet()[[1]]
    group2 <- r_get_donnee_repet()[[2]]
    
    data_compare <- as.data.frame(tibble("C_SH_SNR"=group1, "Cbis_SH_SNR"= group2))
    data_compare['mean',] <- apply(data_compare,2,mean)
    data_compare['sd',] <- apply(data_compare[1:(nrow(data_compare)-1),],2,function(x){sqrt(var(x))})
    data_compare
  })
  
  output$res_TOST_repet <- renderPlot({
    
    group1 <- r_get_donnee_repet()[[1]]
    group2 <- r_get_donnee_repet()[[2]]
    
    if (input$dynamic_eqb){
      equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
    }else{
      equivalence_margin_ <- input$equivalence_margin
    }
    tost_result <- t_TOST(x=group1, y=group2, 
                          eqb = equivalence_margin_,
                          alpha = 0.05)
    plot(tost_result, type = "cd")
  })
  
  #3. Reproductivity####
  # Affichage des résultats de réproductivité
  
  output$results_repro <- DT::renderDataTable({
    datatable(r_repro()[[1]][[input$coif]][[input$prod]]) %>%
      formatStyle(
        columns = c("TOST Lower", "TOST Upper"),
        valueColumns = c("TOST Lower", "TOST Upper"),
        color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffca70", 'lightpink'))
      )%>%
      formatStyle(
        columns = c("t-test"),
        valueColumns = c("t-test"),
        color = styleInterval(c(0.05,0.1), c('red', "#a55e00", 'green')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightpink', "#ffca70", 'lightgreen'))
      )
  })
  
  output$res_repro <- renderText({
    res <- r_repro()[[1]]
    selected_tab <- res[[input$coif]][[input$prod]]
    equivalent_count <- sum(selected_tab$`TOST Lower` < 0.05 & selected_tab$`TOST Upper` < 0.05)
    non_discriminant_count <- sum(selected_tab$`t-test` < 0.05 )
    
    print(paste("Equivalent:",equivalent_count, "\n Non Discriminant:", non_discriminant_count))
  })
  
  r_get_donnee_repro <- reactive({
    coiffeuse <- input$coif
    des <- input$choix_des
    Prod <- input$prod
    data_ind <- data%>%
      filter(CJ==coiffeuse,ProductName==Prod) %>%
      select(ProductName,all_of(des),NR)
    
    group1 <- data_ind[data_ind$NR==1,des]
    group2 <- data_ind[data_ind$NR==2,des]
    if (var(group1) == 0 & var(group2) == 0) {
      cat("RPR_2: All values are equal in one or both groups. Jitter added to perform t_TOST.\n")
      group1 <- group1 + rnorm(length(group1), mean = 0, sd = 1e-8)
      group2 <- group2 + rnorm(length(group2), mean = 0, sd = 1e-8)
    }
    return(list(group1,group2))
  })
  
  output$donnee_repro <- DT::renderDataTable({
    
    group1 <- r_get_donnee_repro()[[1]]
    group2 <- r_get_donnee_repro()[[2]]
    
    data_compare <- as.data.frame(tibble("R1"=group1, "R2"= group2))
    data_compare['mean',] <- apply(data_compare,2,mean)
    data_compare['sd',] <- apply(data_compare[1:(nrow(data_compare)-1),],2,function(x){sqrt(var(x))})
    data_compare
  })
  
  output$res_TOST_repro <- renderPlot({
    
    group1 <- r_get_donnee_repro()[[1]]
    group2 <- r_get_donnee_repro()[[2]]
    
    if (input$dynamic_eqb){
      equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
    }else{
      equivalence_margin_ <- input$equivalence_margin
    }
    tost_result <- t_TOST(x=group1, y=group2, 
                          eqb= equivalence_margin_,
                          alpha = 0.05)
    plot(tost_result, type = "cd")
    
  })
  
  # 4. Agreement####
  
  output$mean_sd_prod <- DT::renderDataTable({
    mean_sd_data
  })
  
  output$rslt_acc_global_2 <- DT::renderDataTable({
    df <- r_acc_global()
    datatable(df, options = list(pageLength = 10)) %>%
      formatStyle('Max_p_Value',
                  color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
                  backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffca70", 'lightpink'))
      )
  })
  
  
  output$res_TOST_agree <- renderPlot({
    group1 <- mean_data[input$coif_com][[1]]
    group2 <- mean_data[input$coif][[1]]
    if (var(group1) == 0 & var(group2) == 0) {
      cat("All values are equal in one or both groups. Statistical tests cannot be performed.\n")
      res_acc[[coif]] <- rep(0, 3)
    } else {
      # Calculate the correlation between group1 and group2
      cor_group1_group2 <- cor(group1, group2)
      
      if (input$dynamic_eqb){
        equivalence_margin_ <- min(input$equivalence_margin,sd(group1),sd(group2))
      }else{
        equivalence_margin_ <- input$equivalence_margin
      }
      tost_result <- t_TOST(x=group1, y=group2, paired = TRUE,
                            eqb = equivalence_margin_/1.5,
                            alpha = 0.05,
                            r12 = cor_group1_group2)
      plot(tost_result, type = "cd")
    }
  })
  
  output$line_graph_acc<- renderPlot({
    
    lst_coif <- unique(data$CJ)
    data_long <- pivot_longer(mean_data_pivot, cols = all_of(lst_coif), names_to = "Coiffeuse", values_to = "Note")
    filtered_data <- subset(data_long, ProductName == input$product)
    
    ggplot(filtered_data, aes(x = Descriptor, y = Note, color = Coiffeuse, group = Coiffeuse)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = paste("Notes moyenne des Coiffeuses pour le Produit", input$product),
           x = "Descripteur",
           y = "Note") +
      scale_color_viridis_d() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Taille du texte des axes x
        axis.text.y = element_text(size = 12),  # Taille du texte des axes y
        axis.title = element_text(size = 14),  # Taille des titres des axes
        legend.text = element_text(size = 12),  # Taille du texte de la légende
        legend.title = element_text(size = 14)  # Taille du titre de la légende
      )
  })
  
  output$rslt_acc_des_2 <- DT::renderDataTable({
    df <- r_des_acc()
    datatable(df, options = list(pageLength = 10)) %>%
      formatStyle(
        columns = names(df)[4:ncol(df)],  # Appliquer la coloration aux colonnes des produits
        color = styleInterval(c(0.05,0.1), c('green', "#a55e00", 'red')),
        backgroundColor = styleInterval(c(0.05,0.1), c('lightgreen', "#ffca70", 'lightpink'))
      )
  })
  
  
  ###PCAs###
  
  # ACP sur l'ensemble des produits
  
  output$pca_plot_ind <- renderPlot({
    res_acp_global<-PCA(mean_data_pca_pivot, scale.unit = FALSE, quali.sup = c(1,2), graph = FALSE)
    fviz_pca_ind(res_acp_global, repel = TRUE, habillage = input$habillage ,  palette = "aaas", ggtheme = theme_light())
  })
  
  output$pca_plot_var <- renderPlot({
    res_acp_global<-PCA(mean_data_pca_pivot, scale.unit = FALSE, quali.sup = c(1,2), graph = FALSE)
    fviz_pca_var(res_acp_global, repel = TRUE, col.var = "darkred", ggtheme = theme_light())
  })
  
  
  # PCA sur un produit spécifique
  
  output$pca_prod_plot <- renderPlot({
    # Filtrer les données pour le descripteur spécifique
    data_filtered <- mean_data_pca_pivot %>%
      filter(ProductName == input$prod_pca) 
    
    rownames(data_filtered) <- NULL
    data_filtered <- column_to_rownames(data_filtered, var = "CJ")
    
    pca_result <- PCA(data_filtered[,-1], scale.unit = FALSE, ncp = 5, graph = FALSE)
    
    fviz_pca_biplot(pca_result, repel = TRUE, palette = "aaas", alpha.var = 0.7, label= "var")+
      geom_point(aes(fill = rownames(data_filtered), color = rownames(data_filtered), shape = rownames(data_filtered)), size=4)+
      guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL), shape = guide_legend(title = NULL))
    
    # # Extraire les coordonnées des variables
    # var_coords <- as.data.frame(pca_result$var$coord)
    # var_coords$varname <- rownames(var_coords)
    # 
    # p <- fviz_pca_biplot(pca_result,
    #                      pointshape = 21,
    #                      pointsize = 2,
    #                      palette = "aaas",
    #                      label = "none",
    #                      repel = TRUE) +
    #   ggtitle("PCA des coiffeuses par produits") +
    #   xlab("Dimension 1") +
    #   ylab("Dimension 2") +
    #   theme_minimal() +
    #   theme(legend.position = "bottom") +
    #   geom_point(aes(color = rownames(data_filtered), text = rownames(data_filtered))) + # Colorier les points par ProductName
    #   geom_segment(data = var_coords, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2, text = varname), arrow = arrow(length = unit(0.2, "cm"))) # Ajouter les lignes des variables avec tooltips
    # 
    # ggplotly(p, tooltip = "text") # Spécifier que les tooltips doivent afficher le texte
  })
}

# Run the application
shinyApp(ui = ui, server = server)