
library(tidyverse)
library(janitor)
library(LBSPR)
library(dplyr)
library(kableExtra)
library(ggalt)
library(ggrepel)
library(knitr)
library(directlabels)
library(gridExtra)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(cowplot)

## LBSPR function
lh_function <- function(row) {
  # Create a new LB_pars object for each species
  MyPars <- new("LB_pars")
  MyPars@Species<-row['species']
  MyPars@Linf <- as.numeric(row['Linf_FL'])
  MyPars@L50 <- as.numeric(row['L50'])
  MyPars@L95 <- as.numeric(row['L95'])
  MyPars@MK <- as.numeric(row['M/K'])
  MyPars@BinWidth <- as.numeric(row['BinWidth'])
  MyPars@Steepness<-0.99
  MyPars@L_units <- "mm"
  MyPars@Walpha <- as.numeric(row['LW_A'])
  MyPars@Walpha_units <- "g"
  MyPars@Wbeta <- as.numeric(row['LW_B'])
  MyPars@FecB <- as.numeric(row['LW_B'])
  MyPars@BinMin <- 0
  #Setup place holder values for these parameters, we will change these later
  MyPars@SL50 <- as.numeric(row['SL50'])
  MyPars@SL95 <- as.numeric(row['SL95'])
  MyPars@FM<-1
  
  return(list(MyPars=MyPars, CurrentLc=as.numeric(row['CurrentLc_mm'])))
}

## YPR Function
ypr_function<-function(MyParsList, SL_options, FM_options){
  rw<-NROW(MyParsList)
  YPR<-list()
  SPR<-list()
  EU<-list()
  
  for (k in 1:rw){
    
    #Eumetric analysis
    EU[[k]]<-list()
    Lc<-seq(0.1*MyParsList[[k]]$MyPars@L50, 0.95*MyParsList[[k]]$MyPars@Linf, 5)
    F_M<-seq(0, 4, 0.1)
    
    #x = seq(0, 1, length.out = nrow(z)),
    #y = seq(0, 1, length.out = ncol(z)),
    #z
    
    SPR_EU<-matrix(nrow=NROW(F_M), ncol=NROW(Lc))
    YPR_EU<-matrix(nrow=NROW(F_M), ncol=NROW(Lc))
    for (i in 1:NROW(F_M)){
      for (j in 1:NROW(Lc)){
        tmpPars<-MyParsList[[k]]$MyPars
        tmpPars@FM<-F_M[i]
        tmpPars@SL50 <- Lc[j]
        tmpPars@SL95 <-Lc[j]+1
        tmpSim <- LBSPRsim(tmpPars, verbose=FALSE)
        SPR_EU[i,j]=tmpSim@SPR
        YPR_EU[i,j]=tmpSim@YPR
      }
    }
    x<-which(YPR_EU==max(YPR_EU), arr.ind=TRUE)[1,]
    optYield<-YPR_EU[x[1],x[2]]
    YPR_EU<-YPR_EU/optYield
    EU[[k]]$SPR_EU<-SPR_EU
    EU[[k]]$YPR_EU<-YPR_EU
    EU[[k]]$Lc<-Lc
    EU[[k]]$F_M<-F_M
    EU[[k]]$L50<-MyParsList[[k]]$MyPars@L50
    
    #Factorial analysis
    YPR[[k]]<-data.frame()
    SPR[[k]]<-data.frame()

    # SL filtering
    if(!is.na(MyParsList[[k]]$CurrentLc)) {
      SL_tmp<-round(c(SL_options*MyParsList[[k]]$MyPars@L50, MyParsList[[k]]$CurrentLc),0)
      SL_names<-c(paste0(SL_options, " x Lm"), "Current size limit")
    }
    if(is.na(MyParsList[[k]]$CurrentLc)) {
      SL_tmp<-round(SL_options*MyParsList[[k]]$MyPars@L50,0)
      SL_names<-paste0(SL_options, " x Lm")
    }
    Keep<-c(SL_tmp < 0.95*MyParsList[[k]]$MyPars@Linf)
    SL_options_mult<-SL_options[Keep]
    SL_options_mm<-SL_tmp[Keep]
    SL_names<-SL_names[Keep]
    SL_options_inch <- round(SL_options_mm/25.4,1)
    
    #Loop over FM options
    for (i in 1:NROW(FM_options)){
      tmpYPR<-data.frame()
      tmpSPR<-data.frame()
      #loop over Lc options
      for (j in 1:NROW(SL_options_mm)){
        tmpPars<-MyParsList[[k]]$MyPars
        tmpPars@FM<-FM_options[i]
        tmpPars@SL50 <- SL_options_mm[j]
        tmpPars@SL95 <- SL_options_mm[j]+1
        tmpSim <- LBSPRsim(tmpPars, verbose=FALSE)
        tmpYPR<-rbind(tmpYPR, list(Name=SL_names[j], Lc_mm=SL_options_mm[j], Lc_inch=SL_options_inch[j], Lc_Linf=SL_options_mm[j]/MyParsList[[k]]$MyPars@Linf, FM=FM_options[i], YPR=tmpSim@YPR))
        tmpSPR<-rbind(tmpSPR, list(Name=SL_names[j], Lc_mm=SL_options_mm[j], Lc_inch=SL_options_inch[j], FM=FM_options[i], SPR=tmpSim@SPR))
      }
      #Save to master list
      YPR[[k]]<-rbind(YPR[[k]], tmpYPR)
      SPR[[k]]<-rbind(SPR[[k]], tmpSPR)
    }
    YPR[[k]]$YPR<-unlist(lapply(YPR[[k]]$YPR/optYield, min, c(x,1)))
    
    
    
  }
  return(list(YPR=YPR, SPR=SPR, EU=EU))
}


### TEXT
# Text template for species info function
species_template <- "#### Species: **%s**
###### **Hawaiian Name:** %s
###### **Common Name:** %s
###### **Family:** %s
###### **Current Minimum Size Limit (FL):** %s

"

# Species text template function
speciesText <- function(fish) {
  text <-
    cat(
      sprintf(
        species_template,
        fish$species,
        fish$hawaii_name,
        fish$common_name,
        fish$common_family,
        fish$CurrentLc_in_label
      )
    )
  
  return(text)
}

# Text template for life history parameters info function
parameters_template <- "##### Life History Parameters
###### **Loo (von Bertalanffy asymtotic size):** %s mm FL
###### **K (von Bertalanffy growth parameter):** %s per year
###### **t0 (von Bertalanffy parameter):** %s
###### **Lm (Length at maturity):** %s mm FL
###### **Lm (Length at maturity):** %s inches FL
###### **M (natural mortality rate):** %s per year
###### **Longevity:** %s years
###### **M/K:** %s
###### **Lm/Loo:** %s

"

# Parameters text template function
parametersText <- function(fish) {
  text <-
    cat(
      sprintf(
        parameters_template,
        round(fish$Linf_FL,0),
        fish$K,
        fish$t0,
        round(fish$LMAT_FL,0),
        round(fish$Lm_in,0),
        fish$M,
        fish$LONG,
        round(fish$M / fish$K, 2),
        round(fish$LMAT_FL / fish$Linf_FL, 2)
      )
    )
  
  return(text)
}

### TABLES

# SPR Table Function
tableSPR <- function(MyParsList, output, k) {
  # SPR Table
  SPR <- output$SPR[[k]]
  SPR_wide <- SPR %>%
    pivot_wider(
      id_cols = c(Name, Lc_mm, Lc_inch),
      names_from = FM,
      values_from = SPR
    )
  options(knitr.kable.NA = "")
  colnames(SPR_wide) <-
    c("Option", "mm", "inches", "Low", "Med", "High")
  nameSPR <- c(NCOL(SPR_wide))
  names(nameSPR) <-
    c(paste0(MyParsList[[k]]$MyPars@Species, " - SPR Values"))
  namesSub <- c(1, 2, (NCOL(SPR_wide) - 3))
  
  return(
    kable(
      SPR_wide,
      align = "llcccc",
      digits = 2,
      caption = names(nameSPR),
      format = "html", table.attr = "style='width:40%;'"
    ) %>%
      kable_styling(
        c("condensed", "responsive", "bordered"),
        bootstrap_options = "striped",
        full_width = T,
        position = "center",
        font_size = 12
      ) %>%
      footnote(general = "Option refers to minimum size limit specified as a multiple of length at maturity (Lm). Current size limit (where applicable) is current DAR regulation.") %>%
      column_spec(4:6, bold = TRUE) %>%
      add_header_above(c(
        "Minimum Size Limit" = 3,
        "Fishing Pressure (F/M)" = 3
      )) %>%
      column_spec(4, border_left = T)
  )
}

# YPR Table Function
tableYPR <- function(MyParsList, output, k) {
  # YPR Table
  ypr_plot <- YPR <- output$YPR[[k]]
  YPR_wide <- YPR %>%
    pivot_wider(
      id_cols = c(Name, Lc_mm, Lc_inch),
      names_from = FM,
      values_from = YPR
    )
  colnames(YPR_wide) <-
    c("Option", "mm", "inches", "Low", "Med", "High")
  options(knitr.kable.NA = "")
  nameYPR <- c(NCOL(YPR_wide))
  names(nameYPR) <-
    c(paste0(MyParsList[[k]]$MyPars@Species, " - YPR Values"))
  namesSub <- c(1, 2, (NCOL(YPR_wide) - 3))
  
  return(
    kable(
      YPR_wide,
      align = "llcccc",
      digits = 2,
      caption = names(nameYPR),
      format = "html", table.attr = "style='width:40%;'"
    ) %>%
      kable_styling(
        c("condensed", "responsive", "bordered"),
        bootstrap_options = "striped",
        full_width = T,
        position = "center",
        font_size = 12
      ) %>%
      footnote(general = "Option refers to minimum size limit specified as a multiple of length at maturity (Lm). Current size limit (where applicable) is current DAR regulation.") %>%
      column_spec(4:6, bold = TRUE) %>%
      column_spec(4, border_left = T) %>%
      add_header_above(c(
        "Minimum Size Limit" = 3,
        "Fishing Pressure (F/M)" = 3
      ))
  )
}

### CONTOUR PLOTS
# SPR Contour Function
contourSPR <- function(MyParsList, output, k) {
  EU_grid <- expand.grid(Lc = output$EU[[k]]$Lc / 25.4,
                         F_M = output$EU[[k]]$F_M)
  EU_grid$ypr <- as.vector(t(output$EU[[k]]$YPR_EU))
  EU_grid$spr <- as.vector(t(output$EU[[k]]$SPR_EU))
  
  sprLine<-matrix(nrow=dim(output$EU[[k]]$SPR_EU)[1], ncol=2)
  for (i in 1:dim(output$EU[[k]]$SPR_EU)[1]){
    x<-which(abs(output$EU[[k]]$SPR_EU[i,]-0.3)==min(abs(output$EU[[k]]$SPR_EU[i,]-0.3)))[1]
    sprLine[i,1]<-output$EU[[k]]$F_M[i]
    sprLine[i,2]<-output$EU[[k]]$Lc[x]/ 25.4
  }
  colnames(sprLine)<-c("F_M", "Lc")
  
  # SPR Contour Plot
  spr_plot <- ggplot() +
    geom_contour_filled(data=EU_grid, aes(x = F_M, y = Lc, z = spr), binwidth = 0.1, alpha = 0.8) +
    scale_fill_brewer(palette = "RdYlGn",
                      labels = c(0, " ", " ", " ", " ", 0.5, " ", " ", " ", " ", 1)) +
    geom_hline(
      yintercept = output$EU[[k]]$L50 / 25.4,
      linetype = "dashed",
      color = "black",
      size = 0.7
    ) +
    geom_text(size = 3.5,
              aes(
                0.2,
                output$EU[[k]]$L50 / 25.4,
                label = "Length at\nmaturity",
                vjust = 0.4,
                hjust = "left"
              )) +
    theme_light() +
    labs(
      y = "Minimum size limit (FL in)",
      x = "Relative Fishing Pressure (F/M)",
      title = MyParsList[[k]]$MyPars@Species,
      subtitle = "SPR Values",
      caption = "Note: \nSPR plotted across combinations of minimum size limit and fishing pressure. Solid line \ncorresponds to minimum size limit - F/M combinations that achieve 30% SPR. Labels \ncorrespond to SPR achieved for minimum size limit - F/M combinations presented in \ntable above."
    ) +
    guides(
      fill = guide_bins(
        title = "SPR Value",
        direction = "horizontal",
        title.position = "left",
        title.vjust = 0.9,
        label.position = "bottom",
        show.limits = TRUE,
        axis = FALSE,
        keywidth = unit(5.5, "mm")
      )
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      panel.border =element_blank(),
      plot.title = element_text(colour = "grey"),
      legend.position = "bottom",
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = 0)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_line(data=data.frame(sprLine), aes(x=F_M, y=Lc), size=1) 
    #geom_label(data=output$SPR[[k]][output$SPR[[k]]$Name != "Current size limit",], aes(x=FM, y=Lc_inch, label=Name), size=3)+
    #expand_limits(x=max(output$SPR[[k]]$FM[output$SPR[[k]]$Name != "Current size limit"])+0.3, y=max(output$SPR[[k]]$Lc_inch[output$SPR[[k]]$Name != "Current size limit"])+0.5)
  
  return(spr_plot)
}

# YPR Contour Function
contourYPR <- function(MyParsList, output, k) {
  EU_grid <- expand.grid(Lc = output$EU[[k]]$Lc / 25.4,
                         F_M = output$EU[[k]]$F_M)
  EU_grid$ypr <- as.vector(t(output$EU[[k]]$YPR_EU))
  EU_grid$spr <- as.vector(t(output$EU[[k]]$SPR_EU))
  
  # YPR Contour Plot
  ypr_plot <- ggplot() +
    geom_contour_filled(data=EU_grid, aes(x = F_M, y = Lc, z = ypr), binwidth = 0.1, alpha = 0.8) +
    scale_fill_brewer(palette = "RdYlGn",
                      labels = c(0, " ", " ", " ", " ", 0.5, " ", " ", " ", " ", 1)) +
    geom_hline(
      yintercept = output$EU[[k]]$L50 / 25.4,
      linetype = "dashed",
      color = "black",
      size = 0.7
    ) +
    geom_text(size = 3.5,
              aes(
                0.1,
                output$EU[[k]]$L50 / 25.4,
                label = "Length at\nmaturity",
                vjust = 0.4,
                hjust = "left"
              )) +
    theme_light() +
    labs(
      y = "Minimum size limit (inches)",
      x = "Relative Fishing Pressure (F/M)",
      title = MyParsList[[k]]$MyPars@Species,
      subtitle = "YPR Values",
      caption = "Note: \nYPR plotted across combinations of minimum size limit and fishing pressure. Labels \ncorrespond to YPR achieved for minimum size limit - F/M combinations presented in \ntable above. \n"
    ) +
    guides(
      fill = guide_bins(
        title = "YPR Value",
        direction = "horizontal",
        title.position = "left",
        title.vjust = 0.9,
        label.position = "bottom",
        show.limits = TRUE,
        axis = FALSE,
        keywidth = unit(5.5, "mm")
      )
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      panel.border =element_blank(),
      plot.title = element_text(colour = "grey"),
      legend.position = "bottom",
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = 0)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) 
    #geom_label(data=output$SPR[[k]][output$SPR[[k]]$Name != "Current size limit",], aes(x=FM, y=Lc_inch, label=Name), size=3) +
    #expand_limits(x=max(output$SPR[[k]]$FM[output$SPR[[k]]$Name != "Current size limit"])+0.3, y=max(output$SPR[[k]]$Lc_inch[output$SPR[[k]]$Name != "Current size limit"])+0.5)
  
  return(ypr_plot)
}

### PARETO PLOTS
# Pareto Gradient Function
paretoColor <- function(MyParsList, output, k,  Flevel=c(0, 4)) {
  EU_grid <- expand.grid(Lc = output$EU[[k]]$Lc / 25.4,
                         F_M = output$EU[[k]]$F_M)
  EU_grid$ypr <- as.vector(t(output$EU[[k]]$YPR_EU))
  EU_grid$spr <- as.vector(t(output$EU[[k]]$SPR_EU))
  
  # Pareto plots prep
 
  EU_tmp <- EU_grid %>%
    filter(F_M > Flevel[1],
           F_M < Flevel[2])
  values <- c(
    0,
    (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc) - 0.01,
    (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc),
    (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc),
    (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc) + 0.01,
    1
  )
  colors <- c("#A50026", "#FDAE61", "black", "#A6D96A", "#006837")
  pareto_title_one <- c(paste0(MyParsList[[k]]$MyPars@Species))
  pareto_subtitle_one <- c(paste0("SPR-YPR tradeoffs"))
  pareto_title_two <- c(paste0(MyParsList[[k]]$MyPars@Species))

  # Pareto plot - gradient
  ggplot(EU_tmp, aes(x = ypr, y = spr, colour = Lc)) +
    geom_point(alpha = 0.6) +
    scale_colour_gradientn(
      name = "Minimum size limit (FL in)",
      colors = colors,
      values = values,
      limits = c(min(EU_tmp$Lc), max(EU_tmp$Lc))
    ) +
    #ylim(0,1) +
    #xlim(0,1) +
    labs(
      y = "Spawning Potential Ratio (SPR)",
      x = "Relative Yield per Recruit (YPR)",
      title = pareto_title_one,
      subtitle = pareto_subtitle_one,
      caption = "Note: \nAll size limit and fishing pressure combinations plotted. Color gradient indicates \nminimum size limit, with black points corresponding to minimum size limit set equal to \nspecies' length at maturity. Green points are minimum size limits greater than species' \nlength at maturity."
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(colour = "grey"),
      legend.position = "bottom",
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = 0)
    ) +
    annotate(
      geom = "text",
      x = 0.75,
      y = 0.9,
      label = paste0(
        "Length at Maturity: ",
        round(output$EU[[k]]$L50 / 25.4, 1),
        " inches"
      )
    )
}

# Pareto Point Function
paretoShadow <- function(MyParsList, output, k, Flevel=c(0, 4)) {
  EU_grid <- expand.grid(Lc = output$EU[[k]]$Lc / 25.4,
                         F_M = output$EU[[k]]$F_M)
  EU_grid$ypr <- as.vector(t(output$EU[[k]]$YPR_EU))
  EU_grid$spr <- as.vector(t(output$EU[[k]]$SPR_EU))
  
  # Pareto plots prep
  EU_tmp <- EU_grid %>%
    filter(F_M > Flevel[1],
           F_M < Flevel[2])
  values <- c(
    0,
    (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc) - 0.01,
    (output$EU[[k]]$L50 / 25.4 - 0.5) / max(EU_tmp$Lc),
    (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc),
    (output$EU[[k]]$L50 / 25.4 + 0.5) / max(EU_tmp$Lc) + 0.01,
    1
  )
  colors <- c("#A50026", "#FDAE61", "black", "#A6D96A", "#006837")
  pareto_title_one <- c(paste0(MyParsList[[k]]$MyPars@Species))
  pareto_subtitle_one <- "SPR-YPR tradeoffs, highlighting options from tables above."
  pareto_title_two <- c(paste0(MyParsList[[k]]$MyPars@Species))

  # Pareto point plot prep
  dt = data.frame(
    YPR = output$YPR[[k]]$YPR[output$YPR[[k]]$Name != "Current size limit"],
    SPR = output$SPR[[k]]$SPR[output$SPR[[k]]$Name != "Current size limit"],
    Lc = output$SPR[[k]]$Lc_inch[output$SPR[[k]]$Name != "Current size limit"],
    F_M = factor(output$SPR[[k]]$FM[output$SPR[[k]]$Name != "Current size limit"], labels =
                   c("L", "M", "H"))
  )
  
  # Pareto Point Plot
  ggplot() +
    geom_point(
      data = EU_tmp,
      aes(x = ypr, y = spr),
      colour = "lightgrey",
      alpha = 0.1
    ) +
    geom_point(data = dt, aes(x = YPR, y = SPR, colour = Lc)) +
    scale_colour_gradientn(
      name = "Minimum size limit (FL in)",
      colors = colors,
      values = values,
      limits = c(min(EU_tmp$Lc), max(EU_tmp$Lc))
    ) +
    geom_label_repel(
      data = dt,
      aes(
        x = YPR,
        y = SPR,
        colour = Lc,
        label = paste0(Lc, F_M)
      ),
      segment.color = 'black'
    ) +
    annotate(
      geom = "text",
      x = 0.75,
      y = 0.9,
      label = paste0(
        "Length at Maturity: ",
        round(output$EU[[k]]$L50 / 25.4, 1),
        " inches"
      )
    ) +
    labs(
      y = "Spawning Potential Ratio (SPR)",
      x = "Relative Yield per Recruit (YPR)",
      title = pareto_title_two,
      subtitle = pareto_subtitle_one,
      caption = "Note: \nAll size limit and fishing pressure combinations plotted in light grey. Labels contain \ninformation on combination of minimum size limit in inches (numbers) and \nfishing pressure (letters: L = low, M = medium, H = high). \n"
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.title = element_text(colour = "grey"),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0)
    )
}