


########################
#Patterns across species
#Create plots of overall patterns in the data useful for main part of report

source("Size_Limit_Functions.R")
MyParsList <- readRDS("MyParsList.rds")
output <- readRDS("output.rds")


#########################
#Size limit chart
species_chart <- read_csv(paste0(getwd(),"/Size_Limit_Chart.csv")) %>% 
  select(c(3:8))

# Make NAs blank in kable tables
options(knitr.kable.NA = "")

#Create table for first tab
kable(species_chart, align = "llllccc", caption = "Kaʻūpūlehu Species") %>% 
  kable_styling(c("condensed", "responsive", "bordered"),
                bootstrap_options = "striped", 
                full_width = F) %>% 
  save_kable(file = paste0(getwd(), "/Main_figures/Species size limit.html"))


##########################
#Trends when SL50 = Lm for each all species
SPR_Lm <- data.frame()
YPR_Lm <- data.frame()
for (k in 1:NROW(output$EU)) {
  x <-
    which(abs(output$EU[[k]]$Lc - output$EU[[k]]$L50) == min(abs(output$EU[[k]]$Lc -
                                                                   output$EU[[k]]$L50)))[1]
  SPR_Lm <-
    rbind(SPR_Lm,
          list(
            Species = rep(MyParsList[[k]]$MyPars@Species, NROW(output$EU[[k]]$SPR_EU[, x])),
            SPR = output$EU[[k]]$SPR_EU[, x],
            F_M = output$EU[[k]]$F_M
          ))
  YPR_Lm <-
    rbind(YPR_Lm,
          list(
            Species = rep(MyParsList[[k]]$MyPars@Species, NROW(output$EU[[k]]$SPR_EU[, x])),
            YPR = output$EU[[k]]$YPR_EU[, x],
            F_M = output$EU[[k]]$F_M
          ))
}

p1 <- ggplot(SPR_Lm, aes(x = F_M, y = SPR)) +
  geom_line(aes(colour = Species)) +
  labs(y = "SPR", x = "F/M") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

p2 <- ggplot(YPR_Lm, aes(x = F_M, y = YPR)) +
  geom_line(aes(colour = Species)) +
  labs(y = "YPR", x = "F/M")


library(gridExtra)
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x)
    x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p2)

p2 <-
  p2 + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    legend.text = element_text(size = 7),
    legend.title = element_blank()
  )

ggsave(
  filename = "All_species_trends.png",
  plot = grid.arrange(
    p1,
    p2,
    legend,
    layout_matrix = rbind(c(1, 2), c(3, 3)),
    heights = c(3.5, 6.5),
    widths = c(3.5, 3.5)
  ),
  device = "png",
  path = here::here("Main_figures"),
  width = 7,
  height = 10,
  units = "in",
  dpi = 300
)


#############################
#Example of contour plots, showing eumetric line corrsponding to 30% spr

k <- 1
ggsave(
  filename = "Contour_example.png",
  plot = grid.arrange(
    contourSPR(MyParsList, output, k),
    contourYPR(MyParsList, output, k),
    ncol = 1
  ),
  device = "png",
  path = here::here("/Main_figures"),
  width = 5,
  height = 12,
  units = "in",
  dpi = 300
)


##############################
#sustainable options @ SPR 30%

outsize <- data.frame()

for (k in 1:NROW(output$YPR)) {
  L <- output$SPR[[k]] %>%
    filter(SPR >= 0.3,
           FM == 1,
           Name != "Current size limit")
  L <- L[which.min(L$Lc_mm), ]
  
  M <- output$SPR[[k]] %>%
    filter(SPR >= 0.3,
           FM == 2,
           Name != "Current size limit")
  M <- M[which.min(M$Lc_mm), ]
  
  H <- output$SPR[[k]] %>%
    filter(SPR >= 0.3,
           FM == 4,
           Name != "Current size limit")
  H <- H[which.min(H$Lc_mm), ]
  
  
  
  outsize <-
    rbind(
      outsize,
      list(
        Species = MyParsList[[k]]$MyPars@Species,
        Lm_inch = MyParsList[[k]]$MyPars@L50 / 25.4,
        Lm_mm = MyParsList[[k]]$MyPars@L50,
        Lc_L = ifelse(NROW(L) > 0, L$Lc_mm, NA),
        Lc_M = ifelse(NROW(M) > 0, M$Lc_mm, NA),
        Lc_H = ifelse(NROW(H) > 0, H$Lc_mm, NA),
        YPR_L = ifelse(NROW(L) > 0, (
          output$YPR[[k]] %>%
            filter(Name == L$Name,
                   FM == L$FM)
        )$YPR, NA),
        YPR_M = ifelse(NROW(M) > 0, (
          output$YPR[[k]] %>%
            filter(Name == M$Name,
                   FM == M$FM)
        )$YPR, NA),
        
        YPR_H = ifelse(NROW(H) > 0, (
          output$YPR[[k]] %>%
            filter(Name == H$Name,
                   FM == H$FM)
        )$YPR, NA),
        M_K = MyParsList[[k]]$MyPars@MK,
        Lm_Loo = MyParsList[[k]]$MyPars@L50 / MyParsList[[k]]$MyPars@Linf
      )
    )
  
}
outsize$option <- round(outsize$Lc_H / outsize$Lm_mm, 1)
outsize$option <-
  ifelse(is.na(outsize$option), "Other", outsize$option)


####################
#Distribution of size limits across species and F/M levels

p1 <- ggplot(outsize, aes(x = round(Lc_L / Lm_mm, 1))) +
  geom_histogram(center = 0.9) +
  coord_flip() +
  ylab("Number of species") +
  xlab("Minimum size limit / Lm") +
  xlim(0.8, 2.1) +
  ylim(0, 35) +
  scale_x_continuous(labels=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0), breaks=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  annotate(
    geom = "text",
    size = 3,
    y = 35,
    x = 2.1,
    hjust = 1,
    label = "F/M = L, Low fishing pressure (A)"
  )

p2 <- ggplot(outsize, aes(x = round(Lc_M / Lm_mm, 1))) +
  geom_histogram(center = 0.9) +
  ylab("Number of species") +
  xlab("Minimum size limit / Lm") +
  xlim(0.8, 2.1) +
  ylim(0, 35) +
  scale_x_continuous(labels=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0), breaks=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0)) +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  annotate(
    geom = "text",
    size = 3,
    y = 35,
    x = 2.1,
    hjust = 1,
    label = "F/M = M, Medium fishing pressure (B)"
  )

p3 <- ggplot(outsize, aes(x = round(Lc_H / Lm_mm, 1))) +
  geom_histogram(center = 0.9) +
  ylab("Number of species") +
  xlab("Minimum size limit / Lm") +
  xlim(0.8, 2.1) +
  ylim(0, 35) +
  scale_x_continuous(labels=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0), breaks=c(0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0)) +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  annotate(
    geom = "text",
    size = 3,
    y = 35,
    x = 2.1,
    hjust = 1,
    label = "F/M = H, High fishing pressure (C)"
  )

ggsave(
  filename = "Distribution of Lc_Lm for 30SPR.png",
  plot = grid.arrange(p1, p2, p3, ncol = 1),
  device = "png",
  path = paste0(getwd(), "/Main_figures/"),
  width = 3.5,
  height = 8,
  units = "in",
  dpi = 300
)


####
#Options for guarding against high F/M

outsize %>%
  mutate(option = cell_spec(
    ifelse(option == "Other", "Other", paste(option, "x", "Lm")),
    
    "html",
    color = "white",
    align = "c",
    background = factor(
      option,
      c(0.9, 1, 1.1, 1.2, 1.3, 1.5, 2, "Other"),
      spec_color(
        1:8,
        end = 0.9,
        option = "A",
        direction = -1
      )
    )
  )) %>%
  kable(
    row.names = FALSE,
    col.names = c(
      "Species",
      "in",
      "mm",
      "L",
      "M",
      "H",
      "L",
      "M",
      "H",
      "M/K",
      "Lm/Loo",
      "F/M = H"
    ),
    align = "lccccccccccc",
    digits = 2,
    format = "html",
    escape = FALSE,
    table.attr = "style='width:70%;'"
  ) %>%
  kable_styling(
    c("condensed", "responsive", "bordered"),
    bootstrap_options = "striped",
    full_width = T,
    position = "center",
    font_size = 12
  ) %>%
  column_spec(12, bold = TRUE) %>%
  add_header_above(
    c(
      " " = 1,
      "Length at maturity (Lm)" = 2,
      "Fishing pressure (F/M)" = 3,
      "Fishing pressure (F/M)" = 3,
      " " = 2,
      "Lm option" = 1
    )
  ) %>%
  add_header_above(c(
    " " = 3,
    "Minimum size limit achieving SPR 30%" = 3,
    "Relative yield achieved" = 3,
    " " = 3
  ))  %>%
  save_kable(file = paste0(getwd(), "/Main_figures/Table size limit SPR30%.html"))

#Life history plot


pred<-data.frame(MKratio=seq(0,3,0.001),
                 Lmratio=3/(3+seq(0,3,0.001)))

p1 <- ggplot(outsize, aes(x = M_K, y = Lm_Loo)) +
  geom_point(aes(colour = option), size = 10) +
  scale_colour_viridis_d(
    name = "Lm option,\nwhen F/M = H",
    end = 0.9,
    option = "A",
    direction = -1
  ) +
  geom_text(aes(label = option), color = "white", fontface = "bold", size=3) +
  xlab("M/K") +
  ylab("Lm/Loo") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggsave(
  filename = "Size limit vs life history.png",
  plot = p1,
  device = "png",
  path = paste0(getwd(), "/Main_figures/"),
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)