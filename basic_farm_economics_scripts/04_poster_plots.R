### Economic data
### J Collins 
### 2024-04-26
###

## 04 POSTER PLOTS ####

## A - YIELD PLOT ####


# this is the legend title with correct notation
title_exp <- expression(Grain~Yield~(t~ha^{-1})) 
y_title <- expression(Yiel~'£'~ha^{-1})

yield_plot <- ggplot(data = yield_sum, 
                     aes(x = treatment, 
                         y = mean, 
                         fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ncol = 2) +
  labs(
    x = "Crop",
    y = title_exp, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_signif(
    data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01, 
    annotations = "NS.", 
    fontface = 'italic', 
    y_position = c(11) # Adjust y-position if necessary
  ) +
  geom_signif(
    data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01,
    annotations = "p = 0.044",
    fontface = 'italic', 
    y_position = c(11) # Adjust y-position if necessary
  ) +
  facet_wrap(~ crop, ncol = 2)

yield_plot











## B - Revenue PLOT ####

# this is the legend title with correct notation
title_exp <- expression(Revenue~('£'~ha^{-1})) 
y_title <- expression(Revenue~('£'~ha^{-1}))


rev_plot <- ggplot(data = rev_sum, 
                   aes(x = treatment, 
                       y = mean, 
                       fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ) +
  labs(
    x = "Crop",
    y = y_title, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "CTreatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_signif(
    data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01, 
    annotations = "NS.", 
    fontface = 'italic', 
    y_position = c(2400) # Adjust y-position if necessary
  ) +
  geom_signif(
    data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01,
    annotations = "p = 0.005", 
    fontface = 'italic', 
    y_position = c(2400) # Adjust y-position if necessary
  ) +
  facet_wrap(~ crop, ncol = 2)


rev_plot












## C - Expenditure plot ####


# this is the legend title with correct notation
title_exp <- expression(Expenditure~("£"~ha^{-1})) 
y_title <- expression(Expenditure~("£"~ha^{-1}))

expenditure_plot <- ggplot(data = expenditure_sum, 
                           aes(x = treatment, 
                               y = mean, 
                               fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ncol = 2) +
  labs(
    x = "Crop",
    y = title_exp, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

expenditure_plot












## D - Gross Margin plot ####

# this is the legend title with correct notation
title_exp <- expression(Gross~Margin~('£'~ha^{-1})) 
y_title <- expression(Gross~Margin~('£'~ha^{-1}))


gm_plot <- ggplot(data = gm_sum, 
                      aes(x = treatment, 
                          y = mean, 
                          fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ) +
  labs(
    x = "Crop",
    y = y_title, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_signif(
    data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01, 
    annotations = "NS.", 
    fontface = 'italic', 
    y_position = c(850) # Adjust y-position if necessary
  ) +
  geom_signif(
    data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01,
    annotations = "NS.",
    fontface = 'italic', 
    y_position = c(850) # Adjust y-position if necessary
  ) +
  facet_wrap(~ crop, ncol = 2)
  

  


## E - Net profit margin proportion plot ####

# this is the legend title with correct notation
title_exp <- expression(Net~Profit~Margin~('%')) 
y_title <- expression(Net~Profit~Margin~('%')) 


npm_plot <- ggplot(data = npm_sum, 
                   aes(x = treatment, 
                       y = mean, 
                       fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ) +
  labs(
    x = "Crop",
    y = y_title, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_signif(
    data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01, 
    annotations = "NS.", 
    fontface = 'italic', 
    y_position = c(50) # Adjust y-position if necessary
  ) +
  geom_signif(
    data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
    comparisons = list(c("Conventional", "Conservation")),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01,
    annotations = "NS.",
    fontface = 'italic', 
    y_position = c(50) # Adjust y-position if necessary
  ) +
  facet_wrap(~ crop, ncol = 2)




npm_plot















## NO STRAW Gross margin PLOT ####
  
  # this is the legend title with correct notation
  title_exp <- expression(Grain~Profit~('£'~ha^{-1})) 
  y_title <- expression(Grain~Profit~('£'~ha^{-1}))
  
  no_straw_plot <- ggplot(data = no_straw_sum, 
                        aes(x = treatment, 
                            y = mean, 
                            fill = treatment)) + 
    geom_bar(stat = "identity", 
             color = "black", 
             position = "dodge") + 
    facet_wrap(~ crop, ) +
    labs(
      x = "Crop",
      y = y_title, 
      title = title_exp) +
    theme_bw() +
    scale_fill_manual(values=c("tomato2", "turquoise3"), 
                      name = "Treatment") +
    theme(strip.text.x = element_text(size = 12, 
                                      color = "black", 
                                      face = "bold.italic"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_errorbar(aes(ymin=mean-se, 
                      ymax=mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    geom_signif(
      data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
      comparisons = list(c("Conventional", "Conservation")),
      map_signif_level = TRUE,
      textsize = 4,
      tip_length = 0.01, 
      annotations = "NS.", 
      fontface = 'italic', 
      y_position = c(850) # Adjust y-position if necessary
    ) +
    geom_signif(
      data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
      comparisons = list(c("Conventional", "Conservation")),
      map_signif_level = TRUE,
      textsize = 4,
      tip_length = 0.01,
      annotations = "NS.",
      fontface = 'italic', 
      y_position = c(850) # Adjust y-position if necessary
    ) +
    facet_wrap(~ crop, ncol = 2)
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  















### OP EXPENDITURE PLOT ####


# this is the legend title with correct notation
title_exp <- expression(Operational~Expenditure~("£"~ha^{-1})) 
y_title <- expression(Expenditure~("£"~ha^{-1}))

op_expenditure_plot <- ggplot(data = op_expenditure_sum, 
                           aes(x = treatment, 
                               y = mean, 
                               fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ncol = 2) +
  labs(
    x = "Crop",
    y = title_exp, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

op_expenditure_plot




### CROP EXPENDITURE PLOT ####


# this is the legend title with correct notation
title_exp <- expression(Crop~Expenditure~("£"~ha^{-1})) 
y_title <- expression(Expenditure~("£"~ha^{-1}))

crop_expenditure_plot <- ggplot(data = crop_expenditure_sum, 
                              aes(x = treatment, 
                                  y = mean, 
                                  fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  facet_wrap(~ crop, ncol = 2) +
  labs(
    x = "Crop",
    y = title_exp, 
    title = title_exp) +
  theme_bw() +
  scale_fill_manual(values=c("tomato2", "turquoise3"), 
                    name = "Treatment") +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

crop_expenditure_plot







## GRID PLOT ####

# 1
figure <- ggarrange(yield_plot, 
          rev_plot,
          expenditure_plot, 
          gm_plot,
          npm_plot, 
          ncol = 5, 
          nrow = 1, 
          align = "v",
          vjust = 1, 
          labels = c("A", "B", "C", "D", "E"), 
          legend = "bottom", 
          common.legend = TRUE,
          widths = 1,
          heights = 1) +
  theme(legend.text=element_text(size = 20))


  annotate_figure(figure,
                  bottom = text_grob("Costs and returns are calculated for each treatment area separately.", 
                                     color = "black",
                                     hjust = 1, 
                                     x = 1, 
                                     face = "italic", 
                                     size = 10))

ggsave(filename = "fig_plot.png", 
       path = "plots/04_all_crops/", 
       width = 20, 
       height = 5, )







## PROPORTIONS ####





## G - crop expenditure proportion plot ####

# this is the legend title with correct notation
title_exp <- expression(Proportion~('%')~of~Crop~Expenditure~('£'~ha^{-1}))
y_title <- expression(Proportion~('%'))

crop_prop_plot <- ggplot(data = app_dat, 
       aes(x = Treatment, 
           y = cost_per_ha, 
           fill = Category)) + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position="bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  facet_wrap(~ crop ) + 
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE))







## H - operations proportion plot ####


# this is the legend title with correct notation
title_exp <- expression(Proportion~('%')~of~Operational~Expenditure~('£'~ha^{-1}))
y_title <- expression(Proportion~('%'))

op_prop_plot <-ggplot(data = op_dat, 
       aes(x = Treatment, 
           y = cost_per_ha, 
           fill = Category)) + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position="bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  facet_wrap(~ crop ) + 
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE))




## I - revenue proportions plot ####


# this is the legend title with correct notation
title_exp <- expression(Proportion~('%')~of~Total~Revenue~('£'~ha^{-1}))
y_title <- expression(Proportion~('%'))


prop_rev_plot <- ggplot(data = ex_proportions, 
                        aes(x = treatment, 
                            y = proportion, 
                            fill = category)) + 
  geom_bar(stat = "identity", 
           position = "fill") + 
  labs(
    x = "Treatment",
    y = y_title,
    subtitle = title_exp) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, 
                                    color = "black", 
                                    face = "bold.italic"), 
        legend.position="bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  facet_wrap(~ crop ) + 
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE))



prop_rev_plot






## GRID PLOT ####

# 1
ggarrange(crop_prop_plot,
          op_prop_plot,
          prop_rev_plot,
          ncol = 3, 
          nrow = 1, 
          align = "v",
          vjust = 1, 
          labels = c("F", "G", "H"), 
          legend = "bottom", 
          common.legend = FALSE,
          widths = 1,
          heights = 1) 

ggsave(filename = "proportions_fig_plot.png", 
       path = "plots/04_all_crops/", 
       width = 15, 
       height = 5, )

