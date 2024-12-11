### Economic data
### J Collins 
### 2024-04-26
###

## 03 PLOTS ####



## 03.1 PACKAGES ####

source(file = "scripts/01_packages.R")


## 03.2 DATA ####

source(file = "scripts/02_data.R")


## 03.3 PLOTS ####


### APPLICATIONS ####

# this is the legend title with correct notation
title_exp <- expression(Spray~and~Fertiliser~Plan~Expenditure~('£'~ha^{-1})) 
y_title <- expression('£'~ha^{-1})

# date_range object
date_range <- which(app_dat$Date %in% as.Date(
  c("2022-03-01", "2022-10-01")) )


ggplot(data = app_dat,
       aes(x = Date, 
           y = accumulated_cost_ha, 
           color = Treatment, 
           group = Treatment)) +
  geom_step(size = 1, 
            show.legend = TRUE) +
  geom_point(color = "black", 
             size = 1) +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold"),
    aspect.ratio = 1/1.5, 
    panel.background = element_rect(fill = "white", 
                                    linetype = 1, 
                                    color = "black")) + 
  scale_color_manual(values = c("turquoise3", "tomato2")) +
  labs(
    x = "Date",
    y = y_title,
    title = "Harper Adams Conservation Agriculture Experiment", 
    subtitle = title_exp, 
    caption = "All prices from invoices") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  annotate(geom = "text", x = as.Date("2022-03-05"),
           y = 800, label = "Spring Beans", fontface = "bold") +
  annotate(geom = "text", x = as.Date("2023-01-05"),
           y = 800, label = "Winter Wheat", fontface = "bold") 


  ggsave(filename = "spray_plot.png", 
         path = "plots/04_all_crops/", 
         width = 5, 
         height = 5)

  
  
  ### OPERATIONS ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Operations~Expenditure~('£'~ha^{-1})) 
  y_title <- expression('£'~ha^{-1})
  
  # date_range object
  date_range <- which(op_dat$Date %in% as.Date(
    c("2022-03-01", "2022-10-01")) )
  
  
  ggplot(data = op_dat,
         aes(x = Date, 
             y = accumulated_cost_ha, 
             color = Treatment, 
             group = Treatment)) +
    geom_step(size = 1, 
              show.legend = TRUE) +
    geom_point(color = "black", 
               size = 1) +
    theme(
      legend.position = c(.95, .25),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.title = element_text(face = "bold"),
      aspect.ratio = 1/1.5, 
      panel.background = element_rect(fill = "white", 
                                      linetype = 1, 
                                      color = "black")) +
    scale_color_manual(values = c("turquoise3", "tomato2")) +
    labs(
      x = "Date",
      y = y_title,
      title = "Harper Adams Conservation Agriculture Experiment", 
      subtitle = title_exp, 
      caption = "All prices from invoices") +
    geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    annotate(geom = "text", x = as.Date("2022-03-05"),
             y = 800, label = "Spring Beans", fontface = "bold") +
    annotate(geom = "text", x = as.Date("2023-01-05"),
             y = 800, label = "Winter Wheat", fontface = "bold") 
  
  
  ggsave(filename = "operations_plot.png", 
         path = "plots/04_all_crops/", , 
         width = 5, 
         height = 5)
  
  
  
  ### TOTAL EXPENDITURE ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Expenditure~('£'~ha^{-1})) 
  y_title <- expression('£'~ha^{-1})
  
  # date_range object
  date_range <- which(dat$Date %in% as.Date(
    c("2022-03-01", "2022-10-01")) )
  
  
  ggplot(data = dat,
         aes(x = Date, 
             y = accumulated_cost_ha, 
             color = Treatment, 
             group = Treatment)) +
    geom_step(size = 1, 
              show.legend = TRUE) +
    geom_point(color = "black", 
               size = 1) +
    theme(
      legend.position = c(.95, .25),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.title = element_text(face = "bold"),
      aspect.ratio = 1/1.5, 
      panel.background = element_rect(fill = "white", 
                                      linetype = 1, 
                                      color = "black")) +
  scale_color_manual(values = c("turquoise3", "tomato2")) +
    labs(
      x = "Date",
      y = y_title,
      title = "Harper Adams Conservation Agriculture Experiment", 
      subtitle = title_exp, 
      caption = "All prices from invoices") +
    geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    annotate(geom = "text", x = as.Date("2022-03-05"),
             y = 7000, label = "Spring Beans", fontface = "bold") +
    annotate(geom = "text", x = as.Date("2023-01-05"),
             y = 7000, label = "Winter Wheat", fontface = "bold") 
  
  
  ggsave(filename = "expenditure_plot.png", 
         path = "plots/04_all_crops/", 
         width = 5, 
         height = 5)
  
  
  
  
## BARPLOTS SUMMARY PLOTS ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Grain~"&"~Straw~Gross~Margin~('£'~ha^{-1})) 
  y_title <- expression(Gross~Margin~'£'~ha^{-1})
  

    ggplot(data = summary_dat, 
           aes(x = block, 
               y = gross_margin, 
               fill = crop)) + 
      geom_bar(stat = "identity", 
               color = "black") + 
      facet_wrap(~ treatment, ) +
      labs(
        x = "Block",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"))
    
    ggsave(filename = "profit_experom_plot.png", path = "plots/04_all_crops/")
    

   
    
    ### profit barplot ####
    
    #grain and straw
    
    ggplot(data = gm_sum, 
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
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "Bean price = £300 / t, Wheat Price = £190 / t, Wheat straw price = £50 / t") +
      theme_bw() +
      scale_fill_manual(values=c("tomato2", "turquoise3"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic")) +
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
    
    ggsave(filename = "profit_treatment_plot.png", path = "plots/04_all_crops/")
    
    
    
    ### block grain gross margin ####
    
    
    ggplot(data = summary_dat, 
           aes(x = block, 
               y = grain_gross_margin, 
               fill = crop)) + 
      geom_bar(stat = "identity", 
               color = "black") + 
      facet_wrap(~ treatment, ) +
      labs(
        x = "Block",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"))
    
    ggsave(filename = "no_straw_experom_plot.png", path = "plots/04_all_crops/")
    
    
    
    
    ### treatment grain profit ####
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Grain~Gross~Margin~('£'~ha^{-1})) 
    y_title <- expression(Gross~Margin~'£'~ha^{-1})
    
    ggplot(data = no_straw_sum, 
           aes(x = treatment, 
               y = mean, 
               fill = treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = "Crop",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "Bean price = £300 / t, Wheat Price = £190 / t, Wheat straw price = £50 / t") +
      theme_bw() +
      scale_fill_manual(values=c("tomato2", "turquoise3"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic")) +
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
    
    ggsave(filename = "no_straw_profit.png", path = "plots/04_all_crops/")
    
    
    
    
    
    ### YIELD ####
    
    # this is the legend title with correct notation
    title_exp <- expression(Grain~Yield~(t~ha^{-1})) 
    y_title <- expression(Yield~'£'~ha^{-1})
    
    ggplot(data = yield_sum, 
           aes(x = treatment, 
               y = mean, 
               fill = treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      labs(
        x = "Crop",
        y = title_exp,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = element_blank()) +
      theme_bw() +
      scale_fill_manual(values=c("tomato2", "turquoise3"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic")) +
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
        annotations = "p = 0.04",
        fontface = 'italic', 
        y_position = c(11) # Adjust y-position if necessary
      ) +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position="bottom", 
            legend.title = element_blank(), 
            legend.text = element_text(size = 7)) +
      facet_wrap(~ crop, ncol = 2)
    
    ggsave(filename = "yield_plot.png", 
           path = "plots/04_all_crops/", 
           width = 5, 
           height = 5)
    
    
    
    
    
    
    
    
    
    ### proportions ####
    
    # this is the legend title with correct notation
    title_exp <- expression(Proportion~of~Revenue~('%'))
    y_title <- expression(Proportion~('%'))
    
    
    ggplot(data = ex_proportions, 
           aes(x = treatment, 
               y = proportion, 
               fill = category)) + 
      geom_bar(stat = "identity", 
               position = "fill") + 
      labs(
        x = "Treatment",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position="bottom", 
            legend.title = element_blank(), 
            legend.text = element_text(size = 7)) +
      facet_wrap(~ crop )
    
    ggsave(filename = "proportion_plot.png", 
           path = "plots/04_all_crops/", 
           width = 7, 
           height = 6)
    
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Proportion~('%')~of~Crop~Expenditure~('£'~ha^{-1}))
    y_title <- expression(Proportion~('%'))
    
    
    ggplot(data = app_dat, 
           aes(x = Treatment, 
               y = cost_per_ha, 
               fill = Category)) + 
      geom_bar(stat = "identity", 
               position = "fill") + 
      labs(
        x = "Treatment",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position="bottom", 
            legend.title = element_blank(), 
            legend.text = element_text(size = 10)) +
      facet_wrap(~ crop )
    
    ggsave(filename = "crop_input_proportion_plot.png", 
           path = "plots/04_all_crops/", 
           width = 7, 
           height = 6)
    
    
    
    
    
    # this is the legend title with correct notation
    title_exp <- expression(Proportion~('%')~of~Operational~Expenditure~('£'~ha^{-1}))
    y_title <- expression(Proportion~('%'))
    
    
    ggplot(data = op_dat, 
           aes(x = Treatment, 
               y = cost_per_ha, 
               fill = Category)) + 
      geom_bar(stat = "identity", 
               position = "fill") + 
      labs(
        x = "Treatment",
        y = y_title,
        title = "Harper Adams Conservation Agriculture Experiment", 
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position="bottom", 
            legend.title = element_blank(), 
            legend.text = element_text(size = 10)) +
      facet_wrap(~ crop )
    
    ggsave(filename = "operations_input_proportion_plot.png", 
           path = "plots/04_all_crops/", 
           width = 7, 
           height = 6)
    

             