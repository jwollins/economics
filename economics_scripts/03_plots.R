### Economic data
### J Collins 
### 2024-04-26
###

# 03 PLOTS ####


setwd(dir = "~/Documents/GitHub/economics/")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# PACKAGES ####

source(file = "economics_scripts/01_packages.R")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# DATA ####

source(file = "economics_scripts/02_data.R")

getwd()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# TIME PLOTS ####


## ~ APPLICATIONS ####

# this is the legend title with correct notation
title_exp <- expression(Application~Expenditure~('£'~ha^{-1})) 
y_title <- expression('£'~ha^{-1})

# date_range object
date_range <- which(app_dat$Date %in% as.Date(
  c("2022-03-01", "2022-10-01")) )

app_time_plot <-
ggplot(data = app_dat,
       aes(x = Date, 
           y = accumulated_cost_ha, 
           color = Treatment, 
           group = Treatment)) +
  geom_step(size = 1, 
            show.legend = TRUE) +
  geom_point(color = "black", 
             size = 1) +
  scale_color_manual(values = c("turquoise3", "tomato2")) +
  labs(
    x = "Date",
    y = y_title,
    subtitle = title_exp) +
  ylim(0,2500) +
  scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2024-10-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2024-03-15")),  # Example date for the vertical line
             linetype = "dashed",
             color = "turquoise3") +
  annotate(geom = "text", 
           x = as.Date("2022-04-05"),
           y = 2500, 
           label = "Spring Beans", 
           fontface = "bold") +
  annotate(geom = "text", 
           x = as.Date("2023-02-05"),
           y = 2500, 
           label = "Winter Wheat", 
           fontface = "bold") +
  annotate(geom = "text", 
           x = as.Date("2023-11-25"),
           y = 2500, 
           label = "Oilseed Rape", 
           fontface = "bold") + 
  annotate(geom = "text", 
           x = as.Date("2024-05-15"),
           y = 2500, 
           label = "Spring Barley", 
           fontface = "bold",
           color = "turquoise3") + 
  theme_linedraw() + 
  theme(legend.position = "bottom")



app_time_plot

  ggsave(filename = "fig_applications_by_time.png", 
         path = "plots/", 
         width = 10, 
         height = 4)

  
  
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
## ~ OPERATIONS ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Operations~Expenditure~('£'~ha^{-1})) 
  y_title <- expression('£'~ha^{-1})
  
  # date_range object
  date_range <- which(op_dat$Date %in% as.Date(
    c("2022-03-01", "2022-10-01")) )
  
op_time_plot <-
  ggplot(data = op_dat,
         aes(x = Date, 
             y = accumulated_cost_ha, 
             color = Treatment, 
             group = Treatment)) +
    geom_step(size = 1, 
              show.legend = TRUE) +
    geom_point(color = "black", 
               size = 1) +
    scale_color_manual(values = c("turquoise3", "tomato2")) +
    labs(
      x = "Date",
      y = y_title,
      subtitle = title_exp) +
    ylim(0,1500) +
  scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2024-10-01"))) +
    geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2024-03-15")),  # Example date for the vertical line
               linetype = "dashed",
               color = "turquoise3") +
    annotate(geom = "text", 
             x = as.Date("2022-04-05"),
             y = 1400, 
             label = "Spring Beans", 
             fontface = "bold") +
    annotate(geom = "text", 
             x = as.Date("2023-02-05"),
             y = 1400, 
             label = "Winter Wheat", 
             fontface = "bold") +
    annotate(geom = "text", 
             x = as.Date("2023-11-25"),
             y = 1400, 
             label = "Oilseed Rape", 
             fontface = "bold") + 
    annotate(geom = "text", 
             x = as.Date("2024-06-15"),
             y = 1400, 
             label = "Spring Barley", 
             fontface = "bold",
             color = "turquoise3") + 
  theme_linedraw() + 
    theme(legend.position = "bottom")
  
op_time_plot
  
  ggsave(filename = "operations_plot.png", 
         path = "plots/", , 
         width = 10, 
         height = 4)
  
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
  ## ~ TOTAL EXPENDITURE ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Total~Expenditure~('£'~ha^{-1})) 
  y_title <- expression('£'~ha^{-1})
  
  # date_range object
  date_range <- which(dat$Date %in% as.Date(
    c("2022-03-01", "2022-10-01")) )
  
glimpse(dat)
  
  
expen_plot <-  
  ggplot(data = dat,
         aes(x = Date, 
             y = accumulated_cost_ha, 
             color = Treatment, 
             group = Treatment)) +
    geom_step(size = 1, 
              show.legend = TRUE) +
    geom_point(color = "black", 
               size = 1) +
  scale_color_manual(values = c("tomato2", "turquoise3")) +
    labs(
      x = "Date",
      y = y_title,
      subtitle = title_exp) +
    ylim(0,3500) +
  scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2024-10-01"))) +
    geom_vline(xintercept = as.numeric(as.Date("2022-10-03")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2023-08-20")),  # Example date for the vertical line
               linetype = "dashed",
               color = "black") +
    geom_vline(xintercept = as.numeric(as.Date("2024-03-15")),  # Example date for the vertical line
               linetype = "dashed",
               color = "turquoise3") +
    annotate(geom = "text", 
             x = as.Date("2022-04-05"),
             y = 3400, 
             label = "Spring Beans", 
             fontface = "bold") +
    annotate(geom = "text", 
             x = as.Date("2023-02-05"),
             y = 3400, 
             label = "Winter Wheat", 
             fontface = "bold") +
    annotate(geom = "text", 
             x = as.Date("2023-11-25"),
             y = 3400, 
             label = "Oilseed Rape", 
             fontface = "bold") + 
    theme_classic() + 
    theme(legend.position = "bottom") +
    annotate(geom = "text", 
             x = as.Date("2024-06-15"),
             y = 3400, 
             label = "Spring Barley", 
             fontface = "bold",
             color = "turquoise3") + 
    theme_linedraw() + 
    theme(legend.position = "bottom")
  
expen_plot
  
ggsave(filename = "fig_total_expenditure.png", 
         path = "plots/", 
         width = 10, 
         height = 4)
  
  
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# ~ Combined plot ####
  
ggarrange(app_time_plot, op_time_plot, expen_plot,
          ncol = 1, nrow = 3, 
          labels = c("A","B","C"), 
          legend = "bottom", common.legend = TRUE)
  
ggsave(filename = "plots/fig_expenditure_by_time.png", width = 10, height = 8)
  
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# BARPLOTS SUMMARY PLOTS ####
  
  
  # this is the legend title with correct notation
  title_exp <- expression(Gross~Margin~('£'~ha^{-1})) 
  y_title <- expression(Gross~Margin~'£'~ha^{-1})
  

    ggplot(data = summary_dat, 
           aes(x = block, 
               y = total_gm_ha, 
               fill = year)) + 
      geom_bar(stat = "identity", 
               color = "black") + 
      facet_wrap(~ treatment, ) +
      labs(
        x = "Block",
        y = y_title,
        subtitle = title_exp) +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom")
    
    ggsave(filename = "gross_margin_by_block.png", path = "plots/")
    

   

    
    
    
# ~ gross margin barplot ####
    
    #grain and straw

gm_plot <- 
    ggplot(data = gm_sum, 
           aes(x = treatment, 
               y = mean, 
               fill = treatment)) + 
      geom_bar(stat = "identity", 
               color = "black", 
               position = "dodge") + 
      scale_fill_manual(values=c("tomato2", "turquoise3"), 
                        name = "Treatment") +
      geom_errorbar(aes(ymin=mean-se,
                        ymax=mean+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      labs(
        x = "Year",
        y = y_title,
        subtitle = expression(Gross~Margin~('£'~ha^{-1})), 
        caption = "Bean price = £300 / t, Wheat Price = £190 / t, Wheat straw price = £50 / t") +
      facet_wrap(~ year, ncol = 4) +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        # color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.title.x = element_blank(), axis.text.x = element_blank()) 
    
    
    gm_plot
    
    ggsave(filename = "fig_gm_by_treatment.png", path = "plots/")
    
    
    
    
    
# ~ block grain gross margin ####

    # this is the legend title with correct notation
    title_exp <- expression(Grain~Gross~Margin~('£'~ha^{-1})) 
    y_title <- expression(Gross~Margin~'£'~ha^{-1})
    
ggplot(data = summary_dat, 
           aes(x = block, 
               y = grain_gm, 
               fill = year)) + 
      geom_bar(stat = "identity", 
               color = "black") + 
      facet_wrap(~ treatment, ) +
      labs(
        x = "Block",
        y = y_title,
        subtitle = title_exp, 
        caption = "All prices from invoices") +
      theme_bw() +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom")
    
    ggsave(filename = "fig_grain_gm_by_block.png", path = "plots/")
    
    
    
  
    
    
# ~ treatment grain profit ####
    
    
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
        x = "Treatment",
        y = y_title,
        subtitle = title_exp) +
      theme_bw() +
      scale_fill_manual(values=c("tomato2", "turquoise3"), 
                        name = "Crop") +
      theme(strip.text.x = element_text(size = 12, 
                                        color = "black", 
                                        face = "bold.italic"), 
            legend.position = "bottom", 
            axis.title.x = element_blank(), 
            axis.text.x = element_blank()) +
      geom_errorbar(aes(ymin=mean-se, 
                        ymax=mean+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      facet_wrap(~ year, ncol = 4)
    
ggsave(filename = "fig_grain_gm_by_year.png", path = "plots/")
    
    
    
    
    
    ### YIELD ####
    
    # # this is the legend title with correct notation
    # title_exp <- expression(Grain~Yield~(t~ha^{-1})) 
    # y_title <- expression(Yield~'£'~ha^{-1})
    # 
    # ggplot(data = yield_sum, 
    #        aes(x = treatment, 
    #            y = mean, 
    #            fill = treatment)) + 
    #   geom_bar(stat = "identity", 
    #            color = "black", 
    #            position = "dodge") + 
    #   labs(
    #     x = "Crop",
    #     y = title_exp,
    #     title = "Harper Adams Conservation Agriculture Experiment", 
    #     subtitle = title_exp, 
    #     caption = element_blank()) +
    #   theme_bw() +
    #   scale_fill_manual(values=c("tomato2", "turquoise3"), 
    #                     name = "Crop") +
    #   theme(strip.text.x = element_text(size = 12, 
    #                                     color = "black", 
    #                                     face = "bold.italic")) +
    #   geom_errorbar(aes(ymin=mean-se, 
    #                     ymax=mean+se),
    #                 width=.2,                    # Width of the error bars
    #                 position=position_dodge(.9)) +
    #   geom_signif(
    #     data = subset(yield_sum, crop == "Spring Beans"), # Subset data for Crop1
    #     comparisons = list(c("Conventional", "Conservation")),
    #     map_signif_level = TRUE,
    #     textsize = 4,
    #     tip_length = 0.01, 
    #     annotations = "NS.", 
    #     fontface = 'italic', 
    #     y_position = c(11) # Adjust y-position if necessary
    #   ) +
    #   geom_signif(
    #     data = subset(yield_sum, crop == "Winter Wheat"), # Subset data for Crop2
    #     comparisons = list(c("Conventional", "Conservation")),
    #     map_signif_level = TRUE,
    #     textsize = 4,
    #     tip_length = 0.01,
    #     annotations = "p = 0.04",
    #     fontface = 'italic', 
    #     y_position = c(11) # Adjust y-position if necessary
    #   ) +
    #   theme(strip.text.x = element_text(size = 12, 
    #                                     color = "black", 
    #                                     face = "bold.italic"), 
    #         legend.position="bottom", 
    #         legend.title = element_blank(), 
    #         legend.text = element_text(size = 7)) +
    #   facet_wrap(~ crop, ncol = 4)
    # 
    # ggsave(filename = "yield_plot.png", 
    #        path = "plots/04_all_crops/", 
    #        width = 5, 
    #        height = 5)
    # 
    
    
    
    
    
    
    
    
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
    

             