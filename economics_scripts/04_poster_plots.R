### Economic data
### J Collins 
### 2024-04-26
###

## 04 POSTER PLOTS ####

## A - YIELD PLOT ####


# # this is the legend title with correct notation
# title_exp <- expression(Grain~Yield~(t~ha^{-1})) 
# y_title <- expression(Yiel~'£'~ha^{-1})
# 
# yield_plot <- ggplot(data = yield_sum, 
#                      aes(x = treatment, 
#                          y = mean, 
#                          fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Crop",
#     y = title_exp, 
#     title = title_exp) +
#   theme_bw() +
#   scale_fill_manual(values=c("tomato2", "turquoise3"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   geom_errorbar(aes(ymin=mean-se, 
#                     ymax=mean+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   facet_wrap(~ year, ncol = 2)
# 
# yield_plot
# 
# 






#_____________________________________________________####
# Revenue ####



# ~ Gross Revenue PLOT ####

# this is the legend title with correct notation
title_exp <- expression(Gross~Revenue~('£'~ha^{-1})) 
y_title <- expression(Gross~Revenue~('£'~ha^{-1}))


rev_plot <- ggplot(data = rev_sum, 
                   aes(x = treatment, 
                       y = mean, 
                       fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  ylim(0,2300) +
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
  facet_wrap(~ year, ncol = 4)


rev_plot





# ~ grain revenue ####

# this is the legend title with correct notation
title_exp <- expression(Grain~Revenue~('£'~ha^{-1})) 
y_title <- expression(Grain~Revenue~('£'~ha^{-1}))


grain_rev_plot <- ggplot(data = grain_rev_sum, 
                   aes(x = treatment, 
                       y = mean, 
                       fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  ylim(0,2300) +
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
  facet_wrap(~ year, ncol = 4)


grain_rev_plot




# ~ straw revenue ####

# this is the legend title with correct notation
title_exp <- expression(Straw~Revenue~('£'~ha^{-1})) 
y_title <- expression(Straw~Revenue~('£'~ha^{-1}))


straw_rev_plot <- ggplot(data = straw_rev_sum, 
                   aes(x = treatment, 
                       y = mean, 
                       fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
  ylim(0,2300) +
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
  facet_wrap(~ year, ncol = 4)


straw_rev_plot





# ~ joint revenue plot ####

ggarrange(
  grain_rev_plot, 
  straw_rev_plot,
  rev_plot,
  ncol = 3, 
  nrow = 1, 
  labels = c("A", "B", "C"), 
  legend = "bottom", 
  common.legend = TRUE)


ggsave(filename = "fig_revenue_joint_plot.png", 
       path = "plots/", 
       width = 12, 
       height = 4)







#________________________________________####
# Expenditure ####

# ~ Gross Expenditure ####


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
  facet_wrap(~ year, ncol = 4) 

expenditure_plot



# ~ Operation expenditure ####


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
  facet_wrap(~ year, ncol = 3)

op_expenditure_plot




# ~ application expenditure ####


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
  facet_wrap(~ year, ncol = 3) 

crop_expenditure_plot



# ~ grain expenditure ####


# this is the legend title with correct notation
title_exp <- expression(Grain~Expenditure~("£"~ha^{-1})) 
y_title <- expression(Expenditure~("£"~ha^{-1}))

grain_expenditure_plot <- ggplot(data = grain_expenditure_sum, 
                                 aes(x = treatment, 
                                     y = mean, 
                                     fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
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
  facet_wrap(~ year, ncol = 3) 

grain_expenditure_plot







#_______________________________________________####
# Gross Margin ####




# ~ GM by year ####

title_exp <- expression(Gross~Margin~('£'~ha^{-1}~year^{-1})) 
y_title <- expression(Gross~Margin~('£'~ha^{-1}))


gm_by_year <- 
  ggplot(data = gm_sum, 
                      aes(x = treatment, 
                          y = mean, 
                          fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
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
  facet_wrap(~ year, ncol = 3)

gm_by_year
  





# ~ grain & straw GM ####

title_exp <- expression(Mean~Gross~Margin~('£'~ha^{-1})) 
y_title <- expression(Gross~Margin~('£'~ha^{-1}))

gm_by_treatment <- 
  ggplot(data = gm_sum_no_year, 
         aes(x = treatment, 
             y = mean, 
             fill = treatment)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge") + 
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
                position=position_dodge(.9)) 

gm_by_treatment






# # ~ grain GM ####
# 
# title_exp <- expression(Total~Grain~Gross~Margin~('£'~ha^{-1})) 
# y_title <- expression(Gross~Margin~('£'~ha^{-1}))
# 
# grain_gm_plot_no_year <- 
#   ggplot(data = no_straw_sum_no_year, 
#          aes(x = treatment, 
#              y = mean, 
#              fill = treatment)) + 
#   geom_bar(stat = "identity", 
#            color = "black", 
#            position = "dodge") + 
#   labs(
#     x = "Crop",
#     y = y_title, 
#     title = title_exp) +
#   theme_bw() +
#   scale_fill_manual(values=c("tomato2", "turquoise3"), 
#                     name = "Treatment") +
#   theme(strip.text.x = element_text(size = 12, 
#                                     color = "black", 
#                                     face = "bold.italic"),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   geom_errorbar(aes(ymin=mean-se, 
#                     ymax=mean+se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) 
# 
# 
# grain_gm_plot_no_year




# ~ joint plot ####

ggarrange(gm_plot, 
          # grain_gm_plot_no_year, 
          gm_plot_no_year,
          ncol = 3, 
          nrow = 1, 
          labels = c("A", "B", "C"), 
          legend = "bottom", 
          common.legend = TRUE)

ggsave(filename = "fig_GM_joint_plot.png", 
       path = "plots/", 
       width = 12, 
       height = 4)




#______________________________________________####
# Net profit margin ####




# ~ NPM by year ####

# this is the legend title with correct notation
title_exp <- expression(Net~Profit~Margin~('%'~year^{-1}))
y_title <- expression(Net~Profit~Margin~('%'))


npm_by_year <- ggplot(data = npm_sum,
                   aes(x = treatment,
                       y = mean,
                       fill = treatment)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
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
  facet_wrap(~ year, ncol = 3)


npm_by_year



# ~ NPM by treatment ####

title_exp <- expression(Mean~Net~Profit~Margin~('%'))
y_title <- expression(Net~Profit~Margin~('%'))

npm_by_treatment <- ggplot(data = npm_sum_no_year,
                   aes(x = treatment,
                       y = mean,
                       fill = treatment)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
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
                position=position_dodge(.9)) 


npm_by_treatment






#______________________________________________####

# ~ joint GM & NPM plot ####

ggarrange(gm_by_year, 
          npm_by_year, 
          gm_by_treatment, 
          npm_by_treatment,
          ncol = 2, 
          nrow = 2, 
          labels = c("A", "B", "C","D"), 
          legend = "bottom", 
          common.legend = TRUE)

ggsave(filename = "fig_GM_NPM_joint_plot.png", 
       path = "plots/", 
       width = 10, 
       height = 7)











#____________________________________________####
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

