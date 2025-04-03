
library(tidyverse)
library('PNWColors')
library('ggpubr')
library('cowplot')

ABCDsens_df <- read_csv(here::here('simulation output/ABCDsens_df.csv'))
glimpse(ABCDsens_df)
# 
# change_par_p <- "K"
# legend_title_d <- "% Decrease in K"
# legend_title_i <- "% Increase in K"
# 
# dec_set <- c(0.15, 0.3, 0.45, 0.6, 0.75, 0.9)
# inc_set <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5)
# # 
# # # pre time period
# # preplot <- gg_ABCD_sens_plot(data_sub = ABCDsens_df[which(ABCDsens_df$change_par==change_par_p), ], time_period = "pre", legend_title = "% change in K")
# # 
# # ggsave(here::here('figures_full/ABCD_sensitivity/ABCDsens_K_pre.pdf'),
# #        width=8,height=12)
# 
# data_sub0 <- ABCDsens_df[which(ABCDsens_df$change_par==change_par_p), ]
# time_period <- "pre"  


# ABCD  function 
gg_ABCD_sens_plot_gcsq <- function(data_sub0, time_period){ # legend_title = % change in K, % change in r, or % change in r + K

  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')
  
  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )
  
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)
  
  data_sub0 <- data_sub0 %>% mutate(strategy = 
                                      case_when( 
                                        strategy == 'GCA' ~ 'gradual',
                                        strategy == 'ACA' ~ 'abrupt',
                                        strategy == 'SQ' ~ 'fixed',
                                        .default = strategy)
  ) %>% 
    filter(strategy == 'gradual' | strategy == 'fixed')
  
  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% dplyr::select(metric, value) %>% filter(metric == 'Bmn' | metric == 'Hcm') %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added 
  
  # make the plots:
  # # step decrease
  # data_sub <- data_sub0 %>% filter(change_type == "step") %>% filter(direction=="decrease")
  # # mean biomass
  # data_sub1 <- data_sub %>% filter(metric == "Bmn")
  # sd_gg_data_sub_B_mn <- ggplot(data_sub1,
  #                               aes(x=strategy,y=value, colour=per_change, group=per_change)) +
  #   geom_point(alpha=.8,size=3)+
  #   #geom_jitter() +
  #   geom_line(alpha=0.8)+
  #   scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
  #   xlab("Management Strategy") +
  #   scale_x_discrete(limits = c('fixed','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
  #   ylab("Mean Population Biomass") +
  #   ggtitle("Step decrease") +
  #   theme_pubr(legend="right")+
  #   theme(
  #     axis.text.x=element_text(size=10),
  #     axis.text.y=element_text(size=10),
  #     axis.title = element_text(size = 14),
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank()
  #   )
  # 
  # # cumulative harvest
  # data_sub1 <- data_sub %>% filter(metric == "Hcm")
  # sd_gg_data_sub_H_cm <- ggplot(data_sub1,
  #                               aes(x=strategy,y=value,colour=per_change, group=per_change)) +
  #   geom_point(alpha=.8,size=3)+
  #   #geom_jitter() +
  #   geom_line(alpha=0.8)+
  #   scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
  #   xlab("Management Strategy") +
  #   scale_x_discrete(limits = c('fixed','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
  #   ylab("Cumulative Harvest") +
  #   ggtitle("Step decrease") +
  #   theme_pubr(legend="right")+
  #   theme(
  #     axis.text.x=element_text(size=10),
  #     axis.text.y=element_text(size=10),
  #     axis.title = element_text(size = 14),
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank()
  #   )
  # 
  # # step increase
  # data_sub <- data_sub0 %>% filter(change_type == "step") %>% filter(direction=="increase")
  # # mean biomass
  # data_sub1 <- data_sub %>% filter(metric == "Bmn")
  # si_gg_data_sub_B_mn <- ggplot(data_sub1,
  #                               aes(x=strategy,y=value, colour=per_change, group=per_change)) +
  #   geom_point(alpha=.8,size=3)+
  #   #geom_jitter() +
  #   geom_line(alpha=0.8)+
  #   scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
  #   xlab("Management Strategy") +
  #   scale_x_discrete(limits = c('fixed','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
  #   ylab("Mean Population Biomass") +
  #   ggtitle("Step increase") +
  #   theme_pubr(legend="right")+
  #   theme(
  #     axis.text.x=element_text(size=10),
  #     axis.text.y=element_text(size=10),
  #     axis.title = element_text(size = 14),
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank()
  #   )
  # 
  # # cumulative harvest
  # data_sub1 <- data_sub %>% filter(metric == "Hcm")
  # si_gg_data_sub_H_cm <- ggplot(data_sub1,
  #                               aes(x=strategy,y=value,colour=per_change, group=per_change)) +
  #   geom_point(alpha=.8,size=3)+
  #   #geom_jitter() +
  #   geom_line(alpha=0.8)+
  #   scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
  #   xlab("Management Strategy") +
  #   scale_x_discrete(limits = c('fixed','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
  #   ylab("Cumulative Harvest") +
  #   ggtitle("Step increase") +
  #   theme_pubr(legend="right")+
  #   theme(
  #     axis.text.x=element_text(size=10),
  #     axis.text.y=element_text(size=10),
  #     axis.title = element_text(size = 14),
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank()
  #   )
  
  
  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease")  # JS added 
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == 'Bmn') %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ylab("Mean Population Biomass") +
    ggtitle(paste0("Gradual decrease in ", change_par_p)) + # JS added 
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == "Hcm")
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == 'Hcm') %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added 
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ylab("Cumulative Harvest") +
    ggtitle(paste0("Gradual decrease in ", change_par_p)) + # JS added 
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  
  # gradual increase
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase")  # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == 'Bmn') %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ylab("Mean Population Biomass") +
    ggtitle(paste0("Gradual increase in ", change_par_p)) + # JS added 
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == "Hcm")
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == 'Hcm') %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added 
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ylab("Cumulative Harvest") +
    ggtitle(paste0("Gradual increase in ", change_par_p)) + # JS added 
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  
  
  # put them together, no legends
  plot_all_gcsq_nl <- plot_grid(#sd_gg_data_sub_B_mn,
                        #sd_gg_data_sub_H_cm,
                        #si_gg_data_sub_B_mn,
                        #si_gg_data_sub_H_cm,
                        gd_gg_data_sub_B_mn + theme(legend.position="none", axis.title.x = element_blank()),
                        gd_gg_data_sub_H_cm + theme(legend.position="none", axis.title.x = element_blank()),
                        gi_gg_data_sub_B_mn + theme(legend.position="none"),
                        gi_gg_data_sub_H_cm + theme(legend.position="none"),
                        labels="auto",
                        ncol=2#,
                        #rel_widths = c(1,-0.3, 1),scale=0.9
  ) #+
  # draw_text("Management Strategy",x=0.45,y=0.04,size = 12)+ # common x-axis label
  # draw_text(letters[1:6],x=c(0.26,0.53,0.26,0.53,0.26,0.53),y=c(0.87,0.87,0.58,0.58,0.3,0.3),size=12)
  
  # extract the legend from one of the gradual decrease plots
  legend_gd <- get_legend(
    # create some space to the left of the legend
    gd_gg_data_sub_B_mn + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  
  # extract the legend from one of the gradual decrease plots
  legend_gi <- get_legend(
    # create some space to the left of the legend
    gi_gg_data_sub_B_mn + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  
  # make one legend plot with 2 columns
  legend_combine <- plot_grid(legend_gd, legend_gi, nrow=2)
  
  # add the legend to the row we made earlier. Give it one-third of 
  # the width of one plot (via rel_widths).
  plot_all_gcsq <- plot_grid(plot_all_gcsq_nl, legend_combine, rel_widths = c(3, .4))
  
  #return(plot_all_gcsq)
  return(list(
    plot_all_gcsq,
    gd_gg_data_sub_B_mn, gd_gg_data_sub_H_cm, gi_gg_data_sub_B_mn, gi_gg_data_sub_H_cm
  )
  )
  
}

# function to glue things together 

# use these lines for testing the function 
# preplot_K_list <- preplot_K
# preplot_r_list <- preplot_r
# preplot_rK_list <- preplot_rK

gg_ABCD_pre_byscenario <- function(preplot_K_list, preplot_r_list, preplot_rK_list){
  
  # set ylims
  ## Bmn
  lims_B_mn <- c(
    min(
      layer_scales(preplot_K_list[[2]])$y$range$range, 
      layer_scales(preplot_K_list[[4]])$y$range$range,
      layer_scales(preplot_r_list[[2]])$y$range$range, 
      layer_scales(preplot_r_list[[4]])$y$range$range,
      layer_scales(preplot_rK_list[[2]])$y$range$range, 
      layer_scales(preplot_rK_list[[4]])$y$range$range
      ),
    max(
      layer_scales(preplot_K_list[[2]])$y$range$range, 
      layer_scales(preplot_K_list[[4]])$y$range$range,
      layer_scales(preplot_r_list[[2]])$y$range$range, 
      layer_scales(preplot_r_list[[4]])$y$range$range,
      layer_scales(preplot_rK_list[[2]])$y$range$range, 
      layer_scales(preplot_rK_list[[4]])$y$range$range
    )
  )
  
  ## Hcm
  lims_H_cm <- c(
    min(
      layer_scales(preplot_K_list[[3]])$y$range$range, 
      layer_scales(preplot_K_list[[5]])$y$range$range,
      layer_scales(preplot_r_list[[3]])$y$range$range, 
      layer_scales(preplot_r_list[[5]])$y$range$range,
      layer_scales(preplot_rK_list[[3]])$y$range$range, 
      layer_scales(preplot_rK_list[[5]])$y$range$range
    ),
    max(
      layer_scales(preplot_K_list[[3]])$y$range$range, 
      layer_scales(preplot_K_list[[5]])$y$range$range,
      layer_scales(preplot_r_list[[3]])$y$range$range, 
      layer_scales(preplot_r_list[[5]])$y$range$range,
      layer_scales(preplot_rK_list[[3]])$y$range$range, 
      layer_scales(preplot_rK_list[[5]])$y$range$range
    )
  )
  
  # grad decrease
  ## Bmn
  # put Bmn plots together, no legends
  gd_plot_B_mn <- plot_grid(
    preplot_K_list[[2]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
      ),
    preplot_r_list[[2]] + ylim(lims_B_mn) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[2]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="A",
    align="hv",
    nrow=1
    ) 
  #gd_plot_B_mn
  
  ## Hcm
  # put Hcm plots together, no legends
  gd_plot_H_cm <- plot_grid(
    preplot_K_list[[3]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
      ),
    preplot_r_list[[3]] + ylim(lims_H_cm) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[3]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="C",
    align="hv",
    nrow=1
  )  
  #gd_plot_H_cm
  
  # grad increase
  ## Bmn
  # put Bmn plots together, no legends
  gi_plot_B_mn <- plot_grid(
    preplot_K_list[[4]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
      ),
    preplot_r_list[[4]] + ylim(lims_B_mn) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[4]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="B",
    align="hv",
    nrow=1
  ) 
  #gi_plot_B_mn
  
  ## Hcm
  # put Hcm plots together, no legends
  gi_plot_H_cm <- plot_grid(
    preplot_K_list[[5]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
      ),
    preplot_r_list[[5]] + ylim(lims_H_cm) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[5]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="D",
    align="hv",
    nrow=1
  ) 
  #gi_plot_H_cm
  
  plot_B_mn <- plot_grid(gd_plot_B_mn, gi_plot_B_mn)
  plot_H_cm <- plot_grid(gd_plot_H_cm, gi_plot_H_cm)
  
  # grad decrease title
  gd_title <- ggdraw() + 
    draw_label(
      "Effects on population biomass", #Gradual decreases in K, r, or r and K
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  # grad increase title
  gi_title <- ggdraw() + 
    draw_label(
      "Effectives on harvest",
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  plot_gd_gi_B_mn_H_cm <- plot_grid(
    plot_grid(
      gd_title,
      plot_B_mn,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    ),
    plot_grid(
      gi_title,
      plot_H_cm,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    ),
    nrow=2
  )
  
  #plot_gd_gi_B_mn_H_cm
  return(plot_gd_gi_B_mn_H_cm)
  
  # ggsave(here::here('figures_full/ABCD_sensitivity/ABCDsens_pre_byscenario.png'),
  #        width=18,height=10)
  # 
}

# function to make plots showing relationships between mean biomass (x axis) and cumulative harvest (y axis) for different percent changes in K, r, or both for all grad decrease/increase scenarios pre

gg_ABCD_sens_Bmn_Hcm_plot_gcsq <- function(data_sub0, time_period){ # legend_title = % change in K, % change in r, or % change in r + K
  
  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')
  
  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )
  
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)
  
  data_sub0 <- data_sub0 %>% mutate(strategy = 
                                      case_when( 
                                        strategy == 'GCA' ~ 'gradual',
                                        strategy == 'ACA' ~ 'abrupt',
                                        strategy == 'SQ' ~ 'fixed',
                                        .default = strategy)
                                    ) %>% 
    filter(strategy == 'gradual' | strategy == 'fixed')
  
  data_sub1 <- data_sub0 %>% pivot_wider(names_from = metric, values_from = value)
  
  # set ylims
  ## Bmn
  lims_B_mn2 <- c(
    min(
      data_sub1$Bmn
    ),
    max(
      data_sub1$Bmn
    )
  )
  
  ## Hcm
  lims_H_cm2 <- c(
    min(
      data_sub1$Hcm
    ),
    max(
      data_sub1$Hcm
    )
  )
  
  # gradual decrease
  data_sub2 <- data_sub1 %>% filter(change_type == "grad") %>% filter(direction=="decrease") # JS added
  gd_gg_data_sub_B_mn_H_cm <- ggplot(data_sub2,
                                     aes(x=Bmn,y=Hcm, colour=per_change, group=per_change, shape = strategy)) +
    geom_point(alpha=.8,size=3) +
    geom_line(alpha=0.8) +
    scale_y_continuous(
      limits = c(lims_H_cm2[1], lims_H_cm2[2]),
      breaks = scales::breaks_pretty(10)) +
    scale_x_continuous(
      limits = c(lims_B_mn2[1], lims_B_mn2[2]),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc) +
    xlab("Mean Population Biomass") +
    ylab("Cumulative Harvest") +
    scale_shape_discrete(name = 'Strategy', limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ggtitle(paste0("Gradual decrease in ", change_par_p)) + # JS added 
    guides(shape = guide_legend(order = 1), colour = guide_colourbar(order = 2)) +
    theme_pubr(legend="right") +
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  # gradual increase
  data_sub3 <- data_sub1 %>% filter(change_type == "grad") %>% filter(direction=="increase")  # JS added
  gi_gg_data_sub_B_mn_H_cm <- ggplot(data_sub3,
                                     aes(x=Bmn,y=Hcm, colour=per_change, group=per_change, shape = strategy)) +
    geom_point(alpha=.8,size=3) +
    geom_line(alpha=0.8) +
    scale_y_continuous(
      limits = c(lims_H_cm2[1], lims_H_cm2[2]),
      breaks = scales::breaks_pretty(10)) +
    scale_x_continuous(
      limits = c(lims_B_mn2[1], lims_B_mn2[2]),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc) +
    xlab("Mean Population Biomass") +
    ylab("Cumulative Harvest") +
    scale_shape_discrete(name = 'Strategy', limits = c('fixed','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added 
    ggtitle(paste0("Gradual increase in ", change_par_p)) + # JS added 
    guides(shape = guide_legend(order = 1), colour = guide_colourbar(order = 2)) +
    theme_pubr(legend="right") +
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  # extract the legend from the gradual decrease plot
  legend_gd_B_mn_H_cm <- get_legend(
    # create some space to the left of the legend
    gd_gg_data_sub_B_mn_H_cm + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  
  # extract the legend from the gradual increase plot
  legend_gi_B_mn_H_cm <- get_legend(
    # create some space to the left of the legend
    gi_gg_data_sub_B_mn_H_cm + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  
  # make one legend plot with 2 columns
  legend_combine_B_mn_H_cm <- plot_grid(legend_gd_B_mn_H_cm, legend_gi_B_mn_H_cm, nrow=2)
  
  # remove legends
  gd_gg_data_sub_B_mn_H_cm <- gd_gg_data_sub_B_mn_H_cm + theme(legend.position="none")
  gi_gg_data_sub_B_mn_H_cm <- gi_gg_data_sub_B_mn_H_cm + theme(legend.position="none")
  
  return(list(
    gd_gg_data_sub_B_mn_H_cm, gi_gg_data_sub_B_mn_H_cm, legend_combine_B_mn_H_cm
  )
  )
  
}

# function that combines into 6 panel plot (1 for ea scenario of r, K, rK increase and decrease)

# use these lines for testing the function 
# preplot_Bmn_Hcm_K_list <- preplot_Bmn_Hcm_K
# preplot_Bmn_Hcm_r_list <- preplot_Bmn_Hcm_r
# preplot_Bmn_Hcm_rK_list <- preplot_Bmn_Hcm_rK

gg_ABCD_pre_Bmn_Hcm_byscenario <- function(preplot_Bmn_Hcm_K_list, preplot_Bmn_Hcm_r_list, preplot_Bmn_Hcm_rK_list){
 
  # set ylims and xlims
  ## Hcm
  lims_H_cm <- c(
    min(
      layer_scales(preplot_Bmn_Hcm_K_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_K_list[[2]])$y$range$range,
      layer_scales(preplot_Bmn_Hcm_r_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_r_list[[2]])$y$range$range,
      layer_scales(preplot_Bmn_Hcm_rK_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_rK_list[[2]])$y$range$range
    ),
    max(
      layer_scales(preplot_Bmn_Hcm_K_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_K_list[[2]])$y$range$range,
      layer_scales(preplot_Bmn_Hcm_r_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_r_list[[2]])$y$range$range,
      layer_scales(preplot_Bmn_Hcm_rK_list[[1]])$y$range$range, 
      layer_scales(preplot_Bmn_Hcm_rK_list[[2]])$y$range$range
    )
  )
  
  ## Bmn
  lims_B_mn <- c(
    min(
      layer_scales(preplot_Bmn_Hcm_K_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_K_list[[2]])$x$range$range,
      layer_scales(preplot_Bmn_Hcm_r_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_r_list[[2]])$x$range$range,
      layer_scales(preplot_Bmn_Hcm_rK_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_rK_list[[2]])$x$range$range
    ),
    max(
      layer_scales(preplot_Bmn_Hcm_K_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_K_list[[2]])$x$range$range,
      layer_scales(preplot_Bmn_Hcm_r_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_r_list[[2]])$x$range$range,
      layer_scales(preplot_Bmn_Hcm_rK_list[[1]])$x$range$range, 
      layer_scales(preplot_Bmn_Hcm_rK_list[[2]])$x$range$range
    )
  )
  
  # grad decrease
  p_d_Bmn_Hcm <- plot_grid(
    preplot_Bmn_Hcm_K_list[[1]] + xlim(lims_B_mn) + ylim(lims_H_cm) + theme(axis.title.x = element_blank()), 
    preplot_Bmn_Hcm_r_list[[1]] + xlim(lims_B_mn) + ylim(lims_H_cm) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()), 
    preplot_Bmn_Hcm_rK_list[[1]] + xlim(lims_B_mn) + ylim(lims_H_cm) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
    align="hv",
    nrow=1, 
    labels="auto"
  )
  
  # grad increase
  
  # pick up here 05-22-2024 need to remove axis titles
  
  p_i_Bmn_Hcm <- plot_grid(
    preplot_Bmn_Hcm_K_list[[2]] + xlim(lims_B_mn) + ylim(lims_H_cm), 
    preplot_Bmn_Hcm_r_list[[2]] + xlim(lims_B_mn) + ylim(lims_H_cm) + theme(axis.title.y = element_blank()),
    preplot_Bmn_Hcm_rK_list[[2]] + xlim(lims_B_mn) + ylim(lims_H_cm) + theme( axis.title.y = element_blank()),
    align="hv",
    nrow=1, 
    labels=c('d','e','f')
  )
  
  # pick up here 05-22-2024 need to add legend
  # combine
  plot_gd_gi_B_mn_H_cm_sixpanel <- plot_grid(
    plot_grid(p_d_Bmn_Hcm, p_i_Bmn_Hcm, nrow=2), 
    preplot_Bmn_Hcm_K_list[[3]],
    rel_widths = c(3, .4)
  )
  
  return(plot_gd_gi_B_mn_H_cm_sixpanel)
}

# # try out the turtle rabbit idea from adrian
# pal_both <- pnw_palette(name="Sunset",n=length(inc_set) + length(dec_set), type = 'continuous')
# 
# # filter out the data with the desired time period
# data_sub0 <- ABCDsens_df %>% filter(period == time_period)
# 
# data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))
# 
# data_sub1 <- data_sub0 %>% pivot_wider(names_from = metric, values_from = value)
# 
# # set ylims
# ## Bmn
# lims_B_mn2 <- c(
#   min(
#     data_sub1$Bmn
#   ),
#   max(
#     data_sub1$Bmn
#   )
# )
# 
# ## Hcm
# lims_H_cm2 <- c(
#   min(
#     data_sub1$Hcm
#   ),
#   max(
#     data_sub1$Hcm
#   )
# )
# 
# 
# ggplot(data_sub1 %>% filter(strategy == 'sq'),
#        aes(x=Bmn,y=Hcm, colour=per_change, group=change_par, shape = change_par)) +
#   facet_wrap(vars(direction)) +
#   geom_point(alpha=.8,size=3) +
#   geom_line(alpha=0.8) +
#   scale_y_continuous(
#     limits = c(lims_H_cm2[1], lims_H_cm2[2]),
#     breaks = scales::breaks_pretty(10)) +
#   scale_x_continuous(
#     limits = c(lims_B_mn2[1], lims_B_mn2[2]),
#     breaks = scales::breaks_pretty(10)) +
#   scale_colour_gradientn(colours = pal_both, name=str_wrap('% Change'), guide = gc) +
#   xlab("Mean Population Biomass") +
#   ylab("Cumulative Harvest") +
#   scale_shape_discrete(name = 'Demographic Shift', limits = c('K','r', 'rK')) +
#   ggtitle("Fixed Management Strategy") +
#   guides(shape = guide_legend(order = 1), colour = guide_colourbar(order = 2)) +
#   theme_pubr(legend="right") +
#   theme(
#     axis.text.x=element_text(size=10),
#     axis.text.y=element_text(size=10),
#     axis.title = element_text(size = 14),
#     strip.background = element_blank(),
#     strip.text.x = element_text(size = 12)
#   )
