# functions for making the ggplots (like plot_abcd_main_fun.R but allowing for plotting abrupt
# case as well, also for sensitivity to B0 and tchange)

# like figure 2 but plot Hr, biomass cv, and harvest cv
gg_ABCD_sens_plot_gcsq2 <- function(df, change_par_p, time_period, plot_pars, y_labs){ # legend_title = % change in K, % change in r, or % change in r + K

  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # get the results for the correct parameter changing
  data_sub0 <- df[which(df$change_par==change_par_p), ]
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% filter(strategy != 'abrupt') %>% dplyr::select(metric, value) %>% filter(metric == plot_pars[1] | metric == plot_pars[2]) %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added

  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease") %>% filter(strategy != 'abrupt') # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate \nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # third parameter
  data_sub1 <- data_sub %>% filter(metric == plot_pars[3])
  gd_gg_data_sub_Hr <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[3]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[3]) +
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
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase") %>% filter(strategy != 'abrupt') # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual increase in ", change_par_p)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  data_sub1 <- data_sub %>% filter(metric == plot_pars[3])
  gi_gg_data_sub_Hr <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[3]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[3]) +
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
    gd_gg_data_sub_Hr + theme(legend.position="none", axis.title.x = element_blank()),
    gi_gg_data_sub_B_mn + theme(legend.position="none"),
    gi_gg_data_sub_H_cm + theme(legend.position="none"),
    gi_gg_data_sub_Hr + theme(legend.position="none"),
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
    gd_gg_data_sub_B_mn, gd_gg_data_sub_H_cm, gd_gg_data_sub_Hr, gi_gg_data_sub_B_mn, gi_gg_data_sub_H_cm, gi_gg_data_sub_Hr
  )
  )

}

# add abrupt climate adaptive strategy to figure 2
gg_ABCD_sens_plot_gcsqac <- function(df, change_par_p, time_period, plot_pars, y_labs){ # legend_title = % change in K, % change in r, or % change in r + K

  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # get the results for the correct parameter changing
  data_sub0 <- df[which(df$change_par==change_par_p), ]
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% dplyr::select(metric, value) %>% filter(metric == plot_pars[1] | metric == plot_pars[2]) %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added

  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease")  # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate \nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
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
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase") # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
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




# add abrupt climate adaptive strategy to the Hr and CV plot
gg_ABCD_sens_plot_gcsqac2 <- function(df, change_par_p, time_period, plot_pars, y_labs){ # legend_title = % change in K, % change in r, or % change in r + K

  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # get the results for the correct parameter changing
  data_sub0 <- df[which(df$change_par==change_par_p), ]
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% dplyr::select(metric, value) %>% filter(metric == plot_pars[1] | metric == plot_pars[2]) %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added

  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease")  # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate \nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # third parameter
  data_sub1 <- data_sub %>% filter(metric == plot_pars[3])
  gd_gg_data_sub_Hr <- ggplot(data_sub1,
                              aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[3]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[3]) +
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
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase") # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual increase in ", change_par_p)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  data_sub1 <- data_sub %>% filter(metric == plot_pars[3])
  gi_gg_data_sub_Hr <- ggplot(data_sub1,
                              aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[3]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[3]) +
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
    gd_gg_data_sub_Hr + theme(legend.position="none", axis.title.x = element_blank()),
    gi_gg_data_sub_B_mn + theme(legend.position="none"),
    gi_gg_data_sub_H_cm + theme(legend.position="none"),
    gi_gg_data_sub_Hr + theme(legend.position="none"),
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
    gd_gg_data_sub_B_mn, gd_gg_data_sub_H_cm, gd_gg_data_sub_Hr, gi_gg_data_sub_B_mn, gi_gg_data_sub_H_cm, gi_gg_data_sub_Hr
  )
  )

}








# same as gg_ABCD_pre_byscenario function but add argument with labels for the rows, and for 3 row plot
gg_ABCD_byscenario2 <- function(preplot_K_list, preplot_r_list, preplot_rK_list, row_labs){

  # set ylims
  ## Bmn
  lims_B_mn <- c(
    min(
      layer_scales(preplot_K_list[[2]])$y$range$range,
      layer_scales(preplot_K_list[[5]])$y$range$range,
      layer_scales(preplot_r_list[[2]])$y$range$range,
      layer_scales(preplot_r_list[[5]])$y$range$range,
      layer_scales(preplot_rK_list[[2]])$y$range$range,
      layer_scales(preplot_rK_list[[5]])$y$range$range
    ),
    max(
      layer_scales(preplot_K_list[[2]])$y$range$range,
      layer_scales(preplot_K_list[[5]])$y$range$range,
      layer_scales(preplot_r_list[[2]])$y$range$range,
      layer_scales(preplot_r_list[[5]])$y$range$range,
      layer_scales(preplot_rK_list[[2]])$y$range$range,
      layer_scales(preplot_rK_list[[5]])$y$range$range
    )
  )

  ## Hcm
  lims_H_cm <- c(
    min(
      layer_scales(preplot_K_list[[3]])$y$range$range,
      layer_scales(preplot_K_list[[6]])$y$range$range,
      layer_scales(preplot_r_list[[3]])$y$range$range,
      layer_scales(preplot_r_list[[6]])$y$range$range,
      layer_scales(preplot_rK_list[[3]])$y$range$range,
      layer_scales(preplot_rK_list[[6]])$y$range$range
    ),
    max(
      layer_scales(preplot_K_list[[3]])$y$range$range,
      layer_scales(preplot_K_list[[6]])$y$range$range,
      layer_scales(preplot_r_list[[3]])$y$range$range,
      layer_scales(preplot_r_list[[6]])$y$range$range,
      layer_scales(preplot_rK_list[[3]])$y$range$range,
      layer_scales(preplot_rK_list[[6]])$y$range$range
    )
  )

  ## Hcm
  lims_Hr <- c(
    min(
      layer_scales(preplot_K_list[[4]])$y$range$range,
      layer_scales(preplot_K_list[[7]])$y$range$range,
      layer_scales(preplot_r_list[[4]])$y$range$range,
      layer_scales(preplot_r_list[[7]])$y$range$range,
      layer_scales(preplot_rK_list[[4]])$y$range$range,
      layer_scales(preplot_rK_list[[7]])$y$range$range
    ),
    max(
      layer_scales(preplot_K_list[[4]])$y$range$range,
      layer_scales(preplot_K_list[[7]])$y$range$range,
      layer_scales(preplot_r_list[[4]])$y$range$range,
      layer_scales(preplot_r_list[[6]])$y$range$range,
      layer_scales(preplot_rK_list[[4]])$y$range$range,
      layer_scales(preplot_rK_list[[7]])$y$range$range
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

  ## Hr
  # put Hr plots together, no legends
  gd_plot_Hr <- plot_grid(
    preplot_K_list[[4]] + ylim(lims_Hr) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
    ),
    preplot_r_list[[4]] + ylim(lims_Hr) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[4]] + ylim(lims_Hr) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="E",
    align="hv",
    nrow=1
  )

  # grad increase
  ## Bmn
  # put Bmn plots together, no legends
  gi_plot_B_mn <- plot_grid(
    preplot_K_list[[5]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
    ),
    preplot_r_list[[5]] + ylim(lims_B_mn) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[5]] + ylim(lims_B_mn) + theme(
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
    preplot_K_list[[6]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
    ),
    preplot_r_list[[6]] + ylim(lims_H_cm) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[6]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="D",
    align="hv",
    nrow=1
  )
  #gi_plot_H_cm

  ## Hcm
  # put Hcm plots together, no legends
  gi_plot_Hr <- plot_grid(
    preplot_K_list[[7]] + ylim(lims_Hr) + theme(
      axis.title.x = element_blank(),
      plot.title = element_blank()
    ),
    preplot_r_list[[7]] + ylim(lims_Hr) + theme(
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    preplot_rK_list[[7]] + ylim(lims_Hr) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    ),
    labels="F",
    align="hv",
    nrow=1
  )
  #gi_plot_H_cm

  plot_B_mn <- plot_grid(gd_plot_B_mn, gi_plot_B_mn)
  plot_H_cm <- plot_grid(gd_plot_H_cm, gi_plot_H_cm)
  plot_Hr <- plot_grid(gd_plot_Hr, gi_plot_Hr)

  # grad decrease title
  gd_title <- ggdraw() +
    draw_label(
      row_labs[1], #Gradual decreases in K, r, or r and K
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
      row_labs[2],
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
  gi_title2 <- ggdraw() +
    draw_label(
      row_labs[3],
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
    plot_grid(
      gi_title2,
      plot_Hr,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    ),
    nrow=3
  )

  #plot_gd_gi_B_mn_H_cm
  return(plot_gd_gi_B_mn_H_cm)

  # ggsave(here::here('figures_full/ABCD_sensitivity/ABCDsens_pre_byscenario.png'),
  #        width=18,height=10)
  #
}











# B0 or tchange comparing fixed to gradual climate adaptive only
# need to manipulate the data frame to change the name of the parameter that was varied to "sens_par"
gg_sens_plot_gcsq <- function(df, change_par_p, time_period, plot_pars, y_labs){ # legend_title = % change in K, % change in r, or % change in r + K

  par_set <- unique(df$sens_par)
  pal_d <- pnw_palette(name="Starfish",n=length(par_set), type = 'continuous')
  pal_i <- pnw_palette(name="Starfish",n=length(par_set), type = 'continuous')
  #pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')

  change_par_p2 <- ifelse(change_par_p=="rK", "r & K", change_par_p)

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # get the results for the correct parameter changing
  data_sub0 <- df[which(df$change_par==change_par_p), ]
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% filter(strategy != 'abrupt') %>% dplyr::select(metric, value) %>% filter(metric == plot_pars[1] | metric == plot_pars[2]) %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added

  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease") %>% filter(strategy != 'abrupt') # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate \nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p2)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p2)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # gradual increase
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase") %>% filter(strategy != 'abrupt') # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
    ggtitle(paste0("Gradual increase in ", change_par_p2)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq','gradual'), labels = c('Fixed', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual increase in ", change_par_p2)) + # JS added
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


# same as above but including the abrupt climate adaptive strategy
gg_sens_plot_gcsqac <- function(df, change_par_p, time_period, plot_pars, y_labs){ # legend_title = % change in K, % change in r, or % change in r + K

  par_set <- unique(df$sens_par)
  pal_d <- pnw_palette(name="Starfish",n=length(par_set), type = 'continuous')
  pal_i <- pnw_palette(name="Starfish",n=length(par_set), type = 'continuous')
  #pal_i <- pnw_palette(name="Sunset",n=length(inc_set), type = 'continuous')

  change_par_p2 <- ifelse(change_par_p=="rK", "r & K", change_par_p)

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # get the results for the correct parameter changing
  data_sub0 <- df[which(df$change_par==change_par_p), ]
  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # get ylim
  df_ylim <- data_sub0 %>% filter(change_type == "grad") %>% dplyr::select(metric, value) %>% filter(metric == plot_pars[1] | metric == plot_pars[2]) %>% group_by(metric) %>% summarise(y_ul = max(value)) # JS added

  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease")  # JS added
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',10), str_wrap('Climate \nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p2)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gd_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title_d,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual decrease in ", change_par_p2)) + # JS added
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
  data_sub1 <- data_sub %>% filter(metric == plot_pars[1])
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[1]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[1]) +
    ggtitle(paste0("Gradual increase in ", change_par_p2)) + # JS added
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # cumulative harvest
  data_sub1 <- data_sub %>% filter(metric == plot_pars[2])
  gi_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=sens_par, group=sens_par)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_y_continuous(
      limits = c(0, df_ylim %>% filter(metric == plot_pars[2]) %>% pull(y_ul)),
      breaks = scales::breaks_pretty(10)) +
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title_i,10), guide = gc)+
    #guides(colour = 'none') + # JS added
    xlab("Management Strategy") +
    #scale_x_discrete(limits = c('sq','gradual'), labels = c(str_wrap('Fixed',20), str_wrap('Climate\nAdaptive',20))) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    scale_x_discrete(limits = c('sq', 'abrupt', 'gradual'), labels = c('Fixed', 'Abrupt', 'Climate \nAdaptive')) + # 'abrupt', str_wrap('Abrupt Climate Adaptive',20), # JS added
    ylab(y_labs[2]) +
    ggtitle(paste0("Gradual increase in ", change_par_p2)) + # JS added
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



# join by scenario
gg_sens_byscenario <- function(preplot_K_list, preplot_r_list, preplot_rK_list){

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
      axis.title.x = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_r_list[[2]] + ylim(lims_B_mn) + theme(
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_rK_list[[2]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
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
      axis.title.x = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_r_list[[3]] + ylim(lims_H_cm) + theme(
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_rK_list[[3]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
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
      axis.title.x = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_r_list[[4]] + ylim(lims_B_mn) + theme(
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_rK_list[[4]] + ylim(lims_B_mn) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
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
      axis.title.x = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_r_list[[5]] + ylim(lims_H_cm) + theme(
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
    ),
    preplot_rK_list[[5]] + ylim(lims_H_cm) + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()#,
      #plot.title = element_blank()
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
      "Effects on harvest",
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











