# functions for plotting the results for different scenarios


#' function for plotting the results of the sensitivity analyses (multiple initial biomasses and course
#' correction timings
#' modified from gg_tot_plot.R in the "code" folder on github

# arguments:
#'data_sub = the data subset (i.e, increasing or decreasing K, step or gradual change) to plot
#'time_period = the time period of the simulation to plot (pre, post, or tot)
#' plot_title = the title of the plot panels (e.g., "Step Decrease in K, pre")


gg_sens_plot <- function(data_sub, time_period, plot_title){

  # filter out the data with the desired time period
  data_sub <- data_sub %>% filter(period == time_period)

  data_sub <- data_sub %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # make the plots:
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gg_data_sub_B_mn <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,shape=factor(chng_time2, levels = c('75','50','25')), colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Course correction timing",
                         labels = c("Lag (75)", "Sync (50)", "Lead (25)")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # biomass cv
  data_sub1 <- data_sub %>% filter(metric == "Bcv")
  gg_data_sub_B_cv <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,shape=factor(chng_time2, levels = c('75','50','25')), colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Course correction timing",
                         labels = c("Lag (75)", "Sync (50)", "Lead (25)")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Population Biomass") +
    ggtitle(plot_title) +
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
  gg_data_sub_H_cm <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,shape=factor(chng_time2, levels = c('75','50','25')), colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Course correction timing",
                         labels = c("Lag (75)", "Sync (50)", "Lead (25)")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # harvest cv
  data_sub1 <- data_sub %>% filter(metric == "Hcv")
  gg_data_sub_H_cv <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,shape=factor(chng_time2, levels = c('75','50','25')), colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Course correction timing",
                         labels = c("Lag (75)", "Sync (50)", "Lead (25)")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # mean harvest rate
  data_sub1 <- data_sub %>% filter(metric == "Hrmn")
  gg_data_sub_Hr_mn <- ggplot(data_sub1, #%>%
                              # filter(
                              #   Binit == 300 & chng_time2 == 50
                              # ),
                              aes(x=strategy,y=value,shape=factor(chng_time2, levels = c('75','50','25')), colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Course correction timing",
                         labels = c("Lag (75)", "Sync (50)", "Lead (25)")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Harvest Rate") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # put them together
  plot_all <- plot_grid(gg_data_sub_B_mn,
                        gg_data_sub_B_cv,
                        gg_data_sub_H_cm,
                        gg_data_sub_H_cv,
                        gg_data_sub_Hr_mn,
                        labels="auto"
                        #ncol=3,
                        #rel_widths = c(1,-0.3, 1),scale=0.9
  ) #+
  # draw_text("Management Strategy",x=0.45,y=0.04,size = 12)+ # common x-axis label
  # draw_text(letters[1:6],x=c(0.26,0.53,0.26,0.53,0.26,0.53),y=c(0.87,0.87,0.58,0.58,0.3,0.3),size=12)

  return(plot_all)

}


# same as above but without the course correction timing
gg_sens_plot2 <- function(data_sub, time_period, plot_title){

  # filter out the data with the desired time period
  data_sub <- data_sub %>% filter(period == time_period)

  data_sub <- data_sub %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))
  
  pal <- pnw_palette(name="Starfish",n=5, type = 'continuous')
  
  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # make the plots:
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gg_data_sub_B_mn <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value, colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # biomass cv
  data_sub1 <- data_sub %>% filter(metric == "Bcv")
  gg_data_sub_B_cv <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Population Biomass") +
    ggtitle(plot_title) +
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
  gg_data_sub_H_cm <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # harvest cv
  data_sub1 <- data_sub %>% filter(metric == "Hcv")
  gg_data_sub_H_cv <- ggplot(data_sub1, #%>%
                             # filter(
                             #   Binit == 300 & chng_time2 == 50
                             # ),
                             aes(x=strategy,y=value,colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # mean harvest rate
  data_sub1 <- data_sub %>% filter(metric == "Hrmn")
  gg_data_sub_Hr_mn <- ggplot(data_sub1, #%>%
                              # filter(
                              #   Binit == 300 & chng_time2 == 50
                              # ),
                              aes(x=strategy,y=value,colour=Binit, group=Binit)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal, name=str_wrap("Initial Biomass",20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Harvest Rate") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # put them together
  plot_all <- plot_grid(gg_data_sub_B_mn,
                        gg_data_sub_B_cv,
                        gg_data_sub_H_cm,
                        gg_data_sub_H_cv,
                        gg_data_sub_Hr_mn,
                        labels="auto"
                        #ncol=3,
                        #rel_widths = c(1,-0.3, 1),scale=0.9
  ) #+
  # draw_text("Management Strategy",x=0.45,y=0.04,size = 12)+ # common x-axis label
  # draw_text(letters[1:6],x=c(0.26,0.53,0.26,0.53,0.26,0.53),y=c(0.87,0.87,0.58,0.58,0.3,0.3),size=12)

  return(plot_all)

}


# same as above but without the course correction timing
gg_ABCD_sens_plot <- function(data_sub0, time_period, legend_title){ # legend_title = % change in K, % change in r, or % change in r + K

  pal_d <- pnw_palette(name="Starfish",n=length(dec_set), type = 'continuous')
  pal_i <- pnw_palette(name="Starfish",n=length(inc_set), type = 'continuous')

  gc <- guide_colorbar(
    frame.colour = "black",
    barheight = 8,
    frame.linewidth = 2,
    ticks.colour = "black",
    ticks.linewidth = 2
  )

  # filter out the data with the desired time period
  data_sub0 <- data_sub0 %>% filter(period == time_period)

  data_sub0 <- data_sub0 %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # make the plots:
  # step decrease
  data_sub <- data_sub0 %>% filter(change_type == "step") %>% filter(direction=="decrease")
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  sd_gg_data_sub_B_mn <- ggplot(data_sub1,
                             aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle("Step decrease") +
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
  sd_gg_data_sub_H_cm <- ggplot(data_sub1,
                             aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle("Step decrease") +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # step increase
  data_sub <- data_sub0 %>% filter(change_type == "step") %>% filter(direction=="increase")
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  si_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle("Step increase") +
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
  si_gg_data_sub_H_cm <- ggplot(data_sub1,
                                aes(x=strategy,y=value,colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle("Step increase") +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # grad decrease
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="decrease")
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gd_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle("Gradual decrease") +
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
    scale_colour_gradientn(colours = pal_d, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle("Gradual decrease") +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # gradual increase
  data_sub <- data_sub0 %>% filter(change_type == "grad") %>% filter(direction=="increase")
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gi_gg_data_sub_B_mn <- ggplot(data_sub1,
                                aes(x=strategy,y=value, colour=per_change, group=per_change)) +
    geom_point(alpha=.8,size=3)+
    #geom_jitter() +
    geom_line(alpha=0.8)+
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle("Gradual increase") +
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
    scale_colour_gradientn(colours = pal_i, name=str_wrap(legend_title,20), guide = gc)+
    xlab("Management Strategy") +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle("Gradual increase") +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )



  # put them together
  plot_all <- plot_grid(sd_gg_data_sub_B_mn,
                        sd_gg_data_sub_H_cm,
                        si_gg_data_sub_B_mn,
                        si_gg_data_sub_H_cm,
                        gd_gg_data_sub_B_mn,
                        gd_gg_data_sub_H_cm,
                        gi_gg_data_sub_B_mn,
                        gi_gg_data_sub_H_cm,
                        labels="auto",
                        ncol=2#,
                        #rel_widths = c(1,-0.3, 1),scale=0.9
  ) #+
  # draw_text("Management Strategy",x=0.45,y=0.04,size = 12)+ # common x-axis label
  # draw_text(letters[1:6],x=c(0.26,0.53,0.26,0.53,0.26,0.53),y=c(0.87,0.87,0.58,0.58,0.3,0.3),size=12)

  return(plot_all)

}


# function for plotting the results of the extension analyses
# data_sub = df with the results for the specific scenario (e.g., K step decrease, etc) to plot
# time_period = time period of simulation over which the summary metrics were calculate (pre, post, or tot)
# plot_title = the title of the plot
gg_ext_plot <- function(data_sub, time_period, plot_title){

  # filter out the data with the desired time period
  data_sub <- data_sub %>% filter(period == time_period)

  data_sub <- data_sub %>% mutate(strategy = if_else(strategy == "GCA", "gradual", if_else(strategy=="ACA", "abrupt", "sq")))

  # make the plots:
  # mean biomass
  data_sub1 <- data_sub %>% filter(metric == "Bmn")
  gg_data_sub_B_mn <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Population Biomass") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # biomass cv
  data_sub1 <- data_sub %>% filter(metric == "Bcv")
  gg_data_sub_B_cv <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Population Biomass") +
    ggtitle(plot_title) +
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
  gg_data_sub_H_cm <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # harvest cv
  data_sub1 <- data_sub %>% filter(metric == "Hcv")
  gg_data_sub_H_cv <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("CV Harvest") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # cumulative discounted revenue
  data_sub1 <- data_sub %>% filter(metric == "Rcm")
  gg_data_sub_R_cm <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Cumulative Revenue") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # mean harvest rate
  data_sub1 <- data_sub %>% filter(metric == "Hrmn")
  gg_data_sub_Hr_mn <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Mean Harvest Rate") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  # years below conservation limit
  data_sub1 <- data_sub %>% filter(metric == "propl_Bcons_true")
  gg_data_sub_propl_Bcons_true <- ggplot(data_sub1) +
    geom_point(alpha=.8,size=3, aes(x = strategy, y = value, colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), shape = factor(errors, levels = c('N', 'Y'))))+ #, , position = position_dodge2(width = 1)
    geom_line(alpha=0.8, aes(x = strategy, y = value, linetype = factor(errors, levels = c('N', 'Y')), colour = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')), group = interaction(factor(errors, levels = c('N', 'Y')), factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update')))))+
    geom_segment(aes(x = strategy, xend = strategy, y = Q025, yend = Q975, color = factor(hcr_type, levels = c('slope','constant_F','constant_Esc', 'update'))))+
    scale_color_manual(name = "HCR", values = c("dodgerblue", "orange", "chartreuse3", "orchid3"))+
    xlab("Management Strategy") +
    scale_shape_discrete(name = "Errors",
                         labels = c("deterministic", "stochastic")) +
    scale_linetype_manual(name = "Errors", values = c(1, 2),
                          labels = c("deterministic", "stochastic")) +
    scale_x_discrete(limits = c('sq','abrupt', 'gradual'), labels = c(str_wrap('Status Quo',20),str_wrap('Abrupt Climate Adaptive',20), str_wrap('Gradual Climate Adaptive',20))) +
    ylab("Proportion Years B < B_cons") +
    ggtitle(plot_title) +
    theme_pubr(legend="right")+
    theme(
      axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title = element_text(size = 14),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )


  # put them together
  plot_all <- plot_grid(gg_data_sub_B_mn,
                        gg_data_sub_B_cv,
                        gg_data_sub_H_cm,
                        gg_data_sub_H_cv,
                        gg_data_sub_R_cm,
                        gg_data_sub_Hr_mn,
                        gg_data_sub_propl_Bcons_true,
                        labels="auto"
                        #ncol=3,
                        #rel_widths = c(1,-0.3, 1),scale=0.9
  ) #+
  # draw_text("Management Strategy",x=0.45,y=0.04,size = 12)+ # common x-axis label
  # draw_text(letters[1:6],x=c(0.26,0.53,0.26,0.53,0.26,0.53),y=c(0.87,0.87,0.58,0.58,0.3,0.3),size=12)

  return(plot_all)

}

# function for plotting helper timeseries
#' sq_sim = simulation output for status quo strategy
#' aca_sim = simulation output for abrupt climate adaptive strategy
#' gca_sim = simulation output for gradual climate adaptive strategy
#' r = true r values at each timepoint
#' r_for2 = r values used in aca strategy
#' r_for3 = r values used in gca strategy
#' K = true K values at each timepoint
#' K_for2 = K values used in aca strategy
#' K_for3 = K values used in gca strategy
helper_ts <- function(sq_sim, aca_sim, gca_sim, r, r_for2, r_for3, K, K_for2, K_for3){
  # biomasses
  B_sq <- sq_sim$biomass
  B_ca <- aca_sim$biomass
  B_gca <- gca_sim$biomass

  # true target
  B_MSY_t <- K/2

  # true max harvest rate
  Hr_max_t <- r/2

  # targets for each strategy
  B_MSY_sq <- rep(K[1]/2, length(K))
  B_MSY_ca <- K_for2/2
  B_MSY_gca <- K_for3/2

  # harvest rates for each strategy
  Hr_sq <- sq_sim$harvest_rate
  Hr_ca <- aca_sim$harvest_rate
  Hr_gca <- gca_sim$harvest_rate

  # max harvest rates for each strategy
  Hr_max_sq <- rep(r[1]/2, length(r))
  Hr_max_ca <- r_for2/2
  Hr_max_gca <- r_for3/2


  # HCR regions (1 = zero, 2 = sloped, 3 = max)
  hcr_sq <- ifelse(sq_sim$biomass/(0.5*K[1]) < 0.2, 1, ifelse(sq_sim$biomass/(0.5*K[1]) >=0.2 & sq_sim$biomass/(0.5*K[1]) < 0.9999, 2, 3))
  hcr_ca <- ifelse(aca_sim$biomass/(0.5*K_for2) < 0.2, 1, ifelse(aca_sim$biomass/(0.5*K_for2) >=0.2 & aca_sim$biomass/(0.5*K_for2) < 0.9999, 2, 3))
  hcr_gca <- ifelse(gca_sim$biomass/(0.5*K_for3) < 0.2, 1, ifelse(gca_sim$biomass/(0.5*K_for3) >=0.2 & gca_sim$biomass/(0.5*K_for3) < 0.9999, 2, 3))

  # harvest
  H_sq <- sq_sim$yield
  H_ca <- aca_sim$yield
  H_gca <- gca_sim$yield

  # make data frames for each strategy
  sq_df <- data.frame(
    t = c(1:years),
    B = B_sq,
    Hr = Hr_sq,
    H = H_sq,
    # B_MSY_t = B_MSY_t,
    B_MSY = B_MSY_sq,
    Hr_max = Hr_max_sq,
    hcr = hcr_sq
  )

  # for changing color for each region of hcr
  sq_df$t_0 <- ifelse(sq_df$hcr==1, sq_df$t, NA)
  sq_df$t_sl <- ifelse(sq_df$hcr==2, sq_df$t, NA)
  sq_df$t_mx <- ifelse(sq_df$hcr==3, sq_df$t, NA)

  ca_df <- data.frame(
    t = c(1:years),
    B = B_ca,
    Hr = Hr_ca,
    H = H_ca,
    #B_MSY_t = B_MSY_t,
    B_MSY = B_MSY_ca,
    Hr_max = Hr_max_ca,
    hcr = hcr_ca
  )

  # for changing color for each region of hcr
  ca_df$t_0 <- ifelse(ca_df$hcr==1, ca_df$t, NA)
  ca_df$t_sl <- ifelse(ca_df$hcr==2, ca_df$t, NA)
  ca_df$t_mx <- ifelse(ca_df$hcr==3, ca_df$t, NA)

  gca_df <- data.frame(
    t = c(1:years),
    B = B_gca,
    Hr = Hr_gca,
    H = H_gca,
    #B_MSY_t = B_MSY_t,
    B_MSY = B_MSY_gca,
    Hr_max = Hr_max_gca,
    hcr = hcr_gca
  )

  # for changing color for each region of hcr
  gca_df$t_0 <- ifelse(gca_df$hcr==1, gca_df$t, NA)
  gca_df$t_sl <- ifelse(gca_df$hcr==2, gca_df$t, NA)
  gca_df$t_mx <- ifelse(gca_df$hcr==3, gca_df$t, NA)

  col1 <- "black"

  lty_lwds <- c(1.5, 1.5, 2) # thickness of the lines
  bar_lwd <- 5.5 #thickness of the bars indicating which region of the HCR is being used

  layout(cbind(c(1,2,3),c(4, 5,6),c(7, 8, 9)))
  par(mar=c(.1,.1,.1,.1),oma=c(4,4,1, 4))
  # FIRST COLUMN: STATUS QUO STRATEGY
  # y limits
  # biomass plot
  B_min <-0 #min(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY, ca_df$B, ca_df$B_MSY, gca_df$B, gca_df$B_MSY)
  B_max <- max(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY, ca_df$B, ca_df$B_MSY, gca_df$B, gca_df$B_MSY)
  # harvest rate plot
  Hr_min <- 0#min(sq_df$Hr, ca_df$Hr, gca_df$Hr)
  Hr_max2 <- max(sq_df$Hr, sq_df$Hr_max, ca_df$Hr, ca_df$Hr_max, gca_df$Hr, gca_df$Hr_max)+ 0.005 # max y value for Hr plot
  # harvest plot
  H_min <- 0#min(sq_df$H, ca_df$H, gca_df$H)
  H_max <- max(sq_df$H, ca_df$H, gca_df$H)

  # first row = biomass
  plot(x = sq_df$t, y = sq_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1])
  mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Status Quo")
  lines(x = sq_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  legend("bottomleft", legend = c("pop. biomass", expression(imposed~B[MSY]), expression(true~B[MSY])), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = sq_df$t, y = sq_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1])#yaxs = "i"
  mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = sq_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = sq_df$t, y = 0*sq_df$hcr + Hr_max2, col = ifelse(sq_df$t %in% sq_df$t_0, "#4CB7D7", ifelse(sq_df$t %in% sq_df$t_sl, "#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  legend("left", inset = 0.03, legend = c("actual", "imposed max", "true max (r/2)"), lty = c(1, 2, 3), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), title = "Harvest rate")
  legend("right", inset = 0.03, legend = c("zero", "sloped", "max (MSY)"), pch = c(15, 15, 15), col = c("#4CB7D7",  "#CB9BA2", "#CF3413"), bty = "n", title = "HCR")

  # third row = harvest (yield)
  plot(x = sq_df$t, y = sq_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1])#yaxs = "i"
  mtext(side = 2, "Harvest", line = 2.5)

  # SECOND COLUMN: ABRUPT CLIMATE ADAPTIVE STRATEGY
  # first row = biomass
  plot(x = ca_df$t, y = ca_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")
  #mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Abrupt climate adaptive")
  lines(x = ca_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = ca_df$t, y = ca_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", "imposed B_MSY", "true B_MSY"), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = ca_df$t, y = ca_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = ca_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = ca_df$t, y = ca_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = ca_df$t, y = 0*ca_df$hcr + Hr_max2, col = ifelse(ca_df$t %in% ca_df$t_0, "#4CB7D7", ifelse(ca_df$t %in% ca_df$t_sl,"#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("bottomleft", legend = c("harvest rate", "max harvest rate"), lty = c(1, 2), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2]))

  # third row = harvest (yield)
  plot(x = ca_df$t, y = ca_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest", line = 2.5)
  mtext(side = 1, "Time", line = 2.5)

  # THIRD COLUMN: GRADUAL CLIMATE ADAPTIVE STRATEGY
  # first row = biomass
  plot(x = gca_df$t, y = gca_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")
  #mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Gradual climate adaptive")
  lines(x = gca_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", "imposed B_MSY", "true B_MSY"), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = gca_df$t, y = gca_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = gca_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = gca_df$t, y = 0*gca_df$hcr + Hr_max2, col = ifelse(gca_df$t %in% gca_df$t_0, "#4CB7D7", ifelse(gca_df$t %in% gca_df$t_sl,"#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("bottomleft", legend = c("harvest rate", "max harvest rate"), lty = c(1, 2), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2]))

  # third row = harvest (yield)
  plot(x = gca_df$t, y = gca_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest", line = 2.5)

}


# newer helper ts function (no abrupt climate adaptive case)
# function for plotting helper timeseries
helper_ts2 <- function(sq_sims, gca_sims, r_list, r_for2_list, r_for3_list, K_list,
                       K_for2_list, K_for3_list){

  # y limits
  
  # true target, SQ
  B_MSY_t1 <- K_list[[1]]/2
  # true max harvest rate
  Hr_max_t1 <- r_list[[1]]/2
  # targets for each strategy
  B_MSY_sq1 <- rep(K_list[[1]][1]/2, length(K_list[[1]]))
  B_MSY_gca1 <- K_for3_list[[1]]/2
  
  # true target, GCA
  B_MSY_t2 <- K_list[[2]]/2
  # true max harvest rate
  Hr_max_t2 <- r_list[[2]]/2
  # targets for each strategy
  B_MSY_sq2 <- rep(K_list[[2]][1]/2, length(K_list[[2]]))
  B_MSY_gca2 <- K_for3_list[[2]]/2
  
  
  # biomass plot
  B_min <-0 
  B_max <- max(sq_sims[[1]]$biomass, B_MSY_t1, B_MSY_sq1, gca_sims[[1]]$biomass, B_MSY_gca1,
               sq_sims[[2]]$biomass, B_MSY_t2, B_MSY_sq2, gca_sims[[2]]$biomass, B_MSY_gca2)
  # harvest rate plot
  Hr_min <- 0
  Hr_max2 <- max(sq_sims[[1]]$harvest_rate, Hr_max_t1, gca_sims[[1]]$harvest_rate, Hr_max_t1,
                 sq_sims[[2]]$harvest_rate, Hr_max_t2, gca_sims[[2]]$harvest_rate, Hr_max_t2)+ 0.005 # max y value for Hr plot
  # harvest plot
  H_min <- 0#min(sq_df$H, ca_df$H, gca_df$H)
  H_max <- max(sq_sims[[1]]$yield, gca_sims[[1]]$yield, sq_sims[[2]]$yield, gca_sims[[2]]$yield)
  
  
  i <- 1
  # biomasses
  B_sq <- sq_sims[[i]]$biomass
  #B_ca <- aca_sim[[i]]$biomass
  B_gca <- gca_sims[[i]]$biomass

  # true target
  B_MSY_t <- K_list[[i]]/2

  # true max harvest rate
  Hr_max_t <- r_list[[i]]/2

  # targets for each strategy
  B_MSY_sq <- rep(K_list[[i]][1]/2, length(K_list[[i]]))
  #B_MSY_ca <- K_for2_list[[i]]/2
  B_MSY_gca <- K_for3_list[[i]]/2

  # harvest rates for each strategy
  Hr_sq <- sq_sims[[i]]$harvest_rate
  #Hr_ca <- aca_sims[[i]]$harvest_rate
  Hr_gca <- gca_sims[[i]]$harvest_rate

  # max harvest rates for each strategy
  Hr_max_sq <- rep(r_list[[i]][1]/2, length(r_list[[i]]))
  #Hr_max_ca <- r_for2_list[[i]]/2
  Hr_max_gca <- r_for3_list[[i]]/2


  # HCR regions (1 = zero, 2 = sloped, 3 = max)
  hcr_sq <- ifelse(sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) < 0.2, 1, ifelse(sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) >=0.2 & sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) < 0.9999, 2, 3))
  #hcr_ca <- ifelse(aca_sims[[i]]$biomass/(0.5*K_for2_list[[i]]) < 0.2, 1, ifelse(aca_sims[[i]]$biomass/(0.5*K_for2_list[[i]]) >=0.2 & aca_sims[[i]]$biomass/(0.5*K_for2_list[[i]]) < 0.9999, 2, 3))
  hcr_gca <- ifelse(gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) < 0.2, 1, ifelse(gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) >=0.2 & gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) < 0.9999, 2, 3))

  # harvest
  H_sq <- sq_sims[[i]]$yield
  #H_ca <- aca_sims[[i]]$yield
  H_gca <- gca_sims[[i]]$yield

  # make data frames for each strategy
  sq_df <- data.frame(
    t = c(1:years),
    B = B_sq,
    Hr = Hr_sq,
    H = H_sq,
    # B_MSY_t = B_MSY_t,
    B_MSY = B_MSY_sq,
    Hr_max = Hr_max_sq,
    hcr = hcr_sq
  )

  # for changing color for each region of hcr
  sq_df$t_0 <- ifelse(sq_df$hcr==1, sq_df$t, NA)
  sq_df$t_sl <- ifelse(sq_df$hcr==2, sq_df$t, NA)
  sq_df$t_mx <- ifelse(sq_df$hcr==3, sq_df$t, NA)

  #ca_df <- data.frame(t = c(1:years), B = B_ca, Hr = Hr_ca, H = H_ca,
   # B_MSY = B_MSY_ca,  Hr_max = Hr_max_ca, hcr = hcr_ca
  #)

  # for changing color for each region of hcr
  #ca_df$t_0 <- ifelse(ca_df$hcr==1, ca_df$t, NA)
  #ca_df$t_sl <- ifelse(ca_df$hcr==2, ca_df$t, NA)
  #ca_df$t_mx <- ifelse(ca_df$hcr==3, ca_df$t, NA)

  gca_df <- data.frame(
    t = c(1:years),
    B = B_gca,
    Hr = Hr_gca,
    H = H_gca,
    #B_MSY_t = B_MSY_t,
    B_MSY = B_MSY_gca,
    Hr_max = Hr_max_gca,
    hcr = hcr_gca
  )

  # for changing color for each region of hcr
  gca_df$t_0 <- ifelse(gca_df$hcr==1, gca_df$t, NA)
  gca_df$t_sl <- ifelse(gca_df$hcr==2, gca_df$t, NA)
  gca_df$t_mx <- ifelse(gca_df$hcr==3, gca_df$t, NA)

  col1 <- "black"

  lty_lwds <- c(1.5, 1.5, 2) # thickness of the lines
  bar_lwd <- 5.5 #thickness of the bars indicating which region of the HCR is being used
  

  layout(cbind(c(1,2,3),c(4, 5,6),c(7, 8, 9), c(10, 11, 12)))
  par(mar=c(.1,.1,.1,.1),oma=c(4,4,4,1))
  # FIRST COLUMN: FIXED STRATEGY, GRAD DECREASE
  # y limits
  # biomass plot
  # B_min <-0 #min(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY, ca_df$B, ca_df$B_MSY, gca_df$B, gca_df$B_MSY)
  # B_max <- max(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY,gca_df$B, gca_df$B_MSY)
  # # harvest rate plot
  # Hr_min <- 0#min(sq_df$Hr, ca_df$Hr, gca_df$Hr)
  # Hr_max2 <- max(sq_df$Hr, sq_df$Hr_max, gca_df$Hr, gca_df$Hr_max)+ 0.005 # max y value for Hr plot
  # # harvest plot
  # H_min <- 0#min(sq_df$H, ca_df$H, gca_df$H)
  # H_max <- max(sq_df$H, gca_df$H)

  # first row = biomass
  plot(x = sq_df$t, y = sq_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1])
  mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Fixed")
  mtext(side = 3, "Gradual decrease", line = 2.5, adj = 2.5)
  lines(x = sq_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", expression(imposed~B[MSY]), expression(true~B[MSY])), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = sq_df$t, y = sq_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1])#yaxs = "i"
  mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = sq_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = sq_df$t, y = 0*sq_df$hcr + Hr_max2, col = ifelse(sq_df$t %in% sq_df$t_0, "#4CB7D7", ifelse(sq_df$t %in% sq_df$t_sl, "#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("left", inset = 0.03, legend = c("actual", "imposed max", "true max (r/2)"), lty = c(1, 2, 3), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), title = "Harvest rate")
 # legend("right", inset = 0.03, legend = c("zero", "sloped", "max (MSY)"), pch = c(15, 15, 15), col = c("#4CB7D7",  "#CB9BA2", "#CF3413"), bty = "n", title = "HCR")

  # third row = harvest (yield)
  plot(x = sq_df$t, y = sq_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1])#yaxs = "i"
  mtext(side = 2, "Harvest", line = 2.5)


  # SECOND COLUMN: GRADUAL CLIMATE ADAPTIVE STRATEGY, GRAD DECREASE
  # first row = biomass
  plot(x = gca_df$t, y = gca_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")
  #mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Climate adaptive")
  lines(x = gca_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", "imposed B_MSY", "true B_MSY"), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = gca_df$t, y = gca_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = gca_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = gca_df$t, y = 0*gca_df$hcr + Hr_max2, col = ifelse(gca_df$t %in% gca_df$t_0, "#4CB7D7", ifelse(gca_df$t %in% gca_df$t_sl,"#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("bottomleft", legend = c("harvest rate", "max harvest rate"), lty = c(1, 2), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2]))

  # third row = harvest (yield)
  plot(x = gca_df$t, y = gca_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest", line = 2.5)
  mtext(side = 1, "Time (year)", line = 2.5, adj = 1.5)

  i <- 2
  # biomasses
  B_sq <- sq_sims[[i]]$biomass; B_gca <- gca_sims[[i]]$biomass

  # true target
  B_MSY_t <- K_list[[i]]/2

  # true max harvest rate
  Hr_max_t <- r_list[[i]]/2

  # targets for each strategy
  B_MSY_sq <- rep(K_list[[i]][1]/2, length(K_list[[i]])); B_MSY_gca <- K_for3_list[[i]]/2

  # harvest rates for each strategy
  Hr_sq <- sq_sims[[i]]$harvest_rate; Hr_gca <- gca_sims[[i]]$harvest_rate

  # max harvest rates for each strategy
  Hr_max_sq <- rep(r_list[[i]][1]/2, length(r_list[[i]])); Hr_max_gca <- r_for3_list[[i]]/2

  # HCR regions (1 = zero, 2 = sloped, 3 = max)
  hcr_sq <- ifelse(sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) < 0.2, 1, ifelse(sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) >=0.2 & sq_sims[[i]]$biomass/(0.5*K_list[[i]][1]) < 0.9999, 2, 3))
  hcr_gca <- ifelse(gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) < 0.2, 1, ifelse(gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) >=0.2 & gca_sims[[i]]$biomass/(0.5*K_for3_list[[i]]) < 0.9999, 2, 3))

  # harvest
  H_sq <- sq_sims[[i]]$yield; H_gca <- gca_sims[[i]]$yield

  # make data frames for each strategy
  sq_df <- data.frame(t = c(1:years), B = B_sq, Hr = Hr_sq, H = H_sq,
    B_MSY = B_MSY_sq, Hr_max = Hr_max_sq, hcr = hcr_sq
  )

  # for changing color for each region of hcr
  sq_df$t_0 <- ifelse(sq_df$hcr==1, sq_df$t, NA)
  sq_df$t_sl <- ifelse(sq_df$hcr==2, sq_df$t, NA)
  sq_df$t_mx <- ifelse(sq_df$hcr==3, sq_df$t, NA)

  gca_df <- data.frame(t = c(1:years), B = B_gca, Hr = Hr_gca,  H = H_gca,
    B_MSY = B_MSY_gca, Hr_max = Hr_max_gca, hcr = hcr_gca
  )

  # for changing color for each region of hcr
  gca_df$t_0 <- ifelse(gca_df$hcr==1, gca_df$t, NA)
  gca_df$t_sl <- ifelse(gca_df$hcr==2, gca_df$t, NA)
  gca_df$t_mx <- ifelse(gca_df$hcr==3, gca_df$t, NA)

  # THIRD COLUMN: FIXED STRATEGY, GRAD INCREASE
  # y limits
  # biomass plot
  # B_min <-0 #min(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY, ca_df$B, ca_df$B_MSY, gca_df$B, gca_df$B_MSY)
  # B_max <- max(sq_df$B, sq_df$B_MSY_t, sq_df$B_MSY, gca_df$B, gca_df$B_MSY)
  # # harvest rate plot
  # Hr_min <- 0#min(sq_df$Hr, ca_df$Hr, gca_df$Hr)
  # Hr_max2 <- max(sq_df$Hr, sq_df$Hr_max, gca_df$Hr, gca_df$Hr_max)+ 0.005 # max y value for Hr plot
  # # harvest plot
  # H_min <- 0#min(sq_df$H, ca_df$H, gca_df$H)
  # H_max <- max(sq_df$H, gca_df$H)

  # first row = biomass
  plot(x = sq_df$t, y = sq_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")
  #mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Fixed")
  mtext(side = 3, "Gradual increase", line = 2.5, adj = 2.5)
  lines(x = sq_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", expression(imposed~B[MSY]), expression(true~B[MSY])), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")

  # second row = harvest rate
  plot(x = sq_df$t, y = sq_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = sq_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = sq_df$t, y = sq_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = sq_df$t, y = 0*sq_df$hcr + Hr_max2, col = ifelse(sq_df$t %in% sq_df$t_0, "#4CB7D7", ifelse(sq_df$t %in% sq_df$t_sl, "#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("left", inset = 0.03, legend = c("actual", "imposed max", "true max (r/2)"), lty = c(1, 2, 3), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), title = "Harvest rate")
  #legend("right", inset = 0.03, legend = c("zero", "sloped", "max (MSY)"), pch = c(15, 15, 15), col = c("#4CB7D7",  "#CB9BA2", "#CF3413"), bty = "n", title = "HCR")

  # third row = harvest (yield)
  plot(x = sq_df$t, y = sq_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1], yaxt ="n")#yaxs = "i"
  #mtext(side = 2, "Harvest", line = 2.5)

  # FOURTH COLUMN: GRADUAL CLIMATE ADAPTIVE STRATEGY, GRAD INCREASE
  # first row = biomass
  plot(x = gca_df$t, y = gca_df$B, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(B_min, B_max), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")
  #mtext(side = 2, "Biomass", line = 2.5)
  mtext(side = 3, "Climate adaptive")
  lines(x = gca_df$t, y = B_MSY_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$B_MSY, type = "l", lty = 2, lwd = lty_lwds[2])
  #legend("bottomleft", legend = c("pop. biomass", "imposed B_MSY", "true B_MSY"), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")
  legend("bottomright", legend = c("Pop. biomass", expression(Imposed~B[MSY]), expression(True~B[MSY])), lty = c(1, 2, 3), lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), bty = "n")


  # second row = harvest rate
  plot(x = gca_df$t, y = gca_df$Hr, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(Hr_min, Hr_max2), xaxt = "n", lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest rate", line = 2.5)
  lines(x = gca_df$t, y = Hr_max_t, type = "l", lty = 3, lwd = lty_lwds[3])
  lines(x = gca_df$t, y = gca_df$Hr_max, type = "l", lty = 2, lwd = lty_lwds[2])
  lines(x = gca_df$t, y = 0*gca_df$hcr + Hr_max2, col = ifelse(gca_df$t %in% gca_df$t_0, "#4CB7D7", ifelse(gca_df$t %in% gca_df$t_sl,"#CB9BA2", "#CF3413")), lwd = bar_lwd, type = "p", pch = 15)
  #legend("bottomleft", legend = c("harvest rate", "max harvest rate"), lty = c(1, 2), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2]))
  legend("bottomright", inset = 0.03, legend = c("Implemented", "Imposed max", "True max (r/2)"), lty = c(1, 2, 3), bty = "n", lwd = c(lty_lwds[1], lty_lwds[2], lty_lwds[3]), title = "Harvest rate")
  legend("topright", inset = c(0.03, 0.2), legend = c("Zero", "Sloped", "Max (MSY)"), pch = c(15, 15, 15), col = c("#4CB7D7",  "#CB9BA2", "#CF3413"), bty = "n", title = "HCR")



  # third row = harvest (yield)
  plot(x = gca_df$t, y = gca_df$H, type = "l", xlab = NA, ylab = NA, las = 1, ylim = c(H_min, H_max), lwd = lty_lwds[1], yaxt = "n")#yaxs = "i"
  #mtext(side = 2, "Harvest", line = 2.5)


}


# function for plotting the time intervals over which the manamgement strategies produce different biomass and harvest values
#' tdiff_list = list where each element is a df with the years for which a focal management strategy differs
#' from one of the others (first element = aca vs. sq, second = gca vs sq, third = gca vs. aca); this is the
#' output of the tdiff_fun in results_funs.R
#' plot_title = title of the plot
tdiff_plot <- function(tdiff_list, plot_title){
  layout(c(1, 2))
  par(mar=c(.15,.1,.1,.1),oma=c(4,0.1,1,0.1))
  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(-10, 100), yaxt = "n", xaxt = "n", ylab = NA, xlab = NA)
  axis(side = 1, at = c(0, 20, 40, 60, 80, 100), labels = NA)
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$B_diff[i])==F){
        if(pdt$B_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$B_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  #mtext(side = 1, "year", line = 2.5)
  mtext(side = 3, "Years when biomass differs", line = -1)
  mtext(side = 3, plot_title, adj = 0)
  text(x = text_pos, y = 1-text_adj, "ACA < SQ", col = small_col, cex = text_cex)
  text(x = text_pos, y = 1 + text_adj, "ACA > SQ", col = big_col, cex = text_cex)
  text(x = text_pos, y = 2-text_adj, "GCA < SQ", col = small_col, cex = text_cex)
  text(x = text_pos, y = 2 + text_adj, "GCA > SQ", col = big_col, cex = text_cex)
  text(x = text_pos, y = 3-text_adj, "GCA < ACA", col = small_col, cex = text_cex)
  text(x = text_pos, y = 3 + text_adj, "GCA > ACA", col = big_col, cex = text_cex)

  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(-10, 100), yaxt = "n", ylab = NA, xlab = NA)
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$H_diff[i])==F){
        if(pdt$H_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$H_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  mtext(side = 1, "year", line = 2.5)
  mtext(side = 3, "Years when harvest differs", line = -1)
  text(x = text_pos, y = 1-text_adj, "ACA < SQ", col = small_col, cex = text_cex)
  text(x = text_pos, y = 1 + text_adj, "ACA > SQ", col = big_col, cex = text_cex)
  text(x = text_pos, y = 2-text_adj, "GCA < SQ", col = small_col, cex = text_cex)
  text(x = text_pos, y = 2 + text_adj, "GCA > SQ", col = big_col, cex = text_cex)
  text(x = text_pos, y = 3-text_adj, "GCA < ACA", col = small_col, cex = text_cex)
  text(x = text_pos, y = 3 + text_adj, "GCA > ACA", col = big_col, cex = text_cex)

}


# updated tdiff_plot focusing just on the fixed vs. gradual climate adaptive cases

tdiff_plot2 <- function(tdiff_list_K, tdiff_list_r, tdiff_list_rK){
  layout(matrix(c(c(1, 2), c(3,4)), nrow = 2))
  par(mar=c(.15,.1,.1,.1),oma=c(4,4,1,0.1))

  # FIRST COLUMN: GRAD DECREASE
  tdiff_list <- list(tdiff_list_K[[1]][[2]], tdiff_list_r[[1]][[2]], tdiff_list_rK[[1]][[2]])

  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(0, 100), yaxt = "n", xaxt = "n", ylab = NA, xlab = NA)
  #axis(side = 1, at = c(0, 20, 40, 60, 80, 100), labels = NA)
  axis(side = 2, at = c(1, 2, 3), labels = c("K", "r", "r & K"), las = 1)
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$B_diff[i])==F){
        if(pdt$B_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$B_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  #mtext(side = 1, "year", line = 2.5)
  mtext(side = 3, "Years when biomass differs", line = -1, cex = 0.8)
  mtext(side = 3, "Gradual decrease")

  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(0, 100), yaxt = "n", ylab = NA, xlab = NA)
  axis(side = 2, at = c(1, 2, 3), labels = c("K", "r", "r & K"), las = 1)
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$H_diff[i])==F){
        if(pdt$H_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$H_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  mtext(side = 1, "Year", line = 2.5, adj = 1.1)
  mtext(side = 2, "Parameter changing", line = 3, adj = -2)
  mtext(side = 3, "Years when harvest differs", line = -1, cex = 0.8)

  # SECOND COLUMN: GRAD INCREASE
  tdiff_list <- list(tdiff_list_K[[2]][[2]], tdiff_list_r[[2]][[2]], tdiff_list_rK[[2]][[2]])

  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(0, 100), yaxt = "n", xaxt = "n", ylab = NA, xlab = NA)
  #axis(side = 1, at = c(0, 20, 40, 60, 80, 100), labels = NA)
  #axis(side = 2, at = c(1, 2, 3), labels = c("K", "r", "r & K"))
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$B_diff[i])==F){
        if(pdt$B_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$B_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  #mtext(side = 1, "year", line = 2.5)
  mtext(side = 3, "Years when biomass differs", line = -1, cex = 0.8)
  mtext(side = 3, "Gradual increase")
  text(x = text_pos, y = 1.5-text_adj, "Climate adaptive < Fixed", col = small_col, cex = text_cex)
  text(x = text_pos, y = 1.5 + text_adj, "Climate adaptive > Fixed", col = big_col, cex = text_cex)


  plot(x = c(1:years), y = rep(NA, years), ylim = c(0.5, 3.5), xlim = c(0, 100), yaxt = "n", ylab = NA, xlab = NA)
  #axis(side = 2, at = c(1, 2, 3), labels = c("K", "r", "r & K"))
  for(j in 1:3){
    pdt <- tdiff_list[[j]] # data to plot
    for(i in 1:years){
      if(is.na(pdt$H_diff[i])==F){
        if(pdt$H_diff[i]=="bigger"){
          points(x = i, y = j, pch = 15, col = big_col, cex = pt_cex)
        }

        if(pdt$H_diff[i]=="smaller"){
          points(x = i, y = j, pch = 15, col = small_col, cex = pt_cex)
        }
      }
    }
  }
  mtext(side = 3, "Years when harvest differs", line = -1, cex = 0.8)


}





