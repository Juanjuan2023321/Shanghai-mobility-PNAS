
# Figure 1 ====

#***********************************************************************#
#   Panel a.
#***********************************************************************#

ggplot() +
  geom_rect(aes(xmax=as.Date("2022-03-01"), ymin = 0, xmin = as.Date("2022-02-15"), ymax = 30000),
            fill = '#fffff7', alpha = 0.5) +
  geom_rect(aes(xmax=as.Date("2022-04-01"), ymin = 0, xmin = as.Date("2022-03-01"), ymax = 30000),
            fill = '#fdfadf', alpha = 0.4) +
  geom_rect(aes(xmax=as.Date("2022-05-01"), ymin = 0, xmin = as.Date("2022-04-01"), ymax = 30000),
            fill = '#fcf4cf', alpha = 0.5) +
  geom_rect(aes(xmax=as.Date("2022-06-01"), ymin = 0, xmin = as.Date("2022-05-01"), ymax = 30000),
            fill = '#fceec0', alpha = 0.5) +
  geom_rect(aes(xmax=max_d, ymin = 0, xmin = as.Date("2022-06-01"), ymax = 30000),
            fill = '#fee7b9', alpha = 0.4) +
  geom_bar(data = case_data, aes(date, n), fill = "grey80", 
           stat = "identity", color = "grey", linewidth = 0.2, width = 0.6) +
  geom_line(data = mobility_data, aes(x = date, y = rate*15000), stat = "identity", color = '#00468BFF', linewidth = 0.55)+
  geom_vline(xintercept = c(as.Date("2022-03-01"), as.Date("2022-04-01"), 
                            as.Date("2022-05-01"),as.Date("2022-06-01")), color = "grey40",
             linetype = "dotted", linewidth =0.45) + 
  labs(tag = " ") +
  scale_x_date(name = "", expand = c(0,0), 
               breaks = c(as.Date("2022-02-20"), as.Date("2022-03-01"), as.Date("2022-03-10"), as.Date("2022-03-20"),
                          as.Date("2022-04-01"),as.Date("2022-04-10"),as.Date("2022-04-20"), as.Date("2022-05-01"),
                          as.Date("2022-05-10"), as.Date("2022-05-20"),
                          as.Date("2022-06-01"),as.Date("2022-06-10"),as.Date("2022-06-20"),
                          as.Date('2022-06-30')),
               labels = c("Feb 20", 
                          "Mar 1",  "Mar 10", "Mar 20",# Mar
                          "Apr 1", "Apr 10",  "Apr 20", # Apr
                          "May 1",  "May 10",  "May 20", # May
                          "Jun 1", 'Jun 10', 'Jun 20', 'Jun 30'))+
  scale_y_continuous(name = "Number of daily trips per person", 
                     expand = c(0.005,0),
                     breaks = c(0,0.5,1,1.5,2)*15000, labels = seq(0,2,0.5),
                     sec.axis = sec_axis(trans = ~., name = "New infections (× 1,000)", breaks = c(seq(0,30000,5000)),
                                         labels = seq(0,30,5))) +
  coord_cartesian(ylim = c(-0.05, 30000), clip = "off", 
                  xlim = c(as.Date("2022-02-15"), as.Date("2022-06-30"))) + 
  # Pre-outbreak
  geom_segment(aes(x=as.Date("2022-02-15"), y = 30000, xend = as.Date("2022-02-16")+0.2, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  geom_segment(aes(x=as.Date("2022-03-01"), y = 30000, xend = as.Date("2022-02-27")+0.3, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  annotate('text', x=as.Date("2022-02-16")+0.1+0.1, y= 30000, size=8.4/.pt,label='Pre-outbreak',vjust = 0.5, hjust = 0)+ 
  
  # Targeted interventions
  geom_segment(aes(x=as.Date("2022-03-01"), y = 30000, xend = as.Date("2022-03-05")+0.3, yend = 30000), size=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  geom_segment(aes(x=as.Date("2022-04-01"), y = 30000, xend = as.Date("2022-03-26")-0.3, yend = 30000), size=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  annotate('text', x=as.Date("2022-03-05"), y= 30000, size=8.6/.pt, 
           label='Targeted interventions', hjust = 0)+ 
  # Citywide lockdown
  geom_segment(aes(x=as.Date("2022-04-01"), y = 30000, xend = as.Date("2022-04-07"), yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  geom_segment(aes(x=as.Date("2022-05-01"), y = 30000, xend = as.Date("2022-04-24")+0.5, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  annotate('text', x=as.Date("2022-04-07")+0.1, y= 30000, size=8.6/.pt,
           label='Citywide lockdown',vjust = 0.5, hjust = 0) +
  # Targeted lifting of interventions
  geom_segment(aes(x=as.Date("2022-05-01"), y = 30000, xend = as.Date("2022-05-09")+0.1, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  geom_segment(aes(x=as.Date("2022-06-01"), y = 30000, xend = as.Date("2022-05-26")-0.5, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  annotate('text', x=as.Date("2022-05-09")+0.1+0.1, y= 30000, size=8.6/.pt,
           label='Targeted lifting of \ninterventions', lineheight = 0.9,
           vjust = 0.5, hjust =0) +
  # Reopening
  geom_segment(aes(x=as.Date("2022-06-01"), y = 30000, xend = as.Date("2022-06-09"), yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  geom_segment(aes(x=as.Date("2022-06-30"), y = 30000, xend = as.Date("2022-06-20")+0.0, yend = 30000), linewidth=0.1,
               arrow = arrow(ends = "first", type = "closed",length = unit(0.1, "cm")))+
  annotate('text', x=as.Date("2022-06-08")+0.1, y= 30000, size=8.6/.pt, label='Reopening', vjust = 0.5, hjust = -0.15) +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.15, color = 'black'),
        plot.margin = margin(l = 8, b = 3, r = 17),
        title = element_text(size = 20/.pt),
        legend.text = element_text(size = 31/.pt), 
        legend.title = element_text(size = 32/.pt, lineheight = 3),
        axis.text.x = element_text(size = 31/.pt, color = "black", vjust = 1),#angle = 25,  hjust = 1
        axis.title.x = element_text(size = 32/.pt, color = "black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 30/.pt,  color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(size = 32/.pt, color = "black", vjust = 1, hjust = 0.5),
        legend.spacing.y = unit(0.01,"cm"),
        legend.position = 'top',
        legend.spacing = unit(0.25, "cm"),
        legend.margin = margin(t = -8, b= -8, unit = "pt"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black')) -> p_a

#***********************************************************************#
#   Panel b.
#***********************************************************************#

# map
ggplot() +
  geom_sf(data = boundary, fill = 'transparent', col = 'grey50', size = 0.2) +
  geom_sf(data = tmp_bs1, aes(fill = ufre1), size = 0.04, alpha = 1, color = NA) +
  labs(title = "Pre-outbreak") +
  scale_fill_gradientn(colors = c("#FCF7D9", "#E8EDB3", "#B2D8B6", "#70C8BB", "#4ABBC0", 
                                  "#1F84B9", "#215FA8", "#293991", "#1F2758"), 
                       name = "Population flows",
                       limits = c(0, 10000), na.value = "grey80",
                       breaks = c(1, 1000,2000,3000, 4000, 5000, 10000),
                       labels = c(1, '1,000','2,000','3,000', '4,000', '5,000', '10,000+'),
                       guide = guide_legend(reverse = T)) +
  theme_void() +
  theme(legend.background = element_rect(fill = "transparent", color = 'transparent'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        axis.text = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.position = c(1.3,0.5), 
        legend.margin = margin(r = 5, l = 0),
        legend.key.size = unit(0.3, 'cm'),
        legend.key.height = unit(0, 'pt'),
        legend.title = element_text(size = 24/.pt),
        legend.text = element_text(size = 22/.pt, hjust = 0, vjust = 0.5, margin = margin(l = 0, t = 1,unit = "pt")),
        plot.title = element_text(size = 34/.pt, margin = margin(t = 1, b = -3), hjust = 0, vjust = 0.5), 
        plot.margin = margin(r = 0, l = -10, t= 0, b = 0)) -> p_b


#***********************************************************************#
#   Panel c.
#***********************************************************************#

ggplot()+
  geom_area(data = dat1[!is.na(distance)], aes(date, perc, fill = distance), color = "white", alpha = 1, linewidth = 0.4)+
  geom_line(data = dat11, aes(date, median_dist/8), color = 'black', linewidth = 0.55, linetype = 'longdash') + # 细节需要调整
  geom_vline(xintercept = c(as.Date("2022-03-01"), as.Date("2022-04-01"), as.Date("2022-05-01"),as.Date("2022-06-01")), 
             color = "grey40", linetype = "dotted", linewidth =0.45) +
  scale_x_date(name = 'Date', expand = c(0,0), 
               breaks = c(as.Date("2022-02-20"), as.Date("2022-03-01"), as.Date("2022-03-10"), as.Date("2022-03-20"),
                          as.Date("2022-04-01"),as.Date("2022-04-10"),as.Date("2022-04-20"), as.Date("2022-05-01"),
                          as.Date("2022-05-10"), as.Date("2022-05-20"),
                          as.Date("2022-06-01"),as.Date("2022-06-10"),as.Date("2022-06-20"),
                          as.Date('2022-06-30')),
               labels = c("Feb 20", 
                          "Mar 1",  "Mar 10", "Mar 20",# Mar
                          "Apr 1", "Apr 10",  "Apr 20", # Apr
                          "May 1",  "May 10",  "May 20", # May
                          "Jun 1", 'Jun 10', 'Jun 20', 'Jun 30'))+
  scale_y_continuous(name= 'Proportion of flows (%)', 
                     labels = label_number(scale = 100, accuracy = 1), 
                     expand = c(0,0), 
                     breaks = c(seq(0,1,0.25)),
                     sec.axis = sec_axis(trans = ~.*8, name = "Median distance (km)",
                                         breaks = c(0, 2, 4, 6, 8),
                                         labels = c(0, 2, 4, 6, 8))) + 
  labs(title = "") +
  scale_fill_manual(name = 'Distance (km)', 
                    values = c("#f5f5f5", '#d8ecd2','#F0A04B','#26532c','#7DB9B6','#4D455D','#4b9f5d'),
                    labels = c('1km <= D < 3km' = '[1,3)',   
                               '3km <= D < 5km' = '[3,5)', 
                               '5km <= D < 10km' = '[5,10)', 
                               '10km <= D < 30km' = '[10,30)',
                               '30km <= D < 50km' = '[30,50)', 
                               'D >= 50km' = '50+')) +
  guides(fill = guide_legend(keyheight = 1, keywidth = 1, ncol = 6, label.hjust = 0))+
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.15, color = 'black'),
        plot.margin = margin(l = 8, b = 3, r = 17),
        panel.border = element_rect(fill=NA, linewidth = 0.12),
        title = element_text(size = 20/.pt),
        legend.text = element_text(size = 31/.pt), 
        legend.title = element_text(size = 32/.pt, lineheight = 3),
        axis.text.x = element_text(size = 31/.pt, color = "black", vjust = 1),
        axis.title.x = element_text(size = 32/.pt, color = "black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 30/.pt,  color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(size = 32/.pt, color = "black", vjust = 1, hjust = 0.5),
        legend.spacing.y = unit(0.01,"cm"),
        legend.position = 'top',
        legend.spacing = unit(0.25, "cm"),
        legend.margin = margin(t = -8, b= -8, unit = "pt"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.15, color = 'black')) -> p_c



#***********************************************************************#
#   Panel d.
#***********************************************************************#

ggplot() +
  geom_line(data = dt1[!is.na(period)], aes(log(x), y, col = period), linewidth = 0.55) +
  scale_x_continuous(name = "Travel distance, D (km)",breaks = log(c(1,10,100)),
                     labels = c(1,10,100))+
  scale_y_continuous(name = expression("Cumulative distribution (p">="D)"), expand = c(0,0), 
                     breaks = c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2))) +
  coord_cartesian(xlim = log(c(0.25,150)), ylim = c(-0.02,1)) +
  labs(title = "")+
  scale_color_manual(name = NULL, values = c('#F0A04B','#183A1D','#7DB9B6','#4D455D','#E96479','#a2dda0'),
                     labels = c("Pre-outbreak", "Targeted interventions","Citywide lockdown",
                                "Targeted lifting of \ninterventions","Reopening")) +
  guides(color = guide_legend(keyheight = 1, keywidth = 1)) +
  theme_classic() +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.74, 0.79),
        title = element_text(size = 20/.pt),
        legend.text = element_text(size = 27/.pt, margin = margin(l = -3)), 
        axis.text.x = element_text(size = 31/.pt, color = "black", vjust = 0.5),
        axis.title.x = element_text(size = 32/.pt, hjust = 0.5, color = "black",
                                    vjust = 0),#vjust = -0.5, 
        axis.text.y = element_text(size = 31/.pt,  color = "black"),
        axis.title.y = element_text(size = 30.5/.pt, color = "black"),
        panel.border = element_rect(fill=NA, linewidth = 0.12),
        legend.spacing = unit(0.0005,"cm"),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black'),
        axis.line = element_line(linewidth = 0.15, color = 'black')) -> p_d

##### export
pdf("Fig 1.pdf", width = 13, height = 6.9)
# panel a
viewport(x = 0.0, y = 0.48, width = 0.712, height = 0.49, just = c("left", "bottom")) -> vp1
# panel b
viewport(x = 0.69, y = 0.51, width = 0.23, height = 0.45, just = c("left", "bottom")) -> vp2
# panel c
viewport(x = 0.0, y = 0, width = 0.705, height = 0.5, just = c("left", "bottom")) -> vp3
# panel d
viewport(x = 0.70, y = 0, width = 0.3, height = 0.48, just = c("left", "bottom")) -> vp4
print(p_a, vp = vp1)
print(p_b, vp = vp2)
print(p_c, vp = vp3)
print(p_d, vp = vp4)
viewport(x = 0.01, y = 0.89, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp8
viewport(x = 0.7, y = 0.89, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp9
viewport(x = 0.01, y = 0.4, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp10
viewport(x = 0.7, y = 0.4, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp11
print(ggplot() + labs(tag = 'a') + theme_void() + 
        theme(plot.tag = element_text(size = 48/.pt, face = 'bold')), vp = vp8)
print(ggplot() + labs(tag = 'b') + theme_void() + 
        theme(plot.tag = element_text(size = 48/.pt, face = 'bold')), vp = vp9)
print(ggplot() + labs(tag = 'c') + theme_void() + 
        theme(plot.tag = element_text(size = 48/.pt, face = 'bold')), vp = vp10)
print(ggplot() + labs(tag = 'd') + theme_void() +
        theme(plot.tag = element_text(size = 48/.pt, face = 'bold')), vp = vp11)
dev.off()

# Figure 3 ====

#***********************************************************************#
#   Panel a  
#***********************************************************************#
map_boundary <- st_read("shanghai_boundary.shp")
map_boundary <- st_transform(map_boundary, CRS("+proj=longlat +datum=WGS84"))
sm1 = matrix(c(2, 1, 0, 0.75), 2, 2)
sm2 = matrix(c(2, 1, 0, 0.72), 2, 2)

tmp3 <- data %>% mutate(geometry = geometry * sm1) %>%
  st_set_crs(st_crs(data))
tmp32 <- data %>% mutate(geometry = geometry * sm2) %>%
  st_set_crs(st_crs(data)) 

ggplot() +
  # top map
  geom_segment(data = tmp3, aes(x = longc, xend = longc, y = latc, yend = latc/1.02039), 
               linewidth = 0.03, col = 'grey40',alpha = 0.6) + 
  geom_sf(data = tmp3, aes(fill = pop2), size = 0.09, alpha = 1, color = NA) +
  labs(title = "Targeted interventions") +
  scale_fill_gradientn(colours  = coli, #ED0000FF
                       name = "Infections\n(March 1-27)", 
                       na.value = "grey50",
                       breaks = c(1, 25,50,75,100),
                       labels = c(1, 25,50,75, '100+'),
                       guide = guide_legend(reverse = TRUE, keyheight = 0,order = 1)) +
  new_scale_fill() +
  # bottom map
  geom_sf(data = tmp32, aes(fill = n1), col = NA, size = 0.001)  + 
  geom_segment(data = tmp32, aes(x = longc, xend = longc, y = latc, yend = latc*1.021), 
               linewidth = 0.03, col = 'grey40',alpha = 0.6) +
  scale_fill_gradientn(colors = c("#006EB3", "#8dc6a9", "#e8f4ed",'#ffbf6e',"#ef8f66", "#e14868"), 
                       limits = c(-1,1),
                       breaks = c(-1, -0.75, -0.5, -0.25,0, 0.5, 1),
                       na.value = "#E6E6E3",
                       labels = c('100', '75' ,'50', '25','0', '-50', '-100+'),
                       name = "Mobility reduction (%)\n(March 27)",
                       guide = guide_legend(reverse = TRUE, keyheight = 0, order = 2)) +
  theme_void() +
  theme(legend.background = element_rect(fill = "transparent", color = 'transparent'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        plot.title =  element_text(size = 36/.pt, margin = margin(t = -1, b = -3), 
                                   hjust = 0.5, vjust = 0.5, face = 'bold'),
        legend.text = element_text(size = 20/.pt, hjust = 0, vjust = 0.5,
                                   margin = margin(l = 0, t = 1,unit = "pt")), 
        legend.title = element_text(size = 23/.pt),
        axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), axis.title = element_blank()) -> p_a

#***********************************************************************#
#   Panel b 
#***********************************************************************#

ggplot() +
  geom_bar(data = tmp, aes(pop1, -mob), stat = 'identity', width = 0.5, fill = '#006EB3',
           color = 'black', linewidth = 0.55, alpha = 0.2) +
  geom_jitter(data = tmp1, aes(pop1, -n), alpha = 0.3, stat = 'identity', width = 0.15,
              color = '#006EB3', size = 0.4) +
  geom_linerange(data = tmp, aes(x = pop1, ymin = -mob.u, ymax = -mob.l),linewidth = 0.5) +
  coord_flip(expand = F) + 
  scale_y_continuous(limits = c(0,1), name = 'Mobility reduction in the cells as of March 27 (%)',
                     breaks = c(seq(0,1,0.25)),
                     label = label_number(scale = 100)) +
  scale_x_discrete(name =' Number of new infections \nin the cells during March 1-April 3',
                   label = c('0','1-24','25-49','50-74','75-99','100+')) +
  theme_classic() +
  theme(axis.line = element_line(size = 0.15, color = 'black'),
        plot.margin = margin(l = 8, b = 3, r = 17, t = 5),
        panel.border = element_rect(fill=NA, linewidth = 0.12),
        title = element_text(size = 20/.pt),
        legend.text = element_text(size = 31/.pt), 
        legend.title = element_text(size = 32/.pt, lineheight = 1),
        axis.text.x = element_text(size = 31/.pt, color = "black", vjust = 1),
        axis.title.x = element_text(size = 32/.pt, color = "black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 30/.pt,  color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(size = 32/.pt, color = "black", vjust = 1, hjust = 0.5),
        legend.spacing.y = unit(0.01,"cm"),
        legend.background = element_blank(),
        legend.position = c(0.75, 0.075),
        legend.spacing = unit(0.25, "cm"),
        legend.margin = margin(t = -8, b= -8, unit = "pt"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black')) -> p_b


#***********************************************************************#
#   Panel c  
#***********************************************************************#
ggplot() +
  # top
  geom_segment(data = tmp_pl1, aes(x = longc, xend = longc, y = latc, yend = latc/1.02039),
               linewidth = 0.03, col = 'grey40',alpha = 0.6) + 
  geom_sf(data = tmp_pl1, aes(fill = inf1), size = 0.09, alpha = 1,  color = NA) +
  scale_fill_gradientn(colours = c("#fef9a7", "#fbdb84", "#f8bb68", "#f49a55", 
                                   "#ee774b", '#d61c4e'), #ED0000FF
                       name = "Infections\n(May 1-31)", 
                       na.value = "grey50",
                       breaks = c(1, 25,50,75,100),
                       labels = c('1', '25','50','75','100+'),
                       guide = guide_legend(reverse = T, keyheight = 0)) +
  labs(title = 'Targeted lifting of interventions') +
  new_scale_fill() +
  # bottom
  geom_sf(data = tmp_pl2, aes(fill = n1), col = NA, size = 0.001)  + 
  geom_segment(data = tmp_pl2,
               aes(x = longc, xend = longc, y = latc, yend = latc*1.021),#*1.0105
               linewidth = 0.03, col = 'grey40', alpha = 0.6) +
  scale_fill_gradientn(colors = c("#006EB3",'#b4e4cb','#fedc98', '#edb161','#f9b890','#df7223',"#e14868"),
                       limits = c(0,3),
                       breaks = c(0, 0.25, 0.5, 0.75,1, 3), na.value = "#E6E6E3",
                       labels = c(0, 25, 50, 75,100, '300+'), 
                       name = "Mobility recovery (%)\n(May 16-31)",
                       guide = guide_legend(reverse = T, keyheight = 0)) +
  theme_void() +
  theme(legend.background = element_rect(fill = "transparent", color = 'transparent'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        plot.title =  element_text(size = 36/.pt, margin = margin(t = -1, b = -3), 
                                   hjust = 0.5, vjust = 0.5, face = 'bold'),
        legend.text = element_text(size = 20/.pt, hjust = 0, vjust = 0.5,
                                   margin = margin(l = 0, t = 1,unit = "pt")), 
        legend.title = element_text(size = 23/.pt),
        axis.text = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), axis.title = element_blank()) -> p_c

#***********************************************************************#
#   Panel d  
#***********************************************************************#

ggplot() +
  geom_bar(data = tmp_pl111, aes(inf1, mob), stat = 'identity', width = 0.5, fill = '#e14868',
           color = 'black', linewidth = 0.55, alpha = 0.2) +
  geom_jitter(data = tmp_pl1, aes(inf1, n), alpha = 0.3, stat = 'identity', width = 0.15,
              color = '#e14868',size = 0.4, shape = 21) +
  geom_linerange(data = tmp_pl111, aes(x = inf1, ymin = mob.l, ymax = mob.u), linewidth = 0.5) +
  coord_flip(expand = F) + 
  scale_y_continuous(limits = c(0,1), name = 'Mobility recovery in the cells during May 16-31 (%)',
                     breaks = c(seq(0,1,0.25)),
                     label = label_number(scale = 100)) +
  scale_x_discrete(name =' Number of new infections \nin the cells during May 1-31',
                   label = c('0','1-24','25-49','50-74','75-99','100+')) +
  theme_classic() +
  theme(axis.line = element_line(size = 0.15, color = 'black'),
        plot.margin = margin(l = 8, b = 3, r = 17, t = 5),
        panel.border = element_rect(fill=NA, linewidth = 0.12),
        title = element_text(size = 20/.pt),
        legend.text = element_text(size = 31/.pt), 
        legend.title = element_text(size = 32/.pt, lineheight = 1),
        axis.text.x = element_text(size = 31/.pt, color = "black", vjust = 1),
        axis.title.x = element_text(size = 32/.pt, color = "black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 30/.pt,  color = "black", vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(size = 32/.pt, color = "black", vjust = 1, hjust = 0.5),
        legend.spacing.y = unit(0.01,"cm"),
        legend.background = element_blank(),
        legend.position = c(0.75, 0.075),
        legend.spacing = unit(0.25, "cm"),
        legend.margin = margin(t = -8, b= -8, unit = "pt"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black')) -> p_d


#### export
tiff("Figure 3.tiff", width = 10.3, height = 6, units = "in", compression = "lzw", res = 326)
# panel a
viewport(x = 0, y = 0.41, width = 0.505, height = 0.6, just = c("left", "bottom")) -> vp4
# panel c
viewport(x = 0.49, y = 0.41, width = 0.505, height = 0.6, just = c("left", "bottom")) -> vp7
# panel b
viewport(x = 0, y = 0.0, width = 0.4, height = 0.4, just = c("left", "bottom")) -> vp41
# panel d
viewport(x = 0.49, y = 0.0, width = 0.4, height = 0.4, just = c("left", "bottom")) -> vp71
# tags
viewport(x = 0.00, y = 0.905, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp8
viewport(x = 0.00, y = 0.36, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp9
viewport(x = 0.47, y = 0.905, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp81
viewport(x = 0.47, y = 0.36, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp91
print(p_a, vp = vp4)
print(p_c, vp = vp7)
print(p_b, vp = vp41)
print(p_d, vp = vp71)
print(ggplot() + labs(tag = 'a') + theme_void() +
        theme(plot.tag = element_text(size = 45/.pt, face = 'bold')), vp = vp8)
print(ggplot() + labs(tag = 'b') + theme_void() +
        theme(plot.tag = element_text(size = 45/.pt, face = 'bold')), vp = vp9)
print(ggplot() + labs(tag = 'c') + theme_void() +
        theme(plot.tag = element_text(size = 45/.pt, face = 'bold')), vp = vp81)
print(ggplot() + labs(tag = 'd') + theme_void() +
        theme(plot.tag = element_text(size = 45/.pt, face = 'bold')), vp = vp91)
dev.off()


# Figure 4 ====

#***********************************************************************#
#   Panel a-c 
#***********************************************************************#
mytheme3 <- theme_classic() +
  theme(legend.background = element_rect(fill = "transparent", color = 'transparent'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_blank(),
        plot.title = element_text(size = 34/.pt, margin = margin(t = -3, b= 3)),
        plot.margin = margin(t = -1, b = -3),
        axis.text.x = element_text(size = 30/.pt, color ='black'),
        axis.text.y = element_text(size = 31/.pt, color ='black'),
        axis.title = element_text(size = 32/.pt, color ='black', face = 'bold'),
        axis.line = element_line(linewidth = 0.15),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black'))

fun_plot <- function(time, time1, w1, lb = NULL, xlabel = c("Overall","16-18 yrs", "19-29 yrs",'30-59 yrs','60-69 yrs', '70+ yrs','Male', 'Female'), 
                     color_p = c('#de0000','#ffa300','#0386c2','#007A39','#6644a6','#B3E820'),
                     hj = -1, typ = c('Overall', 'dt_age','dt_sex', 'df_age','df_sex'), 
                     rect_max = 9, dist_brk, dist_lab, flow_brk, flow_lab, txt = c('','','','',''), yp=0.5, lh=0.25) {
  
  # Trips
  df_overall1 <- df_overall %>% filter(fact %in% xlabel) %>% left_join(boots_trips)
  factor(df_overall1$fact, (unique(df_overall1$fact))) -> df_overall1$fact
  factor(df_overall1$type, (unique(df_overall1$type))) -> df_overall1$type
  
  ggplot() +
    geom_rect(data = df_overall1[df_overall1$period == time,],
              aes(xmin = 0, xmax = rect_max, ymin = 0, ymax = -3), fill = '#ececea', alpha = 0.2)+
    geom_bar(data = df_overall1[df_overall1$period == time,], aes(x=fact, y=-mean, fill = type), col = 'black',
             stat = 'identity',linewidth=0.15, alpha = 0.9, width = 0.65, show.legend = F) +
    geom_linerange(data = df_overall1[df_overall1$period == time,],
                   aes(x=fact, ymin = low, ymax = upp), col = 'black',
                   stat = 'identity',linewidth=0.15, show.legend = F) +
    scale_x_discrete(name = NULL, labels = lb, limits = rev(xlabel), expand = c(0.05,0))+
    scale_y_continuous(name = 'Trips', expand = c(0, 0), breaks = flow_brk, labels = flow_lab) +
    scale_fill_manual(name = NULL, values = color_p) +
    coord_flip(ylim = c(min(flow_brk),0)) + 
    mytheme3 +  labs(title = time1)+ geom_hline(yintercept = 0, linetype = 'dotted', linewidth = 0.1)+
    theme(plot.margin = margin(t= -21, b = 10, l = 3, r= 3, unit = 'pt'),
          plot.title = element_text(size = 34/.pt, hjust = hj, face = 'bold', vjust = 1),
          legend.position = "none",
          plot.background = element_rect(fill = "transparent", color = "transparent"),
          panel.background = element_rect(fill = "transparent", color = "transparent"),
          legend.justification = 'left',
          legend.margin = margin(t = -9, b = 1,r = 3, l = 5, unit = "pt")) -> of 
  
  # Distance
  dt_overall1 <- dt_overall %>% filter(fact %in% xlabel)
  factor(dt_overall1$fact, (unique(dt_overall1$fact))) -> dt_overall1$fact
  factor(dt_overall1$type, (unique(dt_overall1$type))) -> dt_overall1$type
  ggplot() + 
    geom_rect(data = dt_overall1[dt_overall1$period == time,],
              aes(xmin = 0, xmax = rect_max, ymin = 0, ymax = 15), fill = '#f9f9f8', alpha = 0.3)+
    geom_bar(data = dt_overall1[dt_overall1$period == time,],
             aes(x=fact, y=p, fill = type), col = 'black',
             stat = 'identity',linewidth=0.15, alpha = 0.25, width = 0.65, show.legend = F) +
    geom_linerange(data = dt_overall1[dt_overall1$period == time,],
                   aes(x=fact, ymin = pl, ymax = pu), col = 'black',
                   stat = 'identity',linewidth=0.15, show.legend = F) +
    scale_x_discrete(name = NULL, labels = NULL, expand = c(0.05,0), limits = rev(xlabel)) +
    scale_y_continuous(name ="Distance (km)", expand = c(0,0), breaks = dist_brk, labels = dist_lab) +
    scale_fill_manual(name = NULL, values = color_p) + coord_flip(ylim = c(0,max(dist_brk)+0.3)) +
    labs(title = '')+
    mytheme3 + theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
                     legend.position = "none", plot.background = element_rect(fill = "transparent", color = "transparent"),
                     panel.background = element_rect(fill = "transparent", color = "transparent"),
                     legend.justification = 'left', legend.margin = margin(t = -8, b = 1,r = 0, l = 8, unit = "pt"),
                     plot.margin = margin(t= 0, b = 0, l = 3, r= 3, unit = 'pt'))-> ot 
  
  o <- of + plot_spacer() + ot + plot_layout(widths = c(1,w1,1)) &
    theme(plot.background = element_blank())
  
  return(o)
} 

#main figures
xlabel = c("Overall","16-18 yrs", "19-29 yrs",'30-59 yrs','60-69 yrs', '70+ yrs', 'Male', 'Female')
p_a <- fun_plot(time = "Normal times", time1 = "Pre-outbreak",dist_brk = seq(5,15,5),
                dist_lab = c(5,10,15), flow_brk = seq(-3,0,1), 
                flow_lab = abs(seq(-3,0,1)), hj = 2, w1 = -0.1925, lb = rev(xlabel))
p_b <- fun_plot(time = "Shanghai lockdown",time1 = "Citywide lockdown", hj = -35, dist_brk = seq(5,15,5), 
                dist_lab = c(5,10,15), yp=0.48,
                flow_brk = seq(-3,0,1), flow_lab = abs(seq(-3,0,1)), w1 = -0.198, lb = NULL)
p_c <- fun_plot(time = "Post lockdown", time1 = "Reopening",  yp=0.65, lh = 0.65, hj = 1.8, 
                dist_brk = seq(5,15,5), dist_lab = c(5,10,15),
                txt = c("Age group", "Sex"), w1 = -0.198, lb = NULL,
                flow_brk = seq(-3,0,1), flow_lab = abs(seq(-3,0,1)))

#***********************************************************************#
#   Panel d-f 
#***********************************************************************#

theme1 <- theme_classic() +
  theme(legend.background = element_rect(fill = "transparent", color = 'transparent'),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_blank(),
        plot.title = element_text(size = 14/.pt, margin = margin(t = -3, b= 3)),
        plot.margin = margin(t = -1, b = -3),
        axis.text.x = element_text(size = 28/.pt, color ='black'),
        axis.text.y = element_text(size = 31/.pt, color ='black'),
        axis.title = element_text(size = 32/.pt, color ='black', face = 'bold'),
        axis.line = element_line(linewidth = 0.15),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.15, color = 'black'))

fig_plt <- function(time = 'feb', tit = 'Pre-outbreak', typ = c('sex','age'),color_p = c('#ffa300','#0386c2','#007A39','#6644a6','#B3E820')) {
  
  data1 <- data %>% left_join(data_com) %>% filter(type %in% typ) %>% mutate(fact = factor(fact)) %>% as.data.table()
  
  if (tit == 'Pre-outbreak') {
    xlb = rev(levels(data1[period %like% time]$fact)) 
  } else {
    xlb = NULL
  }
  ggplot() +
    geom_bar(data = data1[period %like% time], aes(fact, area.m, fill = type), stat = 'identity', 
             col = 'black', show.legend = F, alpha = 0.75, linewidth = 0.15, width = 0.55) +
    geom_linerange(data = data1[period %like% time], aes(x = fact, ymin = area.l, ymax = area.u), 
                   col = 'black', stat = 'identity',linewidth=0.15, show.legend = F) +
    scale_x_discrete(name = NULL, limits = rev(levels(data1[period %like% time]$fact)),
                     expand = c(0,0), labels=xlb) +
    scale_y_continuous(name = expression(bold('% of total area, ')*bolditalic(alpha)), expand = c(0,0), labels = number_format(scale = 100, accuracy = 1),
                       breaks = c(0,0.25,0.5)) +
    scale_fill_manual(values = color_p) +
    coord_flip(ylim = c(0,0.5)) +
    theme1 -> f1
  
  ggplot() +
    geom_bar(data = data1[period %like% time], aes(fact, over.m, fill = type), 
             stat = 'identity', show.legend = F, alpha = 0.75, linewidth = 0.15, width = 0.55) +
    geom_bar(data = data1[period %like% time], aes(fact, total.m), stat = 'identity', fill = 'transparent',
             col = 'black', show.legend = F, linewidth = 0.15, width = 0.55) +
    geom_linerange(data = data1[period %like% time], aes(x = fact, ymin = total.l, ymax = total.u), 
                   col = 'black', stat = 'identity',linewidth=0.15, show.legend = F) +
    geom_linerange(data = data1[period %like% time], aes(x = fact, ymin = over.l, ymax = over.u), 
                   col = 'black', stat = 'identity',linewidth=0.15, show.legend = F) +
    scale_x_discrete(name = NULL, limits = rev(levels(data1[period %like% time]$fact)),
                     expand = c(0,0), position = 'top', labels = NULL) +
    scale_y_reverse(name = expression(bold('No. of communities, ')*bolditalic('NC')), expand = c(0,0), limits = c(500,0),
                    breaks = c(500,250,0)) +
    scale_fill_manual(values = color_p) +
    coord_flip() + theme1 -> f2
  
  f <- f1+plot_spacer()+f2 +plot_layout(widths = c(1,0.1,1)) &
    theme(plot.background = element_blank(), axis.title = element_text(face = 'bold'))
  
  return(f)
}

p_d <- fig_plt(time = 'feb', tit = 'Pre-outbreak')
p_e <- fig_plt(time = 'apr', tit = 'Shanghai lockdown')
p_f <- fig_plt(time = 'june', tit = 'Post-lockdown')

#***********************************************************************#
#   Panel g-h 
#***********************************************************************#
den_func <- function(typ = 'age', brk = 0:4*2/100, leg_tit, ang = 90, pos = 'left', bin = 5,
                     lg.pos = 'none', dfrom = -Inf, dto = 10000, uni = 0.1, y_name = NULL, sub_name = 'Density',
                     xtitle = expression(bolditalic('log'['10']*'k'))) { 
  plt <- dd[ind == typ] %>%
    filter(period %in% c("Pre-outbreak" ,"Shanghai lockdown", "Post-lockdown")) %>%
    mutate(type = factor(type)) %>%
    mutate(period = factor(period, levels = c('Shanghai lockdown', 'Post-lockdown','Pre-outbreak'))) %>%
    as.data.table()
  
  hj = ifelse(ang == 90, 0.4, 1.1)
  vj = ifelse(ang== 90, 0.25, 0.5)
  ggplot() +
    geom_density(data = plt, aes(log10(degree), after_stat(density), fill = period,alpha = period),
                 linewidth = 0.2) +
    facet_wrap(~type, strip.position = 'bottom', nrow = 1) +
    labs(subtitle = sub_name) +
    scale_fill_manual(values = c('Pre-outbreak'='#ffe355', 'Shanghai lockdown'='#7DB9B6','Post-lockdown'='red'), name = NULL,
                      limits = c('Pre-outbreak', 'Shanghai lockdown','Post-lockdown')) +#
    scale_alpha_manual(values = c('Pre-outbreak'= 0.5, 'Shanghai lockdown'= 0.5,'Post-lockdown'=0.5), name = NULL,
                       limits = c('Pre-outbreak', 'Shanghai lockdown','Post-lockdown')) +
    scale_y_continuous(name = y_name, position = pos, 
                       breaks = scales::breaks_extended(n = 4),
                       expand = c(0,0))+
    scale_x_continuous(name = xtitle, expand = c(0,0), 
                       breaks = c(-Inf, 0,1,2,3,4), limits = c(0,4),
                       labels = c("",0,1,2,3,4)) +
    coord_cartesian(clip = 'off', xlim = c(0, 4)) +
    theme_classic() +
    theme(panel.border = element_rect(color = 'black', linewidth = 0.1, fill = 'transparent'),
          panel.spacing = unit(0, "lines"),
          legend.background = element_rect(fill = "transparent", color = 'transparent'),
          legend.box.background = element_rect(fill = "transparent", color = "transparent"),
          panel.background = element_rect(fill = 'transparent', colour = NA),
          plot.background = element_blank(),
          plot.title = element_text(size = 32/.pt, margin = margin(t = -3, b= 3)),
          axis.title.y = element_text(size = 31/.pt, face = 'bold'),
          axis.line = element_line(linewidth = 0.15),
          axis.ticks.length = unit(0.15, "cm"),
          axis.ticks = element_line(linewidth = 0.15, color = 'black'),
          legend.text = element_text(size = 31/.pt),
          legend.title = element_text(size = 32/.pt, vjust = -0.1, hjust = 0.5),
          legend.margin = margin(l = -5, r = -3, t = -9, b = -4),
          legend.position = lg.pos,
          legend.key.size = unit(0.25, 'cm'),
          panel.spacing = unit(0.25, 'cm'),
          plot.margin = margin(l = 4, r = 6, t = -5),
          plot.subtitle = element_text(size = 32/.pt, hjust = 0.5, vjust = -4, face = 'bold'),
          axis.text.y = element_text(color ='black', angle = ang, hjust = hj,size = 31/.pt, vjust = vj, margin = margin(l = 0,r = 2)),
          axis.text.x = element_text(size = 30/.pt, margin = margin(t = 1)),
          axis.title.x = element_text(size = 32/.pt, margin = margin(t = 2.5))) -> p;p
  
  return(p)
}

p_h <- den_func(typ='sex', ang = 0,leg_tit = "", y_name = '', sub_name = '',bin = 10)
p_g <- den_func(typ = 'age',leg_tit = '', ang = 0, y_name = 'Density', sub_name = '',bin = 4)


l_bar <- ggplot() +
  geom_bar(data = data.frame(a = 1:3, b = 3:1),
           aes(a,b, fill = as.character(a)),position = position_dodge(0.9),color = 'black',
           stat = 'identity', linewidth = 0.2 , alpha = 0.75) +
  scale_fill_manual(values = c('1' = '#ffc265', '2' = '#7DB9B6', '3' = '#E96479'),
                    labels = c('1' = 'Pre-outbreak', '2' = 'Citywide lockdown', '3' = 'Reopening'),
                    name = "\n",
                    guide = guide_legend(keywidth = 1, keyheight = 1)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),legend.position = 'top', legend.direction = 'horizontal',
        legend.text = element_text(size = 30/.pt, margin = margin(r = 5, l = 0)),
        legend.title = element_text(size = 31/.pt, vjust = -0.1, hjust = 0.5),
        # legend.margin = margin(l = -5, r = -3, t = -9, b = -4),
        legend.key.size = unit(0.25, 'cm'),
        legend.key.height = unit(0,'pt')) 
leg_bar <- ggpubr::as_ggplot(ggpubr::get_legend(l_bar))
#### export 
pdf('Figure 4.pdf', width =11.2, height = 9.5)
viewport(x = 0, y = 0.61, width = 0.36, height = 0.39, just = c("left", "bottom")) -> vp1
viewport(x = 0.37, y = 0.61, width = 0.29, height = 0.39, just = c("left", "bottom")) -> vp2
viewport(x = 0.67, y = 0.61, width = 0.29, height = 0.39, just = c("left", "bottom")) -> vp3
# panel d-f
viewport(x = 0.01, y = 0.291, width = 0.35, height = 0.33, just = c("left", "bottom")) -> vp4
viewport(x = 0.382, y = 0.291, width = 0.285, height = 0.33, just = c("left", "bottom")) -> vp5
viewport(x = 0.683, y = 0.291, width = 0.285, height = 0.33, just = c("left", "bottom")) -> vp6
# panel g-h
viewport(x = 0.05, y = 0.001, width = 0.61, height = 0.28, just = c("left", "bottom")) -> vp7
viewport(x = 0.65, y = 0.001, width = 0.32, height = 0.28, just = c("left", "bottom")) -> vp8
viewport(x = 0.23, y = 0.272, width = 0.1, height = 0.01, just = c('left','bottom')) -> vp9
# tags
viewport(x = 0.05, y = 0.9, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp10
viewport(x = 0.37, y = 0.9, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp11
viewport(x = 0.67, y = 0.9, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp12

viewport(x = 0.05, y = 0.5335, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp13
viewport(x = 0.37, y = 0.5335, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp14
viewport(x = 0.67, y = 0.5335, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp15

viewport(x = 0.05, y = 0.20, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp16
viewport(x = 0.67, y = 0.20, width = 0.1, height = 0.1, just = c("left", "bottom")) -> vp17

print(p_a, vp = vp1)
print(p_b, vp = vp2)
print(p_c, vp = vp3)
print(p_d, vp = vp4)
print(p_e, vp = vp5)
print(p_f, vp = vp6)
print(p_g, vp = vp7)
print(p_h, vp = vp8)
print(leg_bar, vp = vp9)
print(ggplot()+ labs(tag = 'a') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp10)
print(ggplot()+ labs(tag = 'b') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp11)
print(ggplot()+ labs(tag = 'c') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp12)
print(ggplot()+ labs(tag = 'd') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp13)
print(ggplot()+ labs(tag = 'e') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp14)
print(ggplot()+ labs(tag = 'f') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp15)
print(ggplot()+ labs(tag = 'g') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp16)
print(ggplot()+ labs(tag = 'h') + theme_void() + theme(plot.tag = element_text(size =46/.pt, face = 'bold'),
                                                       plot.background = element_blank()), vp = vp17)
dev.off()



