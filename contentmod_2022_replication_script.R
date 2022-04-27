#Guilbeault & Centola, 2022
rm(list=ls());gc(); options(max.print=999999)
library(ggplot2); library(ggridges);theme_set(theme_ridges());library(sjPlot)
library(sjmisc);library(sjlabelled);library(RCurl);library(tidytext);library(ggtext)
library(multiwayvcov);library(lmtest);library(lme4);library(stargazer)

###########
#Functions#
###########
get_jaccard_simple<-function(df){
  iids=as.character(unique(df$instance_id))
  iids = iids[!is.na(iids)]
  
  all_jaccard<-data.frame()
  combos<-c()
  
  for(i in 1:length(iids)){
    iid_i<-iids[i]
    for(j in 1:length(iids)){
      if(i != j){
        iid_j<-iids[j]
        
        combo<-paste(min(iid_i, iid_j), max(iid_i, iid_j))
        
        if(!combo %in% combos){
          combos<-c(combos, combo)
          
          df_iid_i<-df[df$instance_id == iid_i,]
          df_iid_j<-df[df$instance_id == iid_j,]
          
          cats_i<-unique(df_iid_i$label)
          cats_j<-unique(df_iid_j$label)
          
          jaccard<- length(intersect(cats_i, cats_j)) / (length(cats_i) + length(cats_j) -length(intersect(cats_i, cats_j)))
          
          all_jaccard<-rbind(all_jaccard, data.frame(iid_i = iid_i, iid_j = iid_j, jaccard = jaccard))
        }
      }
    }
  }
  return(all_jaccard)
}

entropy <- function(target) {
  freq <- table(target)/length(target)
  vec <- as.data.frame(freq)[,2]
  vec<-vec[vec>0]
  -sum(vec * log2(vec))
}

min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

#Load Data
data_path<-"C:/Users/dougl/Desktop/Categories/Content Moderation/Content_Mod_Analysis/Data/Data_org/"
color_palette<-read.csv(paste(data_path, "color_palette.csv", sep=""))
networks<-read.csv(paste(data_path, "network_tagging_data.csv", sep=""))
individuals<-read.csv(paste(data_path, "individual_tagging_data.csv", sep=""))
network_survey<-read.csv(paste(data_path, "network_survey.csv", sep=""))
networks_LIWC<-read.csv(paste(data_path, "networks_LIWC.csv", sep=""))
manual_rating_network_feelings<-read.csv(paste(data_path, "manual_rating_network_feelings.csv", sep=""))
manual_rating_network_feedback<-read.csv(paste(data_path, "manual_rating_network_feedback.csv", sep=""))
individual_survey<-read.csv(paste(data_path, "individual_survey.csv", sep=""))
individuals_LIWC<-read.csv(paste(data_path, "individuals_LIWC.csv", sep=""))
manual_rating_individual_feelings<-read.csv(paste(data_path, "manual_rating_individual_feelings.csv", sep=""))
manual_rating_individual_feedback<-read.csv(paste(data_path, "manual_rating_individual_feedback.csv", sep=""))

##########
#Figure 1#
##########

#Fig. 1: Networks#
network_classifications<-subset(networks, !label %in% c("Don't Remove", ""))
network_classifications<-subset(network_classifications, class=="speaker")
network_top_classifications<-network_classifications %>% group_by(N, instance_id, resp) %>% 
  dplyr::summarise(max_success = sum(success)) %>% group_by(N, instance_id) %>% 
  top_n(3, wt=max_success)

network_top_classifications_plot<-data.frame()
for(iid in unique(network_top_classifications$instance_id)){
  print(iid)
  iid_df_top<-subset(network_top_classifications, instance_id == iid)
  iid_df_wide<-subset(network_classifications, instance_id == iid)
  iid_df_wide_sub<-subset(iid_df_wide, iid_df_wide$resp %in% iid_df_top$resp)
  network_top_classifications_plot<-rbind(network_top_classifications_plot, iid_df_wide_sub)
}

network_top_classifications_plot$label<-as.character(network_top_classifications_plot$label)
network_top_classifications_plot$label<-as.factor(network_top_classifications_plot$label)
network_top_classifications_plot$instance_id<-as.factor(network_top_classifications_plot$instance_id)
network_top_classifications_plot$label<-as.factor(network_top_classifications_plot$label)
network_label_levels<-levels(network_top_classifications_plot$label)

network_colorset<-c()
for(thislabel in network_label_levels){
  this_color<-as.character(subset(color_palette, label == thislabel)$color)
  network_colorset<-c(network_colorset, this_color)
}

network_top_classifications_plot$instance_id<-as.character(as.numeric(as.factor(network_top_classifications_plot$instance_id)))
network_top_classifications_plot<-as.data.frame(network_top_classifications_plot %>% arrange(-desc(instance_id)))
network_top_classifications_plot$instance_id<-as.factor(network_top_classifications_plot$instance_id)
levels(network_top_classifications_plot$instance_id)<-seq(1,8,1)
network_top_classifications_plot$instance_id<-factor(network_top_classifications_plot$instance_id, levels = rev(seq(1,8,1)))

ggplot(network_top_classifications_plot, aes(x = true_img, y = instance_id, fill=label)) +
  geom_density_ridges(aes(scale=3.3, y = instance_id),alpha=1, size=1.2) +
  scale_x_continuous(limits= c(-100,700), breaks=seq(0,600, 100)) + 
  scale_fill_manual(values=network_colorset, aesthetics = "fill") + 
  ylab("Trials") + xlab("Image Continuum") + 
  ggtitle("Networks") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_text(size = 30, vjust = 0.1, hjust = 0.45),
        axis.title.y=element_text(size = 30, vjust = 0.5, hjust = 0.5),
        axis.title.x=element_text(size = 30, vjust=2, hjust = 0.6),
        axis.text.y=element_text(size = 30),
        axis.text.x=element_text(size = 45),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

#Fig. 1: Individuals#
individuals$freq<-1
individual_tagging_data_agg<-individuals %>% group_by(uniqueID, label) %>%
  dplyr::summarise(success=sum(freq)) %>% arrange(uniqueID, -success)

individual_classifications_top<-subset(individual_tagging_data_agg, !label %in% c("Don't Remove", "")) %>% group_by(uniqueID) %>% 
  top_n(3, wt=success)

individual_top_classifications_plot<-data.frame()
for(iid in unique(individual_classifications_top$uniqueID)){
  print(iid)
  iid_df_top<-subset(individual_classifications_top, uniqueID == iid)
  iid_df_wide<-subset(individuals, uniqueID == iid)
  iid_df_wide_sub<-subset(iid_df_wide, iid_df_wide$label %in% iid_df_top$label)
  individual_top_classifications_plot<-rbind(individual_top_classifications_plot, iid_df_wide_sub)
}

individual_top_classifications_plot_select<-
  subset(individual_top_classifications_plot, uniqueID %in% c("3_2", "4_4", "4_6", "2_4", "1_6", "1_2", "3_4", "2_6"))

individual_top_classifications_plot_select$label<-as.character(individual_top_classifications_plot_select$label)
individual_top_classifications_plot_select$label<-as.factor(individual_top_classifications_plot_select$label)
individual_label_levels<-levels(individual_top_classifications_plot_select$label)

individual_colorset<-c()
for(thislabel in individual_label_levels){
  this_color<-as.character(subset(color_palette, label == thislabel)$color)
  individual_colorset<-c(individual_colorset, this_color)
}

individual_top_classifications_plot_select$uniqueID<-as.factor(individual_top_classifications_plot_select$uniqueID)
levels(individual_top_classifications_plot_select$uniqueID)<-sample(levels(individual_top_classifications_plot_select$uniqueID))
levels(individual_top_classifications_plot_select$uniqueID)<-rev(c(1:8))

ggplot(individual_top_classifications_plot_select, aes(x = true_img, y = uniqueID, fill=label)) +
  geom_density_ridges(aes(scale=4.8, y = uniqueID),alpha=1, size=1.2) +
  scale_x_continuous(limits= c(-100,700), breaks=seq(0,600, 100)) + 
  scale_fill_manual(values=individual_colorset, aesthetics = "fill") + 
  ylab("Trials") + xlab("Image Continuum") + 
  ggtitle("Individuals") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        #legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_text(size = 30, vjust = 0.1, hjust = 0.45),
        axis.title.y=element_text(size = 30, vjust = 0.1, hjust = 0.5),
        axis.title.x=element_text(size = 30, vjust=2, hjust = 0.6),
        axis.text.y=element_text(size = 45),
        axis.text.x=element_text(size = 45),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

###############################
#Figure 1 Statistical Analysis#
###############################
individual_classifications_top$instance_id<-individual_classifications_top$uniqueID

individual_jaccard<-get_jaccard_simple(individual_classifications_top); 
mean(individual_jaccard$jaccard)

network_jaccard<-get_jaccard_simple(network_top_classifications_plot)
mean(network_jaccard$jaccard)

wilcox.test(individual_jaccard$jaccard, network_jaccard$jaccard)

##########
#Figure 2#
##########

#Fig. 2: Networks#
networks_success<-subset(networks, success==1)
networks_success_remove<-subset(networks_success, label %in% c("Don't Remove"))
networks_success_remove$instance_id<-as.factor(networks_success_remove$instance_id)

networks_success_remove$instance_id<-as.character(as.numeric(as.factor(networks_success_remove$instance_id)))
networks_success_remove<-as.data.frame(networks_success_remove %>% arrange(-desc(instance_id)))
networks_success_remove$instance_id<-as.factor(networks_success_remove$instance_id)
levels(networks_success_remove$instance_id)<-seq(1,8,1)
networks_success_remove$instance_id<-factor(networks_success_remove$instance_id, levels = rev(seq(1,8,1)))

networks_success_remove_plot<-networks_success_remove %>% group_by(instance_id, N) %>% 
  dplyr::summarise(center_img = median(true_img), sd=sd(true_img),
                   img_low = center_img - (0.5*sd), img_hi = center_img + (0.5*sd) )

networks_success_remove_plot_display<-data.frame()
for(iid in unique(networks_success_remove_plot$instance_id)){
  iid_df<-subset(networks_success_remove_plot, instance_id == iid)
  label_range= seq(round(iid_df$img_low), round(iid_df$img_hi),1)
  all_imgs<-0:600
  success_range<-as.numeric(all_imgs %in% label_range)
  new_row<-data.frame(instance_id=iid, label="Don't Remove", true_img=all_imgs, success_range = success_range)
  networks_success_remove_plot_display<-rbind(networks_success_remove_plot_display, new_row)
}

networks_success_remove_plot_display<-subset(networks_success_remove_plot_display, success_range>0)

networks_success_remove_plot_display_bounds<-networks_success_remove_plot_display %>% 
  group_by(instance_id, label, success_range) %>% dplyr::summarise(min_true_img = min(true_img), max_true_img = max(true_img))

#for each trial, only grab images within the sd range
networks_success_remove_plot_display_bounds_final<-data.frame()
for(iid in unique(networks_success_remove_plot_display_bounds$instance_id)){
  iid_df<-subset(networks_success_remove, instance_id == iid)
  bound_df<-subset(networks_success_remove_plot_display_bounds, instance_id==iid)
  iid_wi_bound<-subset(iid_df, true_img >= bound_df$min_true_img & true_img <= bound_df$max_true_img)
  networks_success_remove_plot_display_bounds_final<-rbind(networks_success_remove_plot_display_bounds_final, iid_wi_bound)
}

ggplot(networks_success_remove_plot_display_bounds_final, aes(x = true_img, y = instance_id, fill=label)) +
  geom_density_ridges(aes(scale=3.8, y = instance_id),alpha=1, size=1.2) +
  scale_x_continuous(limits= c(-160,800), breaks=seq(0,600, 100)) + 
  scale_fill_manual(values=c("red"))+
  ylab("Trials") + xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"), 
        legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_text(size = 40, vjust = 0.1, hjust = 0.45),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 40, vjust=-0.2, hjust = 0.45),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 40),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

#Fig. 2: ndividuals#
individuals_remove<-subset(individuals, label %in% c("Don't Remove"))
individuals_remove$uniqueID<-as.factor(individuals_remove$uniqueID)
individuals_remove$instance_id<-as.character(as.numeric(as.factor(individuals_remove$uniqueID)))
individuals_remove<-as.data.frame(individuals_remove %>% arrange(-desc(instance_id)))
individuals_remove$instance_id<-as.factor(individuals_remove$instance_id)
levels(individuals_remove$instance_id)<-seq(1,length(individuals_remove$instance_id),1)
individuals_remove$instance_id<-factor(individuals_remove$instance_id, levels = rev(seq(1,length(individuals_remove$instance_id),1)))

individuals_remove_plot<-individuals_remove %>% group_by(instance_id, N) %>% 
  dplyr::summarise(center_img = median(true_img), sd=sd(true_img),
                   img_low = center_img - (0.5*sd), img_hi = center_img + (0.5*sd) )

individuals_remove_plot_display<-data.frame()
for(iid in unique(individuals_remove_plot$instance_id)){
  print(iid)
  iid_df<-subset(individuals_remove_plot, instance_id == iid)
  iid_df[is.na(iid_df)] <- 0
  label_range= seq(round(iid_df$img_low), round(iid_df$img_hi),1)
  all_imgs<-0:600
  success_range<-as.numeric(all_imgs %in% label_range)
  new_row<-data.frame(instance_id=iid, label="Don't Remove", true_img=all_imgs, success_range = success_range)
  individuals_remove_plot_display<-rbind(individuals_remove_plot_display, new_row)
}

individuals_remove_plot_display<-subset(individuals_remove_plot_display, success_range>0)

individuals_remove_plot_display_bounds<-individuals_remove_plot_display %>% 
  group_by(instance_id, label, success_range) %>% dplyr::summarise(min_true_img = min(true_img), max_true_img = max(true_img))

#for each trial, only grab images within the sd range
individuals_remove_plot_display_bounds_final<-data.frame()
for(iid in unique(individuals_remove_plot_display_bounds$instance_id)){
  iid_df<-subset(individuals_remove, instance_id == iid)
  bound_df<-subset(individuals_remove_plot_display_bounds, instance_id==iid)
  iid_wi_bound<-subset(iid_df, true_img >= bound_df$min_true_img & true_img <= bound_df$max_true_img)
  individuals_remove_plot_display_bounds_final<-rbind(individuals_remove_plot_display_bounds_final, iid_wi_bound)
}

individuals_remove_plot_subset<-subset(individuals_remove_plot_display_bounds_final, 
                                       instance_id %in% sample(unique(individuals_remove_plot_display_bounds_final$instance_id),8))

individuals_remove_plot_subset$instance_id<-as.factor(as.numeric(as.character(individuals_remove_plot_subset$instance_id)))

ggplot(individuals_remove_plot_subset, aes(x = true_img, y = instance_id, fill=label)) +
  geom_density_ridges(aes(scale=3.8, y = instance_id),alpha=1, size=1.2) +
  scale_x_continuous(limits= c(-160,800), breaks=seq(0,600, 100)) + 
  scale_fill_manual(values=c("red"))+
  ylab("Trials") + xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"), 
        legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_text(size = 40, vjust = 0.1, hjust = 0.45),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 40, vjust=-0.2, hjust = 0.45),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 40),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

##########
#Figure 3#
##########

#Fig. 3: Get Individual Data#
Repub_ind<-subset(individuals, Party=="Republican")
Repub_ind_simp<-Repub_ind %>% select(uniqueID,label,N,true_img,Party)
Repub_ind_simp$freq<-1
Repub_ind_simp_unique<-unique(Repub_ind_simp)

Demo_ind<-subset(individuals, Party=="Democrat")
Demo_ind_simp<-Demo_ind %>% select(uniqueID,label,N,true_img,Party)
Demo_ind_simp$freq<-1
Demo_ind_simp_unique<-unique(Demo_ind_simp)

dem_iids<-unique(Demo_ind_simp_unique$uniqueID)
rep_iids<-unique(Repub_ind_simp_unique$uniqueID)

ind_cross_party<-data.frame()

for(dem in dem_iids){
  print(dem)
  dem_df<-subset(Demo_ind_simp_unique, uniqueID==dem)
  
  for(rep in rep_iids){
    rep_df<-subset(Repub_ind_simp_unique, uniqueID==rep)
    
    cross_party_match<-merge(dem_df, rep_df, by=c("true_img"))
    cross_party_match<-cross_party_match[complete.cases(cross_party_match),]
    
    if(nrow(cross_party_match)>0){
      rate_match<-sum(cross_party_match$label.x==cross_party_match$label.y)/nrow(cross_party_match)
      
      ind_cross_party<-rbind(ind_cross_party, 
                             data.frame(dem_iid=dem, rep_iid=rep, rate_match=rate_match))
    }
  }
}

#Fig. 3: Get Network Data#
net_iids<-unique(networks$instance_id)
net_cross_party<-data.frame()

for(iid in net_iids){
  
  iid_df<-subset(networks, instance_id==iid)
  
  reps_iid<-subset(iid_df, Party=="Republican")
  reps_iid_agg<-reps_iid %>% group_by(Party, true_img, label) %>% 
    dplyr::summarise(numsuccess=sum(success)) %>% 
    group_by(Party, true_img) %>% top_n(1,wt=numsuccess)
  
  demos_iid<-subset(iid_df, Party=="Democrat")
  demos_iid_agg<-demos_iid %>% group_by(Party, true_img, label) %>% 
    dplyr::summarise(numsuccess=sum(success)) %>% 
    group_by(Party, true_img) %>% top_n(1,wt=numsuccess)
  
  cross_party_match<-merge(reps_iid_agg, demos_iid_agg, by=c("true_img"))
  cross_party_match<-cross_party_match[complete.cases(cross_party_match),]
  rate_match<-sum(cross_party_match$label.x==cross_party_match$label.y)/nrow(cross_party_match)
  
  net_cross_party<-rbind(net_cross_party, data.frame(instance_id=iid, rate_match=rate_match))
  
}

#Fig. 3: Plot#
ind_cross_party$condition<-"Individuals"
net_cross_party$condition<-"Networks"
net_cross_party_plot<-net_cross_party %>% select(-instance_id)
ind_cross_party_plot<-ind_cross_party %>% select(-dem_iid, -rep_iid)

partisan_fig_ind_net<-rbind(ind_cross_party_plot, net_cross_party_plot)

partisan_fig_ind_net_plot<-partisan_fig_ind_net %>% group_by(condition) %>% 
  dplyr::summarise(cilow=t.test(rate_match)$conf.int[1],
                   cihi=t.test(rate_match)$conf.int[2],
                   rate_match=mean(rate_match))

partisan_fig_ind_net_plot$condition<-as.factor(partisan_fig_ind_net_plot$condition)

ggplot(partisan_fig_ind_net_plot, aes(x=condition, y=rate_match, fill=condition, group=1)) + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, alpha=0.7) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), 
                position = position_dodge(1), linetype="solid", width=0.2, size=2)+ 
  scale_fill_manual(values=c("blue", "darkgrey"))+
  theme(axis.text.x = element_text(size=35), 
        axis.text.y = element_text(size=35),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=35, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25),  
        legend.position ="none",
        legend.title=element_blank(),
        legend.text=element_text(size=25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y="Cross-party Classification Agreement\n(Republicans and Democrats)", linetype=NULL, x="Group Size") + 
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5))

#Fig. 3: Statistical Analysis# 
wilcox.test(net_cross_party$rate_match, ind_cross_party$rate_match)
mean(ind_cross_party$rate_match)
mean(net_cross_party$rate_match)

##########
#FIGURE 4#
##########
individuals_LIWC_simp<-individuals_LIWC[,!names(individuals_LIWC) %in% c("Source..A.", "Source..B.")]; 
networks_LIWC_simp<-networks_LIWC[,!names(networks_LIWC) %in% c("Source..A.", "Source..B.", "Source..C.", "Source..D.", "Source..E.")]; 
individuals_LIWC_agg<-individuals_LIWC_simp %>% group_by(cond, N) %>% summarise_all(mean, na.rm=T)
networks_LIWC_agg<-networks_LIWC_simp %>% group_by(cond, N) %>% summarise_all(mean, na.rm=T)

all_LIWC<-rbind(individuals_LIWC_simp, networks_LIWC_simp)
all_LIWC$negemomod<--1*all_LIWC$negemo
all_LIWC$aggemo<-all_LIWC$negemomod + all_LIWC$posemo
all_LIWC$aggemo_z<-sapply(all_LIWC$aggemo, function(x) x - mean(all_LIWC$aggemo, na.rm=T)/sd(all_LIWC$aggemo, na.rm=T))

all_LIWC_agg <- all_LIWC %>% group_by(N) %>% 
  dplyr::summarise(
    cilow=t.test(aggemo, conf.level = 0.95)$conf.int[1],
    cihi=t.test(aggemo, conf.level = 0.95)$conf.int[2],
    avg.sent=mean(aggemo, na.rm=T)
  ) 

all_LIWC_agg$N<-as.factor(all_LIWC_agg$N)
levels(all_LIWC_agg$N)<-c("Individuals", "Networks")
all_LIWC_agg$condition<-"LIWC"

ggplot(all_LIWC_agg, aes(x=N, y=avg.sent, color = N, group=1)) + 
  geom_point(aes(color=N), size=8, position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), position = position_dodge(0.9), 
                linetype="solid", width=0.2, size=3)+ 
  theme(axis.text.x = element_text(size=30), 
        axis.text.y = element_text(size=30),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=30, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25),  
        legend.position = "none",
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=2) + 
  labs(y="Sentiment of Subjects' Self-reported\n Task Experience (LIWC)", linetype=NULL, x="Group Size")


############################
###Supplementary Analyses###
############################

#########
#Fig. S4#
#########
individuals_clean<-subset(individuals, label != "Don't Remove")

figs4A_top<-individuals_clean %>% mutate(freq=1) %>% 
  group_by(true_img,label) %>%
  dplyr::summarise(freq=sum(freq)) %>% 
  group_by(true_img) %>% 
  top_n(1,wt=freq)

figs4A_top$abuse_top<-as.numeric(figs4A_top$label=="Abuse")
figs4A_top$terrorism_top<-as.numeric(figs4A_top$label=="Terrorism")

figs4A_top_abuse<-figs4A_top %>% select(true_img, abuse_top) %>% mutate(label="Abuse")
colnames(figs4A_top_abuse)<-c("true_img", "top","label")
figs4A_top_terror<-figs4A_top %>% select(true_img, terrorism_top) %>% mutate(label="Terrorism")
colnames(figs4A_top_terror)<-c("true_img", "top","label")

figs4_top<-rbind(figs4A_top_abuse, figs4A_top_terror)

ggplot(figs4_top, aes(x = true_img, y = top , color = label, group = label)) +
  geom_smooth(size=3, se=F, span=2) + theme_bw() + 
  scale_color_manual(values=c("red", "blue")) +  
  scale_x_continuous(limits= c(-100,650), breaks=seq(0,700, 100)) + 
  ylab("P(Most Popular Label across Individuals)") + 
  xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=35),
        legend.title=element_blank(), 
        legend.position=c(0.8,0.9),
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "white", fill="white"),
        plot.title=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.y=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.x=element_text(size = 35, hjust = 0.5),
        axis.text.y=element_text(size = 35),
        axis.text.x=element_text(size = 35),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

####Fraction of Subjects 
figs4B<-individuals %>% group_by(true_img) %>% dplyr::summarise(ambiguity = length(unique(label)))

ggplot(figs4B, aes(x = true_img, y = ambiguity)) +
  geom_smooth(size=3, color="forestgreen", se=F) + theme_bw() + 
  ylab("Ambiguity\nNum. Unique Labels across Subjects") + 
  xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=35),
        legend.title=element_blank(), 
        legend.position=c(0.35,0.2),
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "white", fill="white"),
        plot.title=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.y=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.x=element_text(size = 35, hjust = 0.5),
        axis.text.y=element_text(size = 35),
        axis.text.x=element_text(size = 35),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_x_continuous(limits= c(-100,650), breaks=seq(0,700, 100)) + 
  coord_cartesian(ylim=c(5,8))

#Sentiment robustness 
#AFINN - feedback - Individual 
individual_survey$id<-1:nrow(individual_survey)
individual_survey_feelings<-individual_survey[,c("id", "feelings") ]
individual_survey_feelings_sent <- individual_survey_feelings %>% unnest_tokens(word, feelings) %>%
  inner_join(get_sentiments("afinn")) 
individual_survey_feelings_sent_agg<-individual_survey_feelings_sent %>% group_by(id) %>% 
  dplyr::summarise(sentiment = mean(value))

#AFINN - feedback - Individual 
individual_survey_feedback<-individual_survey[,c("id", "feedback") ]
individual_survey_feedback_sent <- individual_survey_feedback %>% unnest_tokens(word, feedback) %>%
  inner_join(get_sentiments("afinn")) 
individual_survey_feedback_sent_agg<-individual_survey_feedback_sent %>% group_by(id) %>% 
  dplyr::summarise(sentiment = mean(value))

##NETWORKS##
#AFINN - feedlings - Networks 
network_survey_feelings<-subset(network_survey, name=="feelings")
network_survey_feelings_sent <- network_survey_feelings %>% unnest_tokens(word, value) %>%
  inner_join(get_sentiments("afinn")) 
network_survey_feelings_sent_agg<-network_survey_feelings_sent %>% group_by(node_id, N) %>% 
  dplyr::summarise(sentiment = mean(value))
#AFINN - feedback - Networks 
network_survey_feedback<-subset(network_survey, name=="feedback")
network_survey_feedback_sent <- network_survey_feedback %>% unnest_tokens(word, value) %>%
  inner_join(get_sentiments("afinn")) 
network_survey_feedback_sent_agg<-network_survey_feedback_sent %>% group_by(node_id, N) %>% 
  dplyr::summarise(sentiment = mean(value))

#normalize data 
afinn_individual<-c(individual_survey_feedback_sent_agg$sentiment, individual_survey_feelings_sent_agg$sentiment)
afinn_network<-c(network_survey_feedback_sent_agg$sentiment, network_survey_feelings_sent_agg$sentiment)
afinn_data<-c(afinn_individual, afinn_network)
afinn_individual_z<-sapply(afinn_individual, function(x) x - mean(afinn_data, na.rm=T)/sd(afinn_data, na.rm=T))
afinn_network_z<-sapply(afinn_network, function(x) x - mean(afinn_data, na.rm=T)/sd(afinn_data, na.rm=T))

afinn_plot_df<-rbind(
  data.frame(cilow=t.test(afinn_individual_z)$conf.int[1],
             cihi=t.test(afinn_individual_z)$conf.int[2],
             avg.sent=mean(afinn_individual_z, na.rm=T), N=1 ), 
  data.frame(cilow=t.test(afinn_network_z)$conf.int[1],
             cihi=t.test(afinn_network_z)$conf.int[2],
             avg.sent=mean(afinn_network_z, na.rm=T), N=50) )
afinn_plot_df$N<-as.factor(afinn_plot_df$N)
afinn_plot_df$condition<-"Afinn"

#Get Manual Sentiment classifications 
manual_rating_individual_feelings_agg<-manual_rating_individual_feelings %>% summarise_all(mean, na.rm=T)
manual_rating_network_feelings_agg<-manual_rating_network_feelings %>% summarise_all(mean, na.rm=T)

manual_rating_individual_feedback_agg<-manual_rating_individual_feedback %>% summarise_all(mean, na.rm=T)
manual_rating_network_feedback_agg<-manual_rating_network_feedback %>% summarise_all(mean, na.rm=T)

#Normalize Data# 
#the response "no" was repeated across both conditions; 25 times in the individual condition; 5 times in the network condition
#It was only rated once during crowdsourcing, so here we add these additional scores accordingly 
#Note: the results are equally robust without this addition 
response.no=-0.9
manual_rating_individual<-c(as.numeric(manual_rating_individual_feelings_agg), as.numeric(manual_rating_individual_feedback_agg),rep(response.no, 25))
manual_rating_network<-c(as.numeric(manual_rating_network_feelings_agg), as.numeric(manual_rating_network_feedback_agg),rep(response.no, 5))

manual_rating_data<-c(manual_rating_individual, manual_rating_network)
manual_individual_z<-sapply(manual_rating_individual, function(x) x - mean(manual_rating_data, na.rm=T)/sd(manual_rating_data, na.rm=T))
manual_network_z<-sapply(manual_rating_network, function(x) x - mean(manual_rating_data, na.rm=T)/sd(manual_rating_data, na.rm=T))

manual_sent_plot_df<-rbind(data.frame(
  cilow=t.test(manual_individual_z)$conf.int[1],
  cihi=t.test(manual_individual_z)$conf.int[2],
  avg.sent=mean(manual_individual_z, na.rm=T), N=1 ), 
  data.frame(cilow=t.test(manual_network_z)$conf.int[1],
             cihi=t.test(manual_network_z)$conf.int[2],
             avg.sent=mean(manual_network_z, na.rm=T), N=50)
)

manual_sent_plot_df$N<-as.factor(manual_sent_plot_df$N)
manual_sent_plot_df$condition<-"Crowdsourced"

####################################
#Fig. S5; Sentiment Robustness Plot#
####################################
figS5<-rbind(afinn_plot_df,manual_sent_plot_df)
figS5$plot_id<-paste(figS5$N, figS5$condition, sep="_")

ggplot(figS5, aes(x=condition, y=avg.sent, color = N, group=1)) + 
  geom_point(aes(color=N), size=8, position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), position = position_dodge(0.9), 
                linetype="solid", width=0.2, size=3)+ 
  theme(axis.text.x = element_text(size=30), 
        axis.text.y = element_text(size=30),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=30, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25),  
        legend.position = c(0.15,0.95),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=2) + 
  labs(y="Sentiment of Participants' Self-reported\n Task Experience (Z-score)", linetype=NULL, x="Group Size")

#STATISTICAL CONTROLS FOR LIWC SENTIMENT ANALYSIS RESULTS (FIG.4 - MAIN TEXT)#
all_LIWC$EXP_condition<-all_LIWC$N
all_LIWC$EXP_condition<-as.factor(all_LIWC$EXP_condition)
levels(all_LIWC$EXP_condition)<-c("Individuals", "Networks")

all_LIWC$SurveyQuest.<-all_LIWC$cond
all_LIWC$SurveyQuest.<-as.factor(all_LIWC$SurveyQuest.)
all_LIWC$wordcount<-all_LIWC$WC
all_LIWC$social_lang<-all_LIWC$social
all_LIWC$LIWC_sentiment<-all_LIWC$aggemo
all_LIWC$nonfluent<-all_LIWC$nonflu
all_LIWC$OtherOriented<-all_LIWC$OtherP
all_LIWC$religion<-all_LIWC$relig

tableS1<-lm(LIWC_sentiment ~  EXP_condition + SurveyQuest. + wordcount + social_lang + 
                           female + male + cogproc + insight + cause + 
                           discrep + tentat + certain + differ + percept + drives + 
                           achieve + bio + body + health + power + reward + 
                           risk + focuspast + focuspresent + focusfuture + relativ + motion + OtherOriented + 
                           space + time + work + money + relig + death + informal + swear + nonfluent, 
                         data = all_LIWC)

tab_model(tableS1)

##############################################################
###Get Aggregated Groups of 50 Individuals for WoC Analysis###
##############################################################
ind_review<-individuals %>% group_by(uniqueID) %>% dplyr::summarise(numimgs=length(unique(true_img)))
ind_review<-subset(ind_review, numimgs>90)
individuals_clean<-subset(individuals, uniqueID %in% ind_review$uniqueID)
individuals_clean$freq<-1

#get the same number of images that were classified on average by participants in the networks 
individuals_clean_org<-individuals_clean %>% group_by(uniqueID) %>% sample_n(48) 

aggregated_groups<-data.frame()
for(i in 1:8){
  df<-subset(individuals_clean_org, uniqueID %in% sample(unique(individuals_clean_org$uniqueID), 48))
  df$instance_id<-i
  aggregated_groups<-rbind(aggregated_groups, data.frame(df))
}

aggregated_groups$success_raw<-1
aggregated_groups<-aggregated_groups %>% group_by(instance_id, label, true_img) %>% 
  dplyr::mutate(numsubjs = length(unique(uniqueID)), 
                success_sim = ifelse(numsubjs > 1, 1, 0))
aggregated_groups$refN<-50

###########
#Figure S6#
###########
aggregated_groups_agg<-aggregated_groups %>% 
  group_by(instance_id, true_img, label, refN) %>% 
  dplyr::summarise(allsuccess=sum(success_raw))

aggregated_groups_agg_top<-aggregated_groups_agg %>% group_by(instance_id, true_img, refN) %>% dplyr::top_n(1)

all_instance_ids=unique(aggregated_groups_agg_top$instance_id)

agg_groups_compare<-data.frame()
for(iid_i in all_instance_ids){
  print(iid_i)
  iid_i_df<-subset(aggregated_groups_agg_top, instance_id == iid_i)
  iid_js<-all_instance_ids[all_instance_ids != iid_i]
  for(iid_j in iid_js){
    iid_j_df<-subset(aggregated_groups_agg_top, instance_id == iid_j)
    iids_merge<-merge(iid_i_df, iid_j_df, by=c("true_img"), all= TRUE)
    iids_merge[is.na(iids_merge)] <- 0
    iids_merge$match<-iids_merge$label.x==iids_merge$label.y
    combo_iid<-paste(max(iid_i, iid_j),"_",min(iid_i, iid_j), sep="")
    iids_merge$comp_iid<-combo_iid
    agg_groups_compare<-rbind(agg_groups_compare, iids_merge) 
  }
}

agg_groups_compare_by_iid<-agg_groups_compare %>% group_by(comp_iid) %>%
  dplyr::summarise(rate_agree=sum(match)/length(match))

agg_groups_compare_by_img<-agg_groups_compare %>% group_by(true_img) %>%
  dplyr::summarise(rate_agree=sum(match)/length(match))

###Actual Networks###
network_agg<-networks %>% 
  group_by(instance_id, true_img, label, N) %>% 
  dplyr::summarise(allsuccess=sum(success)) %>% subset(N==50)

network_agg_top<-network_agg %>% group_by(instance_id, true_img, N) %>% dplyr::top_n(1)

all_instance_ids=unique(network_agg_top$instance_id)

network_compare<-data.frame()
for(iid_i in all_instance_ids){
  print(iid_i)
  iid_i_df<-subset(network_agg_top, instance_id == iid_i)
  iid_js<-all_instance_ids[all_instance_ids != iid_i]
  for(iid_j in iid_js){
    iid_j_df<-subset(network_agg_top, instance_id == iid_j)
    iids_merge<-merge(iid_i_df, iid_j_df, by=c("true_img"), all= TRUE)
    iids_merge[is.na(iids_merge)] <- 0
    iids_merge$match<-iids_merge$label.x==iids_merge$label.y
    combo_iid<-paste(max(iid_i, iid_j),"_",min(iid_i, iid_j), sep="")
    iids_merge$comp_iid<-combo_iid
    network_compare<-rbind(network_compare, iids_merge) 
  }
}

network_compare_by_iid<-network_compare %>% group_by(comp_iid) %>%
  dplyr::summarise(rate_agree=sum(match)/length(match))

network_compare_by_img<-network_compare %>% group_by(true_img) %>%
  dplyr::summarise(rate_agree=sum(match)/length(match))

agg_groups_compare_by_img$Condition<-"Aggregated Individuals"
network_compare_by_img$Condition<-"Networks"

#Fig. S6A#
figs6A<-rbind(agg_groups_compare_by_img, network_compare_by_img)

ggplot(figs6A, aes(x = true_img, y = rate_agree , color = Condition, group = Condition)) +
  geom_smooth(size=3) + theme_bw() + 
  scale_color_manual(values=c("blue", "grey30")) +  
  scale_x_continuous(limits= c(-100,650), breaks=seq(0,700, 100)) + 
  ylab("Classification Agreement\n(% Trials with Same Violation Tag)") + 
  xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=35),
        legend.title=element_blank(), 
        legend.position=c(0.35,0.2),
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "white", fill="white"),
        plot.title=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.y=element_text(size = 35, vjust = 0.1, hjust = 0.5),
        axis.title.x=element_text(size = 35, hjust = 0.5),
        axis.text.y=element_text(size = 35),
        axis.text.x=element_text(size = 35),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(limits=c(0,600)) +
  coord_cartesian(ylim=c(0,1)) 

#Fig. S6B#
figs6B<-figs6A %>% group_by(Condition) %>%
  dplyr::summarise(
    cilow=t.test(rate_agree)$conf.int[1], 
    cihi=t.test(rate_agree)$conf.int[2],
    rate_agree=mean(rate_agree)
  )

figs6B$Condition<-as.factor(figs6B$Condition)
levels(figs6B$Condition)<-c("Aggregated\nIndividuals", "Networked\nTeams" )

ggplot(aes(x=Condition, y=rate_agree,group=Condition, color=Condition, ymin=cilow,ymax=cihi), 
       data = figs6B) + 
  geom_point(size=12) + 
  geom_errorbar(width = 0.3, size = 3) + theme_bw() +
  ylab("P(Majority Agreement across Trials)") + 
  scale_color_manual(values=c("blue", "grey30")) +  
  theme(axis.text=element_text(size=40),
        plot.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=40, hjust=0.5),
        legend.text=element_text(size=40),
        legend.title=element_blank(), 
        legend.background = element_blank(),
        legend.position = "none",
        legend.box.background = element_blank(),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(breaks=c(0.42,0.44,0.46,0.48,0.50,0.52,0.54,0.56)) + 
  geom_hline(yintercept = 0.5) + coord_cartesian(ylim=c(0.41,0.57))


#Fig. S6: Statistical Analyses#
wilcox.test(agg_groups_compare_by_iid$rate_agree, network_compare_by_iid$rate_agree)
wilcox.test(agg_groups_compare_by_img$rate_agree, network_compare_by_img$rate_agree)

comp_by_img<-merge(agg_groups_compare_by_img, network_compare_by_img, by="true_img")
wilcox.test(comp_by_img$rate_agree.x, comp_by_img$rate_agree.y, paired=T)

#################
####Figure S7####
#################
aggregated_groups_iter<-data.frame()
number_of_simulations<-100
for(sim in 1:number_of_simulations){
  print(sim)
  for(i in 1:8){
    df<-subset(individuals_clean_org, uniqueID %in% sample(unique(individuals_clean_org$uniqueID), 48))
    df$sim<-sim
    df$instance_id<-i
    aggregated_groups_iter<-rbind(aggregated_groups_iter, data.frame(df))
  }
}

aggregated_groups_iter$success_raw<-1
aggregated_groups_iter$N<-1
aggregated_groups_iter$refN<-50

aggregated_groups_iter_agg<-aggregated_groups_iter %>% 
  group_by(sim,instance_id, true_img, label, refN) %>% 
  dplyr::summarise(allsuccess=sum(success_raw))

aggregated_groups_iter_top<-aggregated_groups_iter_agg %>% group_by(sim, instance_id, true_img, refN) %>% dplyr::top_n(1)
agg_sims<-unique(aggregated_groups_iter_top$sim)
agg_iids<-unique(aggregated_groups_iter_top$instance_id)

aggregated_groups_iter_compare<-data.frame()

for(sim_i in agg_sims){
  print(sim_i)
  sim_df<-subset(aggregated_groups_iter_top, sim==sim_i)
  
  for(iid_i in agg_iids){
    iid_i_df<-subset(sim_df, instance_id == iid_i)
    iid_js<-agg_iids[agg_iids != iid_i]
    for(iid_j in iid_js){
      iid_j_df<-subset(sim_df, instance_id == iid_j)
      iids_merge<-merge(iid_i_df, iid_j_df, by=c("true_img"), all= TRUE)
      iids_merge[is.na(iids_merge)] <- 0
      iids_merge$match<-iids_merge$label.x==iids_merge$label.y
      combo_iid<-paste(max(iid_i, iid_j),"_",min(iid_i, iid_j), sep="")
      iids_merge$comp_iid<-combo_iid
      iids_merge$sim<-sim_i
      aggregated_groups_iter_compare<-rbind(aggregated_groups_iter_compare, iids_merge) 
    }
  }
}

aggregated_groups_iter_agg_by_img<-aggregated_groups_iter_compare %>% group_by(sim, true_img) %>%
  dplyr::summarise(rate_agree=sum(match)/length(match))
mean(aggregated_groups_iter_agg_by_img$rate_agree)

figs7<-data.frame()

for(sim_i in agg_sims){
  print(sim_i)
  sim_df<-subset(aggregated_groups_iter_agg_by_img, sim==sim_i)
  comp<-t.test(network_compare_by_img$rate_agree, sim_df$rate_agree)
  comp_stat<-comp$statistic
  cilow<-comp$conf.int[1]
  cihi<-comp$conf.int[2]
  pval<-comp$p.value
  figs7<-rbind(figs7,data.frame(sim=sim_i, tstat=comp_stat, cilow=cilow, cihi=cihi, pval=pval))
}

figs7$diff<-sapply(1:nrow(figs7), function(x) median(c(figs7[x,]$cihi,figs7[x,]$cilow)))

ggplot(aes(x=sim, y=diff,ymin=cilow,ymax=cihi), data = figs7) + theme_bw() + 
  geom_errorbar(width = 1, size = 0.5) + geom_point(size=2) + 
  ylab("Difference in Classification Agreement\nin Networks vs. in Aggregated Individuals") + 
  xlab("Attempt at Aggregating Independent \nIndividuals into Eight Groups ") + 
  scale_color_manual(values=c("blue", "grey30")) +  
  theme(axis.text=element_text(size=30),
        plot.title = element_blank(),
        axis.title.x=element_text(size=30, hjust=0.5),
        axis.title.y=element_text(size=30, hjust=0.5),
        legend.text=element_text(size=30),
        legend.title=element_blank(), 
        legend.background = element_blank(),
        legend.position = "none",
        legend.box.background = element_blank(),
        axis.text.x=element_text(size = 30, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_cartesian(ylim=c(-0.02,0.15)) + 
  geom_hline(yintercept = 0)

#############
##Figure S8##
#############
shared_images<-intersect(unique(networks$true_img), unique(aggregated_groups$true_img))

#get control variables
net_img_agg<-subset(networks, class=="speaker") %>% group_by(true_img,instance_id) %>% 
  dplyr::summarise(numIND=length(unique(node_id))) 

ind_img_agg<-aggregated_groups %>% group_by(true_img,instance_id) %>% 
  dplyr::summarise(numIND=length(unique(uniqueID))) 

#Fig. S8: Networks#
networks_sub<-subset(networks, true_img %in% shared_images)
networks_sub_agg<-networks_sub %>% group_by(instance_id, true_img, label, N) %>% dplyr::summarise(allsuccess=sum(success))
networks_sub_agg<-subset(networks_sub_agg, allsuccess>0)
networks_sub_agg_top_per_img<-networks_sub_agg %>% group_by(instance_id, true_img, N) %>% dplyr::top_n(1)
networks_sub_agg_DN<-subset(networks_sub_agg_top_per_img, label=="Don't Remove")

networks_DN_compare<-data.frame()
all_net_instance_ids<-unique(networks_sub_agg_DN$instance_id)

for(iid_i in all_net_instance_ids){
  print(iid_i)
  iid_i_df<-subset(networks_sub_agg_DN, instance_id == iid_i)
  iid_i_imgs<-round(unique(iid_i_df$true_img))
  iid_js<-all_net_instance_ids[all_net_instance_ids != iid_i]
  
  iid_i_ind_match<-subset(net_img_agg, instance_id==iid_i)
  iid_i_numINDs=merge(iid_i_df, iid_i_ind_match, by=c("true_img", "instance_id"))
  
  for(iid_j in iid_js){
    iid_j_df<-subset(networks_sub_agg_DN, instance_id == iid_j)
    iid_j_imgs<-round(unique(iid_j_df$true_img))
    
    iid_j_ind_match<-subset(net_img_agg, instance_id==iid_j)
    iid_j_numINDs=merge(iid_j_df, iid_j_ind_match, by=c("true_img", "instance_id"))
    
    
    combo<-c(iid_i_imgs, iid_j_imgs)
    img_var<-var(combo)
    img_ent<-entropy(combo)
    
    combo_iid<-paste(max(iid_i, iid_j),"_",min(iid_i, iid_j), sep="")
    numIND_averge<-mean(c(iid_i_numINDs$numIND, iid_j_numINDs$numIND))
    num_unique_imgs<-length(unique(combo))
    
    networks_DN_compare<-rbind(networks_DN_compare, 
                               data.frame(combo_iid=combo_iid, img_var=img_var, 
                                          img_ent=img_ent, numIND=numIND_averge, 
                                          num_unique_imgs =num_unique_imgs, condition="Networks")) 
  }
}

head(networks_DN_compare)

####Simulated####
aggregated_groups_sub<-subset(aggregated_groups, true_img %in% shared_images)
aggregated_groups_sub$success<-1

aggregated_groups_sub_agg<-aggregated_groups_sub %>% group_by(instance_id, true_img, label) %>% 
  dplyr::summarise(allsuccess=sum(success))
aggregated_groups_sub_agg<-subset(aggregated_groups_sub_agg, allsuccess>0)

aggregated_groups_sub_agg_top_per_img<-aggregated_groups_sub_agg %>% group_by(instance_id, true_img) %>% dplyr::top_n(1)
aggregated_groups_sub_agg_DN<-subset(aggregated_groups_sub_agg_top_per_img, label=="Don't Remove")

agg_group_instance_ids=unique(aggregated_groups_sub_agg_DN$instance_id)

aggregated_groups_DN_compare<-data.frame()

for(iid_i in agg_group_instance_ids){
  print(iid_i)
  iid_i_df<-subset(aggregated_groups_sub_agg_DN, instance_id == iid_i)
  iid_i_imgs<-round(unique(iid_i_df$true_img))
  iid_js<-agg_group_instance_ids[agg_group_instance_ids != iid_i]
  
  iid_i_ind_match<-subset(ind_img_agg, instance_id==iid_i)
  iid_i_numINDs=merge(iid_i_df, iid_i_ind_match, by=c("true_img", "instance_id"))
  
  for(iid_j in iid_js){
    iid_j_df<-subset(aggregated_groups_sub_agg_DN, instance_id == iid_j)
    iid_j_imgs<-round(unique(iid_j_df$true_img))
    
    iid_j_ind_match<-subset(ind_img_agg, instance_id==iid_j)
    iid_j_numINDs=merge(iid_j_df, iid_j_ind_match, by=c("true_img", "instance_id"))
    
    combo<-c(iid_i_imgs, iid_j_imgs)
    img_var<-var(combo)
    img_ent<-entropy(combo)
    
    combo_iid<-paste(max(iid_i, iid_j),"_",min(iid_i, iid_j), sep="")
    numIND_averge<-mean(c(iid_i_numINDs$numIND, iid_j_numINDs$numIND))
    num_unique_imgs<-length(unique(combo))
    
    aggregated_groups_DN_compare<-rbind(aggregated_groups_DN_compare, 
                           data.frame(combo_iid=combo_iid, img_var=img_var, 
                                      img_ent=img_ent, numIND=numIND_averge, 
                                      num_unique_imgs =num_unique_imgs, condition="Individuals")) 
  }
}

figs8<-rbind(networks_DN_compare, aggregated_groups_DN_compare)
figs8$img_var_std<-figs8$img_var/figs8$num_unique_imgs
figs8$img_var_norm<-min_max_norm(figs8$img_var)
figs8$img_var_std_norm<-1 - min_max_norm(figs8$img_var_std)

#Plot Fig. S8
figs8$condition<-as.factor(figs8$condition)
levels(figs8$condition)<-c("Aggregated Individuals", "Networks")

ggplot(figs8, aes(x = img_var_std_norm, fill=condition)) +
  geom_density(alpha=0.8, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("blue","darkgrey")) + 
  labs(x = "Convergence in use of *Do Not Remove*", y = "Density") + 
  theme(legend.text=element_text(size=30),
        legend.title=element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_text(size = 43, vjust = 0.1, hjust = 0.45),
        axis.title.y=element_text(size = 43, vjust = 0.5, hjust = 0.5),
        axis.text.y=element_text(size = 43),
        axis.text.x=element_text(size = 43),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = ggtext::element_markdown(size = 43, vjust=2, hjust = 0.6)) + 
  scale_x_continuous(limits=c(-0.3,1.3)) 

############
##Table S2##
############
figs8$condition<-as.factor(figs8$condition)
figs8$num_unique_imgs<-as.numeric(figs8$num_unique_imgs)
figs8$trial_id<-sapply(1:nrow(figs8),function(x) strsplit(figs8[x,]$combo_iid, "_")[[1]][1] )
figs8$Convergence_in_use_of_Do_Not_Remove<-1-figs8$img_var_norm

tableS2<-lm(Convergence_in_use_of_Do_Not_Remove ~ condition + num_unique_imgs, data=figs8)
summary(tableS2)

figs8_vcov <- cluster.vcov(tableS2, figs8$trial_id)
tableS2_cluster<-coeftest(tableS2, figs8_vcov)
tableS2_cluster

tab_model(tableS2)
stargazer(tableS2_cluster, keep.stat="all")

#############
###Fig. S9###
#############
aggregated_groups_cross_party<-data.frame()

for(iid in unique(aggregated_groups$instance_id)){
  iid_df<-subset(aggregated_groups, instance_id==iid)
  reps_iid<-subset(iid_df, Party=="Republican")
  reps_iid_agg<-reps_iid %>% group_by(Party, true_img, label) %>% 
    dplyr::summarise(numsuccess=sum(success_raw)) %>% 
    group_by(Party, true_img) %>% top_n(1,wt=numsuccess)
  
  demos_iid<-subset(iid_df, Party=="Democrat")
  demos_iid_agg<-demos_iid %>% group_by(Party, true_img, label) %>% 
    dplyr::summarise(numsuccess=sum(success_raw)) %>% 
    group_by(Party, true_img) %>% top_n(1,wt=numsuccess)
  
  cross_party_match<-merge(reps_iid_agg, demos_iid_agg, by=c("true_img"))
  cross_party_match<-cross_party_match[complete.cases(cross_party_match),]
  rate_match<-sum(cross_party_match$label.x==cross_party_match$label.y)/nrow(cross_party_match)
  
  aggregated_groups_cross_party<-rbind(aggregated_groups_cross_party, data.frame(instance_id=iid, rate_match=rate_match))
  
}

mean(aggregated_groups_cross_party$rate_match)

#Plot Figure S9# 
wilcox.test(aggregated_groups_cross_party$rate_match, net_cross_party$rate_match)

aggregated_groups_cross_party$condition<-"Aggregated\nIndividuals"
net_cross_party$condition<-"Networks"

figs9_raw<-rbind(aggregated_groups_cross_party, net_cross_party)

figs9<-figs9_raw %>% group_by(condition) %>% 
  dplyr::summarise(cilow=t.test(rate_match)$conf.int[1],
                   cihi=t.test(rate_match)$conf.int[2],
                   rate_match=mean(rate_match))

ggplot(figs9, aes(x=condition, y=rate_match, fill=condition, group=1)) + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, alpha=0.7) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), 
                position = position_dodge(1), linetype="solid", width=0.2, size=2)+ 
  scale_fill_manual(values=c("blue", "darkgrey"))+
  theme(axis.text.x = element_text(size=35), 
        axis.text.y = element_text(size=35),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=35, hjust=0.5, vjust=1),
        strip.text.x = element_text(size =25),  
        legend.position ="none",
        legend.title=element_blank(),
        legend.text=element_text(size=25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y="Cross-party Classification Agreement\n(Republicans vs. Democrats)", linetype=NULL, x="Group Size") + 
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5))

############################
##Participant Demographics##
############################
network_demo<-unique(networks %>% select(node_id, Party, Race, Ethnicity, Gender))
colnames(network_demo)[1]<-"subjID"
individuals_demo<-unique(individuals %>% select(uniqueID, Party, Race, Ethnicity, Gender))
colnames(individuals_demo)[1]<-"subjID"

all_demo<-rbind(network_demo, individuals_demo)
table(all_demo$Party)/sum(table(all_demo$Party))
table(all_demo$Race)/sum(table(all_demo$Race))
table(all_demo$Ethnicity)/sum(table(all_demo$Ethnicity))
table(all_demo$Gender)/sum(table(all_demo$Gender))


