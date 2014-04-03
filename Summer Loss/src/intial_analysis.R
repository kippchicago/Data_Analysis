require(ProjectTemplate)
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/Summer Loss")


# Summary Tables for math and reading ####
k.dt[!is.na(summer_loss)][
  order(Subtest.Name,-summer_loss)][
    ,list(Subtest.Name, 
          School, 
          KIPP.Region, 
          Grade,
          S2012_Avg.RIT.Score, 
          F2012_Avg.RIT.Score, 
          summer_loss)][Subtest.Name=="MAP.Mathematics"][1:20]


k.dt[!is.na(summer_loss)][
  order(Subtest.Name,-summer_loss)][
    ,list(Subtest.Name, 
          School, 
          KIPP.Region, 
          Grade,
          S2012_Avg.RIT.Score, 
          F2012_Avg.RIT.Score, 
          summer_loss)][Subtest.Name=="MAP.Reading"][1:20]


# plot Summer loss vs spring to spring gains ####


k.plot<-k.dt[Subtest.Name %in% c("MAP.Reading", "MAP.Mathematics") & 
               Grade %in% c(1,2,3,6,7,8)]



p <- ggplot(k.plot, 
            aes(x=summer_loss, y=s2s_diff)) +
  geom_vline(aes(xinctercept=0),
             color="lightgray")+
  geom_hline(aes(yinctercept=0),
             color="lightgray")+
  geom_point() + 
  geom_point(data=k.plot[rank_loss<=5],  
             color="hotpink") +
  geom_text(data=k.plot[rank_loss<=5], aes(label=School), 
            size=2, vjust=-1) +
  stat_smooth(method=lm) +
  #            geom_text(aes(label=School), hjust=0) +
  facet_grid(Subtest.Name ~ Grade) +  
  theme_bw()

p

# Rank bar graphs ####
p.loss.reading.rank <- ggplot(k.plot, 
            aes(x=summer_loss, y= -rank_loss)) +
            geom_vline(aes(xinterpect=0), color="gray") +
            geom_point()+
            geom_point(data=k.plot[rank_loss<=5], color="hotpink") +
            geom_point(data=k.plot[KIPP.Region=="KIPP Chicago"], color="forestgreen") +
            geom_text(data=k.plot[rank_loss<=5], aes(x= summer_loss - 1, 
                                                     label=School), 
                      size=2, hjust=1) +
            geom_text(data=k.plot[KIPP.Region=="KIPP Chicago"], aes(x= summer_loss + 1, 
                                           label=School), 
                      size=2, hjust=0, color="forestgreen") +
            facet_grid(Subtest.Name ~ Grade) + 
            theme_bw()

p.loss.reading.rank
