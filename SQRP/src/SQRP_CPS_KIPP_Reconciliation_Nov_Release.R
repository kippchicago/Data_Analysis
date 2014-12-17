nwea<-read.csv("../SQRP//data/NWEAreportSchool_2014_growth.csv")


nwea_ranked<-nwea %>% filter(X..Tested>30)%>%
  group_by(Subject, Grade) %>% 
  mutate(Citywide_Rank=rank(-National.School.Growth.Percentile, 
                            ties.method = "min"),
         Citywide_Rank_2=rank(-National.School.Growth.Percentile, 
                            ties.method = "random")
         ) %>% #filter(Citywide_Rank==1, Grade=="All Grades Combined") %>% glimpse
  ungroup %>%
  group_by(Network,Subject, Grade) %>%
  mutate(Network_Rank=rank(-National.School.Growth.Percentile, 
                               ties.method = "min"),
         Network_Rank_2=rank(-National.School.Growth.Percentile, 
                           ties.method = "random")
            ) 


kipp_chi_rnaked <- nwea_ranked %>%
  filter(Network=="Charter", 
         grepl("KIPP",School.Name), 
         Grade=="All Grades Combined") %>%
  select(School.ID, School.Name, Subject, Grade, National.School.Growth.Percentile,
         Citywide_Rank, Network_Rank)            






nwea_ranked_plot<-nwea_ranked %>% filter(Grade=="All Grades Combined") %>%
  mutate(School=ifelse(Network=="Charter", "Charter", "Non-Charter")) %>%
  mutate(School=ifelse(School.ID==400044, "Ascend", School)) %>%
  mutate(School=ifelse(School.ID==400146, "Create", School)) %>%
  mutate(School=ifelse(School.ID==400163, "Bloom", School)) %>%
  mutate(School=factor(School, levels=c("Ascend", "Create", "Bloom", "Non-Charter", "Charter"))) %>%
  mutate(KIPP=ifelse(grepl("KIPP", School.Name), TRUE, FALSE))

# Growth Ranked
ggplot(nwea_ranked_plot, aes(x=National.School.Growth.Percentile, y=-Citywide_Rank_2)) +
  geom_point(aes(color=School), alpha=1) +
  geom_point(data=filter(nwea_ranked_plot, KIPP==TRUE), aes(color=School), size=3) +
  geom_segment(data=filter(nwea_ranked_plot, KIPP==TRUE),
               aes(xend=0, yend=-Citywide_Rank_2, color=School)) +
  scale_color_manual(values=c("#439539", #Ascend
                              "#60A2D7", #Create
                              "purple", #Bloom
                              "#CFCCC1", #Non-charter
                              "#F7941E" #Charter
                              )
                     ) +
  facet_grid(Subject~.) + 
  theme_bw()

#  Adding Attainment data ####

nwea_attain <- read.csv("../SQRP/data/NWEAreportSchool_2014_attainment.csv")

summary(nwea_attain)

# Select 2014 tests and All grades 
nwea_growth_attain<-nwea_attain %>% 
  filter(Test_Year==2014, 
         Grade=="Grades 2-8 Combined",
         N>30) %>%
  inner_join(nwea_ranked_plot, 
             by=c("School.ID", "Subject"))


#Attainment Ranked
nwea_attain_plot<-nwea_attain %>% 
  filter(Test_Year==2014, 
         Grade=="Grades 2-8 Combined",
         N>30) %>%
  group_by(Subject, Grade) %>% 
  mutate(Citywide_Rank=rank(-National.School.Attainment.Percentile, 
                            ties.method = "min"),
         Citywide_Rank_2=rank(-National.School.Attainment.Percentile, 
                              ties.method = "random")
  ) %>% #filter(Citywide_Rank==1, Grade=="All Grades Combined") %>% glimpse
  ungroup %>%
  group_by(Network,Subject, Grade) %>%
  mutate(Network_Rank=rank(-National.School.Attainment.Percentile, 
                           ties.method = "min"),
         Network_Rank_2=rank(-National.School.Attainment.Percentile, 
                             ties.method = "random")
         ) %>%
  mutate(School=ifelse(Network=="Charter", "Charter", "Non-Charter")) %>%
  mutate(School=ifelse(School.ID==400044, "Ascend", School)) %>%
  mutate(School=ifelse(School.ID==400146, "Create", School)) %>%
  mutate(School=ifelse(School.ID==400163, "Bloom", School)) %>%
  mutate(School=factor(School, levels=c("Ascend", "Create", "Bloom", "Non-Charter", "Charter"))) %>%
  mutate(KIPP=ifelse(grepl("KIPP", School.Name), TRUE, FALSE))

ggplot(nwea_attain_plot, aes(x=National.School.Attainment.Percentile, y=-Citywide_Rank_2)) +
  geom_point(aes(color=School), alpha=1) +
  geom_point(data=filter(nwea_attain_plot, KIPP==TRUE), aes(color=School), size=3) +
  geom_segment(data=filter(nwea_attain_plot, KIPP==TRUE),
               aes(xend=0, yend=-Citywide_Rank_2, color=School)) +
  scale_color_manual(values=c("#439539", #Ascend
                              "#60A2D7", #Create
                              "purple", #Bloom
                              "#CFCCC1", #Non-charter
                              "#F7941E" #Charter
  )
  ) +
  facet_grid(Subject~.) + 
  theme_bw()






# attainment vs growth ####
ggplot(nwea_growth_attain, 
       aes(y=National.School.Growth.Percentile, 
           x=National.School.Attainment.Percentile)
       ) +
  geom_hline(aes(yintercept=50), 
             color="gray") +
  geom_vline(aes(xintercept=50), 
             color="gray") +
  geom_point(aes(color=School, 
                 size=N
                 )
             ) +
  geom_smooth(method=lm) +
  geom_point(data=filter(nwea_growth_attain, KIPP==TRUE),
             aes(color=School, 
                 size=N)) +
  geom_point(data=filter(nwea_growth_attain, KIPP==TRUE),
             size=7, 
             shape=21) +
  geom_point(data=filter(nwea_growth_attain, School.Name.x %in% c("PENN", "NASH")),
             size=7, 
             shape=21) +
  geom_text(data=filter(nwea_growth_attain, KIPP==TRUE),
            aes(label=School,
                color=School, 
                y=National.School.Growth.Percentile-4
                )
            ) +
  scale_color_manual(values=c("#439539", #Ascend
                              "#60A2D7", #Create
                              "purple", #Bloom
                              "#CFCCC1", #Non-charter
                              "#F7941E" #Charter
                              )
                     ) +
  facet_grid(Subject~.) +
  theme_bw() + 
  xlab("National School Attainment Percentile, Spring 2014\n(Measurures Overall Performance)") +
  ylab("Natioanl School Growth Percentile, 2013 - 2014\n(Measures Growth Performance)")



nwea_growth_attain %>% mutate(Above_50_Attain=National.School.Attainment.Percentile>=50,
                              Above_50_Growth=National.School.Growth.Percentile>=50
                              ) %>%
  group_by(Grade.y, Subject) %>% 
  dplyr::summarize(N=n(), 
                   Pct_50_Attain=sum(Above_50_Attain)/N,
                   Pct_50_Growth=sum(Above_50_Growth)/N
                   )



# Plot Residuals ####
m1<-nwea_growth_attain %>% lm(National.School.Growth.Percentile ~ National.School.Attainment.Percentile*Subject,
                              data=.)


calc_yhat <-function(x, subject="Reading", model=m1){
  betas<-coef(model)
  if (subject=="Reading"){
   D=1
 } else {
   if (subject=="Mathematics"){
    D=0 
   } else {
     stop("subject must be either 'Reading' or 'Mathematics")
   }
 } 
 yhat <- betas[1]  + betas[2]*x + betas[3]*D + betas[4]*D*x
 yhat
}


nwea_resid<-nwea_growth_attain %>% mutate(yhat=ifelse(Subject=="Reading",
                                          calc_yhat(National.School.Attainment.Percentile, 
                                             subject="Reading"),
                                          calc_yhat(National.School.Attainment.Percentile, 
                                                    subject="Mathematics")
                                          ),
                              Residual=National.School.Growth.Percentile - yhat
                              )  %>% 
  group_by(Subject, Grade.x) %>%
  mutate(Rank_Resid=rank(Residual, ties.method = "random"))

# plot residuals
ggplot(nwea_resid, 
       aes(y=Residual, 
           x=National.School.Attainment.Percentile)
) +
  geom_hline(aes(yintercept=0), 
             color="gray") +
  geom_vline(aes(xintercept=50), 
             color="gray") +
  geom_point(aes(color=School, 
                 size=N
  )
  ) +
  geom_smooth(method=loess) +
  geom_point(data=filter(nwea_resid, KIPP==TRUE),
             aes(color=School, 
                 size=N)) +
  geom_point(data=filter(nwea_resid, KIPP==TRUE),
             size=7, 
             shape=21) +
  geom_text(data=filter(nwea_resid, KIPP==TRUE),
            aes(label=School,
                color=School, 
                y=Residual-4
            )
  ) +
  scale_color_manual(values=c("#439539", #Ascend
                              "#60A2D7", #Create
                              "purple", #Bloom
                              "#CFCCC1", #Non-charter
                              "#F7941E" #Charter
  )
  ) +
  facet_grid(Subject~.) +
  theme_bw() + 
  xlab("National School Attainment Percentile, Spring 2014\n(Measurures Overall Performance)") +
  ylab("Residuals, 2013 - 2014\n(Actual - Predicted Growth Percentile)")


# plot residuals rankked
ggplot(nwea_resid, 
       aes(y=Residual, 
           x=Rank_Resid)
) +
  geom_segment(aes(color=School, 
                   xend=Rank_Resid,
                   yend=0
  )
  ) +
  geom_point(data=filter(nwea_resid, KIPP==TRUE),
             aes(color=School)) +
  geom_text(data=filter(nwea_resid, KIPP==TRUE),
            aes(label=School,
                color=School, 
                y=ifelse(School=="Ascend" & Subject=="Reading", Residual+6,Residual+4),
                x=ifelse(School=="Create" & Subject=="Reading", Rank_Resid-20, Rank_Resid)
            )
            ) +
  geom_hline(aes(yintercept=0)) +`
  scale_color_manual(values=c("#439539", #Ascend
                              "#60A2D7", #Create
                              "purple", #Bloom
                              "#CFCCC1", #Non-charter
                              "#F7941E" #Charter
  )
  ) +
  facet_grid(Subject~.) +
  theme_bw() + 
  xlab("Rank by Residual Value") +
  ylab("Residuals, 2013 - 2014\n(Actual - Predicted Growth Percentile)")




