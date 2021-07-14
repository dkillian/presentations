
# BPPS
# priors

## prep ----

path <- "data/BPPS priors.xlsx"

p <- path %>%
  excel_sheets() %>%
  set_names() %>%
  lapply(function(x) read_xlsx(x, path=path))

p_out <- do.call(rbind, p) %>%
  na.omit() %>%
  arrange(item) %>%
  mutate(percent=alpha / (alpha + beta),
         n=alpha+beta)

?split
byItem <- as.list(split(p_out, "expert"))
byItem

## B1. Aware of USAID ----

### prep ----

set.seed(43)

b1_mn <- svymean(~aware,
        na.rm=T,
        deff="replace",
        design=svydat)

b1_mn
# .802 mean, se .0118

b1_ans <- data.frame(expert="Answer",
                     item="B1",
                     x=rbeta(1e5,
                             estBetaParams(.802, .0118^2)$alpha,
                             estBetaParams(.802, .0118^2)$beta)) %>%
  mutate(diff = abs(x-b1_mn))

head(b1_ans)

b1_jac <- data.frame(expert="Jacob",
                     item="B1",
                     x=rbeta(1e5, 2,8)) %>%
  mutate(diff=abs(x - b1_ans$x))


head(b1_jac)

b1_joe <- data.frame(expert="Joseph",
                     item="B1",
                     x=rbeta(1e5, 40,60)) %>%
  mutate(diff=abs(x - b1_ans$x))

head(b1_joe)


b1_dan <- data.frame(expert="Dan",
                     item="B1",
                     x=rbeta(1e5,80,53)) %>%
  mutate(diff=abs(x - b1_ans$x))


b1_car <- data.frame(expert="Carolyn",
                     item="B1",
                     x=rbeta(1e5,5,22)) %>%
  mutate(diff=abs(x - b1_ans$x))



b1_cam <- data.frame(expert="Camille",
                     item="B1",
                     x=rbeta(1e5,750,250)) %>%
  mutate(diff=abs(x - b1_ans$x))


b1_mel <- data.frame(expert="Melanie",
                     item="B1",
                     x=rbeta(1e5,600,400)) %>%
  mutate(diff=abs(x - b1_ans$x))

b1_all <- do.call(rbind, list(b1_ans, b1_jac, b1_joe, b1_dan, b1_car, b1_cam, b1_mel))

head(b1_all)

b1_sum <- b1_all %>%
  filter(expert!="Answer") %>%
  group_by(expert, item) %>%
  summarize(estimate = round(mean(x),3),
            error=round(mean(diff),3)) %>%
  arrange(error) %>%
  ungroup() %>%
  mutate(rank = 1:6)

b1_sum

### density plot ----

#library(ggpmisc)

pal <- wes_palette("Zissou1", type="discrete")

#b1_dens <-

ggplot(b1_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.802), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.1),
                     labels=percent_format(accuracy=1)) +
  #geom_vline(data=b4_mns, aes(xintercept=percent), size=1) +
  #facet_wrap(~expert) +
  labs(title="B1. Aware of USAID") +
  #scale_color_gradient(colors=pal) +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 80.2 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.75, y=31, label="Camille") +
  annotate("text", x=.19, y=7, label="Carolyn") +
  annotate("text", x=.67, y=8, label="Dan") +
  annotate("text", x=.07, y=5, label="Jacob") +
  annotate("text", x=.4, y=10, label="Joseph") +
  annotate("text", x=.6, y=28, label="Melanie") +
  annotate("text", x=.858, y=36, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(b1_sum[,c(1,3:5)],
                              rows=NULL,
                              theme=ttheme_minimal(base_size=9)),
                    xmin=.1,
                    xmax=.25,
                    ymin=25,
                    ymax=25.5)

ggsave("output/viz/priors/B1 aware dens.png",
       device="png",
       type="cairo",
       height=4,
       width=6)

#b1_dens

### dot rank ----

b1_sum

#b1_dot <-

ggplot(b1_all, aes(x, fct_reorder(expert, -diff))) +
  geom_vline(xintercept=b1_mn, color="darkgoldenrod", size=1, alpha=.4) +
  stat_halfeye(color="dodgerblue2",
               fill="cadetblue1",
               alpha=.7) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0, 1,.1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="B1. Aware of USAID")


ggsave("output/viz/priors/B1 aware dot.png",
       type="cairo",
       device="png",
       height=4,
       width=6)

library(patchwork)

#out <- b1_dens / b1_dot

# getwd()
#
# ggsave(out, "output/viz/priors/B1 dens dot.png",
#        type="cairo",
#        device="png",
#        height=5,
#        width=8)
#
# out2 <- b1_dens + b1_dot
#
# ggsave(out2, "output/viz/priors/B1 dens dot horiz.png",
#        type="cairo",
#        device="png",
#        height=5,
#        width=8)




## B4. Positive perception of USAID ----

b4 <- p_out %>%
  filter(item=="B4")
b4

b4_mn <- svymean(~USAID_perception_bin,
        na.rm=T,
        deff="replace",
        design=svydat)

# .5496 mean, se .0127
b4_mn

b4_ans <- data.frame(expert="Answer",
                     item="B4",
                     x=rbeta(1e5,
                             estBetaParams(.5496, .0127^2)$alpha,
                             estBetaParams(.5496, .0127^2)$beta)) %>%
  mutate(diff = abs(x - b4_mn))

head(b4_ans)

b4_jac <- data.frame(expert="Jacob",
                     item="B4",
                     x=rbeta(1e5, 1,9)) %>%
  mutate(diff=abs(x - b4_ans$x))


head(b4_jac)

b4_joe <- data.frame(expert="Joseph",
                     item="B4",
                     x=rbeta(1e5, 60,40)) %>%
  mutate(diff=abs(x - b4_ans$x))

head(b4_joe)


b4_dan <- data.frame(expert="Dan",
                     item="B4",
                     x=rbeta(1e5,25,6)) %>%
  mutate(diff=abs(x - b4_ans$x))


b4_car <- data.frame(expert="Carolyn",
                     item="B4",
                     x=rbeta(1e5,20,80)) %>%
  mutate(diff=abs(x - b4_ans$x))



b4_cam <- data.frame(expert="Camille",
                     item="B4",
                     x=rbeta(1e5,1995,1505)) %>%
  mutate(diff=abs(x - b4_ans$x))


b4_mel <- data.frame(expert="Melanie",
                     item="B4",
                     x=rbeta(1e5,80,20)) %>%
  mutate(diff=abs(x - b4_ans$x))

b4_all <- do.call(rbind, list(b4_ans, b4_jac, b4_joe, b4_dan, b4_car, b4_cam, b4_mel))

head(b4_all)

b4_sum <- b4_all %>%
  filter(expert!="Answer") %>%
  group_by(expert, item) %>%
  summarize(estimate = mean(x),
            error=round(mean(diff),3)) %>%
  arrange(error) %>%
  ungroup() %>%
  mutate(rank=1:6)

b4_sum
b4

ggplot(b4_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.55), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.1),
                     labels=percent_format(accuracy=1)) +
  labs(title="B4. Positive perception of USAID") +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 55 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.63, y=40, label="Camille") +
  annotate("text", x=.29, y=9, label="Carolyn") +
  annotate("text", x=.8, y=12, label="Melanie") +
  annotate("text", x=.077, y=8, label="Jacob") +
  annotate("text", x=.65, y=10, label="Joseph") +
  annotate("text", x=.94, y=5, label="Dan") +
  annotate("text", x=.48, y=25, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(b4_sum[,c(1,3:5)],
                              rows=NULL,
                              theme=ttheme_minimal(base_size=9,
                                                   digits=3)),
                    xmin=.1,
                    xmax=.25,
                    ymin=35,
                    ymax=35.5)

ggsave("output/viz/priors/B4 perception dens.png",
       device="png",
       type="cairo",
       height=4,
       width=6)


### dot rank ----

b4_sum


ggplot(b4_all, aes(x, fct_reorder(expert, -diff))) +
  geom_vline(xintercept=b4_mn, color="darkgoldenrod", size=1, alpha=.4) +
  stat_halfeye(color="dodgerblue2",
               fill="cadetblue1",
               alpha=.7) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0, 1,.1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="B4. Positive perception of USAID")


ggsave("output/viz/priors/B4 perception dot.png",
       type="cairo",
       device="png",
       height=4,
       width=6)




## B8a Seen logo ----

p_out

b8 <- p_out %>%
  filter(item=="B8a")
b8

b8_mn <- svymean(~B8a_bin,
        na.rm=T,
        deff="replace",
        design=svydat)

# .4941 mean, se .0118
b8_mn

b8_ans <- data.frame(expert="Answer",
                     item="B8",
                     x=rbeta(1e5,
                             estBetaParams(.4941, .0118^2)$alpha,
                             estBetaParams(.4941, .0118^2)$beta)) %>%
  mutate(diff = abs(x - b8_mn))

head(b8_ans)

b8_jac <- data.frame(expert="Jacob",
                     item="B8",
                     x=rbeta(1e5, 2,8)) %>%
  mutate(diff=abs(x - b4_ans$x))


b8_joe <- data.frame(expert="Joseph",
                     item="B8",
                     x=rbeta(1e5, 20,80)) %>%
  mutate(diff=abs(x - b8_ans$x))


b8_dan <- data.frame(expert="Dan",
                     item="B8",
                     x=rbeta(1e5,80,80)) %>%
  mutate(diff=abs(x - b8_ans$x))


b8_car <- data.frame(expert="Carolyn",
                     item="B8",
                     x=rbeta(1e5,5,85)) %>%
  mutate(diff=abs(x - b8_ans$x))


b8_cam <- data.frame(expert="Camille",
                     item="B8",
                     x=rbeta(1e5,4800,1220)) %>%
  mutate(diff=abs(x - b8_ans$x))


b8_mel <- data.frame(expert="Melanie",
                     item="B8",
                     x=rbeta(1e5,70,30)) %>%
  mutate(diff=abs(x - b8_ans$x))

b8_all <- do.call(rbind, list(b8_ans, b8_jac, b8_joe, b8_dan, b8_car, b8_cam, b8_mel))

head(b8_all)

b8_sum <- b8_all %>%
  filter(expert!="Answer") %>%
  group_by(expert, item) %>%
  summarize(estimate=mean(x),
            error=round(mean(diff),3)) %>%
  arrange(error) %>%
  ungroup() %>%
  mutate(rank = 1:6)

b8_sum
b8

ggplot(b8_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.494), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.1),
                     labels=percent_format(accuracy=1)) +
  labs(title="B8. Seen USAID logo") +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 49 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.86, y=90, label="Camille") +
  annotate("text", x=.056, y=25, label="Carolyn") +
  annotate("text", x=.7, y=14, label="Melanie") +
  annotate("text", x=.33, y=6, label="Jacob") +
  annotate("text", x=.2, y=15, label="Joseph") +
  annotate("text", x=.556, y=10, label="Dan") +
  annotate("text", x=.55, y=38, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(b8_sum[,c(1,3:5)],
                              rows=NULL,
                              theme=ttheme_minimal(base_size=9,
                                                   digits=3)),
                    xmin=.1,
                    xmax=.25,
                    ymin=85,
                    ymax=85.5)

ggsave("output/viz/priors/B8 seen USAID logo dens.png",
       device="png",
       type="cairo",
       height=4,
       width=6)


### dot rank ----

b8_sum


ggplot(b8_all, aes(x, fct_reorder(expert, -diff))) +
  geom_vline(xintercept=b8_mn, color="darkgoldenrod", size=1, alpha=.4) +
  stat_halfeye(color="dodgerblue2",
               fill="cadetblue1",
               alpha=.7) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0, 1,.1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="B8. Seen USAID logo")


ggsave("output/viz/priors/B8 seen USAID logo dot.png",
       type="cairo",
       device="png",
       height=4,
       width=6)





## C12. Use social media ----

c12 <- p_out %>%
  filter(item=="C12")
c12

dat <- dat %>%
  mutate(C12_prior = case_when(C12==97 ~ 0,
                               C12==4 ~ 1,
                               C12==5 ~ 1,
                               C12==6 ~ 1,
                               C12<4 ~ 0,
                               TRUE ~ 0))

frq(dat$C12_prior)


svydat <- svydesign(data = dat,
                    ids= ~PSU + A9,
                    strata = ~strata,
                    weights = ~Wgt1)


c12_mn <- svymean(~C12_prior,
        na.rm=T,
        design=svydat,
        digits=4)

c12_mn
# .141 se .01


c12_ans <- data.frame(expert="Answer",
                      item="C12",
                      x=rbeta(1e5,
                              estBetaParams(.141, .0088^2)$alpha,
                              estBetaParams(.141, .0088^2)$beta)) %>%
  mutate(diff=abs(x - c12_mn))

head(c12_ans)

c12_jac <- data.frame(expert="Jacob",
                     item="C12",
                     x=rbeta(1e5, 60,40)) %>%
  mutate(diff=abs(x - c12_ans$x))


c12_joe <- data.frame(expert="Joseph",
                     item="C12",
                     x=rbeta(1e5, 55,45)) %>%
  mutate(diff=abs(x - c12_ans$x))


c12_dan <- data.frame(expert="Dan",
                     item="C12",
                     x=rbeta(1e5,20,80)) %>%
  mutate(diff=abs(x - c12_ans$x))


c12_car <- data.frame(expert="Carolyn",
                     item="C12",
                     x=rbeta(1e5,10,85)) %>%
  mutate(diff=abs(x - c12_ans$x))



c12_cam <- data.frame(expert="Camille",
                     item="C12",
                     x=rbeta(1e5,185,315)) %>%
  mutate(diff=abs(x - c12_ans$x))


c12_mel <- data.frame(expert="Melanie",
                     item="C12",
                     x=rbeta(1e5,8500,1500)) %>%
  mutate(diff=abs(x - c12_ans$x))

c12_all <- do.call(rbind, list(c12_ans, c12_jac, c12_joe, c12_dan, c12_car, c12_cam, c12_mel))

head(c12_all)

c12_sum <- c12_all %>%
  filter(expert!="Answer") %>%
  group_by(expert, item) %>%
  summarize(estimate = mean(x),
            error=round(mean(diff),3)) %>%
  arrange(error) %>%
  ungroup() %>%
  mutate(rank = 1:6)

c12_sum

ggplot(c12_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=c12_mn), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.1),
                     labels=percent_format(accuracy=1)) +
  labs(title="C12. Use social media") +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 14 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.37, y=30, label="Camille") +
  annotate("text", x=.07, y=25, label="Carolyn") +
  annotate("text", x=.85, y=180, label="Melanie") +
  annotate("text", x=.66, y=20, label="Jacob") +
  annotate("text", x=.51, y=20, label="Joseph") +
  annotate("text", x=.25, y=22, label="Dan") +
  annotate("text", x=.141, y=57, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(c12_sum[,c(1,3:5)],
                              rows=NULL,
                              theme=ttheme_minimal(base_size=9,
                                                   digits=3)),
                    xmin=.3,
                    xmax=.6,
                    ymin=170,
                    ymax=170.5)

ggsave("output/viz/priors/C12 Use social media dens.png",
       device="png",
       type="cairo",
       height=4,
       width=6)


### dot rank ----

ggplot(c12_all, aes(x, fct_reorder(expert, -diff))) +
  geom_vline(xintercept=c12_mn, color="darkgoldenrod", size=1, alpha=.4) +
  stat_halfeye(color="dodgerblue2",
               fill="cadetblue1") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="C12. Use social media")

ggsave("output/viz/priors/C12 use social media dot.png",
       type="cairo",
       device="png",
       height=4,
       width=6)

library(bayesplot)

mcmc_areas(c12_all,
           pars=expert)

## all items ----

b1_sum

all_sum <- do.call(rbind, list(b1_sum, b4_sum, b8_sum, c12_sum))
all_sum

rank_wide <- all_sum %>%
  select(1,2,5) %>%
  pivot_wider(names_from = "item",
              values_from = "rank")

rank_wide

total <- all_sum %>%
  group_by(expert) %>%
  summarize(se = std.error(error),
            margin = 1.96*mean(se),
            error=mean(error),
            rank_sum=sum(rank),
            rank_ave = round(mean(rank), 4)) %>%
  arrange(error) %>%
  mutate(minave_rank = rank(rank_ave,
                            ties.method="min"),
         minsum_rank = rank(rank_sum,
                         ties.method="min"),
         error_rank=rank(error)) %>%
  select(1,6,7,5,8,4,9)

total

?rank

out <- total %>%
  right_join(rank_wide) %>%
  select(1, 8:11, everything())

out

write_csv(out, "output/tables/priors/BPPS priors final league table.csv")

out_gt <- out %>%
  gt()   %>%
  tab_style(
    style = list(
      cell_fill(color = "darkgoldenrod2", alpha=.4)),
    locations = cells_body(columns = c(minave_rank, minsum_rank, error_rank))) %>%
  tab_header(title="Final league table",
             subtitle="BPPS priors") %>%
  cols_label(expert="Expert",
             rank_ave="Rank\naverage",
             minave_rank = "Rank of average",
             rank_sum = "Rank sum",
             minsum_rank = "Rank of sum",
             error="Error",
             error_rank = "Error rank") %>%
  cols_width(everything() ~ px(70)) %>%
  cols_align(align="center",
             columns=c(7, 9,11))

out_gt

gtsave(out_gt, "output/tables/priors/BPPS priors final league table.rtf")

?cols_align

tab_1 <-
  exibble %>%
  dplyr::select(num, currency) %>%
  gt() %>%
  fmt_number(
    columns = c(num, currency),
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = num,
      rows = num >= 5000
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F9E3D6"),
      cell_text(style = "italic")
    ),
    locations = cells_body(
      columns = currency,
      rows = currency < 100
    )
  )

tab_1

tab_2 <-
  sp500 %>%
  dplyr::filter(
    date >= "2015-12-01" &
      date <= "2015-12-15"
  ) %>%
  dplyr::select(-c(adj_close, volume)) %>%
  gt() %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = close > open)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = open > close)
  )

tab_2

tab_3 <-
  exibble %>%
  dplyr::select(char, fctr) %>%
  gt() %>%
  fmt_missing(everything()) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      "font-variant: small-caps;"
    ),
    locations = cells_body(columns = char)
  )

tab_3


# misc ----



b4

ggplot(b4_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.55), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1)) +
  #geom_vline(data=b4_mns, aes(xintercept=percent), size=1) +
  #facet_wrap(~expert) +
  labs(title="B4. Positive perception of USAID") +
  #scale_color_gradient(colors=pal) +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 55 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.62, y=42, label="Camille") +
  annotate("text", x=.20, y=4.7, label="Carolyn") +
  annotate("text", x=.9, y=6, label="Dan") +
  annotate("text", x=.04, y=3, label="Jacob") +
  annotate("text", x=.65, y=10, label="Joseph") +
  annotate("text", x=.8, y=12, label="Melanie") +
  annotate("text", x=.5, y=32, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(b4_sum[,c(1,3,4)],
                              rows=NULL,
                              theme=ttheme_minimal()),
                    xmin=.05,
                    xmax=.3,
                    ymin=35,
                    ymax=36)

ggsave("output/viz/priors/B4 perception with answer and ranking.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



b8
b8_sum

ggplot(b8_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.494), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1)) +
  #geom_vline(data=b4_mns, aes(xintercept=percent), size=1) +
  #facet_wrap(~expert) +
  labs(title="B8. Seen USAID logo") +
  #scale_color_gradient(colors=pal) +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 49 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.797, y=70, label="Camille") +
  annotate("text", x=.06, y=24, label="Carolyn") +
  annotate("text", x=.55, y=12, label="Dan") +
  annotate("text", x=.18, y=3, label="Jacob") +
  annotate("text", x=.22, y=15, label="Joseph") +
  annotate("text", x=.7, y=13, label="Melanie") +
  annotate("text", x=.49, y=40, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(b8_sum[,c(1,3,4)],
                              rows=NULL,
                              theme=ttheme_minimal()),
                    xmin=.05,
                    xmax=.3,
                    ymin=85,
                    ymax=86)

ggsave("output/viz/priors/B8 seen USAID logo with answer and ranking.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

c12
c12_sum

ggplot(c12_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.141), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1)) +
  #geom_vline(data=b4_mns, aes(xintercept=percent), size=1) +
  #facet_wrap(~expert) +
  labs(title="C12. Use social media") +
  #scale_color_gradient(colors=pal) +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       caption="Correct answer 14 percent") +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=.797, y=70, label="Camille") +
  annotate("text", x=.06, y=24, label="Carolyn") +
  annotate("text", x=.55, y=12, label="Dan") +
  annotate("text", x=.18, y=3, label="Jacob") +
  annotate("text", x=.22, y=15, label="Joseph") +
  annotate("text", x=.7, y=13, label="Melanie") +
  annotate("text", x=.49, y=40, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none") +
  annotation_custom(tableGrob(c12_sum[,c(1,3,4)],
                              rows=NULL,
                              theme=ttheme_minimal()),
                    xmin=.05,
                    xmax=.3,
                    ymin=85,
                    ymax=86)

ggsave("output/viz/priors/B8 seen USAID logo with answer and ranking.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
