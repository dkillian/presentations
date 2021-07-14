# BPPS priors

set.seed(5345)

#p <- read_excel("data/BPPS priors.xlsx")

#p

| ![]("output/viz/MSI logo.png")



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

p_out
str(p_out)



b1 <- p_out %>%
  filter(item=="B1")

b1

weighted.mean(b1$percent, b1$n)

lapply(b1[,4:5], mean)

31.8/(31.8+35.8)

svymean(~aware,
        na.rm=T,
        deff="replace",
        design=svydat)

# .802 mean, se .0118

f <- estBetaParams(.802, .0118^2)
f

b1_ov <- data.frame(item="B1",
                    name="Are you aware of USAID",
                    alpha=f$alpha,
                    beta=f$beta) %>%
  mutate(percent = alpha / (alpha + beta),
         n = alpha + beta)

b1_ov

b1_out <- lapply(function(x))

?svymean
svymean(~aware,
        na.rm=T,
        deff="replace",
        design=svydat)

?svytable
svytable(~aware,
         Ntotal=T,
         #na.rm=T,
         design=svydat)

describe(dat$aware)

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

o <- estBetaParams(.802, .36^2)
o

f <- estBetaParams(.802, .0118^2)
f
f$alpha

estBetaParams(.802, .0118^2)$beta

a <- 914
b <- 226

a / (a+b)

r <- rbeta(1e6, 914, 226)
plot(density(r))

describe(dat$aware)
.36*6.18

.0118*sqrt(7111) # .995
.995^2

## B1. Aware of USAID ----

set.seed(43)
b1_ans <- data.frame(expert="Answer",
                     item="B1",
                     x=rbeta(1e4,
                             estBetaParams(.802, .0118^2)$alpha,
                             estBetaParams(.802, .0118^2)$beta))
head(b1_ans)

ggplot(data.frame(b1_ans), aes(x)) +
  stat_density(geom="line", color="dodgerblue2", size=1.2) +
  labs(caption="")

b1_jac <- data.frame(expert="Jacob",
                     item="B1",
                     x=rbeta(1e4, 2,8),
                     answer = b1_ans$x) %>%
  mutate(diff=abs(x - answer))
,
         p = x,
         q = answer,
         ln_p=log(p),
         ln_q=log(q),
         dev=p*(ln_p-ln_q))

head(b1_jac)


b1_temp <- b1_all %>%
  group_by(expert) %>%
  summarize(deviance = -sum(dev)) %>%
  ungroup() %>%
  mutate(rank=rank(deviance))



plot(density(b1_jac))

ggplot(data.frame(b1_joe), aes(x)) +
  stat_density(geom="line", color="dodgerblue2", size=1.2) +
  labs(caption="")

base_style()

b1

b1_joe <- data.frame(expert="Joseph",
                     item="B1",
                     x=rbeta(1e4, 40,60),
                     answer=b1_ans$x) %>%
  mutate(diff=abs(x - answer))

,
         p = b1_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

head(b1_joe)


b1_dan <- data.frame(expert="Dan",
                     item="B1",
                     x=rbeta(1e4,80,53),
                     answer=b1_ans$x) %>%
  mutate(diff=x - b1_ans$x)

,
         p = b1_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p),
         dev2 = calc.deviance(q, p))
head(b1_dan)
describe(b1_dan$dev)

b1_car <- data.frame(expert="Carolyn",
                     item="B1",
                     x=rbeta(1e4,5,22),
                     answer=b1_ans$x) %>%
  mutate(diff=x - b1_ans$x)

,
         p = b1_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))


b1_cam <- data.frame(expert="Camille",
                     item="B1",
                     x=rbeta(1e4,750,250),
                     answer=b1_ans$x) %>%
  mutate(diff=x - b1_ans$x)

,
         p = b1_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b1_mel <- data.frame(expert="Melanie",
                     item="B1",
                     x=rbeta(1e4,600,400),
                     answer=b1_ans$x) %>%
  mutate(diff=x - b1_ans$x)

,
         p = b1_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))


## B4. Positive perception of USAID ----

b4 <- p_out %>%
  filter(item=="B4")

b4

describe(dat$USAID_perception_bin)

svymean(~USAID_perception_bin,
        na.rm=T,
        design=svydat)

svyrdat %>%
  group_by(USAID_perception_bin) %>%
  summarize(B4 = survey_mean())

b4

b4_ans <- data.frame(expert="Answer", item="B4", x=rnorm(1e4, .549, .0127))
head(b4_ans)

b4_jac <- data.frame(expert="Jacob", item="B4", x=rbeta(1e4, 1,9)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))
b4_jac


b4_joe <- data.frame(expert="Joseph", item="B4", x=rbeta(1e4, 60,40)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))
b4_joe


b4_dan <- data.frame(expert="Dan", item="B4", x=rbeta(1e4,25,6)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))


b4_car <- data.frame(expert="Carolyn", item="B4", x=rbeta(1e4, 20,80)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b4_cam <- data.frame(expert="Camille", item="B4", x=rbeta(1e4, 1995,1505)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b4_mel <- data.frame(expert="Melanie", item="B4", x=rbeta(1e4, 80,20)) %>%
  mutate(diff=x - b4_ans$x,
         p = b4_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

p_out



describe(dat$USAID_perception_bin)

## B8a Seen USAID logo ----

svymean(~B8a_bin,
        na.rm=T,
        design=svydat)

svyrdat %>%
  group_by(B8a_ov) %>%
  summarize(B4 = survey_mean())


b8_ans <- data.frame(expert="Answer", item="B8", x=rnorm(1e4, .494, .0118))
head(b4_ans)

b8 <- p_out %>%
  filter(item=="B8a")

b8

b8_jac <- data.frame(expert="Jacob", item="B8a", x=rbeta(1e4, 2,8)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b8_joe <- data.frame(expert="Joseph", item="B8a", x=rbeta(1e4, 20,80)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b8_dan <- data.frame(expert="Dan", item="B8a", x=rbeta(1e4, 80,80)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b8_car <- data.frame(expert="Carolyn", item="B8a", x=rbeta(1e4,5,85)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b8_cam <- data.frame(expert="Camille", item="B8a", x=rbeta(1e4, 4800,1220)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

b8_mel <- data.frame(expert="Melanie", item="B8a", x=rbeta(1e4, 70,30)) %>%
  mutate(diff=x - b8_ans$x,
         p = b8_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

## C12 Use social media ----

p_out


frq(dat$C12)
describe(dat$C12)

dat <- dat %>%
  mutate(C12_prior = case_when(C12==97 ~ 0,
                               C12==4 ~ 1,
                               C12==5 ~ 1,
                               C12==6 ~ 1,
                               C12<4 ~ 0,
                               TRUE ~ 0))

frq(dat$C12_prior)

svymean(~C12_prior,
        na.rm=T,
        design=svydat)

svyrdat %>%
  group_by(USAID_perception_bin) %>%
  summarize(B4 = survey_mean())


c12_ans <- data.frame(expert="Answer", item="C12", x=rnorm(1e4, .141, .0088))
head(c12_ans)

c12 <- p_out %>%
  filter(item=="C12")

c12


c12_jac <- data.frame(expert="Jacob", item="C12", x=rbeta(1e4,60,40)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

c12_joe <- data.frame(expert="Joseph", item="C12", x=rbeta(1e4, 55,45)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

c12_dan <- data.frame(expert="Dan",item="C12", x=rbeta(1e4,20,80)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

c12_car <- data.frame(expert="Carolyn", item="C12", x=rbeta(1e4, 10,85)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

c12_cam <- data.frame(expert="Camille", item="C12", x=rbeta(1e4, 185,315)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

c12_mel <- data.frame(expert="Melanie", item="C12", x=rbeta(1e4, 8500,1500)) %>%
  mutate(diff=x - c12_ans$x,
         p = c12_ans$x,
         q=x,
         ln_q=log(q),
         ln_p=log(p),
         dev=p*(ln_q-ln_p))

## All items ----

out <- do.call(rbind, list(b1_jac, b1_joe, b1_dan, b1_car, b1_cam, b1_mel,
                           b4_jac, b4_joe, b4_dan, b4_car, b4_cam, b4_mel,
                           b8_jac, b8_joe, b8_dan, b8_car, b8_cam, b8_mel,
                           c12_jac, c12_joe, c12_dan, c12_car, c12_cam, c12_mel)) %>%
  mutate(dif = abs(diff),
         item_name = case_when(item=="B1" ~ "Aware of USAID",
                               item=="B4" ~ "Perception of USAID",
                               item=="B8a" ~ "Recognized USAID logo",
                               item=="C12" ~ "Uses social media"))

head(out)

frq(out$expert)
frq(out$item)

out2 <- out %>%
  group_by(expert, item_name) %>%
  summarize(diff_se = std.error(dif),
            dif=round(mean(dif),4),
            deviance=-sum(dev)) %>%
  ungroup() %>%
  arrange(item_name, dif) %>%
  group_by(item_name) %>%
  mutate(item_rank =rank(dif),
         dev_rank = rank(deviance)) %>%
  arrange(item_name)

str(out2)

out3 <- out2 %>%
  group_by(expert) %>%
  summarize(se_dif = std.error(dif),
            mean_dif = mean(dif)*100,
            mean_dif2 = mean(dif)) %>%
  mutate(rank=rank(mean_dif),
         lower = mean_dif2 - 1.96*se_dif,
         upper = mean_dif2 + 1.96*se_dif) %>%
  arrange(rank) %>%
  rename(contributor=expert)

out3

ggplot(out3, aes(mean_dif2, fct_reorder(contributor, -mean_dif2))) +
  geom_vline(xintercept=0, size=1, color="firebrick3", alpha=.6) +
  geom_point(color="dodgerblue", size=3.5) +
  geom_linerange(aes(xmin=lower, xmax=upper), height=0, color="dodgerblue", size=1) +
  geom_label(aes(label=paste(round(mean_dif,1), "%", sep="")), color="dodgerblue") +
  labs(x="Average error across four BPPS survey items",
       y="",
       title="Aggregate error of prior estimates of BPPS") +
  scale_x_continuous(labels=percent_format(accuracy=1),
                     breaks=seq(0,.6,.1))


ggsave("output/viz/priors/aggregate error.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

ggplot(out, aes(x)) +
  stat_density(geom="line") +
  facet_style() +
  facet_wrap(~expert)

outL <- split(out, out$item)

outL[1]

b1_all <- data.frame(outL[1])
b4_all <- data.frame(outL[2])
b8_all <- data.frame(outL[3])
c12_all <- data.frame(outL[4])

b1_all <- do.call(rbind, list(b1_jac, b1_joe, b1_dan, b1_car, b1_cam, b1_mel))

%>%
  rename(q=x)

b4_all <- do.call(rbind, list(b4_jac, b4_joe, b4_dan, b4_car, b4_cam, b4_mel))
b8_all <- do.call(rbind, list(b8_jac, b8_joe, b8_dan, b8_car, b8_cam, b8_mel))
c12_all <- do.call(rbind, list(c12_jac, c12_joe, c12_dan, c12_car, b1_cam, b8_mel))

head(b1_all)
frq(b1_all$expert)

?svymean
aware_mn <- svymean(~aware,
                    na.rm=T,
                    design=svydat,
                    deff="replace") %>%
  as.data.frame()

aware_mn
.0118^2

?svyvar
svyvar(~aware,
       na.rm=T,
       design=svydat)
sqrt(.159)

aware_mn <- svyrdat %>%
  summarise(aware=survey_mean(aware, na.rm=T),
            std.dev=sd(aware))

set.seed(0987)

b1_all <- b1_all %>%
  mutate(p = rnorm(6000, .802, .399),
         p = case_when(p<0 ~ .001,
                       p>1 ~ 1,
                       TRUE ~ p),
         ln_q = log(q),
         ln_p = log(p),
         dev=p*(ln_q-ln_p))

head(b1_all)

b1_temp <- b1_all %>%
  group_by(expert) %>%
  summarize(deviance = -sum(dev)) %>%
  ungroup() %>%
  mutate(rank=rank(deviance))

b1_summary <- b1[,c(1,4:7)] %>%
  left_join(b1_temp) %>%
  arrange(rank)

b1_summary

b1_sum_gt <- b1_summary %>%
  gt() %>%
  fmt_percent(4, decimals=1) %>%
  fmt_number(c(5,6), decimals=0)

b1_sum_gt

gtsave(b1_sum_gt, "tables/priors/league table for B1. Aware of USAID.rtf")

b1_all$p
log(b1_all$p)
log(1)
log(.00001)
log(.001)
log(-.23929)
log(-1)

library(rethinking)
?lppd

unlist(a[1,4])

score <- a$deviance - unlist(a[1,4])
score

?rank  \
rank(a$deviance)


b1_mns <- p_out[1:6,c(1,6)]
b1_mns

head(b1_all)

b1_all %>%
  group_by(expert) %>%
  summarise(ave = mean(b1))

ggplot(b1_all, aes(x)) +
  stat_density(geom="line", size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.2),
                     labels=percent_format(accuracy=1)) +
  geom_vline(data=b1_mns, aes(xintercept=percent), size=1, color="darkgoldenrod") +
  facet_wrap(~expert) +
  facet_style() +
  labs(title="B1. Are you aware of USAID?")
+
  stat_density(aes(x=rnorm(1000, .8, .56)), geom="line", color="maroon", alpha=.5)

?geom_vline

ex <- data.frame(x=rnorm(1000, .802, .559))
ex

ggplot(dat) %>%
  geom_density(aes(aware))

describe(dat$aware)


head(b1_all)

head(b1_ans)

b1_with_ans <- b1_ans %>%
  rename(q=x) %>%
  mutate(diff=NA) %>%
  rbind(b1_all)

head(b1_with_ans)

mean(b1_ans$x)

frq(b1_with_ans$expert)

library(ggpmisc)
library(wesanderson)
?wes_palette
pal <- wes_palette("Zissou1", type="discrete")

ggplot(b1_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_vline(aes(xintercept=.802), size=1, color="darkblue", alpha=.6) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,.9),
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
  annotate("text", x=.802, y=36, label="Answer") +
  theme(legend.title=element_blank(),
        legend.position="none")

+
  annotation_custom(tableGrob(out3, rows=NULL),
                    xmin=-.05,
                    xmax=.25,
                    ymin=27,
                    ymax=30)

ggsave("output/viz/priors/B1 aware with answer and ranking.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

.8*.2
(.8*.2) / sqrt(7000)

describe(dat$aware)
.85*.15

b4_mns <- p_out[5:8, c(1,6)]
b4_mns

ggplot(b4_all, aes(x)) +
  stat_density(geom="line", size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.2),
                     labels=percent_format(accuracy=1)) +
  geom_vline(data=b4_mns, aes(xintercept=percent), size=1, color="darkgoldenrod") +
  facet_wrap(~expert) +
  facet_style() +
  labs(title="B4. Positive impression of USAID")




b8_mns <- p_out[9:12, c(1,6)]
b8_mns

ggplot(b8_all, aes(x)) +
  stat_density(geom="line", size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.2),
                     labels=percent_format(accuracy=1)) +
  geom_vline(data=b8_mns, aes(xintercept=percent), size=1, color="darkgoldenrod") +
  facet_wrap(~expert) +
  facet_style() +
  labs(title="B8. Seen USAID logo")




b4_mns <- p_out[5:8, c(1,6)]
b4_mns

ggplot(b4_all, aes(x)) +
  stat_density(geom="line", size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.2),
                     labels=percent_format(accuracy=1)) +
  geom_vline(data=b4_mns, aes(xintercept=percent), size=1, color="darkgoldenrod") +
  facet_wrap(~expert) +
  facet_style() +
  labs(title="B4. Positive impression of USAID")


ggplot(b4_all, aes(x, color=expert, fill=expert, group=expert)) +
  geom_density() +
  #stat_density(geom="line", size=1) +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(.1,.9,.2),
                     labels=percent_format(accuracy=1)) +
  #geom_vline(data=b4_mns, aes(xintercept=percent), size=1) +
  #facet_wrap(~expert) +
  base_style() +
  labs(title="B4. Positive impression of USAID") +
  scale_fill_viridis_d(alpha=.3) +
  scale_color_viridis_d()

scale_color_viridis_d()


mu <- .802
v <- .556^2

a1 <- (1-mu) / v
a2 <- a1 - (1/mu)

a <- a2 * mu^2

- (1/mu) ) *mu^2
a

?rbeta

library(modeltools)
?modeltools::estBetaParam

estBetaParam

devtools::install_github("austinnam/modeltools")





