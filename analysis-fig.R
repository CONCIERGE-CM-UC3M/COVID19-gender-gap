source(knitr::purl("analysis.Rmd", output=tempfile()))

ggplot(df.plot.papers) +
  aes(yearmon+2017, n) + facet_wrap(~category, scales="free_y") +
  geom_col(data=df.plot.papers.all) +
  geom_smooth(data=df.plot.papers.all) +
  geom_col() + geom_smooth() +
  zoo::scale_x_yearmon(format="%Y") +
  labs(y="Number of papers per month", x=NULL)
ggsave("img/Fig1.eps", width=190.5, height=100, units="mm", scale=1.6, device=cairo_ps)

ggplot(df.plot.p_male) +
  aes(yearmon+2017, p_male) + facet_wrap(~category) +
  geom_point(data=df.plot.p_male.all) +
  geom_smooth(method="gam", data=df.plot.p_male.all) +
  geom_point() + geom_smooth(method="gam") +
  zoo::scale_x_yearmon(format="%Y") +
  expand_limits(y=1) +
  labs(y="Proportion of male authors", x=NULL)
ggsave("img/Fig2.eps", width=190.5, height=100, units="mm", scale=1.6, device=cairo_ps)

ggplot(filter(df.terms, type=="GLMM" | is.na(type))) +
  aes(estimate, term, xmin=conf.low, xmax=conf.high, color=type) +
  facet_grid("fit", scales="free_y", space="free_y") +
  geom_vline(xintercept=1, color="black", linetype="dashed") +
  geom_errorbarh(height=0, size=1) + geom_point(size=2) +
  ggrepel::geom_text_repel(
    aes(label=p.label), nudge_y=0.3, nudge_x=0.01, segment.size=0, show.legend=FALSE) +
  scale_x_log10(breaks=c(0.2, 0.5, 1, 2, 5)) +
  scale_color_manual(values="black", na.value=grDevices::adjustcolor("grey50", 0.5)) +
  labs(y=NULL, x="Odds ratio", color="Model") +
  theme(legend.position="none")
ggsave("img/Fig3.eps", width=132, height=100, units="mm", scale=1.6, device=cairo_ps)

ggplot(df.re.category) +
  aes(estimate, term, xmin=conf.low, xmax=conf.high, color=group) +
  geom_vline(xintercept=1, color="black", linetype="dashed") +
  geom_errorbarh(height=0, size=1) + geom_point(size=2) +
  scale_x_log10(breaks=c(0.2, 0.5, 1, 2, 5)) +
  scale_color_manual(
    breaks=c("pos", "neg"), values=c("blue", "red"),
    na.value=grDevices::adjustcolor("grey50", 0.5)) +
  labs(y=NULL, x="Odds ratio", title="Random effect: category") +
  theme(legend.position="none")
ggsave("img/S1_Fig.pdf", width=132, height=100, units="mm", scale=1.2, device=cairo_ps)

ggplot(df.re.subcategory) +
  aes(estimate, term, xmin=conf.low, xmax=conf.high, color=group) +
  facet_grid("facet", scales="free_y", space="free_y") +
  geom_vline(xintercept=1, color="black", linetype="dashed") +
  geom_errorbarh(height=0, size=1) + geom_point(size=2) +
  scale_x_log10(breaks=c(0.2, 0.5, 1, 2, 5)) +
  scale_color_manual(
    breaks=c("pos", "neg"), values=c("blue", "red"),
    na.value=grDevices::adjustcolor("grey50", 0.5)) +
  labs(y=NULL, x="Odds ratio", title="Random effect: subcategory") +
  theme(legend.position="none")
ggsave("img/S2_Fig.pdf", width=132, height=132*4, units="mm", scale=1.4, device=cairo_ps)
