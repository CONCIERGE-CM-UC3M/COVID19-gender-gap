for (i in unique(df.agg$subcategory)) {
  p <- ggplot(df.agg[subcategory == i]) + ylim(0.4, 1) +
    aes(month, r_male, color=covidpaper, size=total) + geom_point()
  ggsave(paste0("img/", sub("/", "_", i), ".png"), p)
}

ggplot(df.agg) + ylim(0.4, 1) + facet_grid(category~.) +
  aes(month, r_male, color=covidpaper, size=total) + geom_point(position="jitter", alpha=0.2)

df.agg[, month_name := factor(month - year*12, labels=c("jan", "feb", "mar", "apr", "may"))]

km <- kmeans(df.agg$r_male, 2)
df.agg[, masculinized := xor(km$cluster > 1, rep(diff(km$centers) < 0, .N))]
ggplot(df.agg) + aes(r_male, fill=masculinized) + geom_histogram()

ggplot(df.agg) + aes(diss, r_male) + geom_point() + geom_abline() +
  geom_smooth(method=lm, formula=y~splines::bs(x, 3))
ggplot(df.agg) + aes(r_male, diss) + geom_point() +
  geom_smooth(method=lm, formula=y~splines::bs(x, 3), orientation="y")

fit_glmer <- glmer(
  r_male ~ scale(month, FALSE) + covid + covidpaper + (1 | category/subcategory),
  df.agg, family=binomial, weights=total)

model_performance(fit_glmer)
model_assumptions(fit_glmer)

summary(fit_glmer)

sjPlot::plot_model(fit_glmer2, "est") + ylim(0.85, 1.15)
sjPlot::plot_model(fit_glmer2, "re")

cat <- ranef(fit_glmer)$category
subcat <- ranef(fit_glmer)$subcategory
cat.names <- sapply(strsplit(rownames(cat), ":"), head, 1)
subcat.names <- sapply(strsplit(rownames(subcat), ":"), head, 1)
cat <- data.frame(category=cat.names, cat.coef=cat[[1]])
subcat <- data.frame(subcategory=subcat.names, subcat.coef=subcat[[1]])

df.diss2 <- merge(merge(df.diss, subcat), cat, by="category")
ggplot(df.diss2) + aes(diss, cat.coef+subcat.coef) + geom_point() +
  geom_smooth(method=lm, formula=y~log(x))
ggplot(df.diss2) + aes(diss, exp(cat.coef+subcat.coef)) + geom_point() +
  geom_smooth(method=lm)
ggplot(df.diss2) + aes(log(diss), cat.coef+subcat.coef) + geom_point() +
  geom_smooth(method=lm)
