% -*- root: isamw16.Rnw -*-

\section{Results}

The results are presented in Table 1, and in Figures 4--6. The TERGM estimates are logged odds ratios, and thus can be interpreted in an identical way to logit estimates. Bootstrapped 95\% confidence intervals are in parentheses. Model 1 includes measures of whether the members of each dyad share the same value on various measures. Model 2 disaggregates the political objective variable into specific categories, to examine whether any particular goals drive the relationship. Model 3 replicates Model 1, but excludes the pro-government groups.

Hypothesis 1 predicts that groups with similar political objectives will be more likely to form alliances than other dyads. I am able to reject the null hypothesis in this case, as the ``Same Goals'' coefficient is positive and statistically significant in Models 1 and 3. Furthermore, the substantive effect is large. Alliances are quite rare among groups without similar goals --- a probability of just 0.02 (based on Model 1). Moving from having differing goals to the same goals raises the probability to 0.14. Model 2 suggests that this relationship is driven by Jihadist dyads. Shared goals of a caliphate or Islamist policies do not have a statistically significant relationship with alliance ties, while shared Jihadist goals have positive and significant relationship with alliances.

Hypothesis 2 predicts that more powerful groups will be less likely to form alliances than weaker groups. Instead, Troop Strength has a significant positive relationship with alliance formation in Model 1, and is not related in Models 2 and 3. Even in Model 1, the substantive effect is modest. A one-unit increase in the logged number of troops increase the probability of an alliance by roughly 0.02. I also examined whether similarity in troop strength predicted alliance ties, defining similarity has having a difference in rank of less than or equal to 5 on the troop strength measure. This parameter is not significant in Models 1 and 2, but has a significant negative relationship in Model 3. This suggests that alliances are uncommon among groups that are near parity in size, and are more likely among groups that differ in power.

Hypothesis 3 predicts that groups representing the same social group should be likely to have ties. As there is very little ethnic diversity among the Syrian rebels, I examine the role of shared religious identities. This coefficient is not statistically significant in either Model 1 or Model 3. Thus I fail to reject the null for Hypothesis 3. This is likely a reflection of the fact that there is not a high degree of diversity in the Syrian conflict. Most rebel groups are predominantly composed of Sunni Muslims, and thus religion is perhaps not a particularly salient consideration in alignments. This analysis should be replicated in other cases that feature greater ethnic or religious diversity before this hypothesis is considered to be definitively falsified.

Finally, Hypothesis 4 predicts that groups claiming specific territories in pursuit of secessionist or irredentist goals will be unlikely to form alliances. The ``Both Territorial Aims'' variable in Models 1 and 3 and the ``Both Caliphate'' variable in Model 2 each should capture this dynamic. None of them are statistically significant, however. It would seem that some dyads with territorial aims are able to form alliances, leading to the null result. One interpretation with regards to my theory is simply that more nuance is needed, as some factor allows some dyads to overcome the incentives against cooperation. One of the alliances accounting for this pattern is between former al-Qaeda affiliates IS and the Nusra Front. It should be noted that the two groups have become enemies in recent years, and thus my prediction might hold true on a more limited subset of the data.

<<echo=F, cache=T, message=FALSE, warning=F, results='asis'>>=
library(xergm)
library(texreg)

#creat networkdynamic object
setwd("~/Box Sync/ISAMW16/isamw16_analysis/")
source("syria_dataprep.R")
#
m10 <- btergm(syria ~ edges + triangle + threetrail + twopath + smalldiff('rtroops.low', 5) + nodecov('ltroops.low') + nodematch('main.objective') + nodematch('territory') + nodematch('religion') + nodefactor('polwing'), R = 1000, parallel = 'multicore', ncpus = 3)
#summary(m10)
#plotreg(m10, omit.coef = 'edges')
#g10 <- gof(m10)

m11 <- btergm(syria ~ edges + triangle + threetrail + twopath + smalldiff('rtroops.low', 5) + nodecov('ltroops.low') + nodematch('caliphate') + nodematch('jihadist') + nodematch('islamist') + nodefactor('polwing'), R = 1000, parallel = 'multicore', ncpus = 3)
#summary(m11)
#plotreg(m11, omit.coef = 'edges')
#g11 <- gof(m11)

m12 <- btergm(syria.nonshia ~ edges + triangle + threetrail + twopath + smalldiff('rtroops.low', 5) + nodecov('ltroops.low') + nodematch('main.objective') + nodematch('territory') + nodematch('religion') + nodefactor('polwing'), R = 1000, parallel = 'multicore', ncpus = 3)

labels <- c("Edges","Triads", "3 Trails", "2 Stars", "Troop Similarity", "Troop Strength", "Same Goal", "Both Territorial Aims", "Same Religion", "Political Wing", "Both Caliphate", "Both Jihadist", "Both Islamist")

texreg(list(m10,m11,m12),caption = 'TERGM Models of Alliance Formation', custom.coef.names = labels)
@

For robustness I have included a one-period lag of the network ties. This does not substantially alter the results. In addition, I have explored the robustness of the results to the choice of time periods. Dividing the data into two year blocks (2011-2012, 2013-2014, 2015-2016) again yields similar results.

<<echo=F, message=F, cache=T>>=
texreg::plotreg(m10, omit.coef = 'Edges', custom.model.names = "Model 1", custom.coef.names = c("Edges","Triads", "3 Trails", "2 Stars", "Troop Similarity", "Troop Strength", "Same Goal", "Both Territorial Aims", "Same Religion", "Political Wing"))
@

<<echo=F, message=F, cache=T>>=
texreg::plotreg(m11, omit.coef = 'Edges', custom.model.names = 'Model 2', custom.coef.names = c("Edges","Triads", "3 Trails", "2 Stars", "Troop Similarity", "Troop Strength", "Both Caliphate", "Both Jihadist", "Both Islamist", "Political Wing"))
@

<<echo=F, message=F, cache=T>>=
texreg::plotreg(m12, omit.coef = 'Edges', custom.model.names = 'Model 3', custom.coef.names = c("Edges","Triads", "3 Trails", "2 Stars", "Troop Similarity", "Troop Strength", "Same Goal", "Both Territorial Aims", "Same Religion", "Political Wing"))
@

% <<echo=F, message=F, cache=T>>=
% plot(btergm::gof(m10))
% @
%
% <<echo=F, message=F, cache=T>>=
% plot(btergm::gof(m11))
% @
%
% <<echo=F, message=F, cache=T>>=
% plot(btergm::gof(m12))
% @