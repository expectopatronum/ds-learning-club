#### correlation matrix + heatmapt 

C<-cor(training_set_teams_2015[,-grep("game_id|division",names(training_set_teams_2015))])
heatmap(C, revC = TRUE, col = heat.colors(256),margins = c(10, 10), Rowv=NA, Colv=NA, scale='none')

#### histograms

# http://stackoverflow.com/questions/1497539/fitting-a-density-curve-to-a-histogram-in-r
pdf("score_perc_per_team.pdf", width=20)
par(mfrow=c(4,8))
for (team in unique(stats_teams_2015$team)) {
  X<-stats_teams_2015[stats_teams_2015$team == team,]$score_perc
  hist(X, main = team, xlab="Score percentage", xlim=c(0,1), seq(0, 1, 0.1), ylim=c(0, 10))  
  lines(density(X))             # add a density estimate with defaults
  lines(density(X, adjust=2), lty="dotted") 
}
dev.off()

pdf("score_perc_per_team_all.pdf", width=20)
par(mfrow=c(4,8))
for (team in unique(stats_teams$team)) {
  X<-stats_teams[stats_teams$team == team,]$score_perc
  hist(X, main = team, xlab="Score percentage", xlim=c(0,1), seq(0, 1, 0.1))  
  lines(density(X))             # add a density estimate with defaults
  lines(density(X, adjust=2), lty="dotted") 
}
dev.off()

#### qqplot

pdf("score_perc_2015_vs_all.pdf", width=14)
par(mfrow=c(4,8))
for (team in unique(stats_teams_2015$team)) {
  X<-stats_teams_2015[stats_teams_2015$team == team,]$score_perc
  X1<-stats_teams[stats_teams$team == team,]$score_perc
  qqnorm(X, ylim=range(X, X1), xlim=range(X, X1), col=rgb(0, 0, 1, 0.5), pch=19, asp=1, main=team)
  qqline(X)
  par(new = T)
  qqnorm(X1, ylim=range(X, X1), xlim=range(X, X1), col=rgb(1, 0, 0, 0.3), pch=19, asp=1, main=team)
  qqline(X1)
}
dev.off()

#### line chart

# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
wins_per_year<-aggregate(winner ~ team + season_year, stats_teams[stats_teams$season_type=="Regular",], sum)

pdf("AFC_trend.pdf", width=14)
ggplot(data=wins_per_year[wins_per_year$team %in% teams[substr(teams$division, 1, 3) == "AFC", "team_id"],], aes(x=season_year, y=winner, group=team, colour=team)) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Year") +
  ylab("Games won") +
  ggtitle("AFC")
dev.off()

pdf("NFC_trend.pdf", width=14)
ggplot(data=wins_per_year[wins_per_year$team %in% teams[substr(teams$division, 1, 3) == "NFC", "team_id"],], aes(x=season_year, y=winner, group=team, colour=team)) +
  geom_line() +
  geom_point(size = 3)+
  xlab("Year") +
  ylab("Games won") +
  ggtitle("NFC")
dev.off()
