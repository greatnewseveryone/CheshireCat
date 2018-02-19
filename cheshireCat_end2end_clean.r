
library(syuzhet)
library(data.table)
library(quantmod)


get_quantiles= function(series){
  sapply(series, function(i) sum(series < i)/length(series))
}

get_peaks= function(apsq){
  inds= findPeaks(apsq)-1
  peak_flags= rep(0, length(apsq))
  peak_flags[inds]= 1
  peak_flags
}

median_scale= function(y){
  (y-median(y))/diff(range(y))
}

plot_ind= function(i, dat_all){
  dat_all[ind==i, plot(scaled_sentiment_quantiles~timeline, 
                                     type='l', lwd= 2, col='purple',
                                     xlab= 'Narrative Time',
                                     ylab= 'Emotional Valence',
                                     main= unique(title))]
  dat_all[ind==i & peaks, points(scaled_sentiment_quantiles~timeline, pch=20, lwd= 6, col='darkgreen')]
  dat_all[ind==i & troughs, points(scaled_sentiment_quantiles~timeline, pch=20, lwd= 6, col='orange')]
  dat_all[ind==i & cheshire_cat_ind, points(scaled_sentiment_quantiles~timeline, pch=6, lwd=6, col='blue')]
  abline(h=0, lty=2)
  legend('top', bty= 'n', 
         legend = c('detected peaks', 'detected troughs', 'Cheshire cat'), 
         fill = c('darkgreen', 'orange', 'blue'))
  
}


dat= fread('./data/cheshire_ind.csv')

dat_all= c()
for (ind in dat$ind){
  text= get_text_as_string(paste0("./data/texts/",ind,".txt"))
  s_v= get_sentences(text)
  s_v_sentiment= get_sentiment(s_v)
  smoothed= lowess(s_v_sentiment, f = .25)
  smoothed$x_scaled= smoothed$x/length(smoothed$x)
  scaled_sentiment_quantiles= median_scale(get_quantiles(smoothed$y))
  
  dat_ind= data.table(ind= ind,
                      title= dat$title[dat$ind==ind],
                      sentences= s_v, 
                      sentiment= s_v_sentiment, 
                      sent_ind= 1:length(s_v),
                      timeline= (1:length(s_v))/length(s_v),
                      scaled_sentiment_quantiles= scaled_sentiment_quantiles,
                      peaks= get_peaks(scaled_sentiment_quantiles),
                      troughs= get_peaks(-scaled_sentiment_quantiles),
                      cat_sighting= as.numeric(sapply(s_v, function(s) s %like% "Cheshire cat" | s %like% " Cat " | s %like% "Cheshire puss")))
  
  dat_all= rbind(dat_all, dat_ind)
}


# Alice in Wonderland
png('./plots/alicesAdventures.png')
plot_ind(11, dat_all)
dev.off()


# expect infinite values when peaks/troughs aren't present in the segment
peak_distances= rbind(dat_all[,list(cat_sighting= cat_sighting,
                                            timeline= timeline, 
                                            sentiment= sentiment, 
                                            critical_point= 'peak',
                                            pos= 'after',
                                            timediff= sapply(1:length(timeline), function(i) min(abs(timeline[i]-timeline[i:length(timeline)][peaks[i:length(timeline)]==1])))),
                              by= list(ind, title)],
                       dat_all[,list(cat_sighting= cat_sighting,
                                            timeline= timeline, 
                                            sentiment= sentiment, 
                                            critical_point= 'peak',
                                            pos= 'before',
                                            timediff= sapply(1:length(timeline), function(i) min(abs(timeline[i]-timeline[1:i][peaks[1:i]==1])))),
                                      by= list(ind, title)],
                       dat_all[,list(cat_sighting= cat_sighting,
                                            timeline= timeline, 
                                            sentiment= sentiment, 
                                            critical_point= 'trough',
                                            pos= 'after',
                                            timediff= sapply(1:length(timeline), function(i) min(abs(timeline[i]-timeline[i:length(timeline)][troughs[i:length(timeline)]==1])))),
                                      by= list(ind, title)],
                       dat_all[,list(cat_sighting= cat_sighting,
                                            timeline= timeline, 
                                            sentiment= sentiment, 
                                            critical_point= 'trough',
                                            pos= 'before',
                                            timediff= sapply(1:length(timeline), function(i) min(abs(timeline[i]-timeline[1:i][troughs[1:i]==1])))),
                                      by= list(ind, title)])


cp_dists= rbind(peak_distances[cat_sighting==1 & ! is.infinite(timediff),
                                list(cat_sighting=1, 
                                     timeline= timeline[which.min(timediff)], 
                                     sentiment= sentiment[which.min(timediff)], 
                                     critical_point= critical_point[which.min(timediff)], 
                                     pos= pos[which.min(timediff)], 
                                     timediff= timediff[which.min(timediff)]), 
                                by=list(ind, title)], 
                peak_distances[cat_sighting==0 & ! is.infinite(timediff),
                                list(cat_sighting=0, 
                                     sentiment= sentiment[which.min(timediff)], 
                                     critical_point= critical_point[which.min(timediff)], 
                                     pos= pos[which.min(timediff)], 
                                     timediff= timediff[which.min(timediff)]), 
                                by=list(ind, title, timeline)])

peak_types= cp_dists[,list(count= nrow(.SD)), by=list(cat_sighting, pos, critical_point)]
write.csv(dcast(data= peak_types, pos+critical_point~cat_sighting, value.var= 'count'), './plots/peak_dists.csv', row.names= F)

# freqs: "after trough"  "before trough" "after peak" "before peak"
p= peak_types[cat_sighting==0, count/sum(count)]
size= peak_types[cat_sighting==1, sum(count)]
N= 10000
samples= rmultinom(size= size, p= p, n= N)

data.table(	p_after= sum(apply(samples, 2, function(r) sum(r[c(1,3)])>peak_types[cat_sighting==1, sum(count[c(1,3)])]))/N,
			p_trough= sum(apply(samples, 2, function(r) sum(r[c(1,2)])>peak_types[cat_sighting==1, sum(count[c(1,2)])]))/N,
			p_after_trough= sum(apply(samples, 2, function(r) r[1]>peak_types[cat_sighting==1, count[1]]))/N,
			p_before_trough= sum(apply(samples, 2, function(r) r[2]>peak_types[cat_sighting==1, count[2]]))/N,
			p_after_peak= sum(apply(samples, 2, function(r) r[3]>peak_types[cat_sighting==1, count[3]]))/N,
			p_before_peak= sum(apply(samples, 2, function(r) r[4]>peak_types[cat_sighting==1, count[4]]))/N)







