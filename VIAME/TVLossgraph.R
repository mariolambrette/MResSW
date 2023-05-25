library(ggpubr)
library(wesanderson)

train <- c(0.5963,0.2300,0.2241,0.2249,0.2349,0.2276,0.2235,0.2297,0.2347,0.2336,
           0.2365,0.2325,0.2303,0.2332,0.2366,0.2342,0.2345,0.2336,0.2267)

valid <- c(0.2542,0.2591,0.2513,0.2689,0.2576,0.2533,0.2556,0.2591,0.2605,0.2603,
           0.2578,0.2551,0.2568,0.2555,0.2563,0.2559,0.2544,0.2542,0.2547)

best <- 0.2513

df <- cbind(train,valid)
epochs <- seq(1,nrow(df))
df <- cbind(df, epochs)
df <- as.data.frame(df)


plot <-ggplot(df, aes(x=epochs))+
  geom_line(aes(y=train, color = 'Training'), linewidth = 1) +
  geom_line(aes(y=valid, color = 'Validation'), linewidth = 1) +
  scale_colour_manual(values = wes_palette('Darjeeling1', n=2)) +
  geom_hline(yintercept = best) +
  geom_text(aes(x=min(epochs + 1), y=best, label = paste('Best = ', best, sep ='')), vjust = -1) +
  labs(y = 'Loss', x = 'Epochs', title = 'Training and Validation Loss') +
  theme_pubr() +
  theme(axis.title = element_text(face = 'bold'),
        axis.text = element_text(face = 'bold'))

plot
