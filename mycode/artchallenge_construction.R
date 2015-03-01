(2-47+5*sqrt(196))^2

mess <- strsplit(x = "NEXT MIDNIGHT AT", split = "")[[1]]
long <- strsplit(as.character(48.862787), split = "")[[1]]
lat <- strsplit(as.character(2.392809), split = "")[[1]]
mess2 <- strsplit(x = "BRING FIFTY MILLION USD", split = "")[[1]]

let.old <- c(LETTERS, " ")
let.new <- c(LETTERS[4:26], LETTERS[1:3], " ")
num.old <- c(0:9, ".")
num.new <- c(3:9, 0:2, ".")

relab <- function(old.var, old.labs, new.labs){
  map <- setNames(object = old.labs, nm = new.labs)
  return(paste(map[unlist(eval(old.var))], collapse = ""))
}

relab(mess, let.old, let.new)
relab(long, num.old, num.new)
relab(lat, num.old, num.new)
relab(mess2, let.old, let.new)

# generate the dataset 
set.seed(34034)
Ol <- rnorm(n = 53, mean = 1, sd=0.3)
set.seed(11032)
On <- Ol*rnorm(n = 53, mean = 3, sd=0.3)
set.seed(4234)
Op <- Ol*rnorm(n = 53, mean = 2, sd=0.3)
Tg <- (On-Op)^2
set.seed(42349)
year <- round(1750+8*Ol+15*On+18*Op+14*Tg+rnorm(53, 2, 1)+
                rnorm(53, 0, 2))
dat <- data.frame(year, Ol, On, Op, Tg)
write.csv(dat, "data1.csv", row.names=F)
lm(year~Ol+On+Op+Tg, data = dat)
summary(lm(year~Ol+On+Op+Tg, data = dat))
dat$a <- ifelse(dat$year>1881 & dat$year<1890, 1, 0)
bi <- glm(formula = a~Ol+On+Op+Tg, data = dat, family = "binomial")
summary(bi)
predict(object = bi, newdata = data.frame(Ol=3.5434, On=2.2423, Op=1.6793, Tg=3.8935), type="response")

round(1751.1421+3.5434*17.1088+2.2423*14.2181+1.6793*15.3624+3.8935*13.9145)

library(png)
img.n <- readPNG("~/Dropbox/artchallenge/data/message3.png")
dim(img)
layer1 <- as.matrix(img[,,1])
write.table(as.matrix(img[,,1]), "~/Dropbox/artchallenge/data/layer1.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(as.matrix(img[,,2]), "~/Dropbox/artchallenge/data/layer2.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(as.matrix(img[,,3]), "~/Dropbox/artchallenge/data/layer3.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(as.matrix(img[,,4]), "~/Dropbox/artchallenge/data/layer4.csv", sep = ",", col.names = FALSE, row.names = FALSE)


if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
  plot(1:2, type='n')
  
  if (names(dev.cur()) == "windows") {
    # windows device doesn't support semi-transparency so we'll need
    # to flatten the image
    transparent <- img[,,4] == 0
    img <- as.raster(img[,,1:3])
    img[transparent] <- NA
    
    # interpolate must be FALSE on Windows, otherwise R will
    # try to interpolate transparency and fail
    rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
    
  } else {
    # any reasonable device will be fine using alpha
    rasterImage(img, 1.2, 1.27, 1.8, 1.73)
    rasterImage(img.n, 1.5, 1.5, 1.9, 1.8)
    
  }
}

years <- 1850:1950
Y <- sort(Y)
CBP <- c(100, 98, 92, 94, 96, 97, 92, 90, 88, 89,
          89, 87, 84, 82, 81, 83, 85, 79, 76, 78,
          76, 77, 74, 73, 73, 72, 71, 73, 72, 70,
          71, 67, 68, 66, 64, 65, 62, 60, 61, 59)
