load_lr_df <- function() {
  df <- data.frame(read.csv('data/letter-recognition.data', head=FALSE, sep=','))

  colnames(df) <- c(
    'lettr', # capital letter	(26 values from A to Z)
    'x-box', # horizontal position of box	(integer)
    'y-box', # vertical position of box	(integer)
    'width', # width of box			(integer)
    'high ', # height of box			(integer)
    'onpix', # total # on pixels		(integer)
    'x-bar', # mean x of on pixels in box	(integer)
    'y-bar', # mean y of on pixels in box	(integer)
    'x2bar', # mean x variance			(integer)
    'y2bar', # mean y variance			(integer)
    'xybar', # mean x y correlation		(integer)
    'x2ybr', # mean of x * x * y		(integer)
    'xy2br', # mean of x * y * y		(integer)
    'x-ege', # mean edge count left to right	(integer)
    'xegvy', # correlation of x-ege with y	(integer)
    'y-ege', # mean edge count bottom to top	(integer)
    'yegvx' # correlation of y-ege with x	(integer)
  )

  df$lettr <- factor(df$lettr)
  ycolname <- 'lettr'

  return(df)
}