## ----wrap-hook, echo=FALSE---------------------------------------------------------------------
# Taken from https://github.com/yihui/knitr-examples/blob/master/077-wrap-output.Rmd

library(knitr)
hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x <- knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x <- strwrap(x, width = n)
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})


## ---- echo = FALSE, message=FALSE, eval=FALSE--------------------------------------------------
## # This chunk skips executing any other chunks
## knitr::opts_chunk$set(echo = TRUE, comment = "", eval = FALSE)
## languages <- 3
## dialects <- 3
## str_break <- function(x, width = 80L) {
##   n <- nchar(x)
##   if (n <= width) {
##     return(x)
##   }
##   n1 <- seq(1L, n, by = width)
##   n2 <- seq(width, n, by = width)
##   if (n %% width != 0) n2 <- c(n2, n)
##   x <- substring(x, n1, n2)
##   x <- trimws(x)
## }


## ----echo = FALSE, message=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "")
library(rvest)
library(dplyr)
library(text2speech)
library(ari)
library("knitcitations")
cleanbib()
# doc = xml2::read_html("https://docs.aws.amazon.com/polly/latest/dg/voicelist.html")
# tab = doc %>%
#   html_table()
# tab = tab[[2]]
# dialects = unique(tab$Language)
# languages = sub(", .*", "", dialects)
# languages = sub(" .*", "", languages)
# languages = languages %>%
#   trimws %>%
#   unique()

str_break <- function(x, width = 80L) {
  n <- nchar(x)
  if (n <= width) {
    return(x)
  }
  n1 <- seq(1L, n, by = width)
  n2 <- seq(width, n, by = width)
  if (n %% width != 0) n2 <- c(n2, n)
  x <- substring(x, n1, n2)
  x <- trimws(x)
}

res <- ffmpeg_audio_codecs()
fdk_enabled <- grepl("fdk", res[res$codec == "aac", "codec_name"])
if (fdk_enabled) {
  audio_codec <- "libfdk_aac"
} else {
  audio_codec <- "aac"
}


## ----fig1, eval=TRUE, out.width="100%", echo=FALSE, fig.cap="Ari is designed to fit into several existing workflows for creating lectures and presentations. Videos can be created with Ari from a series of images and a narrative script, from an R Markdown document, or from a PowerPoint presentation with speaker notes. Ari is pre-configured so that videos are ready to be uploaded to popular platforms like YouTube or Coursera."----
knitr::include_graphics("Figure-1-Ari-pdf.pdf")


## ----stitch, message = FALSE, eval = FALSE-----------------------------------------------------
## library(tuneR)
## library(ari)
## result <- ari_stitch(
##   ari_example(c("mab1.png", "mab2.png")),
##   list(noise(), noise()),
##   output = "noise.mp4"
## )
## isTRUE(result)


## ----stitch_run, message = FALSE, echo = FALSE, eval = TRUE------------------------------------
library(tuneR)
library(ari)
result <- ari_stitch(
  ari_example(c("mab1.png", "mab2.png")),
  list(noise(), noise()),
  output = "noise.mp4"
)
isTRUE(result)


## ----stitch_out, message = FALSE, eval = FALSE-------------------------------------------------
## attributes(result)$outfile


## ----stitch_out_run, message = FALSE, echo = FALSE, eval = TRUE--------------------------------
basename(attributes(result)$outfile)


## ---- echo = TRUE, message=FALSE---------------------------------------------------------------
speech <- c(
  "I will now perform part of Mercutio's speech from Shakespeare's Romeo and Juliet.",
  "O, then, I see Queen Mab hath been with you.
   She is the fairies' midwife, and she comes
   In shape no bigger than an agate-stone
   On the fore-finger of an alderman,
   Drawn with a team of little atomies
   Athwart men's noses as they lies asleep;"
)
mercutio_file <- "death_of_mercutio.png"
mercutio_file2 <- "mercutio_actor.png"


## ---- echo = TRUE, eval = FALSE----------------------------------------------------------------
## shakespeare_result <- ari_spin(
##   c(mercutio_file, mercutio_file2),
##   speech,
##   output = "romeo.mp4", voice = "Joanna"
## )
## isTRUE(shakespeare_result)


## ----romeo, echo = FALSE, eval = TRUE, message=FALSE-------------------------------------------
mercutio_file <- "death_of_mercutio.png"
mercutio_file2 <- "mercutio_actor.png"
if (!file.exists(mercutio_file)) {
  mercutio_file_bad <- tempfile(fileext = ".png")
  download.file("https://upload.wikimedia.org/wikipedia/commons/b/bc/Death_of_Mercutio.png?download",
    destfile = mercutio_file_bad
  )
  res <- system2("ffmpeg", args = c("-y", "-i", mercutio_file_bad, mercutio_file))
}
if (!file.exists(mercutio_file2)) {
  mercutio_file_bad <- tempfile(fileext = ".jpg")
  download.file("https://upload.wikimedia.org/wikipedia/commons/7/76/Welles-Mercutio-1933.jpg?download",
    destfile = mercutio_file_bad
  )
  res <- system2("ffmpeg", args = c("-y", "-i", mercutio_file_bad, mercutio_file2))
}

output <- "romeo.mp4"
if (!file.exists(output)) {
  run_voice <- "Joanna"
  ari_spin(
    c(mercutio_file, mercutio_file2),
    speech,
    output = output, voice = run_voice,
    service = "amazon",
    audio_codec = audio_codec
  )
}
TRUE


## ---- echo = FALSE, eval = FALSE---------------------------------------------------------------
## text2speech::tts_voices(service = "amazon") %>%
##   filter(grepl("en", language_code)) %>%
##   knitr::kable(format = "latex")


## ---- echo = TRUE, eval = FALSE----------------------------------------------------------------
## gb_result <- ari_spin(
##   c(mercutio_file, mercutio_file2),
##   speech,
##   output = "romeo_gb.mp4", voice = "Brian"
## )
## isTRUE(gb_result)


## ----romeo_gb, echo = FALSE, eval = TRUE-------------------------------------------------------
output <- "romeo_gb.mp4"
if (!file.exists(output)) {
  run_voice <- "Brian"
  ari_spin(
    c(mercutio_file, mercutio_file2),
    speech,
    output = output, voice = run_voice,
    service = "amazon",
    audio_codec = audio_codec
  )
}
TRUE


## ----------------------------------------------------------------------------------------------
x <- readLines(ari_example("ari_comments.Rmd"))
tail(x[x != ""], 4)


## ----narr_show, eval = FALSE-------------------------------------------------------------------
## # Create a video from an R Markdown file with comments and slides
## res <- ari_narrate(
##   script = ari_example("ari_comments.Rmd"),
##   voice = "Kendra",
##   capture_method = "iterative"
## )


## ----narrate, echo = FALSE, eval = TRUE--------------------------------------------------------
output <- "narrate_example.mp4"
if (!file.exists(output)) {
  res <- ari_narrate(
    script = ari_example("ari_comments.Rmd"),
    voice = "Kendra",
    output = output,
    audio_codec = audio_codec,
    capture_method = "iterative"
  )
}


## ----pptx_convert, eval = FALSE, message=FALSE-------------------------------------------------
## pptx <- "ari.pptx"
## download.file(paste0(
##   "https://s3-eu-west-1.amazonaws.com/",
##   "pfigshare-u-files/16252631/ari.pptx"
## ),
## destfile = pptx
## )
## pdf <- docxtractr::convert_to_pdf(pptx) # >= 0.6.2
## pngs <- pdftools::pdf_convert(pdf, dpi = 300)
## notes <- ariExtra::pptx_notes(pptx)
## notes


## ----pptx_convert_run, eval = TRUE, echo = FALSE-----------------------------------------------
pptx <- "ari.pptx"
pngs <- c("ari_1.png", "ari_2.png")
notes <- ariExtra::pptx_notes(pptx)
show_notes <- notes
names(show_notes) <- NULL
show_notes <- sapply(show_notes, str_break)
colnames(show_notes) <- NULL
show_notes <- apply(show_notes, 2, paste, collapse = "\n")
show_notes <- paste0("[", 1:length(show_notes), "] ", '"', show_notes, '"')
cat(show_notes, sep = "\n\n")


## ----pptx_to_ari, message=FALSE, eval=FALSE----------------------------------------------------
## doc <- ariExtra::pptx_to_ari(pptx)


## ----pptx_to_ari_2, message=FALSE, eval=FALSE--------------------------------------------------
## doc[c("images", "script")]


## ---- echo = TRUE, eval = FALSE----------------------------------------------------------------
## pptx_result <- ari_spin(pngs, notes,
##   output = "pptx.mp4", voice = "Kimberly",
##   divisible_height = TRUE, subtitles = TRUE
## )
## isTRUE(pptx_result)


## ----pptx_mp4, echo = FALSE, eval = TRUE, message=FALSE----------------------------------------
output <- "pptx.mp4"
if (!file.exists(output)) {
  pptx_result <- ari_spin(
    pngs, notes,
    output = output,
    voice = "Kimberly",
    audio_codec = audio_codec, verbose = 2,
    divisible_height = TRUE, subtitles = TRUE
  )
}


## ----show_srt, echo = FALSE, eval = TRUE, message=FALSE----------------------------------------
head(setdiff(readLines("pptx.srt"), ""))


## ----gs_to_ari, message=FALSE, linewidth=80, warning=FALSE-------------------------------------
gs_doc <- ariExtra::gs_to_ari("14gd2DiOCVKRNpFfLrryrGG7D3S8pu9aZ")


## ---- include = FALSE, eval = FALSE------------------------------------------------------------
## # write.bibtex(file = "RJreferences.bib", append = TRUE)

