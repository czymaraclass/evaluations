

### create worldcloude of open evaluation items

evaluations <- read.delim("evaluations.txt", fileEncoding="UTF-8")

evaluations <- as.character(evaluations[,1])

packages <- c("quanteda", "quanteda.textplots")
lapply(packages, library, character.only = TRUE)


toks <- tokens(corpus(evaluations), remove_punct = T,
               remove_numbers = T,
               remove_symbols = T,
               remove_separators = T,
               remove_hyphens = T)

toks <-  tokens_remove(toks, c(stopwords("german"),
                               "dass", "h", "wäre", "wären", "z.b", "mal",
                               "bzw", "pro", "eher", "h", "denen",
                               "dafür", "innen", "ja", "wurde"), case_insensitive = TRUE, padding = FALSE)
toks <-  tokens_remove(toks, stopwords(), case_insensitive = TRUE, padding = FALSE)

DFM <- dfm(toks)

### wordcloud
win.metafile("eval_wordcloud.wmf")

textplot_wordcloud(DFM,
                  # min_size = 1.5,
                 #  min_count = 3,
                   color = "black",
                 adjust = )
# dev.copy(png, width = 350, height = 350, "C:/Users/czymara.local/Nextcloud/teaching/teachingevaluations/out/eval_wordcloud.png")
dev.off()

