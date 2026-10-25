# Alt-text audit for Carpentries lessons.
# - R chunks: parse code into statements, track ggplot objects assigned in
#   earlier chunks of the same file, and count a chunk as drawing a figure when
#   it prints a ggplot (inline, by name, print(), (p <- ...), patchwork,
#   grid.arrange/plot_grid/ggarrange), calls base graphics, or include_graphics().
# - Python chunks ({python} in .Rmd/.qmd): count a chunk as drawing a figure
#   when it calls matplotlib, pandas .plot, seaborn, plotnine, plotly express
#   or altair. Python in plain .md lessons is not run; its plots are static
#   images and are covered by the Markdown image check.
# - Markdown images: ![text](src){alt=...}, ![text][ref], <img ...>, outside
#   code fences, in .Rmd and in hand-written .md (not .md rendered from .Rmd).
# Status: alt = explicit alt text; caption_only = only fig.cap / image text
# (used as fallback, but not a description); decorative = alt=""; none.
library(stringr)

gg_re <- "\\bggplot\\(|\\bqplot\\("
gg_combine_re <- "\\b(grid\\.arrange|arrangeGrob|plot_grid|ggarrange|wrap_plots)\\("
base_re <- "(^|[^A-Za-z0-9_.])(plot|hist|boxplot|barplot|image|heatmap|pairs|matplot|pie|dotchart|stripchart|mosaicplot|curve|smoothScatter|coplot|biplot)\\("
img_re <- "include_graphics\\("
py_re <- paste0(
  "\\bplt\\.(plot|scatter|hist|bar|barh|boxplot|violinplot|imshow|pie|errorbar|",
  "fill_between|stem|step|subplots|figure|show)\\(|",
  "\\.plot(\\.[a-z]+)?\\(|\\.(hist|boxplot|imshow)\\(|",
  "\\bsns\\.([a-z]+plot|heatmap|clustermap)\\(|",
  "\\bggplot\\(|\\bpx\\.[a-z_]+\\(|\\balt\\.Chart\\("
)
name_re <- "[A-Za-z.][A-Za-z0-9._]*"
assign_re <- paste0("^\\s*(", name_re, ")\\s*(<-|=(?!=))")
paren_assign_re <- paste0("^\\s*\\(\\s*(", name_re, ")\\s*<-")

statements <- function(code) {
  out <- character()
  buf <- character()
  bal <- 0
  for (l in code) {
    s <- str_remove(l, "\\s+#[^\"']*$")
    if (!length(buf) && str_detect(s, "^\\s*$")) next
    buf <- c(buf, s)
    nostr <- str_remove_all(s, "\"[^\"]*\"|'[^']*'")
    bal <- bal + str_count(nostr, "[(\\[{]") - str_count(nostr, "[)\\]}]")
    if (bal <= 0 && !str_detect(s, "(\\+|%>%|\\|>|,|<-|=)\\s*$")) {
      out <- c(out, paste(buf, collapse = "\n"))
      buf <- character()
      bal <- 0
    }
  }
  if (length(buf)) out <- c(out, paste(buf, collapse = "\n"))
  out
}

uses_plot_obj <- function(st, objs) {
  if (!length(objs)) return(FALSE)
  alt <- paste(str_replace_all(objs, "\\.", "\\\\."), collapse = "|")
  str_detect(st, paste0("^\\s*(print\\(\\s*)?(", alt, ")\\s*(\\)\\s*)?($|\\+|/|\\|)"))
}

# Classify one chunk's statements; updates the set of ggplot object names.
classify_chunk <- function(code, objs) {
  kinds <- character()
  if (any(str_detect(code, img_re))) kinds <- c(kinds, "image")
  if (any(str_detect(code, base_re))) kinds <- c(kinds, "base")
  for (st in statements(code)) {
    m <- str_match(st, assign_re)
    if (!is.na(m[, 2])) {
      rhs <- str_remove(st, assign_re)
      if (str_detect(rhs, gg_re) || uses_plot_obj(rhs, objs)) objs <- union(objs, m[, 2])
      next
    }
    pm <- str_match(st, paren_assign_re)
    if (!is.na(pm[, 2]) && str_detect(st, gg_re)) objs <- union(objs, pm[, 2])
    if (str_detect(st, "^\\s*ggsave\\(")) next
    if (str_detect(st, gg_re) || str_detect(st, gg_combine_re)) kinds <- c(kinds, "ggplot")
    else if (uses_plot_obj(st, objs)) kinds <- c(kinds, "ggplot-obj")
  }
  list(kinds = unique(kinds), objs = objs)
}

audit_chunks <- function(path, lines) {
  starts <- grep("^\\s*```+\\s*\\{(r|python)([ ,}].*)?$", lines)
  global_alt <- any(str_detect(lines, "opts_chunk\\$set\\([^#]*fig\\.alt"))
  objs <- character()
  out <- list()
  for (s in starts) {
    e <- s + which(str_detect(lines[(s + 1):length(lines)], "^\\s*```+\\s*$"))[1]
    if (is.na(e)) next
    header <- lines[s]
    body <- if (e - 1 > s) lines[(s + 1):(e - 1)] else character()
    optstr <- paste(c(header, str_subset(body, "^\\s*#\\|")), collapse = " ")
    if (str_detect(optstr, "eval\\s*[=:]\\s*(FALSE|false|F\\b)")) next
    code <- str_subset(body, "^\\s*#", negate = TRUE)
    if (str_detect(header, "\\{python")) {
      res <- list(kinds = if (any(str_detect(code, py_re))) "python" else character())
    } else {
      res <- classify_chunk(code, objs)
      objs <- res$objs
    }
    hidden <- str_detect(optstr, "include\\s*[=:]\\s*(FALSE|false)") ||
      str_detect(optstr, "fig[.-]show\\s*[=:]\\s*['\"]?hide")
    if (hidden || !length(res$kinds)) next
    status <- if (global_alt || str_detect(optstr, "fig[.-]alt") ||
                  any(str_detect(body, "labs\\(.*\\balt\\s*="))) "alt"
              else if (str_detect(optstr, "fig[.-]cap")) "caption_only"
              else "none"
    out[[length(out) + 1]] <- data.frame(
      file = path, line = s, type = "chunk",
      what = paste0(coalesce_na(str_match(header, "\\{(?:r|python)[ ,]*([^,}= ]+)\\s*[,}]")[, 2], "<unnamed>"),
                    " [", paste(res$kinds, collapse = "+"), "]"),
      status = status
    )
  }
  do.call(rbind, out)
}

coalesce_na <- function(x, y) ifelse(is.na(x), y, x)

audit_images <- function(path, lines) {
  in_fence <- FALSE
  keep <- lines
  for (i in seq_along(lines)) {
    if (str_detect(lines[i], "^\\s*(```|~~~)")) {
      in_fence <- !in_fence
      keep[i] <- ""
    } else if (in_fence) keep[i] <- ""
  }
  keep <- str_remove_all(keep, "`[^`]*`")
  text <- paste(keep, collapse = "\n")
  line_of <- function(pos) str_count(str_sub(text, 1, pos), "\n") + 1
  out <- list()
  add <- function(pos, what, status) {
    out[[length(out) + 1]] <<- data.frame(file = path, line = line_of(pos),
                                          type = "md-image", what = what, status = status)
  }
  # Image text may hold one level of brackets, e.g. ![x[0] code](src).
  md <- "!\\[((?:[^\\[\\]]|\\[[^\\]]*\\])*)\\](\\(([^)\\s]*)[^)]*\\)|\\[[^\\]]*\\])(\\{[^}]*\\})?"
  loc <- str_locate_all(text, md)[[1]]
  m <- str_match_all(text, md)[[1]]
  for (k in seq_len(nrow(m))) {
    attrs <- coalesce_na(m[k, 5], "")
    alt <- str_match(attrs, "alt\\s*=\\s*(\"([^\"]*)\"|'([^']*)')")
    alt_val <- coalesce_na(coalesce_na(alt[, 3], alt[, 4]), NA)
    status <- if (!is.na(alt_val) && nzchar(str_trim(alt_val))) "alt"
              else if (!is.na(alt_val)) "decorative"
              else if (nzchar(str_trim(m[k, 2]))) "caption_only"
              else "none"
    add(loc[k, 1], coalesce_na(m[k, 4], m[k, 3]), status)
  }
  html <- "<img\\b[^>]*>"
  loc <- str_locate_all(text, html)[[1]]
  tags <- str_extract_all(text, html)[[1]]
  for (k in seq_along(tags)) {
    alt <- str_match(tags[k], "\\balt\\s*=\\s*(\"([^\"]*)\"|'([^']*)')")
    alt_val <- coalesce_na(alt[, 3], alt[, 4])
    status <- if (is.na(alt_val)) "none" else if (nzchar(str_trim(alt_val))) "alt" else "decorative"
    add(loc[k, 1], coalesce_na(str_match(tags[k], "src\\s*=\\s*[\"']([^\"']*)")[, 2], tags[k]), status)
  }
  do.call(rbind, out)
}

audit_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  rbind(if (str_detect(path, "(?i)\\.(rmd|qmd)$")) audit_chunks(path, lines),
        audit_images(path, lines))
}

src <- list.files(pattern = "\\.(Rmd|qmd)$", recursive = TRUE)
md <- list.files(pattern = "\\.md$", recursive = TRUE)
md <- md[str_detect(md, "(^|/)(episodes|learners|instructors)/|^([^/]+/)?index\\.md$")]
md <- md[!str_remove(md, "\\.md$") %in% str_remove(src, "\\.(Rmd|qmd)$")]
res <- do.call(rbind, lapply(c(src, md), audit_file))
# Run inside one lesson repo, or in a folder holding several lesson repos.
single_repo <- dir.exists(".git")
res$lesson <- if (single_repo) basename(getwd()) else str_split_i(res$file, "/", 1)
# A caption is not a description, so caption_only counts as missing.
res$missing <- res$status %in% c("none", "caption_only")
res <- res[, c("lesson", "file", "line", "type", "what", "status", "missing")]
out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "alt-audit.csv"
write.csv(res, out, row.names = FALSE)

if (single_repo) {
  miss <- res[res$missing, ]
  miss <- miss[order(miss$file, miss$line), ]
  cat("\nFigures missing alt text:\n")
  cat(sprintf("%s:%d: %s %s (%s)\n", miss$file, miss$line, miss$type, miss$what, miss$status), sep = "")
}

cat("\nMissing alt text by lesson and type:\n")
agg <- aggregate(cbind(total = 1, missing = missing) ~ lesson + type, res, sum)
agg$pct_missing <- round(100 * agg$missing / agg$total)
print(agg[order(agg$lesson, agg$type), ], row.names = FALSE)
cat("\nTotal missing:", sum(res$missing), "of", nrow(res),
    "figures (incl.", sum(res$status == "caption_only"), "caption only)\n")
