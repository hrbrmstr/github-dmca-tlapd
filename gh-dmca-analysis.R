library(tools)
library(stringi)
library(hrbrthemes)
library(tidyverse)

download.file("https://rud.is/dl/github-dmca.json.gz", "~/Data/github-dmca.json.gz")

jsonlite::stream_in(gzfile("~/Data/github-dmca.json.gz")) %>%
  tbl_df() %>%
  mutate(notice_day = as.Date(notice_day)) -> dmca

filter(dmca, !retraction) %>%
  mutate(
    notice_year = lubridate::year(notice_day),
    notice_ym = as.Date(format(notice_day, "%Y-%m-01"))
  ) %>%
  dplyr::count(notice_ym) %>%
  arrange(notice_ym) %>%
  ggplot(aes(notice_ym, n)) +
  ggalt::stat_xspline(
    geom="area", fill=alpha(ft_cols$blue, 1/3), color=ft_cols$blue
  ) +
  scale_y_comma() +
  labs(
    x = NULL, y = "# Notices",
    title = "GitHub DMCA Notices by Month Since 2011"
  ) +
  theme_ft_rc(grid="XY")

count(dmca, notice_org, sort=TRUE)

filter(dmca, !retraction, !counter_notice, notice_day >= as.Date("2015-01-01")) %>%
  mutate(
    notice_year = lubridate::year(notice_day),
  ) %>%
  dplyr::count(notice_year, notice_org) %>%
  group_by(notice_year) %>%
  top_n(15) %>%
  slice(1:15) %>%
  dplyr::ungroup() %>%
  mutate( # a-z order with "a" on top
    notice_org = factor(notice_org, levels = unique(sort(notice_org, decreasing = TRUE)))
  ) %>%
  ggplot(aes(n, notice_org, xend=0, yend=notice_org)) +
  geom_segment(size = 2, color = ft_cols$peach) +
  facet_wrap(~notice_year, scales = "free") +
  scale_x_comma(limits=c(0, 60)) +
  labs(
    x = NULL, y = NULL,
    title = "Top 15 GitHub DMCA Filers by Year Since 2015"
  ) +
  theme_ft_rc(grid="X")


dmca %>%
  mutate(
    ghusers = notice_content %>%
      map(~{
        stri_match_all_regex(.x, "http[s]*://github.com/([^/]+)/.*") %>%
          discard(~is.na(.x[,1])) %>%
          map_chr(~.x[,2]) %>%
          unique() %>%
          discard(`==`, "github") %>%
          discard(~grepl(" ", .x))
      })
  ) %>%
  unnest(ghusers) %>%
  dplyr::count(ghusers, sort=TRUE) %>%
  print() -> offenders

ggplot(offenders, aes(x="", n)) +
  ggbeeswarm::geom_quasirandom(
    color = ft_cols$white, fill = alpha(ft_cols$red, 1/10),
    shape = 21, size = 3, stroke = 0.125
  ) +
  scale_y_comma(breaks=1:16, limits=c(1,16)) +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Distribution of the Number of GitHub DMCA Complaints Received by a User"
  ) +
  theme_ft_rc(grid="X")

library(magick)

dir.create("gh-pirates")
dir.create("gh-pirates-jpeg")

# this kinda spoils the surprise; i should have renamed it
download.file("https://rud.is/dl/jolly-roger.jpeg", "jolly-roger.jpeg")

ghs <- safely(gh::gh) # no need to add cruft to our namespace for one function

filter(offenders, n>2) %>%
  pull(ghusers) %>%
  { .pb <<- progress_estimated(length(.)); . } %>% # there are a few hundred of them
  walk(~{
    .pb$tick()$print()
    user <- ghs(sprintf("/users/%s", .x))$result # the get-user and then download avatar idiom shld help us not bust GH API rate limits
    if (!is.null(user)) {
      download.file(user$avatar_url, file.path("gh-pirates", .x), quiet=TRUE) # can't assume avatar file type
    }
  })

# we'll convert them all to jpeg and resize them at the same time plus make sure they aren't greyscale
dir.create("gh-pirates-jpeg")
list.files("gh-pirates", full.names = TRUE, recursive = FALSE) %>%
  walk(~{
    image_read(.x) %>%
      image_scale("72x72") %>%
      image_convert("jpeg", type = "TrueColor", colorspace = "rgb") %>%
      image_write(
        path = file.path("gh-pirates-jpeg", sprintf("%s.jpeg", basename(.x))),
        format = "jpeg"
      )
  })

set.seed(20180919) # seemed appropriate for TLAPD
RsimMosaic::composeMosaicFromImageRandomOptim( # this takes a bit
  originalImageFileName = "jolly-roger.jpeg",
  outputImageFileName = "gh-pirates-flag.jpeg",
  imagesToUseInMosaic = "gh-pirates-jpeg",
  removeTiles = TRUE,
  fracLibSizeThreshold = 0.1
)

mutate(
  dmca,
  files = notice_content %>%
    map(~{
      paste0(.x, collapse = " ") %>%
        stri_extract_all_regex(gh_url_pattern, omit_no_match=FALSE, opts_regex = stri_opts_regex(TRUE)) %>%
        unlist() %>%
        stri_replace_last_regex("[[:punct:]]+$", "")
    })
) -> dmca_with_files

filter(dmca_with_files, map_lgl(files, ~!is.na(.x[1]))) %>%
  select(notice_day, notice_org, files) %>%
  mutate(num_refs = lengths(files)) %>%
  arrange(desc(num_refs)) %>%  # take a peek at the heavy hitters
  print() -> files_with_counts

ggplot(files_with_counts, aes(x="", num_refs)) +
  ggbeeswarm::geom_quasirandom(
    color = ft_cols$white, fill = alpha(ft_cols$red, 1/10),
    shape = 21, size = 3, stroke = 0.125
  ) +
  scale_y_comma(trans="log10") +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Distribution of the Number of Files/Repos per-GitHub DMCA Complaint",
    caption = "Note: Log10 Scale"
  ) +
  theme_ft_rc(grid="X")


mutate(
  files_with_counts,
  extensions = map(files, ~tools::file_ext(.x) %>%
    discard(`==` , "")
  )
) %>%
  select(notice_day, notice_org, extensions) %>%
  unnest(extensions) %>%
  mutate(year = lubridate::year(notice_day)) -> file_types

count(file_types, year, extensions) %>%
  filter(year >= 2014) %>%
  group_by(year) %>%
  top_n(10) %>%
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(year, n)) +
  ggrepel::geom_text_repel(
    aes(label = extensions, size=n),
    color = ft_cols$green, family=font_ps, show.legend=FALSE
  ) +
  scale_size(range = c(3, 10)) +
  labs(
    x = NULL, y = NULL,
    title = "Top 10 File-type GitHub DMCA Takedowns Per-year"
  ) +
  theme_ft_rc(grid="X") +
  theme(axis.text.y=element_blank())




