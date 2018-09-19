library(tools)
library(stringi)
library(hrbrthemes)
library(tidyverse)

list.files(
  path = sprintf("/data/github/dmca/%s", 2011:2018),
  pattern = "\\.md$|\\.markdown$",
  full.names = TRUE
) -> dmca_files

map_df(dmca_files, ~{

  file_path_sans_ext(.x) %>% # remove extension
    basename() %>% # get just the filename
    stri_match_all_regex(
      "([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{1,2})-(.*)" # try to find the date and the org
    ) %>%
    unlist() -> date_org

  if (is.na(date_org[2])) { # handle a special case where the date pattern above didn't work
    file_path_sans_ext(.x) %>%
      basename() %>%
      stri_match_all_regex(
        "([[:digit:]]{4}-[[:digit:]]{2})-(.*)"
      ) %>%
      unlist() -> date_org
  }

  # a few files are still broken so we'll deal with them as special cases

  if (stri_detect_fixed(.x, "2017/2017-11-06-1776.md")) {
    date_org <- c("", "2017-11-06", "1776")
  } else if (stri_detect_fixed(.x, "2017/2017-Offensive-Security-7.md")) {
    date_org <- c("", "2017-12-30", "Offensive-Security-7")
  } else if (stri_detect_fixed(.x, "2017/Offensive-Security-6.md")) {
    date_org <- c("", "2017-12-29", "Offensive-Security-6")
  }

  # we used a somewhat liberal regex to capture dates since some are
  # still broken. We'll deal with those first, then turn them
  # into proper Date objects

  list(
    notice_day = case_when(
      date_org[2] == "2015-12-3"  ~ "2015-12-03",
      date_org[2] == "2015-12-7"  ~ "2015-12-07",
      date_org[2] == "2016-08"    ~ "2016-08-01",
      date_org[2] == "2016-10-7"  ~ "2016-10-07",
      date_org[2] == "2016-11-1"  ~ "2016-11-01",
      date_org[2] == "2016-11-3"  ~ "2016-11-03",
      date_org[2] == "2017-06"    ~ "2017-06-01",
      date_org[2] == "0107-05-22" ~ "2017-05-22",
      date_org[2] == "2017-11-1"  ~ "2017-11-01",
      TRUE ~ date_org[2]
    ) %>%
      lubridate::ymd(),
    notice_org = date_org[3] %>% # somtimes the org name is messed up so we need to clean it up
      stri_replace_last_regex("[-]*[[:digit:]]+$", "") %>%
      stri_replace_all_fixed("-", " "),
    notice_content = list(read_lines(.x)) # grab the content
  ) -> ret

  # and there are still some broken org names
  if (stri_detect_fixed(.x, "2017/2017-11-06-1776.md")) {
    ret$notice_org <- "1776"
  }

  ret

}) -> dmca

mutate(
  dmca,
  counter_notice = stri_detect_fixed(notice_org, "counternotice|counter notice"), # handle inconsistency
  retraction = stri_detect_fixed(notice_org, "retraction"),
  notice_org = stri_trans_tolower(notice_org) %>%
    stri_replace_first_regex("\ *(counternotice|counter notice)\ *", "") %>% # clean up org names with tags
    stri_replace_first_regex("\ *retraction\ *", "")
) -> dmca

mutate(
  dmca,
  notice_org = case_when(
    stri_detect_fixed(notice_org, "accenture")        ~ "accenture",
    stri_detect_fixed(notice_org, "adobe")            ~ "adobe",
    stri_detect_fixed(notice_org, "amazon")           ~ "amazon",
    stri_detect_fixed(notice_org, "ansible")          ~ "ansible",
    stri_detect_fixed(notice_org, "aspengrove")       ~ "aspengrove",
    stri_detect_fixed(notice_org, "apple")            ~ "apple",
    stri_detect_fixed(notice_org, "aws")              ~ "aws",
    stri_detect_fixed(notice_org, "blizzard")         ~ "blizzard",
    stri_detect_fixed(notice_org, "o reilly")         ~ "oreilly",
    stri_detect_fixed(notice_org, "random")           ~ "random house",
    stri_detect_fixed(notice_org, "casado")           ~ "casadocodigo",
    stri_detect_fixed(notice_org, "ccp")              ~ "ccp",
    stri_detect_fixed(notice_org, "cisco")            ~ "cisco",
    stri_detect_fixed(notice_org, "cloudsixteen")     ~ "cloud sixteen",
    stri_detect_fixed(notice_org, "collinsharper")    ~ "collins ’harper",
    stri_detect_fixed(notice_org, "contentanalytics") ~ "content analytics",
    stri_detect_fixed(notice_org, "packt")            ~ "packt",
    stri_detect_fixed(notice_org, "penguin")          ~ "penguin",
    stri_detect_fixed(notice_org, "wiley")            ~ "wiley",
    stri_detect_fixed(notice_org, "wind river")       ~ "windriver",
    stri_detect_fixed(notice_org, "windriver")        ~ "windriver",
    stri_detect_fixed(notice_org, "wireframe")        ~ "wireframe shader",
    stri_detect_fixed(notice_org, "listen")           ~ "listen",
    stri_detect_fixed(notice_org, "wpecommerce")      ~ "wpecommerce",
    stri_detect_fixed(notice_org, "yahoo")            ~ "yahoo",
    stri_detect_fixed(notice_org, "youtube")          ~ "youtube",
    stri_detect_fixed(notice_org, "x pressive")       ~ "xpressive",
    stri_detect_fixed(notice_org, "ximalaya")         ~ "ximalaya",
    stri_detect_fixed(notice_org, "pragmatic")        ~ "pragmatic",
    stri_detect_fixed(notice_org, "evadeee")          ~ "evadeee",
    stri_detect_fixed(notice_org, "iaai")             ~ "iaai",
    stri_detect_fixed(notice_org, "line corp")        ~ "line corporation",
    stri_detect_fixed(notice_org, "mediumrare")       ~ "medium rare",
    stri_detect_fixed(notice_org, "profittrailer")    ~ "profit trailer",
    stri_detect_fixed(notice_org, "smartadmin")       ~ "smart admin",
    stri_detect_fixed(notice_org, "microsoft")        ~ "microsoft",
    stri_detect_fixed(notice_org, "monotype")         ~ "monotype",
    stri_detect_fixed(notice_org, "qualcomm")         ~ "qualcomm",
    stri_detect_fixed(notice_org, "pearson")          ~ "pearson",
    stri_detect_fixed(notice_org, "sony")             ~ "sony",
    stri_detect_fixed(notice_org, "oxford")           ~ "oxford",
    stri_detect_fixed(notice_org, "oracle")           ~ "oracle",
    stri_detect_fixed(notice_org, "out fit")          ~ "outfit",
    stri_detect_fixed(notice_org, "nihon")            ~ "nihon",
    stri_detect_fixed(notice_org, "opencv")           ~ "opencv",
    stri_detect_fixed(notice_org, "newsis")           ~ "newsis",
    stri_detect_fixed(notice_org, "nostarch")         ~ "nostarch",
    stri_detect_fixed(notice_org, "stardog")          ~ "stardog",
    stri_detect_fixed(notice_org, "mswindows")        ~ "microsoft",
    stri_detect_fixed(notice_org, "moody")            ~ "moody",
    stri_detect_fixed(notice_org, "minecraft")        ~ "minecraft",
    stri_detect_fixed(notice_org, "medinasoftware")   ~ "medina software",
    stri_detect_fixed(notice_org, "linecorporation")  ~ "line corporation",
    stri_detect_fixed(notice_org, "steroarts")        ~ "stereoarts",
    stri_detect_fixed(notice_org, "mathworks")        ~ "mathworks",
    stri_detect_fixed(notice_org, "tmssoftware")      ~ "tmssoftware",
    stri_detect_fixed(notice_org, "toontown")         ~ "toontown",
    stri_detect_fixed(notice_org, "wahoo")            ~ "wahoo",
    stri_detect_fixed(notice_org, "webkul")           ~ "webkul",
    stri_detect_fixed(notice_org, "whmcs")            ~ "whmcs",
    stri_detect_fixed(notice_org, "viber")            ~ "viber",
    stri_detect_fixed(notice_org, "totalfree")        ~ "totalfreedom",
    stri_detect_fixed(notice_org, "successacademies") ~ "success academies",
    stri_detect_fixed(notice_org, "ecgwaves")         ~ "ecgwaves",
    stri_detect_fixed(notice_org, "synology")         ~ "synology",
    stri_detect_fixed(notice_org, "infistar")         ~ "infistar’",
    stri_detect_fixed(notice_org, "galleria")         ~ "galleria",
    stri_detect_fixed(notice_org, "jadoo")            ~ "jadoo",
    stri_detect_fixed(notice_org, "dofustouch")       ~ "dofus touch",
    stri_detect_fixed(notice_org, "gravityforms")     ~ "gravity forms",
    stri_detect_fixed(notice_org, "fujiannewland")    ~ "fujian newland",
    stri_detect_fixed(notice_org, "dk uk")            ~ "dk",
    stri_detect_fixed(notice_org, "dk us")            ~ "dk",
    stri_detect_fixed(notice_org, "dkuk")             ~ "dk",
    stri_detect_fixed(notice_org, "dkus")             ~ "dk",
    stri_detect_fixed(notice_org, "facet")            ~ "facet",
    stri_detect_fixed(notice_org, "fh admin")         ~ "fhadmin",
    stri_detect_fixed(notice_org, "electronicarts")   ~ "electronic arts",
    stri_detect_fixed(notice_org, "daikonforge")      ~ "daikon forge",
    stri_detect_fixed(notice_org, "corgiengine")      ~ "corgi engine",
    stri_detect_fixed(notice_org, "epicgames")        ~ "epic  games",
    stri_detect_fixed(notice_org, "essentialmode")    ~ "essentialmode",
    stri_detect_fixed(notice_org, "jetbrains")        ~ "jetbrains",
    stri_detect_fixed(notice_org, "foxy")             ~ "foxy themes",
    stri_detect_fixed(notice_org, "cambridgemobile")  ~ "cambridge mobile",
    stri_detect_fixed(notice_org, "offensive")        ~ "offensive security",
    stri_detect_fixed(notice_org, "outfit")           ~ "outfit",
    stri_detect_fixed(notice_org, "haihuan")          ~ "shanghai haihuan",
    stri_detect_fixed(notice_org, "schuster")         ~ "simon & schuster",
    stri_detect_fixed(notice_org, "silicon")          ~ "silicon labs",
    TRUE ~ notice_org
  )) %>%
  arrange(notice_day) -> dmca
