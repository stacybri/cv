# Pull Brian's own publications from ORCID + OpenAlex and write them into
# data/publications_auto.csv using the same schema as data/positions.csv,
# so resume.Rmd can bind_rows() the two without any special-casing.
#
# ORCID is treated as the authoritative "this is mine" list (self-curated);
# OpenAlex is used to enrich each matched work with venue, co-authors, and
# citation counts. This avoids the noise OpenAlex's algorithmic author
# disambiguation can introduce (see docs.openalex.org authorship-object notes).
#
# NOTE: jsonlite's default simplifyDataFrame=TRUE silently turns arrays of
# same-shaped JSON objects (like ORCID's "group" array) into a data.frame,
# which breaks row-wise iteration with map()/map_dfr(). Always parse with
# simplifyDataFrame = FALSE here and navigate the resulting nested lists.
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

orcid_id <- "0000-0002-3039-2004"
contact_email <- "stacybw@gmail.com"

normalize_doi <- function(doi) {
  if (is.null(doi) || length(doi) == 0) return(NA_character_)
  tolower(str_remove(doi, "^https?://doi\\.org/"))
}

# --- 1. ORCID: authoritative list of Brian's own works ---------------------
orcid_resp <- GET(
  glue("https://pub.orcid.org/v3.0/{orcid_id}/works"),
  add_headers(Accept = "application/json")
)
stop_for_status(orcid_resp)
orcid_json <- content(orcid_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)

orcid_works <- map_dfr(orcid_json$group, function(g) {
  ws <- g[["work-summary"]][[1]]
  eids <- ws[["external-ids"]][["external-id"]]
  doi <- NA_character_
  for (eid in eids) {
    if (identical(tolower(eid[["external-id-type"]] %||% ""), "doi")) {
      doi <- normalize_doi(eid[["external-id-value"]])
    }
  }
  tibble(
    orcid_title = ws[["title"]][["title"]][["value"]] %||% NA_character_,
    doi = doi
  )
})

# --- 2. OpenAlex: enrichment (venue, co-authors, citation counts, links) ---
author_resp <- GET(
  glue("https://api.openalex.org/authors/orcid:{orcid_id}"),
  query = list(mailto = contact_email)
)
stop_for_status(author_resp)
author_json <- content(author_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)
works_api_url <- author_json$works_api_url

fetch_all_openalex_works <- function(works_api_url) {
  page <- 1
  all_results <- list()
  repeat {
    resp <- GET(works_api_url, query = list(mailto = contact_email, `per-page` = 200, page = page))
    stop_for_status(resp)
    page_json <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(simplifyDataFrame = FALSE)
    results <- page_json$results
    if (length(results) == 0) break
    all_results <- c(all_results, results)
    if (length(results) < 200) break
    page <- page + 1
  }
  all_results
}

openalex_works <- fetch_all_openalex_works(works_api_url)

known_institutions <- c("world bank", "usda", "economic research service", "michigan state")

openalex_df <- map_dfr(openalex_works, function(w) {
  doi <- normalize_doi(w$doi)
  authorships <- w$authorships
  author_names <- if (!is.null(authorships)) {
    map_chr(authorships, ~ .x$author$display_name %||% NA_character_)
  } else {
    character(0)
  }
  is_stacy <- str_detect(author_names, fixed("Stacy"))
  coauthors <- author_names[!is_stacy]
  # A few OpenAlex works list the same co-author twice under different name
  # order/variants (e.g. "Patrick Canning" and "Canning, Patrick") that
  # weren't merged to one author id - dedupe by a word-set comparison.
  name_key <- map_chr(coauthors, ~ paste(sort(str_split(str_to_lower(.x), "[^a-z]+")[[1]]), collapse = " "))
  coauthors <- coauthors[!duplicated(name_key)]
  venue <- w$primary_location$source$display_name %||% NA_character_
  link <- w$doi %||% w$primary_location$landing_page_url %||% NA_character_
  # Institutions listed against Brian's own authorship entry (used below as a
  # fallback signal for works not yet registered on ORCID).
  stacy_institutions <- if (any(is_stacy)) {
    authorships[is_stacy] %>%
      map(~ .x$institutions) %>%
      unlist(recursive = FALSE) %>%
      map_chr(~ .x$display_name %||% NA_character_)
  } else {
    character(0)
  }
  tibble(
    title = w$title %||% NA_character_,
    doi = doi,
    year = w$publication_year %||% NA_integer_,
    venue = venue,
    coauthors = paste(coauthors, collapse = ", "),
    cited_by_count = w$cited_by_count %||% 0,
    link = link,
    type = w$type %||% NA_character_,
    known_institution_match = length(stacy_institutions) > 0 &&
      any(map_lgl(known_institutions, ~ any(str_detect(str_to_lower(stacy_institutions), fixed(.x))))),
    counts_by_year = list(w$counts_by_year)
  )
})

# Data deposits (Figshare/ICPSR replication files, etc.) aren't "writing" -
# drop them before matching so they never show up as CV entries.
openalex_df <- openalex_df %>% filter(!type %in% c("dataset", "supplementary-materials"))

# --- 3. Restrict OpenAlex results to works that are plausibly Brian's own:
# matched against his self-registered ORCID list (by DOI, falling back to a
# normalized substring title match - see below), OR his own authorship entry
# on the paper lists one of his known institutions. The ORCID-only version of
# this filter dropped legitimate recent papers that hadn't been added to his
# ORCID record yet (e.g. a 2025 World Development article, still ORCID-less,
# whose 2023 working-paper precursor *was* on ORCID) - the institution check
# catches those without opening the door to a different "Brian Stacy". ---
norm_title <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>% str_squish()
}

orcid_dois <- orcid_works$doi[!is.na(orcid_works$doi)]
orcid_titles_norm <- unique(na.omit(norm_title(orcid_works$orcid_title)))
orcid_titles_norm <- orcid_titles_norm[nchar(orcid_titles_norm) > 8] # drop junk/short entries

openalex_df <- openalex_df %>%
  mutate(
    norm_title = norm_title(title),
    title_matches = map_lgl(norm_title, function(t) {
      if (is.na(t) || t == "") return(FALSE)
      any(str_detect(orcid_titles_norm, fixed(t)) | str_detect(t, fixed(orcid_titles_norm)))
    })
  )

matched <- openalex_df %>%
  filter((!is.na(doi) & doi %in% orcid_dois) | title_matches | known_institution_match) %>%
  distinct(title, .keep_all = TRUE)

# Working papers often get re-listed once the peer-reviewed version comes
# out (same paper, multiple OpenAlex records - a "Working Paper #NN" suffix,
# a preprint, and the final journal article, sometimes all typed "article").
# A "Working Paper #NN" suffix strip only catches title changes of that exact
# shape; some papers get retitled more loosely between the preprint and final
# version (e.g. "A Comparison of Growth Percentile..." became "...Student
# Growth Percentile..."). Cluster by word-set (Jaccard) similarity instead, so
# any two titles sharing most of their words collapse to one entry, keeping
# the most recent year (the final published version) - not the highest
# citation count, since OpenAlex splits citations across duplicate records
# and an SSRN preprint can show more than the journal version it was
# superseded by.
strip_wp_suffix <- function(t) str_remove(t, "\\s*working paper\\s*#?\\s*\\d*\\.?\\s*$")
word_set <- function(t) unique(str_split(t, "\\s+")[[1]])
jaccard_sim <- function(a, b) {
  sa <- word_set(a); sb <- word_set(b)
  length(intersect(sa, sb)) / length(union(sa, sb))
}

matched <- matched %>% mutate(dedup_key = strip_wp_suffix(norm_title))

keys <- unique(matched$dedup_key)
cluster_id <- setNames(seq_along(keys), keys)
if (length(keys) > 1) {
  for (i in seq_len(length(keys) - 1)) {
    for (j in seq((i + 1), length(keys))) {
      if (jaccard_sim(keys[i], keys[j]) > 0.75) {
        old_id <- cluster_id[[keys[j]]]
        new_id <- cluster_id[[keys[i]]]
        cluster_id[cluster_id == old_id] <- new_id
      }
    }
  }
}

matched <- matched %>%
  mutate(cluster_id = cluster_id[dedup_key]) %>%
  group_by(cluster_id) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-cluster_id, -dedup_key)

# --- 4. Shape into data/positions.csv schema and write out ------------------
publications_auto <- matched %>%
  transmute(
    section = "academic_articles",
    in_resume = TRUE,
    title = title,
    loc = coalesce(venue, "N/A"),
    institution = "N/A",
    start = year,
    end = year,
    description_1 = ifelse(coauthors == "", NA_character_, paste0("Authored with ", coauthors, ".")),
    description_2 = paste0("Cited by ", cited_by_count, " (OpenAlex)"),
    description_3 = ifelse(is.na(link), NA_character_, paste0("[link](", link, ")"))
  ) %>%
  arrange(desc(end))

write_csv(publications_auto, "data/publications_auto.csv", na = "NA")
cat(glue("Wrote {nrow(publications_auto)} publications to data/publications_auto.csv\n"))

# --- 5. Aggregate per-year citation counts across matched works, for the
# "Citations" bar chart in resume.Rmd (replaces the old scholar::get_citation_history) ---
citations_by_year <- matched %>%
  pull(counts_by_year) %>%
  compact() %>%
  map_dfr(function(years_list) {
    map_dfr(years_list, function(y) tibble(year = y$year, cited_by_count = y$cited_by_count))
  }) %>%
  group_by(year) %>%
  summarise(cites = sum(cited_by_count), .groups = "drop") %>%
  arrange(year)

write_csv(citations_by_year, "data/citations_by_year_auto.csv")
cat(glue("Wrote {nrow(citations_by_year)} year rows to data/citations_by_year_auto.csv\n"))
