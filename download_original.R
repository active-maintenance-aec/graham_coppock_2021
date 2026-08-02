# graham_coppock_2021/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every file
#   against the manifest. Run this once before running anything in maintained/;
#   run_all.R sources it first and again at the end. Re-running is free: files already
#   present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. All twelve agree here,
#   but they do not always: another deposit in this program carries three published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against md5_served and any disagreement is reported.
#
#   Five of the twelve files were ingested by Dataverse into tabular .tab
#   representations; the served_as column records the name Dataverse gives the derived
#   file and the unf column its Universal Numeric Fingerprint. `?format=original`
#   returns the deposited .csv bytes in every case, so the UNF is recorded rather than
#   checked: it describes the derived table, which nothing here downloads.
#
#   The three checks below are gates, not diagnostics. A script that writes into
#   original/ would corrupt the deposit silently, and the size and extra-file checks
#   are what catch an overwrite or a stray file that an MD5 comparison alone would miss.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/GFF78K"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than the tabular
# representation Dataverse derives for ingested files.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify checksums and sizes ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_on_disk = file.size(path),
    md5_ok = md5_downloaded == md5_served,
    bytes_ok = bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_served, md5_downloaded, md5_ok,
         published_agrees)

print(verified |> select(file, bytes, bytes_ok, md5_ok, published_agrees), n = nrow(verified))

if (!all(verified$md5_ok)) {
  stop("Checksum mismatch in original/: ",
       paste(verified$file[!verified$md5_ok], collapse = ", "),
       ". Delete the offending files and re-run to refetch them from Dataverse.")
}

if (!all(verified$bytes_ok)) {
  stop("Byte size mismatch in original/: ",
       paste(verified$file[!verified$bytes_ok], collapse = ", "), ".")
}

# original/ must hold the deposit and nothing else ----
# An archive script that writes into its own directory adds files as well as
# overwriting them, and a stray output or a bulk-download zip makes a fresh clone
# differ from the working copy.
extra <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)

if (length(extra) > 0) {
  stop("original/ holds files the manifest does not list: ",
       paste(extra, collapse = ", "),
       ". Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("All {nrow(verified)} files match on MD5 and byte size, and original/ holds ",
               "nothing else. {sum(!verified$published_agrees)} carry a published checksum ",
               "that disagrees with what Dataverse serves."))
print(str_glue("Archive: {dataset_doi}"))
