f050_check_result_path <- function(filename) {
  installed <- system.file("check-results", "f050", filename, package = "didbjs")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "check-results", "f050", filename)
}

f050_scan_file_text <- function(paths) {
  unlist(lapply(paths, function(path) {
    size <- file.info(path)$size
    if (!is.finite(size) || size <= 0) {
      return(character())
    }
    bytes <- readBin(path, what = "raw", n = size)
    text <- rawToChar(bytes[bytes != as.raw(0)], multiple = FALSE)
    Encoding(text) <- "latin1"
    text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
    strsplit(text, "\n", fixed = TRUE)[[1]]
  }), use.names = FALSE)
}

f050_private_tokens <- function() {
  c(
    paste0("/", "Users", "/", "aniket"),
    paste0("/", "Users", "/", "aniketpanjwani"),
    paste0("/", "home", "/", "aniket"),
    paste0("het", "zner"),
    paste0("95.", "216.", "14.", "40"),
    paste0("ssh ", "het", "zner"),
    paste0("did", "-", "kirill"),
    paste0("did", "_", "kirill"),
    paste0("did", "-", "kirill", "-", "stata"),
    paste0("/", "tmp", "/", "did", "-", "kirill", "-", "refs"),
    paste0("MacBook", "-", "Pro"),
    paste0("aniket@", "contentquant.io"),
    paste0("oracle", "-", "review"),
    paste0("/", "goal")
  )
}

f050_matching_tokens <- function(lines, tokens = f050_private_tokens()) {
  tokens[vapply(tokens, function(token) {
    any(grepl(token, lines, fixed = TRUE))
  }, logical(1))]
}

test_that("F050 committed package-check gates are successful and offline", {
  manifest <- jsonlite::fromJSON(f050_check_result_path("manifest.json"))
  summary <- readLines(f050_check_result_path("check-summary.txt"), warn = FALSE)

  expect_equal(manifest$schema_version, "f050.package-checks.v1")
  expect_equal(manifest$fixture_id, "F050")
  expect_equal(manifest$status, "success")
  expect_equal(manifest$gates$source_build, "success")
  expect_equal(manifest$gates$clean_install_smoke, "success")
  expect_equal(manifest$gates$r_cmd_check, "success")
  expect_equal(manifest$gates$default_tests_offline, "success")
  expect_true(manifest$gates$no_mandatory_test_skips)
  expect_true(manifest$gates$no_check_errors)
  expect_true(manifest$gates$no_check_warnings)
  expect_true(manifest$gates$no_network_required_by_default)
  expect_true(manifest$gates$no_stata_python_ssh_required_by_default)
  expect_gte(manifest$testthat$assertions, 6000L)
  expect_equal(manifest$testthat$fail, 0L)
  expect_equal(manifest$testthat$warn, 0L)
  expect_equal(manifest$testthat$skip, 0L)
  expect_true(any(grepl("Status: OK", summary, fixed = TRUE)))
  expect_false(any(grepl("\\bERROR\\b|\\bWARNING\\b", summary)))
})

test_that("F050 runtime package files avoid user paths and default network calls", {
  package_root <- system.file(package = "didbjs")
  runtime_dirs <- file.path(package_root, c("R", "help", "html", "Meta", "spec", "bench", "check-results"))
  runtime_files <- unlist(lapply(runtime_dirs[file.exists(runtime_dirs)], function(path) {
    list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  }), use.names = FALSE)
  runtime_files <- runtime_files[file.info(runtime_files)$isdir == FALSE]
  runtime_text <- f050_scan_file_text(runtime_files)

  expect_equal(f050_matching_tokens(runtime_text), character())
  expect_false(any(grepl("download\\.file\\(|install\\.packages\\(|curl::|httr::|\\bwget\\b|\\bssh\\b", runtime_text)))

  test_files <- list.files(testthat::test_path(), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  test_files <- test_files[basename(test_files) != "test-f050-package-check-gates.R"]
  test_text <- f050_scan_file_text(test_files)
  expect_equal(f050_matching_tokens(test_text), character())
  active_network_calls <- grep("download\\.file\\(|install\\.packages\\(|curl::|httr::|\\bwget\\b|system2\\([^\\n]*(ssh|curl|wget)", test_text, value = TRUE)
  expect_equal(active_network_calls, character())
})
