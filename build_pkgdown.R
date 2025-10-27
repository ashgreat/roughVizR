Sys.setenv(R_USER_CACHE_DIR = file.path(getwd(), 'pkgcache'))
options(pkgdown.internet = FALSE)
ns <- asNamespace('pkgdown')
if (bindingIsLocked('cached_dependency', ns)) {
  unlockBinding('cached_dependency', ns)
}
assign('cached_dependency', function(name, version, files) {
  cache_dir <- file.path(tempdir(), 'pkgdown-deps', name, version)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  for (file in files) {
    dest <- file.path(cache_dir, basename(file$url))
    if (!file.exists(dest)) {
      file.create(dest)
    }
  }
  entries <- list.files(cache_dir)
  scripts <- entries[grepl('\\.js$', entries)]
  styles <- entries[grepl('\\.css$', entries)]
  htmltools::htmlDependency(
    name = name,
    version = version,
    src = cache_dir,
    script = scripts,
    stylesheet = styles
  )
}, envir = ns)
if (bindingIsLocked('build_favicons', ns)) {
  unlockBinding('build_favicons', ns)
}
assign('build_favicons', function(pkg, ...) invisible(pkg), envir = ns)
if (bindingIsLocked('cran_link', ns)) {
  unlockBinding('cran_link', ns)
}
assign('cran_link', function(pkg) {
  list(repo = 'GitHub', url = 'https://github.com/ashgreat/roughVizR')
}, envir = ns)
if (bindingIsLocked('build_rmarkdown_article', ns)) {
  unlockBinding('build_rmarkdown_article', ns)
}
assign('build_rmarkdown_article', function(pkg,
                                           input_file,
                                           input_path,
                                           output_file,
                                           output_path,
                                           depth,
                                           seed = NULL,
                                           new_process = TRUE,
                                           pandoc_args = character(),
                                           quiet = TRUE,
                                           call = rlang::caller_env()) {
  cli::cli_inform("Reading {pkgdown:::src_path(input_file)}")
  digest <- pkgdown:::file_digest(output_path)
  data <- pkgdown:::data_article(pkg, input_file, call = call)
  if (data$as_is) {
    if (identical(data$ext, "html")) {
      setup <- pkgdown:::rmarkdown_setup_custom(pkg, input_path, depth = depth, data = data)
    } else {
      setup <- list(format = NULL, options = NULL)
    }
  } else {
    setup <- pkgdown:::rmarkdown_setup_pkgdown(pkg,
      depth = depth,
      data = data,
      pandoc_args = pandoc_args
    )
  }
  pkgdown:::local_envvar_pkgdown(pkg)
  pkgdown:::local_texi2dvi_envvars(input_path)
  withr::local_envvar(R_CLI_NUM_COLORS = 256)
  args <- list(
    input = input_path,
    output_file = fs::path_file(output_path),
    output_dir = fs::path_dir(output_path),
    intermediates_dir = tempdir(),
    encoding = "UTF-8",
    seed = seed,
    output_format = setup$format,
    output_options = setup$options,
    quiet = quiet
  )
  path <- do.call(pkgdown:::rmarkdown_render_with_seed, args)
  is_html <- identical(fs::path_ext(path)[[1]], "html")
  if (is_html) {
    pkgdown:::local_options_link(pkg, depth = depth)
    pkgdown:::update_html(path, pkgdown:::tweak_rmarkdown_html,
      input_path = fs::path_dir(input_path),
      pkg = pkg
    )
    pkgdown:::update_html(path, function(html) {
      pkgdown:::activate_navbar(html, fs::path_rel(path, pkg$dst_path), pkg)
    })
  }
  if (digest != pkgdown:::file_digest(output_path)) {
    pkgdown:::writing_file(fs::path_rel(output_path, pkg$dst_path), output_file)
  }
  if (is_html) {
    pkgdown:::copy_article_images(path, input_path, output_path)
    pkgdown:::check_missing_images(pkg, input_path, output_file)
  }
  invisible(path)
}, envir = ns)

pkgdown::build_site(new_process = FALSE, devel = FALSE)
