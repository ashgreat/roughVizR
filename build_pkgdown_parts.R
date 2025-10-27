Sys.setenv(R_USER_CACHE_DIR = file.path(getwd(), 'pkgcache'))
options(pkgdown.internet = FALSE)
assignInNamespace <- get('assignInNamespace', asNamespace('base'))
assignInNamespace('cached_dependency', function(name, version, files) {
  cache_dir <- file.path(tempdir(), 'pkgdown-deps', name, version)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  for (file in files) {
    dest <- file.path(cache_dir, basename(file$url))
    if (!file.exists(dest)) file.create(dest)
  }
  entries <- list.files(cache_dir)
  scripts <- entries[grepl('\\.js$', entries)]
  styles <- entries[grepl('\\.css$', entries)]
  htmltools::htmlDependency(name = name, version = version, src = cache_dir, script = scripts, stylesheet = styles)
}, ns = 'pkgdown')
assignInNamespace('build_favicons', function(pkg, ...) invisible(pkg), ns = 'pkgdown')
assignInNamespace('cran_link', function(pkg) list(repo = 'GitHub', url = 'https://github.com/ashgreat/roughVizR'), ns = 'pkgdown')

pkg <- pkgdown:::as_pkgdown('.')
pkgdown:::build_site_meta(pkg)
pkgdown:::build_reference(pkg)
pkgdown:::build_articles(pkg, lazy = FALSE)
pkgdown:::build_news(pkg)
