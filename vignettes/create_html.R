# build_docs_site.R
# ------------------------------------------------------------
# SPASAM.MSE GitHub Pages site builder (OFFICIAL)
#
# What this script does
# - Renders vignettes/news/bug_report Rmd -> docs/
# - Flattens nested HTML into docs/ (so relative links work)
# - Writes docs/index.html (landing page)
# - Injects a single custom navbar + CSS + JS into all other docs/*.html
#
# IMPORTANT CHANGE (per your request):
# - Icons/logos are sourced ONLINE from GitHub (raw.githubusercontent.com)
# - No local copying of icons into docs/icons (prevents "deleted icons reappear")
# - A version string is computed each build and shown on the landing page + navbar
#
# UPDATED (per your latest request):
# - "Build: vX.X.X • YYYY-MM-DD" appears SMALL on the RIGHT of "SPASAM.MSE"
#   (near the title in the main hero section)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(rmarkdown)
})

# ----------------------------
# Settings
# ----------------------------
main_dir <- "C:/Users/liche/Desktop/SPASAM.MSE/"
setwd(main_dir)

pkg_name   <- "SPASAM.MSE"
repo_owner <- "lichengxue"
repo_slug  <- "SPASAM.MSE"
repo_base  <- sprintf("https://github.com/%s/%s", repo_owner, repo_slug)
branch     <- "main"

# Main developer
main_dev_name   <- "Chengxue Li"
email_primary   <- "chengxue.li@stonybrook.edu"
email_secondary <- "chengxue.li@noaa.gov"

wham_link <- "https://timjmiller.github.io/wham/"

docs_dir  <- "docs"

# Where icons live INSIDE your repo (path relative to repo root)
# (Your script comment says: docs/icons/*.png)
icons_path_in_repo <- "docs/icons"

# Core SPASAM members (landing page)
core_members <- list(
  list(name = "Jonathan Deroba",   org = "NOAA Federal (NEFSC)", email = "jonathan.deroba@noaa.gov"),
  list(name = "Daniel Goethel",    org = "NOAA Federal (AFSC)",  email = "daniel.goethel@noaa.gov"),
  list(name = "Aaron Berger",      org = "NOAA Federal (NWFSC)", email = "aaron.berger@noaa.gov"),
  list(name = "Amy Schueller",     org = "NOAA Federal (SEFSC)", email = "amy.schueller@noaa.gov"),
  list(name = "Brian Langseth",    org = "NOAA Federal (NWFSC)", email = "brian.langseth@noaa.gov"),
  list(name = "Dana Hanselman",    org = "NOAA Federal (AFSC)",  email = "dana.hanselman@noaa.gov"),
  list(name = "Emily Liljestrand", org = "NOAA Federal (NEFSC)", email = "emily.liljestrand@noaa.gov"),
  list(name = "Timothy Miller",    org = "NOAA Federal (NEFSC)", email = "timothy.j.miller@noaa.gov")
)

# ----------------------------
# Helpers
# ----------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ---- Version helpers ----
get_site_version <- function() {
  has_git <- nzchar(Sys.which("git"))
  is_git_repo <- dir.exists(".git")
  
  if (has_git && is_git_repo) {
    ver <- tryCatch(
      system("git describe --tags --always --dirty", intern = TRUE),
      error = function(e) ""
    )
    if (length(ver) && nzchar(ver[1])) return(paste0("v", ver[1]))
  }
  
  desc_ver <- tryCatch({
    d <- read.dcf("DESCRIPTION")
    as.character(d[1, "Version"])
  }, error = function(e) "0.0.0")
  
  paste0("v", desc_ver)
}

site_version <- get_site_version()
build_date   <- format(Sys.Date(), "%Y-%m-%d")

# Online (raw GitHub) icon URL
icon_url <- function(fname) {
  # e.g. https://raw.githubusercontent.com/lichengxue/SPASAM.MSE/main/docs/icons/home_icon.png
  sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s/%s",
          repo_owner, repo_slug, branch, icons_path_in_repo, fname)
}

render_many <- function(files, output_dir = docs_dir) {
  if (length(files) == 0) return(invisible(NULL))
  for (f in files) {
    rmarkdown::render(input = f, output_dir = output_dir, quiet = TRUE)
  }
  invisible(NULL)
}

flatten_html <- function(root = docs_dir) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  htmls <- list.files(root, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
  
  for (f in htmls) {
    f_norm <- normalizePath(f, winslash = "/", mustWork = TRUE)
    target <- file.path(root, basename(f_norm))
    
    if (dirname(f_norm) == root) next
    if (normalizePath(target, winslash = "/", mustWork = FALSE) == f_norm) next
    
    file.copy(f_norm, target, overwrite = TRUE)
  }
  invisible(NULL)
}

to_html <- function(rmd_files) sub("\\.Rmd$", ".html", basename(rmd_files))

build_vignette_links <- function(vignette_files) {
  if (length(vignette_files) == 0) return('<a href="#">(No vignettes yet)</a>')
  
  vignettes <- lapply(vignette_files, function(file) {
    nm  <- basename(file)
    ttl <- gsub("_", " ", sub("\\.Rmd$", "", nm))
    url <- sub("\\.Rmd$", ".html", nm)
    list(name = ttl, url = url)
  })
  
  paste0(
    vapply(vignettes, function(v) sprintf('<a href="%s">%s</a>', v$url, v$name), character(1)),
    collapse = "\n"
  )
}

build_members_html <- function(members) {
  paste0(
    vapply(members, function(m) {
      sprintf(
        '<div class="member"><b>%s</b> — %s &lt;<a href="mailto:%s">%s</a>&gt;</div>',
        m$name, m$org, m$email, m$email
      )
    }, character(1)),
    collapse = "\n"
  )
}

# ----------------------------
# 0) Ensure docs/
# ----------------------------
ensure_dir(docs_dir)

# ----------------------------
# 1) Render Rmd pages into docs/
# ----------------------------
vignette_files   <- list.files("vignettes",  pattern = "\\.Rmd$", full.names = TRUE)
news_files       <- list.files("news",       pattern = "\\.Rmd$", full.names = TRUE)
bug_report_files <- list.files("bug_report", pattern = "\\.Rmd$", full.names = TRUE)

render_many(vignette_files, docs_dir)
render_many(news_files, docs_dir)
render_many(bug_report_files, docs_dir)

# Flatten nested HTML so links work from root docs/
flatten_html(docs_dir)

# ----------------------------
# 2) Navigation link targets
# ----------------------------
vignette_links <- build_vignette_links(vignette_files)

news_html <- if (length(news_files) > 0) to_html(news_files)[1] else "#"
bug_html  <- if (length(bug_report_files) > 0) to_html(bug_report_files)[1] else "#"

# ----------------------------
# 3) NAVBAR HTML (Source before Projects) + version badge
# ----------------------------
navbar_html <- c(
  '  <!-- SPASAM_NAV_START -->',
  '  <div class="nav spasam-nav">',
  '    <div class="nav-inner">',
  '      <div class="brand"><span class="dot"></span><span>SPASAM-MSE</span></div>',
  '      <div class="nav-links">',
  sprintf('        <a href="index.html"><img class="icon" src="%s" alt="Home" />Home</a>', icon_url("home_icon.png")),
  '        <div class="dropdown" id="vignetteDropdown">',
  sprintf('          <button class="dropbtn" type="button"><img class="icon" src="%s" alt="Vignettes" />Vignettes</button>', icon_url("vignettes_icon.png")),
  '          <div class="dropdown-content">',
  vignette_links,
  '          </div>',
  '        </div>',
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="Source" />Source</a>', repo_base, icon_url("source_icon.png")),
  sprintf('        <a href="projects.html"><img class="icon" src="%s" alt="Projects" />Projects</a>', icon_url("projects_icon.png")),
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="News" />News</a>', news_html, icon_url("news_icon.png")),
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="Bug report" />Bug report</a>', bug_html, icon_url("bug_report_icon.png")),
  sprintf('        <a href="mailto:%s"><img class="icon" src="%s" alt="Contact" />Contact</a>', email_primary, icon_url("contact_icon.png")),
  '      </div>',
  sprintf('      <div class="nav-version"><span class="kbd">%s</span></div>', site_version),
  '    </div>',
  '  </div>',
  '  <!-- SPASAM_NAV_END -->'
)

# ----------------------------
# 4) CSS + JS injection (site-wide)
# ----------------------------
inject_css <- c(
  "<style>",
  "  :root{",
  "    --bg:#0b1220; --bg2:#0f1b33; --text:#e9eefc; --muted:#b7c3e6;",
  "    --line:rgba(255,255,255,.10); --accent:#6ea8ff; --accent2:#7ef0d4;",
  "    --shadow:0 10px 30px rgba(0,0,0,.35); --radius:18px;",
  "  }",
  "  body{",
  "    background:",
  "      radial-gradient(1000px 600px at 20% -10%, rgba(110,168,255,.25), transparent 60%),",
  "      radial-gradient(800px 600px at 90% 10%, rgba(126,240,212,.18), transparent 55%),",
  "      linear-gradient(180deg,var(--bg),var(--bg2)) !important;",
  "    color: var(--text) !important;",
  "  }",
  "  a{color:var(--accent) !important;}",
  
  "  /* Hide default rmarkdown/bootstrap navbar */",
  "  .navbar, nav.navbar, .navbar.navbar-default, .navbar.navbar-inverse,",
  "  .navbar.navbar-fixed-top, .navbar.navbar-static-top, #navbar{",
  "    display:none !important;",
  "  }",
  
  "  /* Remove reserved top padding */",
  "  html, body{ margin:0 !important; }",
  "  body{ padding-top:0 !important; }",
  "  .main-container, .container-fluid.main-container, .toc-content, .content{",
  "    padding-top:0 !important;",
  "    margin-top:0 !important;",
  "  }",
  "  body[style*='padding-top']{ padding-top:0 !important; }",
  "  .main-container[style*='padding-top']{ padding-top:0 !important; }",
  
  "  /* Navbar */",
  "  .nav.spasam-nav{",
  "    position:sticky !important;",
  "    top:0 !important;",
  "    z-index:999999 !important;",
  "    backdrop-filter:blur(10px);",
  "    background:rgba(11,18,32,.65);",
  "    border-bottom:1px solid rgba(255,255,255,.10);",
  "  }",
  "  .nav-inner{max-width:1100px;margin:0 auto;padding:10px 18px;display:flex;",
  "    gap:12px;align-items:center;justify-content:space-between;flex-wrap:wrap;}",
  "  .brand{display:flex;align-items:center;gap:10px;font-weight:800;letter-spacing:.3px;}",
  "  .brand .dot{width:10px;height:10px;border-radius:999px;",
  "    background:linear-gradient(135deg,var(--accent),var(--accent2));",
  "    box-shadow:0 0 0 4px rgba(110,168,255,.15);}",
  "  .nav-links{display:flex;align-items:center;gap:8px;flex-wrap:wrap;}",
  "  .nav-links a,.dropbtn{display:inline-flex;align-items:center;gap:8px;padding:10px 12px;",
  "    border-radius:999px;color:var(--text) !important;border:1px solid rgba(255,255,255,.08);",
  "    background:rgba(255,255,255,.04);font-size:14px;}",
  "  .nav-links a:hover,.dropbtn:hover{background:rgba(255,255,255,.08);text-decoration:none;}",
  "  .icon{width:18px;height:18px;object-fit:contain;}",
  "  .dropdown{position:relative;}",
  "  .dropbtn{cursor:pointer;}",
  "  .dropdown-content{display:none;position:absolute;top:40px;left:0;min-width:260px;padding:10px;",
  "    border-radius:14px;background:rgba(15,26,47,.98);border:1px solid rgba(255,255,255,.10);",
  "    box-shadow:var(--shadow);z-index:99999;}",
  "  .dropdown-content a{display:block;padding:10px 12px;border-radius:10px;color:var(--text) !important;}",
  "  .dropdown-content a:hover{background:rgba(255,255,255,.06);text-decoration:none;}",
  "  .dropdown:hover .dropdown-content{display:block;}",
  "  .dropdown.open .dropdown-content{display:block;}",
  
  "  .nav-version{margin-left:auto;}",
  
  "  .kbd{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;",
  "    background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.10);",
  "    padding:2px 6px;border-radius:8px;font-size:12px;color:var(--text);}",
  
  "  /* Dark sidebar/TOC */",
  "  .tocify, .tocify-wrapper, #TOC{",
  "    background: rgba(15,26,47,.95) !important;",
  "    border: 1px solid rgba(255,255,255,.10) !important;",
  "    box-shadow: var(--shadow) !important;",
  "  }",
  "  .tocify .list-group-item, .tocify-wrapper .list-group-item, #TOC .list-group-item{",
  "    background: transparent !important;",
  "    color: var(--text) !important;",
  "    border-color: rgba(255,255,255,.10) !important;",
  "  }",
  "  .tocify .list-group-item:hover, .tocify-wrapper .list-group-item:hover, #TOC .list-group-item:hover{",
  "    background: rgba(255,255,255,.06) !important;",
  "  }",
  "  .tocify .list-group-item.active, .tocify-wrapper .list-group-item.active, #TOC .list-group-item.active{",
  "    background: rgba(110,168,255,.18) !important;",
  "    color: var(--text) !important;",
  "    border-radius: 10px !important;",
  "  }",
  "  .tocify a, .tocify-wrapper a, #TOC a{ color: var(--text) !important; }",
  "  .tocify a:hover, .tocify-wrapper a:hover, #TOC a:hover{ color: var(--accent) !important; text-decoration:none !important; }",
  
  "  /* prevent white blocks */",
  "  .main-container, .container, .container-fluid, .page-inner, .row,",
  "  .col-xs-12, .col-sm-12, .col-md-9, .col-md-3{",
  "    background: transparent !important;",
  "  }",
  
  "  pre, code{ background: rgba(255,255,255,.06) !important; color: var(--text) !important;",
  "    border:1px solid rgba(255,255,255,.10) !important; }",
  "</style>"
)

inject_js <- c(
  "<script>",
  "  (function(){",
  "    function killTopPadding(){",
  "      try {",
  "        document.documentElement.style.marginTop = '0px';",
  "        document.body.style.paddingTop = '0px';",
  "        document.body.style.marginTop = '0px';",
  "        var mc = document.querySelector('.main-container, .container-fluid.main-container');",
  "        if(mc){ mc.style.paddingTop='0px'; mc.style.marginTop='0px'; }",
  "      } catch(e) {}",
  "    }",
  "    killTopPadding();",
  "    window.addEventListener('load', killTopPadding);",
  "    setTimeout(killTopPadding, 50);",
  "    setTimeout(killTopPadding, 250);",
  
  "    var dd = document.getElementById('vignetteDropdown');",
  "    if(!dd) return;",
  "    var btn = dd.querySelector('.dropbtn');",
  "    if(!btn) return;",
  "    btn.addEventListener('click', function(e){",
  "      e.stopPropagation();",
  "      dd.classList.toggle('open');",
  "    });",
  "    document.addEventListener('click', function(){",
  "      dd.classList.remove('open');",
  "    });",
  "  })();",
  "</script>"
)

# ----------------------------
# 5) Build docs/index.html (landing page)
# ----------------------------
members_html <- build_members_html(core_members)

main_dev_html <- sprintf(
  '<div class="small" style="margin-top:6px;"><b>Main developer</b>: %s &lt;<a href="mailto:%s">%s</a>&gt;</div>',
  main_dev_name, email_primary, email_primary
)

alt_email_html <- sprintf(
  '<div class="small" style="margin-top:4px;"><b>Alt</b>: &lt;<a href="mailto:%s">%s</a>&gt;</div>',
  email_secondary, email_secondary
)

index_out <- c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <meta charset="utf-8" />',
  '  <meta name="viewport" content="width=device-width, initial-scale=1" />',
  sprintf('  <title>%s | Spatial Processes and Stock Assessment Methods - MSE</title>', pkg_name),
  '  <style>',
  '    :root{',
  '      --bg:#0b1220; --bg2:#0f1b33; --text:#e9eefc; --muted:#b7c3e6;',
  '      --line:rgba(255,255,255,.10); --accent:#6ea8ff; --accent2:#7ef0d4;',
  '      --shadow:0 10px 30px rgba(0,0,0,.35); --radius:18px;',
  '    }',
  '    *{box-sizing:border-box;}',
  '    body{margin:0;font-family:ui-sans-serif,-apple-system,Segoe UI,Roboto,Arial,sans-serif;',
  '      color:var(--text);line-height:1.6;',
  '      background:',
  '        radial-gradient(1000px 600px at 20% -10%, rgba(110,168,255,.25), transparent 60%),',
  '        radial-gradient(800px 600px at 90% 10%, rgba(126,240,212,.18), transparent 55%),',
  '        linear-gradient(180deg,var(--bg),var(--bg2));}',
  '    a{color:var(--accent);text-decoration:none;} a:hover{text-decoration:underline;}',
  '    .nav{position:sticky;top:0;z-index:50;backdrop-filter:blur(10px);',
  '      background:rgba(11,18,32,.65);border-bottom:1px solid var(--line);}',
  '    .nav-inner{max-width:1100px;margin:0 auto;padding:10px 18px;display:flex;',
  '      gap:12px;align-items:center;justify-content:space-between;flex-wrap:wrap;}',
  '    .brand{display:flex;align-items:center;gap:10px;font-weight:800;letter-spacing:.3px;}',
  '    .brand .dot{width:10px;height:10px;border-radius:999px;',
  '      background:linear-gradient(135deg,var(--accent),var(--accent2));',
  '      box-shadow:0 0 0 4px rgba(110,168,255,.15);}',
  '    .nav-links{display:flex;align-items:center;gap:8px;flex-wrap:wrap;}',
  '    .nav-links a,.dropbtn{display:inline-flex;align-items:center;gap:8px;padding:10px 12px;',
  '      border-radius:999px;color:var(--text);border:1px solid rgba(255,255,255,.08);',
  '      background:rgba(255,255,255,.04);font-size:14px;}',
  '    .nav-links a:hover,.dropbtn:hover{background:rgba(255,255,255,.08);text-decoration:none;}',
  '    .icon{width:18px;height:18px;object-fit:contain;}',
  '    .dropdown{position:relative;}',
  '    .dropbtn{cursor:pointer;}',
  '    .dropdown-content{display:none;position:absolute;top:40px;left:0;min-width:260px;padding:10px;',
  '      border-radius:14px;background:rgba(15,26,47,.98);border:1px solid rgba(255,255,255,.10);',
  '      box-shadow:0 10px 30px rgba(0,0,0,.35);z-index:9999;}',
  '    .dropdown-content a{display:block;padding:10px 12px;border-radius:10px;color:var(--text);}',
  '    .dropdown-content a:hover{background:rgba(255,255,255,.06);text-decoration:none;}',
  '    .dropdown:hover .dropdown-content{display:block;}',
  '    .dropdown.open .dropdown-content{display:block;}',
  '    .wrap{max-width:1100px;margin:0 auto;padding:26px 18px 80px;}',
  '    .hero{margin-top:18px;border:1px solid rgba(255,255,255,.10);background:rgba(255,255,255,.04);',
  '      border-radius:18px;box-shadow:0 10px 30px rgba(0,0,0,.35);padding:28px;}',
  '    .hero-grid{display:grid;grid-template-columns:1.35fr .65fr;gap:18px;}',
  '    @media (max-width:900px){.hero-grid{grid-template-columns:1fr;}}',
  '    h1{margin:0 0 6px 0;font-size:34px;line-height:1.15;}',
  '    h2{margin:0 0 10px 0;font-size:18px;}',
  '    .subtitle{color:var(--muted);margin:10px 0 0 0;font-size:15px;}',
  '    .pillrow{margin-top:14px;display:flex;gap:10px;flex-wrap:wrap;}',
  '    .pill{padding:8px 10px;border-radius:999px;background:rgba(255,255,255,.06);',
  '      border:1px solid rgba(255,255,255,.10);font-size:13px;white-space:nowrap;}',
  '    .logo-row{margin-top:18px;display:flex;align-items:center;gap:22px;flex-wrap:wrap;}',
  '    .logo-img{height:280px;width:auto;object-fit:contain;filter:drop-shadow(0 15px 34px rgba(0,0,0,.45));}',
  '    @media (max-width:900px){.logo-img{height:210px;}}',
  '    .card{border-radius:18px;border:1px solid rgba(255,255,255,.10);',
  '      background:rgba(255,255,255,.04);box-shadow:0 10px 30px rgba(0,0,0,.35);padding:18px;}',
  '    .card p{margin:0;color:var(--muted);}',
  '    .members{margin-top:12px;padding-top:12px;border-top:1px solid rgba(255,255,255,.10);}',
  '    .members .member{margin:8px 0;color:var(--muted);font-size:13px;line-height:1.45;}',
  '    .members .member b{color:var(--text);} ',
  '    .section{margin-top:18px;display:grid;grid-template-columns:1fr 1fr;gap:18px;}',
  '    @media (max-width:900px){.section{grid-template-columns:1fr;}}',
  '    .list{margin:10px 0 0 0;padding:0;list-style:none;}',
  '    .list li{margin:10px 0;padding-left:16px;position:relative;color:var(--text);}',
  '    .list li:before{content:"";position:absolute;left:0;top:9px;width:7px;height:7px;border-radius:999px;',
  '      background:linear-gradient(135deg,var(--accent),var(--accent2));}',
  '    .footer{margin-top:26px;color:rgba(233,238,252,.85);display:flex;justify-content:space-between;',
  '      gap:12px;flex-wrap:wrap;border-top:1px solid var(--line);padding-top:16px;}',
  '    .small{color:var(--muted);font-size:13px;}',
  '    .kbd{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;',
  '      background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.10);',
  '      padding:2px 6px;border-radius:8px;font-size:12px;color:var(--text);}',
  
  # NEW: title row with build tag on right
  '    .h1row{display:flex;align-items:baseline;justify-content:space-between;gap:12px;flex-wrap:wrap;}',
  '    .buildtag{color:var(--muted);font-size:12px;white-space:nowrap;}',
  '  </style>',
  '</head>',
  '<body>',
  
  # NAV (NO build string here anymore)
  '  <div class="nav">',
  '    <div class="nav-inner">',
  '      <div class="brand"><span class="dot"></span><span>SPASAM-MSE</span></div>',
  '      <div class="nav-links">',
  sprintf('        <a href="index.html"><img class="icon" src="%s" alt="Home" />Home</a>', icon_url("home_icon.png")),
  '        <div class="dropdown" id="vignetteDropdown">',
  sprintf('          <button class="dropbtn" type="button"><img class="icon" src="%s" alt="Vignettes" />Vignettes</button>', icon_url("vignettes_icon.png")),
  '          <div class="dropdown-content">',
  vignette_links,
  '          </div>',
  '        </div>',
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="Source" />Source</a>', repo_base, icon_url("source_icon.png")),
  sprintf('        <a href="projects.html"><img class="icon" src="%s" alt="Projects" />Projects</a>', icon_url("projects_icon.png")),
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="News" />News</a>', news_html, icon_url("news_icon.png")),
  sprintf('        <a href="%s"><img class="icon" src="%s" alt="Bug report" />Bug report</a>', bug_html, icon_url("bug_report_icon.png")),
  sprintf('        <a href="mailto:%s"><img class="icon" src="%s" alt="Contact" />Contact</a>', email_primary, icon_url("contact_icon.png")),
  '      </div>',
  '    </div>',
  '  </div>',
  
  # CONTENT
  '  <div class="wrap">',
  '    <div class="hero">',
  '      <div class="hero-grid">',
  '        <div>',
  
  # NEW: H1 row with build on right
  '          <div class="h1row">',
  sprintf('            <h1>%s</h1>', pkg_name),
  sprintf('            <div class="buildtag">Build: <span class="kbd">%s</span> • %s</div>', site_version, build_date),
  '          </div>',
  
  '          <div class="subtitle">',
  '            SPASAM-MSE (Spatial Processes and Stock Assessment Methods - Management Strategy Evaluation) is a',
  '            spatially explicit, closed-loop MSE platform built on WHAM (Woods Hole Assessment Model), a',
  '            state-space, age-structured stock assessment framework used in NOAA/NEFSC applications.',
  '          </div>',
  sprintf('          <div class="subtitle" style="margin-top:8px;">WHAM: <a href="%s" target="_blank">%s</a></div>', wham_link, wham_link),
  
  # Pills FIRST
  '          <div class="pillrow">',
  '            <div class="pill">Multi-stock • Multi-region</div>',
  '            <div class="pill">Connectivity • Mixing • Natal homing • Metapopulation</div>',
  '            <div class="pill">Configurable OM spatial structure</div>',
  '            <div class="pill">Alternative EM spatial structure</div>',
  '            <div class="pill">State-space features</div>',
  '            <div class="pill">Flexible data-generation pipeline</div>',
  '            <div class="pill">Flexible projection pipeline</div>',
  '            <div class="pill">Flexible harvest control rules</div>',
  '            <div class="pill">Implementation uncertainty</div>',
  '            <div class="pill">Parallel simulations</div>',
  '            <div class="pill">Automated reporting</div>',
  '          </div>',
  
  # Logos BELOW pills, bigger (ONLINE)
  '          <div class="logo-row">',
  sprintf('            <img class="logo-img" src="%s" alt="NOAA logo" />', icon_url("NOAA_logo.png")),
  sprintf('            <img class="logo-img" src="%s" alt="SPASAM.MSE logo" />', icon_url("SPASAM.MSE_logo.png")),
  '          </div>',
  
  '        </div>',
  
  # Quick install card (REMOVED the duplicate "Site build:" line)
  '        <div class="card">',
  '          <h2>Quick install</h2>',
  '          <p class="small">Development version from GitHub:</p>',
  sprintf('          <p style="margin-top:10px;"><span class="kbd">remotes::install_github("%s/%s")</span></p>', repo_owner, repo_slug),
  sprintf('          <p class="small" style="margin-top:12px;">Package name: <span class="kbd">%s</span></p>', pkg_name),
  
  '          <div class="members">',
  main_dev_html,
  alt_email_html,
  '          </div>',
  
  '          <div class="members">',
  '            <div class="small" style="margin-bottom:6px;"><b>Core SPASAM members</b>:</div>',
  members_html,
  '          </div>',
  '        </div>',
  
  '      </div>',
  '    </div>',
  
  '    <div class="section">',
  '      <div class="card">',
  '        <h2>What SPASAM-MSE does</h2>',
  '        <p>',
  '          SPASAM.MSE provides a flexible spatial operating model and an end-to-end workflow to test management',
  '          procedures under realistic spatial structure and movement. It is designed for research and method development',
  '          where spatial processes can drive estimation bias and management risk.',
  '        </p>',
  '        <ul class="list">',
  '          <li>Simulate spatial population structure, mixing, and connectivity (including natal homing and metapopulation dynamics).</li>',
  '          <li>Generate fishery-dependent and fishery-independent data under alternative spatial sampling designs.</li>',
  '          <li>Fit alternative estimation models with different spatial structures and compare against operating-model truth across scenarios and replicates.</li>',
  '          <li>Evaluate trade-offs and risk across regions and stocks under closed-loop feedback.</li>',
  '        </ul>',
  '      </div>',
  
  '      <div class="card">',
  '        <h2>Core capabilities</h2>',
  '        <ul class="list">',
  '          <li><b>Closed-loop MSE</b>: OM -> data -> EM -> projections -> HCR -> implementation uncertainty -> feedback.</li>',
  '          <li><b>Spatial control</b>: spatial heterogeneity in population biology and fishing dynamics across regions, with explicit connectivity linking regions.</li>',
  '          <li><b>Flexible EM spatial structure</b>: evaluate estimation models with alternative spatial assumptions (e.g., spatial aggregation, region/fleet structure, and movement simplifications).</li>',
  '          <li><b>Movement scenarios</b>: bidirectional vs unidirectional connectivity, natal homing and metapopulation structure, movement trends, and optional environmental covariate effects on movement.</li>',
  '          <li><b>State-space structure</b>: WHAM-based design that treats key biological and fishery processes as random effects (time- and/or age-varying) in both OM and EM.</li>',
  '          <li><b>Performance evaluation</b>: bias/precision, status metrics, risk of overfishing/overfished, and spatial trade-offs in catch, SSB, and F.</li>',
  '          <li><b>Parallel simulations</b>: scalable pipelines for multi-scenario experiments.</li>',
  '          <li><b>Automated reporting</b>: PNG/HTML/PDF outputs for large simulation studies.</li>',
  '        </ul>',
  '      </div>',
  '    </div>',
  
  # Footer (removed duplicate Build line)
  '    <div class="footer">',
  '      <div>',
  sprintf('        <div><b>Main developer</b>: %s</div>', main_dev_name),
  sprintf('        <div class="small">&lt;<a href="mailto:%s">%s</a>&gt; • &lt;<a href="mailto:%s">%s</a>&gt;</div>',
          email_primary, email_primary, email_secondary, email_secondary),
  '      </div>',
  sprintf('      <div class="small">Source: <a href="%s">%s</a></div>', repo_base, repo_base),
  '    </div>',
  
  '  </div>',
  
  '  <script>',
  '    (function(){',
  '      var dd = document.getElementById("vignetteDropdown");',
  '      if(!dd) return;',
  '      var btn = dd.querySelector(".dropbtn");',
  '      btn.addEventListener("click", function(e){',
  '        e.stopPropagation();',
  '        dd.classList.toggle("open");',
  '      });',
  '      document.addEventListener("click", function(){',
  '        dd.classList.remove("open");',
  '      });',
  '    })();',
  '  </script>',
  
  '</body>',
  '</html>'
)

writeLines(index_out, file.path(docs_dir, "index.html"), useBytes = TRUE)
message("Wrote: docs/index.html")

# ----------------------------
# 6) Inject navbar + CSS + JS into every other docs/*.html
#    IMPORTANT: always refresh injection each build (no stale content)
# ----------------------------
inject_into_html <- function(html_file,
                             navbar_html,
                             inject_css,
                             inject_js,
                             marker = "SPASAM_NAV_INJECTED") {
  
  x <- readLines(html_file, warn = FALSE)
  
  # Remove prior injected navbar block if present
  start_nav <- which(grepl("SPASAM_NAV_START", x, fixed = TRUE))
  end_nav   <- which(grepl("SPASAM_NAV_END",   x, fixed = TRUE))
  if (length(start_nav) > 0 && length(end_nav) > 0 && end_nav[1] > start_nav[1]) {
    x <- x[-seq(start_nav[1], end_nav[1])]
  }
  
  # Remove prior marker line if present (we'll re-add)
  marker_line <- paste0("<!-- ", marker, " -->")
  x <- x[!grepl(marker_line, x, fixed = TRUE)]
  
  # Insert CSS near </head>
  head_end <- which(grepl("</head>", x, ignore.case = TRUE))
  if (length(head_end) > 0) {
    i <- head_end[1]
    x <- append(x, c(marker_line, inject_css), after = i - 1)
  } else {
    x <- c(marker_line, inject_css, x)
  }
  
  # Insert navbar right after <body...>
  body_start <- which(grepl("<body[^>]*>", x, ignore.case = TRUE))
  if (length(body_start) > 0) {
    i <- body_start[1]
    x <- append(x, navbar_html, after = i)
  } else {
    x <- c(navbar_html, x)
  }
  
  # Insert JS near </body>
  body_end <- which(grepl("</body>", x, ignore.case = TRUE))
  if (length(body_end) > 0) {
    i <- body_end[1]
    x <- append(x, inject_js, after = i - 1)
  } else {
    x <- c(x, inject_js)
  }
  
  writeLines(x, html_file, useBytes = TRUE)
  TRUE
}

all_html <- list.files(docs_dir, pattern = "\\.html$", full.names = TRUE)
all_html <- all_html[basename(all_html) != "index.html"]

changed <- vapply(all_html, function(f) {
  inject_into_html(f, navbar_html, inject_css, inject_js)
}, logical(1))

message(sprintf("Processed: %d pages", sum(changed)))

# ----------------------------
# 7) Sanity checks
# ----------------------------
message("----- Sanity checks -----")
message(sprintf("Site version: %s", site_version))
message(sprintf("Build date: %s", build_date))
message("docs HTML files:")
print(list.files(docs_dir, pattern = "\\.html$", full.names = FALSE))

message("NOTE: Icons/logos are loaded ONLINE from GitHub raw URLs.")
