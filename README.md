# REDs CAT2 calculator

Public calculator: https://ming-chang-tsai.github.io/reds_calculator/

The GitHub Pages version runs R/Shiny entirely in the visitor's browser using
Shinylive. It does not connect to the Posit-hosted calculator for calculations.
The first visit downloads the browser R runtime and packages.

## Independent deployments

- `master` remains the existing Posit Connect Cloud source. Its `app.R`,
  `manifest.json`, and `www/ioc.svg` have not been changed for this migration.
- `codex/github-pages-browser` holds the browser build tooling and generated
  `docs/` site. GitHub Pages publishes this branch's `/docs` directory.
- The existing Posit address remains available independently:
  https://ming-chang-tsai-reds-calculator.share.connect.posit.cloud/

Do not push browser-only changes to `master` while preserving the existing
Posit deployment. A push to its configured branch may trigger a Posit republish.

## Update the browser site

On `codex/github-pages-browser`, from the repository root:

```r
install.packages("shinylive") # once; initial build used 0.3.0 / assets 0.9.1
```

```sh
Rscript scripts/build-browser.R docs
Rscript scripts/check-browser.R docs
Rscript -e 'httpuv::runStaticServer("docs")'
```

Check both sexes, risk categories, and mutually exclusive indicators in a
browser before publishing. Commit the updated source and generated `docs/`
on the browser branch and push that branch. GitHub Pages rebuilds automatically.
The exporter stages only `app.R` and `www/ioc.svg`, excluding local histories,
deployment configuration, and unrelated documents from the browser bundle.
The check script confirms the bundled source is unchanged and every reachable
answer combination has exactly one lookup-table result. It is a deployment
regression check, not an independent clinical validation of the scoring rules.

The default build destination is `.browser-site/` for local previews, so running
the build without arguments does not replace the published files accidentally.

## QR code

`qr_code.R` generates `reds_calculator_qr.png` for the stable GitHub Pages URL.
Run `Rscript qr_code.R` after installing the `qrcode` R package if needed.
The URL is unchanged from the previous redirect, so existing printed codes
continue to work and now open the browser calculator.

## Rollback

If a browser deployment fails, change GitHub Settings → Pages → source back to
`master` / `/docs`. That restores the previous redirect to Posit, provided the
Posit app is available and has sufficient usage credits. No Posit redeployment
is needed. Keep the Posit service active until the browser version is accepted.
