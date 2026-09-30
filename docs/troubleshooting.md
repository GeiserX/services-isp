# Troubleshooting

## A tab stays blank

The page hides every R error: `ui.R` lines 11 to 13 set `.shiny-output-error` to invisible, so a failed table shows as empty space. Read the errors in the terminal where `Rscript` runs, or delete those three lines while you debug.

## The app does not start

- `there is no package called 'rPython'`: `rPython` is no longer on CRAN. Install it from the archive: `install.packages("https://cran.r-project.org/src/contrib/Archive/rPython/rPython_0.0-6.tar.gz", repos = NULL, type = "source")`. It needs the Python development headers.
- Any other missing package: install it from CRAN. The list is in [Getting started](getting-started.md#what-it-needs).

## Messages in the lookup and Diagnosis tabs

| Message | Meaning |
|---|---|
| "No existe este cliente PPPoE" | no user with that exact name in `rm_users` |
| "Este cliente no ha usado nunca su cliente PPPoE" | the user exists but has no session in `radacct` |
| "No se encuentra la antena del cliente" | no DHCP lease on the NAS has the expected host name. The app gives four reasons: the antenna's name is wrong, the antenna is set up as a router, it has been off for more than three days, or the client is on ADSL |
| "Error al conectar a la antena" | the SSH login to the antenna failed: wrong credentials, not an airOS device, or unreachable |

The antenna's host name must be the PPPoE user without its `t` plus the first letter of the client's first name. See [How it works](how-it-works.md#a-client-lookup-step-by-step).

## The Diagnosis PDF does not download

- pandoc 2.0 renamed `--latex-engine` to `--pdf-engine`, and later versions reject the old name. On a current pandoc, change `server.R` line 833 to `pandoc -s diagnosis.md --pdf-engine=xelatex -o Diagnosis.pdf`.
- xelatex must be installed, with the fonts and packages the report uses.
- Run the lookup first: the report uses the tables the last search built.

## The VDSL table is empty

- The SSH login to the DSLAM failed or timed out (after 4 seconds). `data/logSSHsession.txt` holds the session as the DSLAM answered it.
- `rPython` is built against Python 3: the VDSL code is Python 2 and fails with a syntax error. Build `rPython` against Python 2.
- The DSLAM's output differs from what the parser expects. The parser reads the port table from line 48 of the session log.

## The alarm lists are empty or old

- The files `data/listaEXCEED.csv` and `data/listaEXCEEDfijo.csv` do not exist until a scraper has run once.
- The scrapers stop when the portal's page changes, because they read it by line position. Watch their terminal output: each round prints `Done` and the time.
- PhantomJS is no longer developed. A portal that needs a current browser will not load in it.

## Reporting a bug

Open an [issue](https://github.com/GeiserX/services-isp/issues) with:

- the R version and the output of `sessionInfo()`;
- the tab and the button you pressed;
- the error from the terminal where `Rscript` runs (the page does not show it);
- for VDSL, the DSLAM model and the relevant lines of `data/logSSHsession.txt` with addresses removed.

Never paste a real password, client name or IP address into an issue. Report security problems through the [security policy](https://github.com/GeiserX/services-isp/blob/main/SECURITY.md).
