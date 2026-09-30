# Getting started

services-isp is the web app one ISP ran on an internal server, published the way it ran. It is not a package. To run it you connect it to the same kind of systems it was written for, and you edit the code to point at yours.

## What it needs

| Piece | Used by | Notes |
|---|---|---|
| R with `shiny`, `shinyjs`, `DT`, `stringr`, `readr`, `knitr`, `RMySQL`, `RSelenium` and `rPython` | the app | `rPython` was removed from CRAN on 2020-04-13. Install it from the [CRAN archive](https://cran.r-project.org/src/contrib/Archive/rPython/). The app loads it at startup, so without it nothing starts. |
| Python 2 with `paramiko` and `pexpect`, the Python `rPython` is built against | Diagnosis (antenna stats), VDSL | The VDSL code uses Python 2 syntax. |
| Python 3 | `GetAntena.py` (antenna lookup) | Standard library only. |
| `pandoc` and a LaTeX install with `xelatex` | the Diagnosis PDF | See [Troubleshooting](troubleshooting.md#the-diagnosis-pdf-does-not-download) for pandoc 2.0 and later. |
| `wkhtmltoimage` | the JPG download in Firmas | |
| `ping`, `traceroute`, `nmap` | the Diagnosis options | |
| MySQL access to the RADIUS database `radius` | Datos CPE, Diagnosis | Tables `rm_users`, `rm_services`, `nas` and `radacct`. |
| MikroTik routers as the NAS, with the API on port 8728 | the antenna lookup | The client's antenna must have a DHCP lease there. |
| Client antennas that answer `mca-status` over SSH | Diagnosis | `mca-status` is the status command of Ubiquiti airOS. |
| A DSLAM reachable over SSH | VDSL | It must accept `display vdsl line operation board 0/N` and `display port desc 0/N`. |
| PhantomJS, a Selenium server on `localhost:4448` and `localhost:4446`, and a login to the carrier's GeCo portal | the scrapers that fill Alarmas GeCo | See [The background jobs](#the-background-jobs). |

## Install

Every script reads and writes under `/home/tecnico/WebServicios` by full path, so clone it there:

```bash
sudo mkdir -p /home/tecnico && sudo chown "$USER" /home/tecnico
git clone https://github.com/GeiserX/services-isp.git /home/tecnico/WebServicios
cd /home/tecnico/WebServicios
```

Then replace the placeholders (`x.x.x.x`, `PASSWORD`, the portal's `user` and `password`, the mail addresses) with your hosts and credentials. [Configuration](configuration.md) lists every one with its file and line. Keep that copy private: once filled in, it holds your passwords.

## First run

```bash
Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'
```

`AtBoot.sh` holds the same command with the full path, for starting it at boot. Open http://127.0.0.1:8081.

It worked when the page shows a navigation bar with five tabs: Datos CPE, Alarmas GeCo, Firmas, Diagnosis and VDSL. Two tabs work before anything else is connected. Firmas builds a signature (the JPG needs `wkhtmltoimage`), and VDSL lists the three placeholder DSLAMs from `data/infoDSLAMs.csv`. The rest need the database, and Alarmas GeCo needs the scrapers to have run. The page hides R errors, so a tab that fails stays blank. [Troubleshooting](troubleshooting.md#a-tab-stays-blank) shows how to see the error.

## The background jobs

Two R scripts fill the alarm lists. Each one opens the carrier's portal in PhantomJS, reads the billing summary and writes a CSV to `data/`, then does it again every 30 minutes:

```bash
sh GeCoMovil.sh                                          # mobile lines -> data/listaEXCEED.csv
Rscript /home/tecnico/WebServicios/GECOfija.R            # fixed lines  -> data/listaEXCEEDfijo.csv
```

Both connect to a Selenium server on `localhost` (port 4448 for mobile, 4446 for fixed). No script in the repo starts that server; `selenium-server-standalone.jar` in the repo is presumably it. `mailR.R` mails both CSVs when you run it, and nothing in the repo schedules it: use cron. [How it works](how-it-works.md#the-background-jobs) has the details.

Next: [Usage](usage.md) for what each tab does.
