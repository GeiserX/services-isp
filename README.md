<p align="center">
  <img src="docs/images/banner.svg" alt="services-isp" />
</p>

<h1 align="center">services-isp</h1>

<p align="center">
  <a href="https://github.com/GeiserX/services-isp/releases"><img src="https://img.shields.io/github/v/release/GeiserX/services-isp" alt="Release" /></a>
  <a href="LICENSE"><img src="https://img.shields.io/github/license/GeiserX/services-isp" alt="License" /></a>
</p>

<p align="center">Task automation webapp for ISP operations</p>

---

An R/Shiny webapp that automates common tasks at an ISP.

## Features

- Look up a client by PPPoE user, client number or name: the account, whether it is online, its IP, CPE and antenna, and every session with its traffic.
- Diagnose a line: the antenna's radio stats over SSH, ping, traceroute and nmap, and a one-page PDF report for the client.
- List the mobile and fixed lines whose extra charges passed the alarm threshold, refreshed every 30 minutes from the carrier's billing portal.
- Build the company email signature from a form and download it as HTML or JPG.
- Read a VDSL DSLAM board's 32 ports in a table, and label a port, without opening an SSH session.

## Quick start

You need R with `shiny`, `shinyjs`, `DT`, `stringr`, `readr`, `RMySQL`, `RSelenium`, `rPython` and `knitr`, Python 3 for `GetAntena.py`, Python with `paramiko` and `pexpect` for the SSH and DSLAM steps (run through `rPython`), and MySQL access to the ISP's databases. The code reads and writes under `/home/tecnico/WebServicios`, so clone it there. Set the hosts and passwords in `server.R`, then:

```bash
Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'
```

Open http://127.0.0.1:8081. The diagnosis PDF is rendered from `RMD/diagnosis.rmd`; the GeCo tabs drive a browser through `selenium-server-standalone.jar`.

The full list of what it needs, and every setting to fill in: [Getting started](https://geiserx.github.io/services-isp/getting-started/).

## Documentation

The docs are at [geiserx.github.io/services-isp](https://geiserx.github.io/services-isp/).

- [Getting started](https://geiserx.github.io/services-isp/getting-started/): what it needs, where to clone it, the first run, the background jobs
- [Configuration](https://geiserx.github.io/services-isp/configuration/): every host, password, path, port and threshold, with its file and line
- [Usage](https://geiserx.github.io/services-isp/usage/): the five tabs, what you type and what comes back
- [How it works](https://geiserx.github.io/services-isp/how-it-works/): what talks to what, and the security model
- [Troubleshooting](https://geiserx.github.io/services-isp/troubleshooting/): blank tabs, the messages, the PDF, VDSL and the alarm lists

Bugs go to the [issues](https://github.com/GeiserX/services-isp/issues); security problems to the [security policy](SECURITY.md), never a public issue.

## Related projects

- [genieacs-container](https://github.com/GeiserX/genieacs-container): Helm chart and container for GenieACS TR-069
- [router-express](https://github.com/GeiserX/router-express): auto-configures client routers and syncs databases
- [statix](https://github.com/GeiserX/statix): ISP network statistics dashboard
- [ScriptPoblar](https://github.com/GeiserX/ScriptPoblar): adopts a whole network of devices into CRM Control in parallel

## License

[GPL-3.0-or-later](LICENSE)
