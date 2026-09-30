---
hide:
  - navigation
---

# services-isp { .si-visually-hidden }

<p align="center">
  <img src="images/banner.svg" alt="services-isp" width="100%">
</p>

<p align="center">
  <a href="https://github.com/GeiserX/services-isp/stargazers"><img alt="GitHub Stars" src="https://img.shields.io/github/stars/GeiserX/services-isp?style=flat-square&logo=github"></a>
  <a href="https://github.com/GeiserX/services-isp/releases"><img alt="Release" src="https://img.shields.io/github/v/release/GeiserX/services-isp?style=flat-square"></a>
  <a href="https://github.com/GeiserX/services-isp/blob/main/LICENSE"><img alt="License: GPL-3.0-or-later" src="https://img.shields.io/github/license/GeiserX/services-isp?style=flat-square"></a>
</p>

---

**services-isp** is the R/Shiny web app an ISP's technicians used for the jobs they did many times a day. Given a client number, it pulls the account from the RADIUS database, finds the client's antenna on the MikroTik router, and asks the antenna for its radio stats. It can ping and scan the line and hand you a PDF report for the client. Without it, the same answer means a database query, a login to the router, an SSH session on the antenna and a report written by hand. It also lists the lines that ran up extra charges, builds the company email signature and shows a VDSL DSLAM's ports in a table. Start with [Getting started](getting-started.md), then [Usage](usage.md).

<div class="grid cards" markdown>

-   :material-server-network: **[Getting started](getting-started.md)**

    ---

    What it needs (R, Python, the RADIUS database, the network gear), where to clone it and how to start it.

-   :material-tab: **[Usage](usage.md)**

    ---

    The five tabs, what you type into each one, and what comes back.

-   :material-tune: **[Configuration](configuration.md)**

    ---

    Every host, password, path, port and threshold, with the file and line where it lives.

-   :material-sitemap-outline: **[How it works](how-it-works.md)**

    ---

    What talks to what, the background jobs behind the alarm lists, and the security model.

</div>

## The five tabs

| Tab | You give it | You get |
|---|---|---|
| Datos CPE (client data) | a PPPoE user, a client number or a name | the account, online or offline, IP, CPE MAC, NAS, the antenna's DHCP lease and every session with its traffic |
| Alarmas GeCo (billing alarms) | mobile or fixed | the lines whose extra spend passed the alarm threshold |
| Firmas (signatures) | name, job title, department, email | the company email signature, as HTML and as a JPG |
| Diagnosis | a PPPoE user and the checks to run | the lookup, the antenna's firmware, signal, noise, CCQ, distance and rates, ping, traceroute and nmap output, and a PDF report for the client |
| VDSL | a DSLAM and a board | the 32 ports with SNR margin, attenuation, rates and power, and a form to label a port |

The interface is in Spanish. [Usage](usage.md) walks through each tab with the labels as they appear.

## What it does

- Looks up a client by exact PPPoE user, by client number prefix or by name, and colours the user green when a session is open and red when it is not.
- Finds the client's antenna in the MikroTik router's DHCP leases and links its web page, the CPE's web page and the NAS.
- Reads the antenna's radio stats over SSH and builds a one-page PDF report with the account, the radio and the last five sessions.
- Keeps two lists of lines with high extra charges, refreshed every 30 minutes from the carrier's billing portal.
- Shows a DSLAM board's VDSL ports without an SSH session, and writes a port's description from a form.

## How it runs

- One R process: `Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'` from `/home/tecnico/WebServicios`, the path the code expects. See [Getting started](getting-started.md#install).
- It reads the RADIUS database over MySQL and reaches the routers, antennas and DSLAMs over the RouterOS API and SSH. Every host and password is a constant in the code. See [Configuration](configuration.md).
- Two scraper scripts run beside it and write the alarm lists to `data/`. See [How it works](how-it-works.md#the-background-jobs).

## What it does not do

- It has no login and no user accounts. Anyone who can open the page can use every tab.
- It is not a package and has no installer, container or configuration file. You edit the code.
- It does not run on a current toolchain unchanged: `rPython` left CRAN in 2020, the VDSL code is Python 2, and the PDF step uses a pandoc flag that pandoc 2.0 renamed. [Troubleshooting](troubleshooting.md) lists the fixes.

## Privacy and security

- Client data stays on your network. The app reads your RADIUS database and your devices and sends nothing out. The alarm scrapers log in to the carrier's portal, and `mailR.R` mails the two lists to the address you set.
- Form input goes into SQL queries, shell commands and DSLAM commands without escaping. Run it only on a trusted internal network, behind something that authenticates. See [How it works](how-it-works.md#security-model).

## Getting help

- Something broken: read [Troubleshooting](troubleshooting.md), then open an [issue](https://github.com/GeiserX/services-isp/issues) with the details it lists.
- A security problem: follow the [security policy](https://github.com/GeiserX/services-isp/blob/main/SECURITY.md), never a public issue.
- The other ISP tools (GenieACS, router provisioning, network statistics): [Related projects](related.md).

## License

services-isp is released under the [GPL-3.0-or-later](https://github.com/GeiserX/services-isp/blob/main/LICENSE) license.
