# Configuration

There is no configuration file and there are no environment variables. Every host, password and path is a constant in the code, and the repository ships placeholders. Edit your own copy, and never push it anywhere public once it holds real credentials.

## Database

The client lookup and Diagnosis read the RADIUS database over MySQL. The connection is written four times in `server.R`, once per search button, on lines 25, 74, 486 and 543:

```r
radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="x.x.x.x")
```

Set `user`, `password` and `host` in all four. The app only reads, so a read-only MySQL user is enough. It reads these tables and columns:

| Table | Columns |
|---|---|
| `rm_users` | `username`, `firstname`, `srvid`, `phone`, `mobile`, `address`, `city`, `state`, `expiration`, `lastlogoff`, `comment`, `staticipcpe`, `createdon` |
| `rm_services` | `srvid`, `srvname` |
| `nas` | `nasname`, `shortname` |
| `radacct` | `radacctid`, `username`, `nasipaddress`, `framedipaddress`, `acctstarttime`, `acctstoptime`, `callingstationid`, `acctinputoctets`, `acctoutputoctets` |

The NAS link in the client table uses the last word of `nas.shortname` as the router's address.

## Routers

`GetAntena.py` logs in to the NAS over the plain (non-TLS) RouterOS API on port 8728 and prints its DHCP leases. Set the API user and password on line 157 (`apiros.login("admin", "PASSWORD")`). The port is on line 138.

## Antennas

Diagnosis logs in to the client's antenna over SSH and runs `mca-status`. The user and password are on `server.R` line 641 (`username="admin", password="PASSWORD"`).

Two more addresses are derived, not configured: the CPE's web page is the client's IP on port 8080 (`server.R` lines 112 and 584), and the access point is the address ending in `.10` in the antenna's subnet (`server.R` lines 655 to 657).

## DSLAMs

`data/infoDSLAMs.csv` lists the DSLAMs the VDSL tab offers, one per row. A `1` in a board column means that board exists:

```text
Ubicacion,IP,Board1,Board2,Board3,Board4
X,1.1.1.1,,1,,
```

`Ubicacion` is the site name shown in the list. The shipped file has three placeholder rows.

The VDSL tab logs in over SSH as user `tecnico`. The user is on `server.R` lines 913 and 1058 (`tecnico@%s`) and the password on lines 921 and 1066 (`child.sendline('PASSWORD')`). Each session is logged to `data/logSSHsession.txt` and `data/logSSHanotacion.txt`.

## Billing alarm scrapers

| Setting | `GECOselenium.R` (mobile) | `GECOfija.R` (fixed) |
|---|---|---|
| Portal user | line 36, `list("user")` | line 31 |
| Portal password | line 38, `list("password")` | line 33 |
| Selenium server port | 4448 (line 16) | 4446 (line 15) |
| PhantomJS port | 4449 (line 13) | 4449 (line 13) |
| Alarm rule | extra over 50% of tariff, or over 5 € with no tariff (lines 81 to 90) | total of 10 € or more (line 77) |
| Interval | 30 minutes (line 96) | 30 minutes (line 83) |
| Output | `data/listaEXCEED.csv` | `data/listaEXCEEDfijo.csv` |

Both scripts use the same PhantomJS port, so run them one at a time or change one of them.

## Mail

`mailR.R` sends both alarm lists as attachments. Set `from`, `to`, the SMTP `host.name`, `port`, `user.name` and `passwd` on lines 2 to 10. The shipped values are placeholders for a Gmail account.

## Email signatures

- The company choices are in `ui.R` lines 54 and 55.
- The preview template is in `server.R` from line 257, and the downloaded file is built from `firma.html`. Both hold the first company's logo URL, phone numbers, website and social links.
- For the other two choices, `server.R` lines 365 to 386 swap those values with `gsub`. Each `gsub` must match the template text exactly, or the value is not replaced.
- Whether the fault line's number appears is handled on line 364.

## Diagnosis report

`RMD/diagnosis.rmd` holds the PDF's title, author, greeting, closing and footer link. The pandoc command is on `server.R` line 833.

## Paths and port

- The app directory `/home/tecnico/WebServicios` is written in full in `server.R`, `AtBoot.sh`, `GeCoMovil.sh`, `GECOselenium.R`, `GECOfija.R` and `mailR.R`. To use another directory, replace it in all six.
- `AtBoot.sh` starts the app on `127.0.0.1:8081`. The comment on line 1 of `ui.R` and `server.R` shows the older `0.0.0.0:8080`. See [How it works](how-it-works.md#security-model) before you listen on anything other than localhost.

## Files the app does not use

`ssh.py` and `VDSL.py` are earlier versions of the antenna and DSLAM steps, and nothing calls them. `diagnosis.log` is a pdfTeX log from a failed report in 2016.
