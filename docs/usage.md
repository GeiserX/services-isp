# Usage

The app has five tabs, in this order in the navigation bar. Labels are quoted as the app shows them, in Spanish, with the meaning next to them. In the two lookup boxes, pressing Enter runs the search.

## Datos CPE: client data

The left panel has two searches.

**"Si no sabes la extensión"** (if you do not know the extension). Type a client number in "Introduzca número de cliente", or part of a name in "O introduzca el nombre del cliente", and press "Buscar usuarios". A number matches every PPPoE user that starts with it; a name matches every client whose first name contains it. The table lists each match with its service, phones, address, city, region, expiry date, last disconnection and comment, five per page. The user name is green when the client's last session is still open and red when it is closed.

**"Si sabes la extensión"** (if you know the extension). Type the full PPPoE user name and press "Buscar". Three tables come back:

- The client: user (green or red, as above), name, service, IP (a link to the CPE's web page on port 8080), "Dinámica" or "Estática" IP, CPE MAC and NAS (a link to the router).
- The antenna, from the NAS's DHCP leases: its IP (a link to its web page), when the lease expires, its MAC and its name. The antenna is found by its host name, which must be the user name without its letter `t` plus the first letter of the client's first name.
- Every session, newest first, five per page: NAS IP, CPE IP, start, stop (empty while the session is open), time online, CPE MAC, download and upload.

A message replaces a table when something is missing. [Troubleshooting](troubleshooting.md#messages-in-the-lookup-and-diagnosis-tabs) explains each one.

## Alarmas GeCo: billing alarms

The heading reads "Facturas que exceden el 50% de su tarifa / O que excedan 5€ sin tarifa" (bills over 50% of their tariff, or over 5 € without a tariff). Pick "Móvil" (mobile) or "Fija" (fixed) in "Escoja Tipo de Facturación".

| List | File | Columns | Who is on it |
|---|---|---|---|
| Móvil | `data/listaEXCEED.csv` | phone, holder, duration, tariff, extra, total | lines whose extra spend is over half their tariff, and lines with no tariff and more than 5 € extra |
| Fija | `data/listaEXCEEDfijo.csv` | phone, notes, duration, base, margin, total | lines with a total of 10 € or more |

The tab only reads the two files. The scraper scripts write them every 30 minutes; see [Getting started](getting-started.md#the-background-jobs). The fixed-line rule differs from the heading, which describes only the mobile list.

## Firmas: email signatures

Pick the company in "Elija empresa", fill in "Nombre" (name), "Cargo" (job title), "Departamento" (department) and "Correo electrónico" (email), and choose in "¿Debe de aparecer el teléfono de averías?" whether the fault line's phone number appears. The preview on the right follows what you type. "Crear" (create) writes the signature and shows two buttons, "Descarga HTML" and "Descarga JPG". Both files are also kept in `data/`, named `Firma` + the first name + the date + the company.

The three companies, their logos, phone numbers and links are written into the code. [Configuration](configuration.md#email-signatures) says where to put your own.

## Diagnosis

The same two searches as Datos CPE, on the right-hand side ("Buscar PPPoE's" and "Buscar"). Under them, "Opciones" (options) chooses what runs:

| Option | What it does | Default |
|---|---|---|
| Conexión a MikroTik | looks up the antenna in the NAS's DHCP leases | on |
| Conexión a Antena | logs in to the antenna over SSH and reads its radio status | on; turning MikroTik off turns this off too |
| Ping | `ping -c 4` to the client's IP | off |
| Traceroute | `traceroute` to the client's IP | off |
| NMap | `nmap -T4 -A -v` against the client's IP | off |

The client table adds the PPPoE expiry and creation dates, phones, address and comment to the Datos CPE columns. The antenna's radio table shows its firmware and hardware, the access point it is connected to (linked to the address ending in `.10` in the antenna's subnet), wireless and total uptime in days, frequency, signal and noise, CCQ, distance in metres, transmit and receive rates, latency and LAN speed. Ping, traceroute and nmap print their raw output below.

"Descarga PDF de Diagnosis" downloads a one-page report addressed to the client: the account, the radio table and the last five sessions. It is built from `RMD/diagnosis.rmd` with knitr, pandoc and xelatex.

## VDSL

Pick a DSLAM in "Escoge DSLAM" and a board in "Escoge board", then press "Conectar" (connect). The app logs in to the DSLAM over SSH and shows the board's 32 ports, 16 per page, with upstream and downstream SNR margin, signal attenuation, maximum rate, output power, actual rate and the port's description. The DSLAM list and each one's boards come from `data/infoDSLAMs.csv`; see [Configuration](configuration.md#dslams).

"Añadir anotaciones" (add notes) labels a port: pick the port in "Escoge puerto", type the text in "Anotación" and press "Marcar". The app sets the port's description on the DSLAM and shows "Correcto!". Press "Conectar" again to see it in the table.
