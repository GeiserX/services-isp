<p align="center">
  <img src="docs/images/banner.svg" alt="services-isp banner" />
</p>

<h1 align="center">services-isp</h1>

<p align="center">
  <a href="LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT" /></a>
</p>

<p align="center">Task automation webapp for ISP operations</p>

---

Webapp which automates common tasks for a common ISP.

It includes:

1) An online checker, which generates a report in PDF to detect failures in a client's connection. It connects to the Antenna, the router, and checks the traffic historical data. This has two tabs, the first one being the simplest/quick version, and the second one the full version of it.
2) An alarm for customers who have exceeded the quota for the month, sending a report each month.
3) Email signature creation. This automates the process to create a simple HTML signature for the company to use.
4) A web interface to a web-less VDSL DSLAM. Very handy in case you do not want to connect each time via SSH. Simply connect to the web and perform actions.

## Author

[@GeiserX](https://github.com/GeiserX)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
