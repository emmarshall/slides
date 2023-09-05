# Quarto Intro

## Instructions

You have some options to get bits installed:

* Follow along on [posit.cloud](#follow-along-on-posit-cloud) or
* Follow along on your [Desktop](#follow-along-on-your-desktop)

You can optionally

* Sign up for a [Quarto Pub account](https://quartopub.com/)

## Follow along on posit.cloud

* Please go to [posit.cloud](https://posit.cloud/) and create an account (it is free).

## Follow along on your Desktop

Quarto is available in a [number of editors](https://quarto.org/docs/get-started/), including VS Code, Jupyter Lab, and more. However, for this workshop, we'll be working in RStudio.

Using `podman` / Podman Desktop or `docker` you can use this oneliner (see [rocker-project.org](https://rocker-project.org/):

```bash
docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 rocker/rstudio
# and then open http://localhost:8787 in your browser
```

### Install RStudio Desktop

NB: this path (installing locally) is more involved than running the IDE in a container.

* [Download and install the latest release of RStudio](https://www.rstudio.com/products/rstudio/download/)

### Install Quarto

* Quarto already comes installed with your RStudio. If you are using an earlier version, please ensure you have Quarto installed. 
* Please ensure you have Quarto v1.0 or above. To check, run `quarto --help` in your Terminal.

If you need to install Quarto:

1. Navigate to [the Getting Started page on quarto.org](https://quarto.org/docs/get-started/).
2. Click Download Quarto CLI.

![](download-quarto.png)
