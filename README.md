# Eval In R

Template obtained from [here](https://www.ieee.org/conferences/publishing/templates.html).

## Setup

To build the paper, run the following command:

```sh
make
```

To clean build artifacts, run the following command:

```r
make clean
```

## Running local webserver

``` sh
docker run --rm -dit --name evalr-httpd -p 8788:80 -v $(pwd):/usr/local/apache2/htdocs/home/rstudio/evalR httpd:2.4
```
