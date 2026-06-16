FROM rocker/r2u:jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
    zip \
    wget \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# --- JAVA ---
RUN wget -qO- https://download.java.net/java/GA/jdk21.0.2/f2283984656d49d69e91c558476027ac/13/GPL/openjdk-21.0.2_linux-x64_bin.tar.gz \
    | tar xz -C /opt/ \
    && ln -s /opt/jdk-21.0.2 /opt/java

ENV JAVA_HOME=/opt/java
ENV PATH="${JAVA_HOME}/bin:${PATH}"
ENV LD_LIBRARY_PATH="${JAVA_HOME}/lib/server"

RUN R CMD javareconf

# --- CROQUIS ---
COPY DESCRIPTION NAMESPACE /tmp/croquis/

RUN R -q -e "options(warn = 2); install.packages(c('Rcpp', 'remotes')); remotes::install_deps('/tmp/croquis', dependencies = NA, upgrade = 'never')"

COPY . /tmp/croquis

RUN R -q -e "options(warn = 2); remotes::install_local('/tmp/croquis', dependencies = FALSE, force = TRUE, upgrade = 'never'); if (!requireNamespace('croquis', quietly = FALSE)) stop('croquis package not installed!')" \
    && rm -rf /tmp/croquis

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp(croquis::croquis(), host = '0.0.0.0', port = 3838)"]
