FROM ocaml/opam:fedora-39-ocaml-4.14

USER root

RUN dnf install -y \
    pkgconf openblas-devel lapack-devel m4 \
    zeromq-devel python3 python3-jupyter-core jupyterlab \
    wget gmp gmp-devel libcurl-devel zlib-devel sqlite-devel \
    && dnf clean all

RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.10.20/quarto-1.10.20-linux-arm64.tar.gz \
    && mkdir -p /opt/quarto \
    && tar -zxvf quarto-1.10.20-linux-arm64.tar.gz -C /opt/quarto --strip-components 1 \
    && ln -s /opt/quarto/bin/quarto /usr/local/bin/quarto \
    && rm quarto-*.tar.gz

USER opam

RUN opam pin add -n -y owl-base.dev git+https://github.com/owlbarn/owl.git && \
    opam pin add -n -y owl.dev git+https://github.com/owlbarn/owl.git

RUN opam update && \
    opam install -y dune utop \
    ocaml-lsp-server ocamlformat base core core_unix \
    ezcurl lambdasoup caqti sqlite3 \
    owl jupyter ounit2 yojson

RUN opam exec -- ocaml-jupyter-opam-genspec && \
    python3 -m jupyter kernelspec install --user --name ocaml-jupyter "$(opam var share)/jupyter"

RUN echo 'eval $(opam env)' >> /home/opam/.bashrc