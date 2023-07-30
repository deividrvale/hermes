FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

USER root
RUN apt-get -y update
RUN apt-get install -y sudo
RUN adduser --disabled-password --gecos '' zeus
RUN adduser zeus sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER zeus
RUN sudo chown -R zeus /home/zeus
ENV HOME /home/zeus
WORKDIR ${HOME}

RUN opam init --disable-sandboxing --yes
RUN eval $(opam env)

RUN sudo apt-get install  -y python3 libgmp-dev
USER zeus
RUN opam install dune menhir z3
COPY . ${HOME}

ENV OPAM_SWITCH_PREFIX="/home/zeus/.opam/default"
ENV CAML_LD_LIBRARY_PATH="/home/zeus/.opam/default/lib/stublibs:/home/zeus/.opam/default/lib/ocaml/stublibs:/home/zeus/.opam/default/lib/ocaml"
ENV OCAML_TOPLEVEL_PATH="/home/zeus/.opam/default/lib/toplevel"
ENV MANPATH=":/home/zeus/.opam/default/man"
ENV PATH="/home/zeus/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN sudo chown -R zeus /home/zeus
RUN dune build
RUN dune install
