FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

USER root
RUN apt-get -y update
RUN apt-get install -y sudo python3 libgmp-dev
RUN adduser --disabled-password --gecos '' hermes
RUN adduser hermes sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER hermes
RUN sudo chown -R hermes /home/hermes
ENV HOME /home/hermes
WORKDIR ${HOME}

RUN opam init --disable-sandboxing --yes
RUN eval $(opam env)

USER hermes
RUN opam install dune menhir z3
COPY . ${HOME}

RUN sudo chown -R hermes /home/hermes
RUN dune build
RUN dune install
