# Hermes

Hermes is a termination and complexity analyzer for innermost first-order rewriting.

Currently, v1.0.0, it implements techniques from the paper
[https://doi.org/10.4204/EPTCS.376.5](https://doi.org/10.4204/EPTCS.376.5)
which describes the complexity analysis of such systems in the framework of tuple interpretations.

## Build Instructions (Linux/macOS)

We use [opam](https://opam.ocaml.org/doc/Install.html) to build Hermes.
See [opam installation instruction page](https://opam.ocaml.org/doc/Install.html) for installation instructions for your system.

Here's the **package dependency** list,
which can be installed using ``opam``.

- **ocaml**, v4.14.0 or higher, the ocaml compiler.
- **dune** v3.5.0 or higher, the building tool.
- **menhir** v20230608 or higher, the parser generator.
- **z3** v4.12.2-1 or higher, the SMT solver.
  - Note: z3 ``opam`` package will require the following system dependencies:
  **libgmp** and **python3**.
  Make sure those are installed on your system before proceeding.


### Building Hermes from source

We recommend creating a fresh ``opam switch`` with OCaml v4.14.1 or higher.

```bash
opam switch create hermes 4.14.1
eval $(opam env)
```

After creating the Hermes ``switch``, we switch to it using:

```bash
opam switch hermes
opam install dune menhir z3
```

With all the above dependencies installed in your ``opam switch``,
run the following from the root of the repository:
```bash
dune build
```
This will use ``dune`` to build the source code.
We recommend users install the binaries for Hermes locally in their system.
This can be done via the command:
```bash
dune install
```
This will install Hermes locally on your system as an opam package.
Hermes then is called using the name ``hermes`` in the command line.

## How to use Hermes

Hermes receives as input a file describing the term rewriting system to be analyzed.
This version, v1.0.0, only accepts file in the ``onijn`` format.
The file format is explained in the [API](https://deividrvale.github.io/hermes/hermes/index.html).

We assume Hermes' binaries are installed locally.
Then simply run:

```bash
hermes /path/to/input/input/file.onijn
```

The output is a human-readable description of the tuple interpretation found (if any).

## Docker Image
Hermes can also be run in isolation from a docker image builder provided.
We assume you have docker properly installed in your system.
From the root of this repository simply run:
```bash
# Build the Dockerfile.
docker build -t hermes_img .
# Run the docker image interactively
docker run -i -it hermes_img:latest
```
