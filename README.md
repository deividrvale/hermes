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

### Building Hermes from source

With all the above dependencies installed in your ``opam switch``,
run the following from the root of the repository
```bash
dune build
```
This will use ``dune`` to build the source code.
We recommend users to install the binaries for Hermes locally in your system.
Run the following
```bash
dune install
```
This will install Hermes locally on your system as an opam package.
Hermes then is called using the name ``hermes`` in the command line.

### Managing opam switches

If your current ``opam switch``
doesn't have OCaml v4.14.0 or higher,
we recommend creating a fresh ``opam switch``.

```bash
opam switch create hermes 4.14.2
eval $(opam env)
opam install dune menhir z3
```

To see the list of switches, use

```bash
opam switch
```

and switching to a new switch is simple.
For instance

```bash
opam switch hermes
```
Then, the aforementioned instructions on building Hermes from source apply.

## How to use Hermes

Hermes receives as input a file describing the term rewriting system to be analyzed.
This version, v1.0.0, only accepts file in the ``onijn`` format.
The file format is explained in the [API](https://deividrvale.github.io/nijn-coq-script-generation/onijn/index.html#input-file-format).

For practical reasons, it would be better to locally install Hermes on your system. So it can be called from any folder.
Assuming this is the case, invoking the command ``hermes`` is simple:

```bash
hermes /path/to/input/input/file.onijn
```

The output is a human-readable description of the tuple interpretation found (if any).
