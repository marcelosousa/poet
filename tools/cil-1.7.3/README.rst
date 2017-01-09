
CIL Frontend for POET
=====================

This is a modified version of CIL based on v.1.7.3 (as downloaded from the
github repository).  The modifications add a new option ``--dopoet`` to the
commandline tool ``cilly``.  This option does a preprocessing pass necessary to
feed code to poet.

Prerequisites
-------------

- Standard Ocaml packages ??
- Package ``libc6-dev-i386`` if you are in a 64 bit machine.

Installation
------------

::

 ./configure
 make

The tool ``cilly`` is now available in the ``./bin/`` folder, and can be used
from there. If you wish to install it, then edit ``setup.sh`` and update
variable ``D`` so that ``$D/bin`` points somewhere in your ``$PATH``.
Now execute::
 
 ./setup.sh

Make sure that the following command produces at least one line of output::

 cilly --help | grep -i poet


Extracting full program sources
-------------------------------

Go to your project's ``Makefile`` and redefine the variable ``CC`` as follows::

 CC=cilly --merge --out=full.c

Explanation: when ``CC`` is called with option ``-c``, the option ``--merge``
will dump the preprocessed C code into the ``.o`` file. When ``CC`` is called
without option ``-c``, ``cilly`` will extract the C code from the .c and .o
files passed in commandline, merge the C code together, and dump the whole
program code into ``full.c``.

Full program extraction + poet preprocessing
--------------------------------------------

Simply add the option ``--dopoet`` to the command provided in the previous
section::

 CC=cilly --merge --out=full.c --dopoet

It will additionally run a pass that do the necessary inlining and cleaning that
we need for poet. The pass will be executed right before the file ``full.c`` is
produced.
