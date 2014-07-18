
Installation
============

There are three options for installation given below. They have
progressively more dependencies but also offer more functionality and
performance. Each contains all the features of the previous package.

**If you have no intention of using RRTM with python**, just install
the :ref:`install.cli`.

**If you want to use RRTM with python**, try to install the
:ref:`install.native`. If you run into difficulties, try installing
the :ref:`install.netcdfonly` instead (has exactly the same
functionality but with some performance overhead). See :doc:`native`
for details on the differences between the two installations.

All options require the code to be downloaded from github::
 
  git clone https://github.com/tomflannaghan/pyrrtm.git

.. _install.cli:

Command line interface using NetCDF
-----------------------------------

Requires:

- gfortran and gcc
- libgfortran
- netcdf and netcdf-devel

To compile::

  $ cd pyrrtm
  $ make cli_netcdf

To install, copy `build/rrtm_nc` and `build/rrtm_sw_nc` into your path
or working directory. See :file:`README.org` for the command line
interface documentation.

.. _install.netcdfonly:

Python interface with NetCDF
----------------------------

Requires:

- gfortran and gcc
- libgfortran
- netcdf and netcdf-devel
- python, scipy and numpy

To compile::

  $ cd pyrrtm
  $ make pymodule_netcdf

To test::

  $ make test

To install the python module, copy `build/pyrrtm` into your python path.

.. _install.native:

Python interface with optional native interface
-----------------------------------------------

Requires:

- gfortran and gcc
- libgfortran
- netcdf and netcdf-devel
- python, scipy and numpy
- cython

To compile::

  $ cd pyrrtm
  $ make pymodule_native

To test::

  $ make test

To install the python module, copy `build/pyrrtm` into your python path.

.. warning::

   I have had some issues with old compilers. Best results come from
   recent versions of `gfortran`. Always test this build with `make
   test`.
