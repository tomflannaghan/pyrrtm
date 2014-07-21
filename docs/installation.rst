
Installation
============

There are two options for installation given below. Try to install the
:ref:`install.native` first. If you run into difficulties, try
installing the :ref:`install.netcdfonly` instead (has exactly the same
functionality but with some performance overhead). See :doc:`native`
for details on the differences between the two installations.

.. _install.native:

pyrrtm with native interface and NetCDF
---------------------------------------

Requires:

- gfortran and gcc
- libgfortran
- netcdf and netcdf-devel
- python, scipy and numpy
- cython

To download and compile::

  $ git clone https://github.com/tomflannaghan/pyrrtm.git
  $ cd pyrrtm
  $ make pymodule_native

To test::

  $ make test

To install the python module, copy ``build/pyrrtm`` into your python path.

.. warning::

   I have had some issues with old compilers. Best results come from
   recent versions of ``gfortran``. Always test this build with ``make
   test``.


.. _install.netcdfonly:

pyrrtm with NetCDF
------------------

Requires:

- gfortran and gcc
- libgfortran
- netcdf and netcdf-devel
- python, scipy and numpy

To download and compile::

  $ git clone https://github.com/tomflannaghan/pyrrtm.git
  $ cd pyrrtm
  $ make pymodule_netcdf

To test::

  $ make test

To install the python module, copy ``build/pyrrtm`` into your python path.
