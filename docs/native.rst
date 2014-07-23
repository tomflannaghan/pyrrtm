
Native Interface to the RRTM Code
=================================

By default, pyrrtm uses intermediate NetCDF files to communicate with
the RRTM binaries. This is a very reliable method, but has an overhead
associated with writing the data to disk, calling RRTM, and finally
reading the output from disk.

If you installed the :ref:`install.native` version of pyrrtm, you have
access to a much faster native interface to RRTM, meaning that RRTM is
loaded as a shared object by the python interpreter. Practically
speaking, this should increase the performance of the longwave code by
**5 to 10 times**. The shortwave code performance is not so
dramatically affected as the code itself takes much longer to run than
the overhead. A big disadvantage of the native interface is that
errors thrown by RRTM are not handled at all and will often kill the
python interpreter. As such, the native interface is always disabled
by default but can be activated using :func:`use_native`.

.. function:: pyrrtm.use_native(state=True)

   Activates and deactivates the native binary interface. `state` is a
   boolean value that sets the state of the native binary interface.

.. attribute:: pyrrtm.has_native

   Indicates whether pyrrtm has been compiled with support for the
   native interface.
