
Shortwave Radiation (``SW``)
============================

The shortwave radiation component of RRTM (``rrtm_sw``) is supported
with the following restrictions:

- Specified temperature and pressure profiles, so ``IATM = 0``
- No clouds, so ``ICLD = 0``
- No aerosols, so ``IAER = 0``
- Output fluxes only for 10-3250 cm-1, so ``IOUT = 0``

These restrictions could probably be lifted relatively easily so
please contact me if you are interested in helping to support these
features. As I do not require these features so am not planning
on implementing them myself.

The shortwave radiation code is accessed using the ``SW`` class. The
example below (see :ref:`sw.example`) shows a simple usage of the
radiation code. The full documentation for the ``SW`` class is given
in :ref:`sw.doc`.

.. _sw.example:

Example
-------

We assume that ``T``, ``p``, ``H2O`` and ``O3`` are numpy arrays of equal
length. The surface temperature is set as equal to ``T[0]``. Shortwave
radiative transfer is then performed as follows::
  
  sw = pyrrtm.SW(len(T))
  sw.tavel = T
  sw.pavel = p
  sw.set_species('H2O', H2O, 'mmr')
  sw.set_species('co2', 0.000358, 'vmr')
  sw.set_species('o3', O3, 'vmr')
  output = sw.run()

The ``output`` object has the following members::

  output.totuflux   # Upward flux through each level (W/m2)
  output.totdflux   # Downward flux through each level (W/m2)
  output.fnet       # Net flux through each level (W/m2)
  output.htr        # Heating rate for each layer (K/day)
  
Each member is a numpy array.

.. _sw.doc:

The ``SW`` Class
----------------

.. class:: SW(nlayers)
   
   Performs the shortwave radiative transfer calculations. The
   attributes and methods for ``SW`` are very similar to those for
   :class:`LW`, the longwave radiation class.

.. _sw.profile:
   
The Profile of Temperature and Pressure
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   
.. attribute:: SW.tavel

   The average temperature of each layer. The layers are specified in
   ascending order.

   :units: Kelvin
   :shape: ``(nlayers, )``

.. attribute:: SW.pavel

   The average pressure of each layer. The layers are specified in
   ascending order, so ``pavel[i] > pavel[i+1]``.

   :units: hPa
   :shape: ``(nlayers, )``

.. attribute:: SW.tz

   The temperature at each level. Levels are specified in ascending order.

   :units: Kelvin
   :shape: ``(nlayers+1, )``

.. attribute:: SW.pz

   The pressure at each level. The levels are specified in ascending order.

   :units: hPa
   :shape: ``(nlayers+1, )``


.. note::

   RRTM requires both layer average and level data for pressure and
   temperature.

   Given the level data :attr:`SW.pz` and :attr:`SW.tz`, the layer
   average data :attr:`SW.pavel` and :attr:`SW.tavel` can be
   reconstructed assuming linear interpolation of temperature between
   levels. Therefore, if :attr:`SW.pz` and :attr:`SW.tz` are
   specified, :attr:`SW.pavel` and :attr:`SW.tavel` are not required
   and will be calculated automatically.

   Similarly, given :attr:`SW.pavel` and :attr:`SW.tavel`, values for
   :attr:`SW.pz` and :attr:`SW.tz` can be reconstructed. However, this
   is not a well posed problem, and so the method used is heuristic,
   and looks for smooth profiles that are consistent with the layer
   average data. In these circumstances, the resulting profiles for
   :attr:`SW.pz` and :attr:`SW.tz` should be good enough for most
   purposes, but you should check that they are satisfactory before
   using them.

.. _sw.surface:

Surface
^^^^^^^

.. attribute:: SW.semis
   
   Surface emissivity, between 0 and 1. Note that here reflectance is
   defined as 1 - emissivity, and so ``albedo = 1 - semis`` (see
   ``rrtm_sw_instructions``). Default is 1.0.

.. _sw.chemical:

Chemical Composition
^^^^^^^^^^^^^^^^^^^^

.. method:: SW.get_species(species, unit='vmr')

   Gets a profile of the concentration of one of the chemical
   species.
   
   :param species: String, one of 'H2O', 'CO2', 'O3', 'N2O', 'CO',
                   'CH4', 'O2' (case insensitive).
   :param unit: One of 'vmr', 'mmr', 'molecules/cm2'. Specifies the
                unit of ``value``.
   :returns: Array of shape ``(nlayers,)``

.. method:: SW.set_species(species, value, unit='vmr')

   Sets a profile of one of the chemical species.
   
   :param species: String, one of 'H2O', 'CO2', 'O3', 'N2O', 'CO',
                   'CH4', 'O2' (case insensitive).
   :param value: (Array of shape ``(nlayers,)`` or float). Specifies
                 the concentration of the gas in each layer. If a
                 float, the concentration is constant for all layers.
   :param unit: One of 'vmr', 'mmr', 'molecules/cm2'. Specifies the
                unit of ``value``.

.. _sw.solar:

Solar Forcing Parameters
^^^^^^^^^^^^^^^^^^^^^^^^
   
.. attribute:: SW.juldat 

   Julian day of the year, from 1 to 365. This is only used to
   calculate the Earth-Sun distance. 0 means that the code does not
   take into account the day and uses an average Earth-Sun
   distance. Default is 0.

.. attribute:: SW.solvar

   Rescales the solar source.  Default is 1.0 (no rescaling).

.. attribute:: SW.sza

   Solar zenith angle (degrees). Default is 0 degrees (overhead).

.. _sw.run:

Running the Model
^^^^^^^^^^^^^^^^^

.. method:: SW.run()

   Performs the radiative transfer using RRTM.

   :returns: An instance of the :class:`Output` class.


.. _sw.adv-args:

Advanced arguments
^^^^^^^^^^^^^^^^^^

.. attribute:: SW.ireflect
   
   0 for Lambertian reflection (default).  1 for specular reflection,
   where angle is equal to downwelling angle.

.. attribute:: SW.nstr

   Controls the number of streams used by the DISORT code. Options are
   0, 1 and 2 (corresponding to 4, 8 and 16 streams).
