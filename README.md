# NRS_Software
ENVI/IDL extensions for NRS department

A collection of ENVI/IDL extensions for remote sensing processing.
Most extensions will run in __ENVI classic + IDL__ or __ENVI GUI + IDL__.

The _documentation_ folder contains basic user documentation for most extensions (although not always complete and/or up to date).

The NRS_utils folder contain library functions that are needed by most extensions.

When running within ENVI sometimes IDL library functions could not be found. To overcome this a separate module was build (nrs_libs.sav) containing the following IDL functions:

- ARRAY_INDICES
- COMFIT
- EXP_FUNC
- GEO_FUNC
- GOM_FUNC
- HYP_FUNC
- LABEL_DATE
- LABEL_DATE_CONVERT_FORMAT
- LOGSQ_FUNC
- LOG_FUNC
- WRITE_CSV
- WRITE_CSV_CONVERT
