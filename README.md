# NRS_Software
ENVI/IDL extensions for NRS department

A collection of ENVI/IDL extensions for remote sensing processing.
Most extensions will run in `ENVI classic + IDL` or `ENVI GUI + IDL`.

The `documentation` folder contains basic user documentation for most extensions (although not always complete and/or up to date).

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

### Extension structure

Folders ending with `_tools` contain a collection of modules each consisting of three files:

- `<module>.pro`: code to do the actual calculations
- `<module>_gui.pro`: code defining the user interface
- `<module>_gui_eventcb.pro`: callback functions to handle user actions in the user interface

Next to these there are two additional files:

- `<tools name>_menu`: defines the menu items for ENVI (can be both `ENVI classic` and `ENVI GUI`)
- `_build_all_<tools name>`: a build file to generate `.sav` file only the code from this tool. (Using IDL to build a `.sav` file can also include unwanted dependencies that will lead to issues when multiple tools use the same dependencies)

**Note:** `<tools name>` does not follow a clear pattern, but the tool name is always included

