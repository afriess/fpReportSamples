# fpReportSamples
Some samples about fpReport for Lazarus

fpReport is NOT LazReport or FreeReport !!!

# Information from the Wiki
see https://wiki.freepascal.org/FPReport
## Architecture
FPReport is a banded reporting tool.

It was designed from zero, to be able to run on a headless webserver, with minimal dependencies. An important use-case was a Linux server running in a container without X libraries installed. Creating a report is just placing squares with contents on a page, usually based on a data loop. This can be done entirely in-memory, and does not need a display or GUI.

Reports can be created entirely in code, or they can be read from a definition file. The default shipped file uses JSON as the format to store the report definition. The reports can also be designed visually, using a designer. A stand-alone designer exists, as well as the possibility to design a report within the Lazarus IDE.

Once the report is created, it can be saved to disk, or rendered. Rendering is the act of creating visual output.

Various renderers have been implemented:

* To PDF. The PDF renderer is the renderer of choice
* To a series of images. The FPImage renderer creates a bitmap per page, and the bitmaps can be saved in a directory in any of the supported FPImage formats.
* To HTML: each page is rendered as a HTML page using appropriate CSS markup.
Parts that cannot be rendered as HTML and CSS will be rendered as an image and placed in the HTML page.
* To the LCL (a canvas).
This is in fact the basis of the preview, the designer and the printing support.
* To an AggPas canvas. AggPas is a powerful graphics library, and this renderer is similar in scope to the FPImage renderer
* To fpGUI - this can be used to preview the report in a FPGUI report
