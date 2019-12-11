02_SimpleReportCSV

This sample load a json file from designer and the dataset used in designer. The definition of the dataset was stored in the json file

you must move the files Etiketten.csv (the database) and Etiketten.json (is the stored definition of the report and the datset) 
to the bin directory (where the executable resides) to make the sample working

--------------------------
If the programm didn't compile or run, look if you have the correct freetype(-6).dll/.so and zlib1.dll/.so installed or copied in the directory.
fpreport.patch is a patch to store the colounlayout in JSON. In actual trunk it is applied.

AF 2019