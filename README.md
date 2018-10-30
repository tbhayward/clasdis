# clasdis-nocernlib
clas SIDIS MC based on `PEPSI LUND MC`

To install
`git clone https://github.com/JeffersonLab/clasdis-nocernlib.git`

`cd clasdis-nocernlib` 
`make`

To get the command line options

`./clasdis --help`

Example
`./clasdis --trig 10000000 --nmax 10000`
will write `gemc lund type` data files with 10K events in the directory `eventfiles` (should be created by hand) with total of 10M events.
