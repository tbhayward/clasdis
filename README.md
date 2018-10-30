# clasdis-nocernlib
clas SIDIS MC based on `PEPSI LUND MC`

To install
`git clone https://github.com/JeffersonLab/clasdis-nocernlib.git`

`cd clasdis-nocernlib` 
`make`

Example
`./clasdis --trig 10000000 --nmax 10000`
will write `gemc lund type` data files with 10K events in the directory `eventfiles` (should be created by hand) with total of 10M events.

To get more command line options

`./clasdis --help`
```
 clasDIS [options]
          option  value      default  comment
   --v    verbos_level    0     additional printout
   --trig nevents         10     number of triggers
   --beam beam_energy     10.6   e- momentum in GeV
   --e e-min e-max  min[750] max[0.95*e] Eprime in GeV
   --t tmin tmax  5.0    theta min/max for \theta(deg)
   --x xmin xmax        0.05 0.95    x_b min/max
   --q Q2min Q2max        0.85 20.0 Q2 min max in GeV^2
   --w W2min W2max        4.0  50     W^2 min max GeV^2
   --z                 0.3       z=E_pi/nu min value
   --zpos z-position      0    target z position in cm
   --zwidth z-width 0    width in z cm (zpos+/-zwidth)
   --raster diameter 12   raster diameter in cm
   --radgen                   include radgen
   --radtab         generate radgen table for SIDIS
   --targ target proton   deuteron/neutron possible
   --pol  polarization    0%  -100 - 100% target pol
   --print nprint     1000 print every nprint event
--printid id       0   print tmp-file forid=321
--accclas12            acceptance for clas12 ON
--pid  LUND_ID  0   0-all,211-pip,3122-Lambda..
--norand             keep the old random number
--mstu                     set mstu(16)=2
--parj12        0.4        set PARJ(12)
--parj33        0.3        set PARJ(33)
--lst15         145        set the PDF set
--lst8          1        set QCD mechanism
--hermes              set HERMES JETSET variables
--lst37   0          set LST(37) 1-Ak Cahn+Sivers
--parl3         0.64       set PARL(3)
--HT    iht    0   higher twists on (LST(11)=iht)
      --nmax   value     0  maximum events in a file
      --single   value   .FALSE. /true gen fixed tracks
    ```  
