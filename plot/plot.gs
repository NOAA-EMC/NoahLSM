'reinit'
'set display color white'
'c'
'open THERMO.ctl'
'open OBS_DATA.ctl'
'open THERMO1.ctl'

n=20
while (n<24)
n=n+1
'c'
'gxprint fig'n'.png x1024 y768'
'set font 1'
if(n=1);ti=0;te=721;endif
if(n=2);ti=721;te=1489;endif
if(n=3);ti=1489;te=2209;endif
if(n=4);ti=2209;te=2833;endif
if(n=5);ti=2833;te=3553;endif
if(n=6);ti=3553;te=4321;endif
if(n=7);ti=4321;te=5041;endif
if(n=8);ti=5041;te=5761;endif
if(n=9);ti=5761;te=6481;endif
if(n=10);ti=6481;te=7249;endif
if(n=11);ti=7249;te=7969;endif
if(n=12);ti=7969;te=8689;endif
if(n=13);ti=8689;te=9409;endif
if(n=14);ti=9409;te=10177;endif
if(n=15);ti=10177;te=10897;endif
if(n=16);ti=10897;te=11665;endif
if(n=17);ti=11665;te=12385;endif
if(n=18);ti=12385;te=13105;endif
if(n=19);ti=13105;te=13825;endif
if(n=20);ti=13825;te=14593;endif
if(n=21);ti=14593;te=15313;endif
if(n=22);ti=15313;te=16033;endif
if(n=23);ti=16033;te=16753;endif
if(n=24);ti=16753;te=17520;endif


'set t 'ti' 'te
'set line 2 1 2'
'draw line 8 7.5 8.5 7.5'
'draw string 9 7.5 Version 2.7.1'
'set line 4 1 2'
'draw line 8 7.25 8.5 7.25'
'draw string 9 7.25 Version 2.7GFS'
'set line 3 1 2'
'draw line 8.0 7.0 8.5 7.0'
'draw string 9.0 7.0 observation'
'set vrange -30 400'
'set ccolor 2'
'set cmark 0'
'd AET.1'
'set ccolor 4'
'set cmark 0'
'd AET.3'
'set ylpos 9 -l'
'draw ylab Latent Heat Flux (W M-2)'
'set ccolor 3'
'set cstyle 3'
'set cmark 0'
'd ETAOBS.2'
*print'
*disable print fig'n'.meta'
endwhile
