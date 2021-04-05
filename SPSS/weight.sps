* Encoding: UTF-8.
*Kategorizálás.

RECODE kor (0 thru 30=1) (31 thru 50=2) (ELSE=3) INTO kor_kat.
EXECUTE.
val lab kor_kat
1 'Fiatal'
2 'Középkorú'
3 'Idős'.

RECODE htsz (1 thru 2=1) (3 thru 4=2) (5 thru 8=3) INTO ht_kat.
EXECUTE.
val lab ht_kat
1 'Kisméretű'
2 'Közepes'
3 'Nagy'.

RECODE isk (0 thru 3=1) (4 thru 5=2) (ELSE=3) INTO isk_3kat.
EXECUTE.
val lab isk_3kat
1 'Alapfokú'
2 'Érettségi'
3 'Magasabb, mint érettségi'.

RECODE neme (2=0).
EXECUTE.
val lab neme
0 'Nő'
1 'Férfi'.

RECODE teltip (1=1) (4=3) (2 thru 3=2) INTO teltip_3kat.
EXECUTE.
val lab teltip_3kat.
1 'Budapest'
2 'Megyeszékhely és város'
3 'Község'.

freq kor_kat ht_kat isk_3kat neme teltip_3kat.

*Súlyozás - jövedelem.

IF(kor_kat = 1) kor_w = 0.22.
IF(kor_kat = 2) kor_w = 0.15.
IF(kor_kat = 3) kor_w = 0.02.
EXECUTE.

IF (neme = 0) nem_w = 0.03.
IF (neme = 1) nem_w = 0.12.
EXECUTE.

IF(isk_3kat = 1) isk_w = 0.02.
IF(isk_3kat = 2) isk_w = 0.15.
IF(isk_3kat = 3) isk_w = 0.3.
EXECUTE.

IF(ht_kat = 1) ht_w = 0.01.
IF(ht_kat = 2) ht_w = 0.1.
IF(ht_kat = 3) ht_w = 0.2.
EXECUTE.

IF(teltip_3kat = 1) tel_w = 0.02.
IF(teltip_3kat = 2) tel_w = 0.1.
IF(teltip_3kat = 3) tel_w = 0.16.
EXECUTE.


COMPUTE weight=kor_w+nem_w+isk_w+ht_w+tel_w.
EXECUTE.

*Súlyozás - demográfiai.

IF(neme = 0) w_dn = 0.5.
IF(neme = 1) w_dn = 1.

IF(teltip = 1) w_dt = 1.
IF(teltip = 2) w_dt = 0.75.
IF(teltip = 3) w_dt = 0.5.
IF(teltip = 4) w_dt = 0.25.

compute weight_d = w_dn + w_dt.