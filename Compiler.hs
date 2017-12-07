{-module Compiler
(Sexpr(Symbol, Number, Floating, Nil, Cons), 
 Value(VIntegral, VFloating, VString, VSymbol, VBool, VStatement, VIntegerList, VPair, Null), 
 Bytecode(End, Push, Print, PrintBang, Add, Mult, Sub, Div, Load, Store, Input, Equal, NotEqual, Greater, GEqual, Less, LEqual, IfThen, Goto, PushCallstack, PopCallstack, NextLine, Spaces, CastInt, Rand, Log, Abs, Pow, And, Or, ALoad, ALoad2D, AStore, NewArray, NewArray2D, OnGoto, OnGosub), 
 analyze, car, cdr, compile, printBytecode, line) where -}

 module Compiler
 (Sexpr(..),
  Value(..),
  Bytecode(..),
  analyze, car, cdr, compile, printBytecode) where

import Parselib
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.IO.Unsafe
--import Control.Monad.State
import Control.Monad.Trans.State

foo = "(define foo " ++
      "'((100 input \"What is the value of A\" a )" ++
      " (110 input \"What is the value of B\" b )" ++
      " (120 input \"What is the value of C\" c )" ++
      " (130 let d = ((b * b) - (4.0 * (a * c))) )" ++
      " (140 print d) (150 end)))"

quadratic1 = "(define quadratic1 '(" ++
    "(100 input \"What is the value of A\" a )" ++
    "(110 input \"What is the value of B\" b )" ++
    "(120 input \"What is the value of C\" c )" ++
    "(130 let d = ((b * b) - (4.0 * (a * c))) )" ++
    "(140 if (d < 0) then 230 )" ++ 
    "(150 let i = 0 )" ++ 
    "(160 let s = 1 )" ++
    "(170 let s = ((s + (d / s)) / 2.0) )" ++
    "(180 let i = (i + 1) )" ++
    "(190 if (i < 10) then 170 )" ++
    "(200 print \"The 1st root is: \" (((-1.0 * b) + s) / (2.0 * a)) )" ++ 
    "(210 print \"The 2nd root is: \" (((-1.0 * b) - s) / (2.0 * a)) )" ++ 
    "(220 end )" ++
    "(230 print \"Imaginary roots.\" )" ++
    "(240 end )))"

quadratic2 = "(define quadratic2 '(" ++
    "(100 input \"What is the value of A\" a )" ++
    "(110 input \"What is the value of B\" b )" ++
    "(120 input \"What is the value of C\" c )" ++
    "(130 let d = ((b * b) - (4.0 * (a * c))) )" ++
    "(140 if (d < 0) then 190 )" ++
    "(150 gosub 210 )" ++
    "(160 print \"The 1st root is: \" (((-1.0 * b) + s) / (2.0 * a)) )" ++
    "(170 print \"The 2nd root is: \" (((-1.0 * b) - s) / (2.0 * a)) )" ++
    "(180 end )" ++
    "(190 print \"Imaginary roots.\" )" ++
    "(200 end )" ++
    "(210 let s = 1 )" ++
    "(220 for i = 1 to 10 )" ++
    "(230 let s = ((s + (d / s)) / 2.0) )" ++
    "(240 next i )" ++
    "(250 return )" ++
    "(260 i = 2)))"

guess = "(define guess '( (100 print tab(33) \"GUESS\" ) (110 print tab(15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (120 print ) (130 print \"THIS IS A NUMBER GUESSING GAME. I'LL THINK\" ) (140 print \"OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.\" ) (150 print \"THEN YOU HAVE TO GUESS WHAT IT IS.\" ) (160 print ) (170 input \"WHAT LIMIT DO YOU WANT\" l ) (180 print ) (190 let l1 = int (((log (l) / log (2)) + 1)) ) (200 print \"I'M THINKING OF A NUMBER BETWEEN 1 AND \" l ) (210 let g = 1 ) (220 let m = int (((l * rnd (1)) + 1)) ) (230 print ) (240 input \"WHAT IS YOUR GUESS\" n ) (250 print ) (260 if (n > 0) then 290 ) (270 print \"ILLEGAL VALUE.\" ) (280 goto 230 ) (290 if (n <= l) then 320 ) (300 print \"ILLEGAL VALUE.\" ) (310 goto 230 ) (320 if (n = m) then 390 ) (330 let g = (g + 1) ) (340 if (n > m) then 370 ) (350 print \"TOO LOW. TRY A BIGGER ANSWER.\" ) (360 goto 230 ) (370 print \"TOO HIGH. TRY A SMALLER ANSWER.\" ) (380 goto 230 ) (390 print \"THAT'S IT! YOU GOT IT IN \" g \" TRIES.\" ) (400 if (g < l1) then 440 ) (410 if (g = l1) then 450 ) (420 print \"YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY \" l1 \" TRIES.\" ) (430 end ) (440 print! \"VERY \" ) (450 print \"GOOD.\" ) (460 end )))"

hamurabi = "(define hamurabi '((1000 print \"HAMURABI: Game of Hamurabi - Version 1.01\" )(1010 print )(1020 print \"Corona Data Systems, Inc.\" )(1030 print )(1040 print \"HAMURABI - \" )(1050 print \"WHERE YOU GOVERN THE ANCIENT KINGDOM OF SUMERIA.\" )(1060 print \"THE OBJECT IS TO FIGURE OUT HOW THE GAME WORKS!!\" )(1070 print \"IF YOU WANT TO QUIT, SELL ALL YOUR LAND.\" )(1080 let a1 = 100 )(1090 let a2 = 5 )(1100 let a3 = 0 )(1110 let b1 = 2800 )(1120 let b2 = 200 )(1130 let b3 = 3 )(1140 let b4 = 3000 )(1150 let c1 = 1000 )(1160 let j = 1 )(1170 print )(1180 print \"HAMURABI, I BEG TO REPORT THAT LAST YEAR \" a3 \" PEOPLE\" )(1190 print \"STARVED AND \" a2 \" PEOPLE CAME TO THE CITY.\" )(1200 if (j > 0) then 1230 )(1210 let a1 = (a1 - int ((a1 / 2))) )(1220 print \"THE PLAGUE KILLED HALF THE PEOPLE.\" )(1230 print \"THE POPULATION IS NOW \" a1 \".\" )(1240 print \"WE HARVESTED \" b4 \" BUSHELS AT \" b3 \" BUSHELS PER ACRE.\" )(1250 print \"RATS DESTROYED \" b2 \" BUSHELS, LEAVING \" b1 \" BUSHELS\" )(1260 print \"IN THE STOREHOUSES.\" )(1270 print \"THE CITY OWNS \" c1 \" ACRES OF LAND.\" )(1280 let c2 = (17 + int ((6 * rnd (1)))) )(1290 print \"LAND IS WORTH \" c2 \" BUSHELS PER ACRE.\" )(1300 print )(1310 print \"HAMURABI...\" )(1320 print )(1330 input \"HOW MANY ACRES DO YOU WISH TO BUY\" i )(1340 print )(1350 let i = int (abs (i)) )(1360 if (i = 0) then 1430 )(1370 let j = (i * c2) )(1380 if (j <= b1) then 1410 )(1390 gosub 1850 )(1400 goto 1330 )(1410 let b1 = (b1 - j) )(1420 let c1 = (c1 + i) )(1430 input \"HOW MANY ACRES DO YOU WISH TO SELL\" i )(1440 print )(1450 let i = abs (i) )(1460 if (i = 0) then 1530 )(1470 if (i < c1) then 1510 )(1480 if (i = c1) then end )(1490 gosub 1850 )(1500 goto 1430 )(1510 let c1 = (c1 - i) )(1520 let b1 = (b1 + (c2 * i)) )(1530 input \"HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD\" i )(1540 print )(1550 let i = int (abs (i)) )(1560 if (i <= b1) then 1590 )(1570 gosub 1850 )(1580 goto 1530 )(1590 let b1 = (b1 - i) )(1600 let a3 = (a1 - int ((i / 20))) )(1610 let a2 = 0 )(1620 if (a3 >= 0) then 1650 )(1630 let a2 = ((-1 * a3) / 2) )(1640 let a3 = 0 )(1650 input \"HOW MANY ACRES SHALL WE PLANT\" i )(1660 print )(1670 let i = int (abs (i)) )(1680 if (i > c1) then 1710 )(1690 let j = int ((i / 2)) )(1700 if (j <= b1) then 1730 )(1710 gosub 1850 )(1720 goto 1650 )(1730 if (i > (10 * a1)) then 1710 )(1740 let b1 = (b1 - j) )(1750 let b3 = (int ((5 * rnd (1))) + 1) )(1760 let b4 = (b3 * i) )(1770 let b2 = int ((((b1 + b4) * 0.07) * rnd (1))) )(1780 let b1 = ((b1 - b2) + b4) )(1790 let j = int ((10 * rnd (1))) )(1800 let a2 = int ((a2 + ((((5 - b3) * b1) / 600) + 1))) )(1810 if (a2 <= 50) then 1830 )(1820 let a2 = 50 )(1830 let a1 = (a1 + (a2 - a3)) )(1840 goto 1170 )(1850 print \"HAMURABI, THINK AGAIN - \" )(1860 print \"YOU ONLY HAVE \" a1 \" PEOPLE, \" c1 \" ACRES, AND \" )(1870 print b1 \" BUSHELS IN STOREHOUSES.\" )(1880 return )))"

battleship = "(define battle '((1020 rem -- battle written by ray westergard 1 / 7 )(1030 rem copyright 1971 by the regents of the univ of calif )(1040 rem produced at the lawrence hall of science berkeley )(1050 dim f (6 6) )(1060 dim w (6 6) )(1070 dim u (4) )(1080 dim v (4) )(1090 dim t (6) )(1100 dim l (3) )(1110 for x = 1 to 6 )(1120 for y = 1 to 6 )(1130 let f (x y) = 0 )(1140 next y )(1150 next x )(1160 for i0 = 1 to 3 )(1170 let n = (4 - i0) )(1180 for j0 = 1 to 2 )(1190 let a = int (((6 * rnd (1)) + 1)) )(1200 let b = int (((6 * rnd (1)) + 1)) )(1210 let d = int (((4 * rnd (1)) + 1)) )(1220 if (f (a b) > 0) then 1190 )(1230 let m = 0 )(1240 on d goto 1250 1470 1750 1970 )(1250 let v (1) = b )(1260 let v (2) = 7 )(1270 let v (3) = 7 )(1280 for k0 = 1 to n )(1290 if (m > 1) then 1350 )(1300 if (v (k0) = 6) then 1340 )(1310 if (f (a (v (k0) + 1)) > 0) then 1340 )(1320 let v ((k0 + 1)) = (v (k0) + 1) )(1330 goto 1410 )(1340 let m = 2 )(1350 if ((v (1) < v (2)) and (v (1) < v (3))) then let z = v (1) )(1360 if ((v (2) < v (1)) and (v (2) < v (3))) then let z = v (2) )(1370 if ((v (3) < v (1)) and (v (3) < v (2))) then let z = v (3) )(1380 if (z = 1) then 1190 )(1390 if (f (a (z - 1)) > 0) then 1190 )(1400 let v ((k0 + 1)) = (z - 1) )(1410 next k0 )(1420 let f (a b) = ((9 - (2 * i0)) - j0) )(1430 for k1 = 1 to n )(1440 let f (a v ((k1 + 1))) = f (a b) )(1450 next k1 )(1460 goto 2280 )(1470 let u (1) = a )(1480 let v (1) = b )(1490 let u (2) = 0 )(1500 let u (3) = 0 )(1510 let v (2) = 0 )(1520 let v (3) = 0 )(1530 for k2 = 1 to n )(1540 if (m > 1) then 1620 )(1550 if ((u (k2) = 1) or (v (k2) = 1)) then 1610 )(1560 if (f ((u (k2) - 1) (v (k2) - 1)) > 0) then 1610 )(1570 if ((f ((u (k2) - 1) v (k2)) > 0) and (f ((u (k2) - 1) v (k2)) = f (u (k2) (v (k2) - 1)))) then 1610 )(1580 let u ((k2 + 1)) = (u (k2) - 1) )(1590 let v ((k2 + 1)) = (v (k2) - 1) )(1600 goto 1730 )(1610 let m = 2 )(1620 if ((u (1) > u (2)) and (u (1) > u (3))) then let z1 = u (1) )(1630 if ((u (2) > u (1)) and (u (2) > u (3))) then let z1 = u (2) )(1640 if ((u (3) > u (1)) and (u (3) > u (2))) then let z1 = u (3) )(1650 if ((v (1) > v (2)) and (v (1) > v (3))) then let z2 = v (1) )(1660 if ((v (2) > v (1)) and (v (2) > v (3))) then let z2 = v (2) )(1670 if ((v (3) > v (1)) and (v (3) > v (2))) then let z2 = v (3) )(1680 if ((z1 = 6) or (z2 = 6)) then 1190 )(1690 if (f ((z1 + 1) (z2 + 1)) > 0) then 1190 )(1700 if ((f (z1 (z2 + 1)) > 0) and (f (z1 (z2 + 1)) = f ((z1 + 1) z2))) then 1190 )(1710 let u ((k2 + 1)) = (z1 + 1) )(1720 let v ((k2 + 1)) = (z2 + 1) )(1730 next k2 )(1740 goto 2240 )(1750 let u (1) = a )(1760 let u (2) = 7 )(1770 let u (3) = 7 )(1780 for k3 = 1 to n )(1790 if (m > 1) then 1850 )(1800 if (u (k3) = 6) then 1840 )(1810 if (f ((u (k3) + 1) b) > 0) then 1840 )(1820 let u ((k3 + 1)) = (u (k3) + 1) )(1830 goto 1910 )(1840 let m = 2 )(1850 if ((u (1) < u (2)) and (u (1) < u (3))) then let z = u (1) )(1860 if ((u (2) < u (1)) and (u (2) < u (3))) then let z = u (2) )(1870 if ((u (3) < u (1)) and (u (3) < u (2))) then let z = u (3) )(1880 if (z = 1) then 1190 )(1890 if (f ((z - 1) b) > 0) then 1190 )(1900 let u ((k3 + 1)) = (z - 1) )(1910 next k3 )(1920 let f (a b) = ((9 - (2 * i0)) - j0) )(1930 for k4 = 1 to n )(1940 let f (u ((k4 + 1)) b) = f (a b) )(1950 next k4 )(1960 goto 2280 )(1970 let u (1) = a )(1980 let v (1) = b )(1990 let u (2) = 7 )(2000 let u (3) = 7 )(2010 let v (2) = 0 )(2020 let v (3) = 0 )(2030 for k5 = 1 to n )(2040 if (m > 1) then 2120 )(2050 if ((u (k5) = 6) or (v (k5) = 1)) then 2110 )(2060 if (f ((u (k5) + 1) (v (k5) - 1)) > 0) then 2110 )(2070 if ((f ((u (k5) + 1) v (k5)) > 0) and (f ((u (k5) + 1) v (k5)) = f (u (k5) (v (k5) - 1)))) then 2110 )(2080 let u ((k5 + 1)) = (u (k5) + 1) )(2090 let v ((k5 + 1)) = (v (k5) - 1) )(2100 goto 2230 )(2110 let m = 2 )(2120 if ((u (1) < u (2)) and (u (1) < u (3))) then let z1 = u (1) )(2130 if ((u (2) < u (1)) and (u (2) < u (3))) then let z1 = u (2) )(2140 if ((u (3) < u (1)) and (u (3) < u (2))) then let z1 = u (3) )(2150 if ((v (1) > v (2)) and (v (1) > v (3))) then let z2 = v (1) )(2160 if ((v (2) > v (1)) and (v (2) > v (3))) then let z2 = v (2) )(2170 if ((v (3) > v (1)) and (v (3) > v (2))) then let z2 = v (3) )(2180 if ((z1 = 1) or (z2 = 6)) then 1190 )(2190 if (f ((z1 - 1) (z2 + 1)) > 0) then 1190 )(2200 if ((f (z1 (z2 + 1)) > 0) and (f (z1 (z2 + 1)) = f ((z1 - 1) z2))) then 1190 )(2210 let u ((k5 + 1)) = (z1 - 1) )(2220 let v ((k5 + 1)) = (z2 + 1) )(2230 next k5 )(2240 let f (a b) = ((9 - (2 * i0)) - j0) )(2250 for k6 = 1 to n )(2260 let f (u ((k6 + 1)) v ((k6 + 1))) = f (a b) )(2270 next k6 )(2280 next j0 )(2290 next i0 )(2300 for i3 = 1 to 6 )(2310 for j3 = 1 to 6 )(2320 let w (i3 j3) = 0 )(2330 next j3 )(2340 next i3 )(2350 for i4 = 1 to 3 )(2360 let l (i4) = 0 )(2370 next i4 )(2380 let t (1) = 2 )(2390 let t (2) = 2 )(2400 let t (3) = 1 )(2410 let t (4) = 1 )(2420 let t (5) = 0 )(2430 let t (6) = 0 )(2440 let s = 0 )(2450 let h = 0 )(2455 rem modified by lrw 4/9/04 so game is only 2/3 pointless instead of 9/10 pointless :)(2460 print )(2465 print \"  1 2 3 4 5 6\")(2470 for i5 = 1 to 6 )(2475 print! i5 \" \")(2480 for j5 = 1 to 6 )(2490 if (w (i5 j5) < 1) then print! \". \")(2495 if (w (i5 j5) > 0) then print! w (i5 j5) \" \")(2500 next j5 )(2510 print )(2520 next i5 )(2530 print )(2540 input \"ROW\" y )(2550 input \"COLUMN\" x )(2560 if (((x < 1) or (x > 6)) or (int (x) <> abs (x))) then 2580 )(2570 if (((y > 0) and (y < 7)) and (int (y) = abs (y))) then 2600 )(2580 print \"INVALID INPUT.  TRY AGAIN.\" )(2590 goto 2540 )(2600 let r = y )(2610 let c = x )(2620 if (f (r c) > 0) then 2660 )(2630 let s = (s + 1) )(2640 print \"SPLASH!  TRY AGAIN.\" )(2650 goto 2540 )(2660 if (t (f (r c)) < 4) then 2710 )(2670 print \"THERE USED TO BE A SHIP AT THAT POINT, BUT YOU SUNK IT.\" )(2680 print \"SPLASH!  TRY AGAIN.\" )(2690 let s = (s + 1) )(2700 goto 2540 )(2710 if (w (r c) > 0) then 2790 )(2720 let h = (h + 1) )(2730 let w (r c) = f (r c) )(2740 print \"A DIRECT HIT ON SHIP NUMBER \" f (r c) )(2750 let t (f (r c)) = (t (f (r c)) + 1) )(2760 if (t (f (r c)) >= 4) then 2840 )(2770 print \"TRY AGAIN.\" )(2780 goto 2460 )(2790 print \"YOU ALREADY PUT A HOLE IN SHIP NUMBER \" f (r c) )(2800 print \"AT THAT POINT.\" )(2810 print \"SPLASH!  TRY AGAIN.\" )(2820 let s = (s + 1) )(2830 goto 2460 )(2840 let l ((int (((f (r c) - 1) / 2)) + 1)) = (l ((int (((f (r c) - 1) / 2)) + 1)) + 1) )(2850 print \"AND YOU SUNK IT.  HURRAH FOR THE GOOD GUYS.\" )(2860 print \"SO FAR, THE BAD GUYS HAVE LOST\" )(2870 print! l (1) \" DESTROYER(S), \" l (2) \" CRUISER(S), AND \" )(2880 print l (3) \" AIRCRAFT CARRIER(S).\" )(2890 print \"YOUR CURRENT SPLASH/HIT RATIO IS \" (s / h) )(2900 if (((l (1) + l (2)) + l (3)) < 6) then 2460 )(2910 print )(2920 print \"YOU HAVE TOTALLY WIPED OUT THE BAD GUYS' FLEET\" )(2930 print \"WITH A FINAL SPLASH/HIT RATIO OF \" (s / h) )(2940 if ((s / h) > 0) then 2960 )(2950 print \"CONGRATULATIONS -- A DIRECT HIT EVERY TIME.\" )(2960 print )(2970 print \"****************************\" )(2980 print )(2990 end )))"

amazing = "(define amazing '( (1000 print tab (28) \"AMAZING PROGRAM\" ) (1010 print tab (15) \"CREATIVE COMPUTING MORRISTOWN, NEW JERSEY\" ) (1020 print ) (1030 input \"WHAT IS YOUR WIDTH\" h ) (1040 input \"WHAT IS YOUR HEIGHT\" u ) (1050 dim w (h u) ) (1060 dim v (h u) ) (1070 print ) (1080 let q = 0 ) (1090 let z = 0 ) (1100 let x = int (((rnd (1) * h) + 1)) ) (1110 for i1 = 1 to h ) (1120 if (i1 = x) then 1150 ) (1130 print! \".--\" ) (1140 goto 1160 ) (1150 print! \". \" ) (1160 next i1 ) (1170 print \".\" ) (1180 let c = 1 ) (1190 let w (x 1) = c ) (1200 let c = (c + 1) ) (1210 let r = x ) (1220 let s = 1 ) (1230 goto 1340 ) (1240 if (r <> h) then 1320 ) (1250 if (s <> u) then 1290 ) (1260 let r = 1 ) (1270 let s = 1 ) (1280 goto 1330 ) (1290 let r = 1 ) (1300 let s = (s + 1) ) (1310 goto 1330 ) (1320 let r = (r + 1) ) (1330 if (w (r s) = 0) then 1240 ) (1340 if ((r - 1) = 0) then 1700 ) (1350 if (w ((r - 1) s) <> 0) then 1700 ) (1360 if ((s - 1) = 0) then 1510 ) (1370 if (w (r (s - 1)) <> 0) then 1510 ) (1380 if (r = h) then 1420 ) (1390 if (w ((r + 1) s) <> 0) then 1420 ) (1400 let x = int (((rnd (1) * 3) + 1)) ) (1410 on x goto 2070 2140 2210 ) (1420 if (s <> u) then 1460 ) (1430 if (z = 1) then 1490 ) (1440 let q = 1 ) (1450 goto 1470 ) (1460 if (w (r (s + 1)) <> 0) then 1490 ) (1470 let x = int (((rnd (1) * 3) + 1)) ) (1480 on x goto 2070 2140 2300 ) (1490 let x = int (((rnd (1) * 2) + 1)) ) (1500 on x goto 2070 2140 ) (1510 if (r = h) then 1620 ) (1520 if (w ((r + 1) s) <> 0) then 1620 ) (1530 if (s <> u) then 1570 ) (1540 if (z = 1) then 1600 ) (1550 let q = 1 ) (1560 goto 1580 ) (1570 if (w (r (s + 1)) <> 0) then 1600 ) (1580 let x = int (((rnd (1) * 3) + 1)) ) (1590 on x goto 2070 2210 2300 ) (1600 let x = int (((rnd (1) * 2) + 1)) ) (1610 on x goto 2070 2210 ) (1620 if (s <> u) then 1660 ) (1630 if (z = 1) then 1690 ) (1640 let q = 1 ) (1650 goto 1670 ) (1660 if (w (r (s + 1)) <> 0) then 1690 ) (1670 let x = int (((rnd (1) * 2) + 1)) ) (1680 on x goto 2070 2300 ) (1690 goto 2070 ) (1700 if ((s - 1) = 0) then 1910 ) (1710 if (w (r (s - 1)) <> 0) then 1910 ) (1720 if (r = h) then 1830 ) (1730 if (w ((r + 1) s) <> 0) then 1830 ) (1740 if (s <> u) then 1780 ) (1750 if (z = 1) then 1810 ) (1760 let q = 1 ) (1770 goto 1790 ) (1780 if (w (r (s + 1)) <> 0) then 1810 ) (1790 let x = int (((rnd (1) * 3) + 1)) ) (1800 on x goto 2140 2210 2300 ) (1810 let x = int (((rnd (1) * 2) + 1)) ) (1820 on x goto 2140 2210 ) (1830 if (s <> u) then 1870 ) (1840 if (z = 1) then 1900 ) (1850 let q = 1 ) (1860 goto 1880 ) (1870 if (w (r (s + 1)) <> 0) then 1900 ) (1880 let x = int (((rnd (1) * 2) + 1)) ) (1890 on x goto 2140 2300 ) (1900 goto 2140 ) (1910 if (r = h) then 2010 ) (1920 if (w ((r + 1) s) <> 0) then 2010 ) (1930 if (s <> u) then 1970 ) (1940 if (z = 1) then 2000 ) (1950 let q = 1 ) (1960 goto 2150 ) (1970 if (w (r (s + 1)) <> 0) then 2000 ) (1980 let x = int (((rnd (1) * 2) + 1)) ) (1990 on x goto 2210 2300 ) (2000 goto 2210 ) (2010 if (s <> u) then 2050 ) (2020 if (z = 1) then 1240 ) (2030 let q = 1 ) (2040 goto 2060 ) (2050 if (w (r (s + 1)) <> 0) then 1240 ) (2060 goto 2300 ) (2070 let w ((r - 1) s) = c ) (2080 let c = (c + 1) ) (2090 let v ((r - 1) s) = 2 ) (2100 let r = (r - 1) ) (2110 if (c = ((h * u) + 1)) then 2500 ) (2120 let q = 0 ) (2130 goto 1340 ) (2140 let w (r (s - 1)) = c ) (2150 let c = (c + 1) ) (2160 let v (r (s - 1)) = 1 ) (2170 let s = (s - 1) ) (2180 if (c = ((h * u) + 1)) then 2500 ) (2190 let q = 0 ) (2200 goto 1340 ) (2210 let w ((r + 1) s) = c ) (2220 let c = (c + 1) ) (2230 if (v (r s) = 0) then 2260 ) (2240 let v (r s) = 3 ) (2250 goto 2270 ) (2260 let v (r s) = 2 ) (2270 let r = (r + 1) ) (2280 if (c = ((h * u) + 1)) then 2500 ) (2290 goto 1700 ) (2300 if (q = 1) then 2400 ) (2310 let w (r (s + 1)) = c ) (2320 let c = (c + 1) ) (2330 if (v (r s) = 0) then 2360 ) (2340 let v (r s) = 3 ) (2350 goto 2370 ) (2360 let v (r s) = 1 ) (2370 let s = (s + 1) ) (2380 if (c = ((h * u) + 1)) then 2500 ) (2390 goto 1340 ) (2400 let z = 1 ) (2410 if (v (r s) = 0) then 2450 ) (2420 let v (r s) = 3 ) (2430 let q = 0 ) (2440 goto 1240 ) (2450 let v (r s) = 1 ) (2460 let q = 0 ) (2470 let r = 1 ) (2480 let s = 1 ) (2490 goto 1330 ) (2500 for j1 = 1 to u ) (2510 print! \"I\" ) (2520 for i2 = 1 to h ) (2530 if (v (i2 j1) < 2) then 2560 ) (2540 print! \" \" ) (2550 goto 2570 ) (2560 print! \" I\" ) (2570 next i2 ) (2580 print ) (2590 for i3 = 1 to h ) (2600 if (v (i3 j1) = 0) then 2640 ) (2610 if (v (i3 j1) = 2) then 2640 ) (2620 print! \": \" ) (2630 goto 2650 ) (2640 print! \":--\" ) (2650 next i3 ) (2660 print \".\" ) (2670 next j1 ) (2680 end )))"

data Sexpr = Symbol String | Number Int | Floating Double | Nil | Cons Sexpr Sexpr deriving (Eq)

car (Cons a b) = a
cdr (Cons a b) = b

instance Show Sexpr where
    show (Symbol x) = x
    show (Number x) = show x
    show (Floating x) = show x
    show Nil = "()"
    show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"

showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

data Bytecode = End {line :: Int} | Push {line :: Int, arg :: Value} | Print {line :: Int} | PrintBang {line :: Int} | 
                Add {line :: Int} | Mult {line :: Int} | Sub {line :: Int} | Div {line :: Int} | Pow {line :: Int} |
                Load {line :: Int} | Store {line :: Int} | Input {line :: Int} | Equal {line :: Int} | 
                NotEqual {line :: Int} | Greater {line :: Int} | GEqual {line :: Int} | Less {line :: Int} | 
                LEqual {line :: Int} | IfThen {line :: Int} | Goto {line :: Int} | NextLine {line :: Int} | 
                PushCallstack {line :: Int} | PopCallstack {line :: Int} | Spaces {line :: Int} | CastInt {line :: Int} |
                Rand {line :: Int} | Log {line :: Int} | Abs {line :: Int} | And {line :: Int} | Or {line :: Int} |
                ALoad {line :: Int} | ALoad2D {line :: Int} | NewArray {line :: Int} | NewArray2D {line :: Int} |
                AStore {line :: Int} | OnGoto {line :: Int} | OnGosub {line :: Int} deriving (Eq)

instance Show Bytecode where
    show (End l) = "end"
    show (Push l a) = "push " ++ show a
    show (Print l) = "print"
    show (PrintBang l) = "printbang"
    show (Add l) = "add"
    show (Mult l) = "mult"
    show (Sub l) = "sub"
    show (Div l) = "div"
    show (Pow l) = "pow"
    show (Load l) = "load"
    show (Store l) = "store"
    show (Input l) = "input"
    show (Equal l) = "equal"
    show (NotEqual l) = "notequal"
    show (Greater l) = "greater"
    show (GEqual l) = "gequal"
    show (Less l) = "less"
    show (LEqual l) = "lequal"
    show (IfThen l) = "ifthen"
    show (Goto l) = "goto"
    show (NextLine l) = "nextline"
    show (PushCallstack l) = "pushcallstack"
    show (PopCallstack l) = "popcallstack"
    show (Spaces l) = "spaces"
    show (CastInt l) = "castint"
    show (Rand l) = "rand"
    show (Log l) = "log"
    show (Abs l) = "abs"
    show (And l) = "and"
    show (Or l) = "or"
    show (ALoad l) = "aload"
    show (ALoad2D l) = "aload2d"
    show (NewArray l) = "newarray"
    show (NewArray2D l) = "newarray2d"
    show (AStore l) = "astore"
    show (OnGoto l) = "ongoto"
    show (OnGosub l) = "ongosub"

data Value = VIntegral Int | VFloating Double | VString String | VSymbol {name :: String, val :: Value} |
             VBool Bool | VStatement [Bytecode] | VIntegerList [Int] | Null | VPair (Value, Value) deriving (Eq)

instance Show Value where
    show (VIntegral x) = show x
    show (VFloating d) = show d
    show (VString s) = s
    show (VBool b) = show b
    show Null = "Null"
    show (VSymbol vr vl) = show vl
    show (VStatement s) = show s
    show (VIntegerList is) = show is

{-

{symbol} ::= {first} {symbolic}* | {string}
{first} ::= {misc} | {lower}
{symbolic} ::= {first} | {digit}
{misc} ::= '<' | '>' | '^' | '+' | '-' | '*' | '/' | '='
{integernum} ::= {digit}+
{number} ::= {digit}+ . {digit}+
S ::= () | (E) | '(E) | A | (S . S)
E ::= (E)E | S E | S                --> This returns a pair of sexprs
A ::= {symbol} | {number} | {integernum}

-}

quote = do
    x <- char '"'
    y <- (many quotedString)
    z <- symb "\""
    return ([x] ++ y ++ z)

quotedString = alphanum +++ (sat isSpace) +++ quotedMisc

quotedMisc = do
    r <- item--token item
    if (r `elem` ['"']) then mzero else
        if (isPrint r == True) then return r else mzero

cdigit = do 
    c <- sat isDigit
    return c

integernum = (do
    r <- many1 cdigit
    return (read r :: Int)) +++
    (do
        s <- char '-'
        r <- many1 cdigit
        return (read ([s] ++ r) :: Int))

number = (do
    r <- many1 cdigit
    d <- symb "."
    l <- many1 cdigit
    return (read (r ++ d ++ l) :: Double)) +++
    (do
        s <- char '-'
        r <- many1 cdigit
        d <- symb "."
        l <- many1 cdigit
        return (read ([s] ++ r ++ d ++ l) :: Double))

misc = do
    r <- item--token item
    let miscVals = ['<', '>', '^', '+', '-', '*', '/', '=', '!', ':', '.', '\'', ',', '$']
    if (r `elem` miscVals) then return r else mzero

first = misc +++ lower

symbolic = first +++ cdigit --(token first) +++ (token cdigit)

symbol = (do
    f <- first
    s <- token (many symbolic)
    return (f:s)) +++ quote

nil = do
    symb "("
    symb ")"
    return Nil

wrappedE = do
    (symb "(" +++ symb "'(")
    res <- token e
    symb ")"
    return res

ss = do
    symb "("
    left <- token s
    symb "."
    right <- token s
    symb ")"
    return $ Cons left right
    
ee = do
    res <- wrappedE
    recur <- token e
    return $ Cons res recur

se = do
    left <- token s
    right <- token e
    return $ Cons left right

s = nil +++ wrappedE +++ a +++ ss

e = ee +++ se +++ (do {res <- s; return $ Cons res Nil})

--A :: Parser Sexpr
a :: Parser Sexpr
a =     (do
            n <- number
            return $ Floating n) +++
            (do
                n <- integernum
                return $ Number n) +++
                (do
                    s <- symbol
                    return $ Symbol s)

p str = let result = parse s str
        in if (result == []) then Symbol "Error parsing string"
           else fst (result !! 0)

-- Parses the string and returns it in a form that is compiler-friendly
analyze :: [Char] -> Sexpr
analyze str = let result = p str
              in case result of 
                (Symbol s) -> Symbol s -- Parser returned an error
                s -> car $ cdr $ cdr s

printBytecode :: [Bytecode] -> IO ()
printBytecode [] = putStr ""
printBytecode (x:xs) = do
    putStr (show $ line x)
    putStr ": "
    putStrLn (show x)
    printBytecode xs

data Basic = String' String | Integer' Int | Floating' Double | StatementList [Basic] |
             Line Int Basic | Lines [Basic] | Variable Basic | Function String Basic | Value Basic | 
             Constant Basic | Statement {getName :: String, body :: Basic} | 
             Expression String Basic | ExpressionList [Basic] | 
             Array' String Int Basic | IntegerList [Basic] | None deriving (Eq)

-- This helps us implement (super simple) compile-time type checking
data Type = TVariable {typeid :: String} | TArray {typeid :: String} deriving (Show, Eq)

instance Show Basic where
    show (String' s) = s
    show (Integer' i) = show i
    show (Floating' f) = show f
    show (StatementList xs) = show xs
    show (Line l b) = show l ++ ": " ++ show b
    show (Variable b) = "Var " ++ show b
    show (Function s b) = "Function " ++ s ++ " -> " ++ show b
    show (Array' n i b) = "Array -> " ++ n ++ " " ++ show i ++ " " ++ show b
    show (Statement s b) = "{Statement " ++ s ++ " -> " ++ show b ++ "}"
    show (Expression s b) = "{Expression " ++ s ++ " -> " ++ show b ++ "}"
    show (ExpressionList xs) = show xs
    show (Constant c) = "Const " ++ show c
    show (IntegerList is) = "{IntegerList -> " ++ show is ++ "}"
    show None = "None"

type ParseTree a = StateT (Sexpr, [Type]) Maybe a

item' :: ParseTree Sexpr
item' = StateT $ \(s, t) -> if s == Nil then Nothing else 
    case (car s) of
        Nil -> Nothing
        s' -> Just (s', (cdr s, t))

string' cs = do {i <- item'; if (Symbol cs) == i then return i else mzero}

symb' :: Sexpr -> ParseTree Sexpr
symb' cs = do {i <- item'; if cs == i then return i else mzero}

-- Functions for converting a Sexpr (parse tree) into Basic

lookupEnv' s@(Symbol s') [] = Nothing
lookupEnv' s@(Symbol s') (t:ts) = if typeid t == s' then Just t else lookupEnv' s ts

lookupEnv s@(Symbol _) = StateT $ \(s', t) ->
    case (lookupEnv' s t) of
        Nothing -> Nothing
        Just t' -> Just (t', (s', t))

modifyEnv tid v = StateT $ \(s, t) ->
    let entry = runStateT (lookupEnv tid) (s, t) in
        case entry of
            Nothing -> Just (v, (s, [v] ++ t))
            Just (t', _) -> Just (v, (s, [v] ++ (filter (\x -> typeid x /= typeid v) t)))

iD = do
    (Symbol (s:ss)) <- item'
    if s == '"' then mzero
    else if isLetter s == False then mzero
    else return $ Variable $ String' (s:ss)

basicString =
    do
        (Symbol s@('"':xs)) <- item'
        return $ Constant $ String' s

integer' :: ParseTree Basic
integer' = do
    (Number i) <- item'
    return $ Constant $ Integer' i

integerList :: ParseTree Basic
integerList =
    (do
        i <- integer'
        (IntegerList is) <- integerList
        return $ IntegerList $ [i] ++ is) `mplus`
    (do
        i <- integer'
        return $ IntegerList [i])

statements :: ParseTree Basic
statements =
    (do
        s <- statement
        string' ":"
        (StatementList ss) <- statements
        return $ StatementList ([s] ++ ss)) `mplus`
    (do
        s <- statement
        return $ StatementList [s])

statement :: ParseTree Basic
statement = dim `mplus` end `mplus` for `mplus` goto `mplus` if' `mplus` input `mplus` let' `mplus` next `mplus` on' `mplus` print' `mplus` return' `mplus` setVar

rem :: ParseTree Basic
rem = do
    (Symbol r) <- string' "rem"
    return None

dim :: ParseTree Basic
dim = do
    (Symbol d) <- string' "dim"
    a@(Array' s i e) <- array'
    modifyEnv (Symbol s) (TArray s)
    return $ Statement d a

end :: ParseTree Basic
end = do
    (Symbol e) <- string' "end"
    return $ Statement e None

for :: ParseTree Basic
for = do
    (Symbol f) <- string' "for"
    i <- iD
    string' "="
    ts@(Statement t (ExpressionList es)) <- to
    return $ Statement f $ ExpressionList [i, (Statement t $ ExpressionList ([i] ++ es))]

to :: ParseTree Basic
to = do
    e <- expression
    (Symbol t) <- string' "to"
    e' <- expression
    return $ Statement t $ ExpressionList [e, e']

goto :: ParseTree Basic
goto = do
    (Symbol e) <- (string' "goto" `mplus` string' "gosub")
    (Number i) <- item'
    return $ Statement e $ Integer' i

if' :: ParseTree Basic
if' = do
    (Symbol i) <- string' "if"
    e <- expression
    t <- then'
    return $ Statement i $ ExpressionList [e, t]

then' :: ParseTree Basic
then' = do
    (Symbol t) <- string' "then"
    e <- (nesting statement `mplus` statement `mplus` expression)
    return $ Statement t e

input :: ParseTree Basic
input = do
    (Symbol i) <- string' "input"
    s <- basicString
    var <- iD
    return $ Statement i $ ExpressionList [s, var]

let' :: ParseTree Basic
let' = do
    (Symbol l) <- string' "let"
    v <- variable
    string' "="
    e <- expression
    return $ Statement l $ ExpressionList [v, e]

next :: ParseTree Basic
next = do
    (Symbol n) <- string' "next"
    i <- iD
    return $ Statement n i

on' :: ParseTree Basic
on' =
    do
        (Symbol o) <- string' "on"
        e <- expression
        (Symbol g) <- item'
        if (g `elem` ["goto", "gosub"]) then do
            is <- integerList
            return $ Statement o $ ExpressionList [e, is, Statement g None]
        else mzero

print' :: ParseTree Basic
print' = 
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        t <- tab
        (ExpressionList es) <- expressionList
        return $ Statement p $ ExpressionList ([t] ++ es)) `mplus`
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        es <- expressionList
        return $ Statement p es) `mplus`
    (do
        (Symbol p) <- (string' "print" `mplus` string' "print!")
        return $ Statement p None)

return' :: ParseTree Basic
return' = do
    (Symbol r) <- string' "return"
    return $ Statement r None

tab :: ParseTree Basic
tab = do
    (Symbol t) <- string' "tab"
    e <- expression
    return $ Statement t e

setVar :: ParseTree Basic
setVar = do
    v <- variable
    string' "="
    e <- expression
    return $ Statement "let" $ ExpressionList [v, e]

nesting :: ParseTree Basic -> ParseTree Basic
nesting function = StateT $ \(s, t) -> if s == Nil then Nothing else
    case s of
        (Cons c@(Cons s s') s'') ->
            case (runStateT function (c, t)) of
                Nothing -> Nothing
                Just (res, _) -> Just (res, (s'', t))
        _ -> Nothing

expression :: ParseTree Basic
expression = 
    (do
        a <- addExp
        (Symbol s) <- item'
        if (s `elem` ["and", "or"]) then do
            e <- expression
            --return $ Expression s $ ExpressionList [a, e]
            case e of
                (Expression op@"or" (ExpressionList (l:es))) -> return $ Expression op $ ExpressionList $ [Expression s $ ExpressionList [a, l]] ++ es
                (Expression op@"and" (ExpressionList (l:es))) -> return $ Expression op $ ExpressionList $ [Expression s $ ExpressionList [a, l]] ++ es
                _ -> return $ Expression s $ ExpressionList [a, e]
        else mzero) `mplus`
    addExp `mplus` value

expressionList :: ParseTree Basic
expressionList =
    (do
        e <- expression
        (ExpressionList es) <- expressionList
        return $ ExpressionList ([e] ++ es)) `mplus`
    (do
        e <- expression
        return $ ExpressionList [e])

addExp =
    (do
        m <- value
        (Symbol op) <- item'
        if (op `elem` ["+", "-", "*", "^", "/", "=", "<>", ">", ">=", "<", "<="]) then do
            a <- addExp
            return $ Expression op $ ExpressionList [m, a]
            else mzero) `mplus` value

value = (nesting expression) `mplus` function `mplus` variable `mplus` constant

--variable = array' `mplus` iD
variable =
    (do
        st <- get
        a@(Array' s i e) <- array'
        t <- lookupEnv (Symbol s) -- if this fails we don't continue
        case t of
            (TArray _) -> return a
            _ -> do {put st; mzero}) `mplus`
    (do
        v@(Variable (String' s)) <- iD
        modifyEnv (Symbol s) (TVariable s)
        return v)

array' :: ParseTree Basic
array' = do
    (Variable (String' s)) <- iD
    e@(ExpressionList es) <- nesting expressionList
    return $ Array' s (length es) e

function = 
    do 
        (Symbol i) <- item'
        if (i `elem` ["int", "rnd", "log", "abs", "sqrt"]) then do
            e <- nesting expression
            return $ Function i e
            else mzero

constant = (do {(Number i) <- item'; return $ Constant $ Integer' i}) `mplus`
           (do {(Floating f) <- item'; return $ Constant $ Floating' f}) `mplus`
           basicString

printBasic [] = putStr ""
printBasic ((Line l x):xs) = do
    putStr (show l ++ ": ")
    putStrLn (show x)
    printBasic xs
printBasic (x:xs) = do
    putStrLn (show x)
    printBasic xs

translateSexpr' :: Sexpr -> [Type] -> ([Basic], [Type])
translateSexpr' Nil env = ([], env)
translateSexpr' (Cons (Number i) s) env = let result = runStateT statements (s, env)
                                          in case result of
                                            Just (r@(StatementList ss), (s,e)) -> ([Line i r], e)
                                            Nothing -> ([None], env)
translateSexpr' (Cons s s') env = let (l, e) = translateSexpr' s env
                                      (r, e') = translateSexpr' s' e
                                  in (l ++ r, e')

{-
translateSexpr :: Sexpr -> [Basic]
translateSexpr Nil = []
translateSexpr (Cons (Number i) s) = let result = runStateT statements s
                                     in case result of
                                        (Just (r@(StatementList ss), _)) -> [Line i r]
                                        Nothing -> []
translateSexpr (Cons s s') = translateSexpr s ++ translateSexpr s'
-}

translateSexpr :: Sexpr -> [Basic]
translateSexpr s = let (result, env) = translateSexpr' s [] in
    result

renumber :: [Basic] -> Int -> Int -> [(Int, Int)] -> ([Basic], [(Int, Int)])
renumber [] _ _ mapping = ([], mapping)
renumber (None:ls) line newline mapping = renumber ls line newline mapping
renumber basic@((Line l b):ls) line newline mapping = if l /= line then renumber basic l (newline + 1) mapping
                                                      else let s = [Line newline b]
                                                               (ys, mapping') = renumber ls line newline mapping
                                                           in if ((l, newline) `elem` mapping) then (s, mapping')
                                                              else (s ++ ys, [(l, newline)] ++ mapping')

-- Begin the actual compiler

generatePush line (Constant c) mapping = generatePush line c mapping
generatePush line (Integer' i) mapping = [Push line (VIntegral i)]
generatePush line (Floating' f) mapping = [Push line (VFloating f)]
generatePush line (String' s) mapping = [Push line (VString s)]
generatePush line (Variable (String' v)) mapping = [Push line (VString v), Load line]
generatePush line a@(Array' _ i _) mapping =
    if (i == 1) then generateSimpleArrayPush line a mapping ++ [ALoad line]
    else generateSimpleArrayPush line a mapping ++ [ALoad2D line]

-- Will push the array name and indices, but will not append a load/store operation after
generateSimpleArrayPush line (Array' s i b) mapping =
    if (i == 1) then [Push line (VString s)] ++ evalExpression line b mapping
    else [Push line (VString s)] ++ evalExpression line b mapping

renumberLine :: Int -> [(Int, Int)] -> Int
renumberLine line ((orig, new):ls) = if line == orig then new else renumberLine line ls

evalExpressionList line (ExpressionList []) mapping = []
evalExpressionList line (ExpressionList (x:xs)) mapping = evalExpression line x mapping ++ 
                                                          evalExpressionList line (ExpressionList xs) mapping

-- evalExpression deals with things like arithmetic and addition
evalExpression line None mapping = []
evalExpression line (Expression "+" rest) mapping = evalExpression line rest mapping ++ [Add line]
evalExpression line (Expression "-" rest) mapping = evalExpression line rest mapping ++ [Sub line]
evalExpression line (Expression "*" rest) mapping = evalExpression line rest mapping ++ [Mult line]
evalExpression line (Expression "/" rest) mapping = evalExpression line rest mapping ++ [Div line]
evalExpression line (Expression "^" rest) mapping = evalExpression line rest mapping ++ [Pow line]
evalExpression line (Expression "=" rest) mapping = evalExpression line rest mapping ++ [Equal line]
evalExpression line (Expression "<>" rest) mapping = evalExpression line rest mapping ++ [NotEqual line]
evalExpression line (Expression ">" rest) mapping = evalExpression line rest mapping ++ [Greater line]
evalExpression line (Expression "<" rest) mapping = evalExpression line rest mapping ++ [Less line]
evalExpression line (Expression "<=" rest) mapping = evalExpression line rest mapping ++ [LEqual line]
evalExpression line (Expression ">=" rest) mapping = evalExpression line rest mapping ++ [GEqual line]
evalExpression line (Expression "and" rest) mapping = evalExpression line rest mapping ++ [And line]
evalExpression line (Expression "or" rest) mapping = evalExpression line rest mapping ++ [Or line]
evalExpression line (Function "int" rest) mapping = evalExpression line rest mapping ++ [CastInt line]
evalExpression line (Function "log" rest) mapping = evalExpression line rest mapping ++ [Log line]
evalExpression line (Function "rnd" rest) mapping = evalExpression line rest mapping ++ [Rand line]
evalExpression line (Function "abs" rest) mapping = evalExpression line rest mapping ++ [Abs line]
evalExpression line (Function "sqrt" rest) mapping = 
    evalExpression line rest mapping ++ generatePush line (Floating' 0.5) mapping ++ [Pow line]
evalExpression line e@(ExpressionList xs) mapping = evalExpressionList line e mapping
evalExpression line s@(Statement _ _) mapping = evalStatement line s mapping
evalExpression line (Constant i) mapping = generatePush line i mapping
evalExpression line v@(Variable i) mapping = generatePush line v mapping
evalExpression line a@(Array' _ _ _) mapping = generatePush line a mapping

evalIfThenStatement line (Constant (Integer' i)) mapping = 
    let jump = VIntegral $ renumberLine i mapping in
        [Push line (VStatement $ [Push line jump, Goto line])] ++ [IfThen line]
evalIfThenStatement line e mapping = [Push line $ VStatement $ evalExpression line e mapping] ++ [IfThen line]

evalLetStatement line (ExpressionList ((Variable s):xs)) mapping = 
    generatePush line s mapping ++ evalExpressionList line (ExpressionList xs) mapping ++ [Store line]
evalLetStatement line (ExpressionList (a@(Array' s i b):xs)) mapping = 
    generatePush line a mapping ++ evalExpressionList line (ExpressionList xs) mapping ++ [AStore line]

-- Assumes "on" statement was already extracted
evalOnGoStatement line (ExpressionList (e:(IntegerList is):(Statement g _):rest)) mapping =
    let is' = map (\(Constant (Integer' i)) -> renumberLine i mapping) is in
        evalExpression line e mapping ++ [Push line (VIntegerList is')] ++ 
        (if g == "goto" then [OnGoto line]
        else [Push line (VIntegral (line + 1)), PushCallstack line, OnGoto line])

-- evalStatement deals with things like if, for, goto, etc.
evalStatement line None mapping = []
evalStatement line (Statement "end" _) mapping = [End line]
evalStatement line (Statement "if" e) mapping = evalStatement line e mapping
evalStatement line (Statement "then" e) mapping = evalIfThenStatement line e mapping
evalStatement line (Statement "input" e) mapping = evalExpression line e mapping ++ [Input line]
evalStatement line (Statement "print" e) mapping = evalExpression line e mapping ++ [Print line]
evalStatement line (Statement "print!" e) mapping = evalExpression line e mapping ++ [PrintBang line]
evalStatement line (Statement "let" e) mapping = evalLetStatement line e mapping
evalStatement line (Statement "for" (ExpressionList ((Variable v):es))) mapping = generatePush line v mapping ++ evalExpressionList line (ExpressionList es) mapping
evalStatement line (Statement "tab" e) mapping = evalExpression line e mapping ++ [Spaces line]
evalStatement line (Statement "on" e) mapping = evalOnGoStatement line e mapping
evalStatement line (Statement "dim" a@(Array' s i b)) mapping = 
    generateSimpleArrayPush line a mapping ++ [if i == 1 then NewArray line else NewArray2D line]
evalStatement line (Statement "to" (ExpressionList ((Variable (String' v)):e:es))) mapping =
    evalExpression line e mapping ++ [Store line] ++ [Push line (VString ("$" ++ v ++ "maxrange"))] ++ evalExpressionList line (ExpressionList es) mapping ++ [Store line] ++
    [Push line (VString ("$" ++ v ++ "jmp")), Push line (VIntegral $ line + 1), Store line] -- Stores the start of the for loop inside of (var name)+jmp
    --[Push line (VIntegral $ line + 1)] ++ [PushCallstack line]
evalStatement line (Statement "next" (Variable var@(String' v))) mapping =
    generatePush line var mapping ++ generatePush line var mapping ++ [Load line] ++ [Push line (VIntegral 1)] ++ [Add line] ++ [Store line] ++ -- This increments the variable
    generatePush line var mapping ++ [Load line] ++ [Push line (VString ("$" ++ v ++ "maxrange"))] ++ [Load line] ++ [LEqual line] ++ -- This performs the comparison to see if the loop is done
    [Push line $ VStatement $ [ -- Creating code which can jump to the top of the loop if necessary
        Push line (VString ("$" ++ v ++ "jmp")), Load line, Goto line -- Loads the jump location onto the stack and jumps
        --Push line (VString (v ++ "jmp")), PopCallstack line, Store line, -- Stores the jump location in variable (v ++ jmp)
        --Push line (VString (v ++ "jmp")), Load line, PushCallstack line, -- Loads the jump location to the stack and pushes it back to the callstack (for the next iteration to see it again)
        --Push line (VString (v ++ "jmp")), Load line, Goto line -- Loads the jump location onto the stack and jumps to it
        ]] ++ [IfThen line] 
        -- ++ [Push line (VString (v ++ "jmp"))] ++ [PopCallstack line] ++ [Store line] -- IfThen compares the result of checking if the loop is done, and if it isn't it executes the code which restarts the loop at the top
evalStatement line (Statement "goto" (Integer' i)) mapping = 
    generatePush line (Integer' $ renumberLine i mapping) mapping ++ [Goto line]
evalStatement line (Statement "gosub" (Integer' i)) mapping = 
    let renumbered = renumberLine i mapping 
        nextLine = line + 1 in
        generatePush line (Integer' renumbered) mapping ++ generatePush line (Integer' nextLine) mapping ++
        [PushCallstack line] ++ [Goto line]
evalStatement line (Statement "return" _) mapping = [PopCallstack line] ++ [Goto line]
evalStatement line e@(ExpressionList _) mapping = evalExpressionList line e mapping
evalStatement line e@(Expression _ _) mapping = evalExpression line e mapping
evalStatement line _ mapping = []

--evalStatement :: Basic -> [(Int, Int)] -> [Bytecode]
evalStatementList _ None mapping = []
evalStatementList _ (StatementList []) mapping = []
evalStatementList line (StatementList (s:ss)) mapping = evalStatement line s mapping ++ 
                                                        evalStatementList line (StatementList ss) mapping

compile' :: [Basic] -> [(Int, Int)] -> [Bytecode]
compile' [] mapping = []
compile' (b@(Line l s):bs) mapping = evalStatementList l s mapping ++ compile' bs mapping

compile :: Sexpr -> [Bytecode]
compile s = case s of
    (Symbol s) -> []
    _ -> let (basic, mapping) = renumber (translateSexpr s) 0 (-1) [] in
        compile' basic mapping