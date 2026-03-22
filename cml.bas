   10 rem -------------------------------
   20 rem    commodore markup language
   25 rem         by jeff ledger
   30 rem web fetch routines by c.garrett
   40 rem -------------------------------
   45 clr: dim hl$(9)
   50 tg$="":tt=0 :rem flag for tracking html tags (1 = inside tag, 0 = outside)
   60 dr=56832 :rem data register (read = receive, write = transmit)
   70 sr=56833 :rem status register (flags: rx ready, tx ready, errors)
   80 cm=56834 :rem command register (parity, echo, dtr, interrupts)
   90 ct=56835 :rem control register (baud rate, word size, stop bits)
  100 rem clear screen
  102 poke 53281,0:poke 53280,0:a$=""
  104 gosub 6000
  106 input "http://";a$
  107 if a$="q" or a$="quit" then end
  108 ul$=a$
  109 if a$="" then ul$="gunstar.one"
  110 poke53269,0: print chr$(147);chr$(5);"connecting.";:s=0:poke53297,15:tg$="":tt=0
  115 gosub 6500 : rem split domain/page
  120 rem reset acia by writing to status register
  130 poke sr,0
  140 rem control register
  150 rem 31 = 8 data bits, 1 stop bit, internal clock,
  160 rem baud setting doubled by swiftlink crystal = 38,400
  170 poke ct,31:rem poke ct,31 on c64u and 16 on vice
  180 rem command register
  190 rem no parity, no echo, dtr on, receive enabled
  200 poke cm,9
  210 rem short delay to let hardware settle
  220 for i=1 to 500:next
  230 gosub 3000
  270 rem dial tcp server (ip:port)
  280 ts$="atdt "+dm$+":80"+chr$(13)
  290 gosub 700
  291 rs$=""
  292 s=peek(sr): if (s and 8)=0 then goto 292
  293 c=peek(dr): if len(rs$)<255 then rs$=rs$+chr$(c)
  294 if right$(rs$,7)<>"connect" and right$(rs$,7)<>"connect" then goto 292
  300 crlf$=chr$(13)+chr$(10)
  310 ts$="get /"+pg$+" http/1.1"+crlf$+"host: "+dm$+crlf$+crlf$
  312 rem print:print ts$:stop
  320 gosub 700
  325 ht=0: rem flag for if html started (1 = started, 0 = not started)
  330 s=peek(sr)
  335 if (s and 8)=0 then goto 330
  340 c=peek(dr)
  345 if c<>10 and c<>13 then cr=0: goto 330
  350 cr=cr+1
  355 if cr<4 then goto 330
  360 ht=1:poke53297,15:rem turbo
  362 rt=0: rem search for <!-- tag
  364 s=peek(sr)
  366 if (s and 8)=0 then goto 364
  368 c=peek(dr)
  369 rem print chr$(c);  : rem test line
  370 if c>=97 and c<=122 then c=c-32
  372 if c=asc("<") then rt=1 : goto 364
  374 if c=asc("!") and rt=1 then rt=2: goto 364
  376 if c=asc("-") and rt=2 then rt=3: goto 364
  378 if c=asc("-") and rt=3 then rt=0: rt=0 : goto 400
  380 if c=13 then rt=0: goto 364 : rem remark not detected
  382 if pd=3 then print ".";:pd=0
  383 pd=pd+1
  385 goto 364
  400 print chr$(147);
  410 rem loop to read & process data
  420 rem -------------------------------
  440 s=peek(sr)
  442 rem get kb$ : if kb$="n" then gosub 3000 : goto 100
  450 if (s and 8)=0 then goto 440
  455 c=peek(dr)
  460 if c>=97 and c<=122 then c=c-32
  465 if c=asc("<") then tt=1: tg$="": goto 440
  470 if c=asc(">") then tt=0: goto 440
  475 if tt=1 then tg$=tg$+chr$(c)
  480 if tt=0 and tg$<>"" then gosub 1000 : tg$=""
  585 if tt=0 and tg$="" then gosub 3200
  590 goto 440
  600 end
  700 rem ----------------------------
  710 rem send string ts$ char by char
  720 rem ----------------------------
  730 if len(ts$)>0 then for i=1 to len(ts$)
  740 b=asc(mid$(ts$,i,1))
  750 gosub 800
  760 next
  770 return
  800 rem -------------------------------
  810 rem send one byte in b
  820 rem wait until transmit register is empty
  830 rem bit 4 (value 16) = transmit ready
  840 rem -------------------------------
  850 s=peek(sr)
  860 if (s and 16)=0 then 850
  870 poke dr,b
  880 return
 1000 rem -----------------------
 1010 rem process html tag in tg$
 1020 rem -----------------------
 1025 rem  print "*";tg$;"*"
 1026 if tg$="/html" then print chr$(20)+chr$(20);: gosub 3000: goto 5000
 1040 if tg$="fr" then print chr$(18);:tg$="": return
 1042 if tg$="br" then print chr$(13);: tg$=".": return
 1050 if tg$="fn" then print chr$(156):tg$="": return
 1055 if left$(tg$,2)="bc" then gosub 3300: poke53280,co:tg$="" : return
 1057 if left$(tg$,2)="tb" then gosub 3300: poke53297,co:tg$="" : return
 1060 if left$(tg$,2)="sc" then gosub 3300: poke53281,co:tg$="" : return
 1065 if left$(tg$,2)="fc" then gosub 3300: poke646,co : tg$="" : return
 1070 if left$(tg$,2)="hl" then gosub 3300: gosub3400 : tg$="" : return
 1075 if left$(tg$,2)="pk" then gosub 3300: gosub6600 : tg$="" : return
 1080 if left$(tg$,3)="mpk" then gosub 3300: gosub6800 : tg$="" : return
 1095 rem
 2000 tg$="": return
 3000 rem hangup
 3005 rs$=""
 3010 for i=1 to 3
 3015 b=asc("+")
 3020 gosub 850
 3025 for w=1 to 500:next w
 3050 next i
 3052 rs$=""
 3055 return
 3200 rem build strings to store
 3210 if len(st$)<40 and c<>13 then st$=st$+chr$(c)
 3220 if left$(st$,15)="400 bad request" then print chr$(5);" trying again!":goto 10: rem try loading again
 3240 print chr$(c);
 3250 return
 3300 rem ------------------
 3302 rem parse command data
 3304 rem ------------------
 3310 co$=mid$(tg$,3,1)
 3330 if asc(co$)>64 then co=asc(co$)-55 : return
 3340 co=val(co$) : return
 3400 rem ----------------
 3410 rem parse hyperlinks
 3420 rem ----------------
 3430 hl$(co)=mid$(tg$,5,len(tg$)):hl=co :return
 5000 rem --------------------------
 5010 rem wait for user to pick link
 5020 rem --------------------------
 5040 get b$
 5050 if b$="" then goto 5040
 5060 if b$="q" then end
 5065 if b$="n" or b$=chr$(13) then ul$="gunstar.one": goto 100
 5070 if val(b$)<10 then ul$=hl$(val(b$))
 5080 if val(b$)<10 then print chr$(147);:poke53281,0:poke53280,0 : goto 110
 5090 goto 5040
 6000 print chr$(147);chr$(142);
 6010 print chr$(153);"       commodore markup language"
 6020 print chr$(13);chr$(5);"            by: jeff ledger"
 6030 print : print
 6040 print chr$(154);"instructions:":print:print
 6050 print chr$(5);"type in a compatible address or"
 6060 print"[";chr$(154);"return";chr$(5);"] to load ";chr$(159);"www.gunstar.one"
 6070 for x=1to7 :print:next
 6080 print chr$(5);"at the end of page load:":print
 6090 print "press [";chr$(154);"q";chr$(5);"]uit to exit."
 6100 print "      [";chr$(154);"n";chr$(5);"]ew page."
 6110 print:  print "or select provided links."
 6111 print : for x=1to12:print chr$(145);:next
 6112 poke 53269,0 : rem turn off sprites
 6200 poke 53297,0:return
 6500 rem -----------------------
 6510 rem find domain name & page
 6520 rem -----------------------
 6525 pg=0 : dm$="" : pg$=""
 6530 for x=1 to len(ul$)
 6540 z$=mid$(ul$,x,1)
 6550 if z$="/" then pg=1 : z$=""
 6560 if pg=0 then dm$=dm$+z$
 6570 if pg=1 then pg$=pg$+z$
 6580 next
 6590 return
 6600 rem --------------------
 6610 rem parse pokes and data
 6620 rem --------------------
 6630 pg=0:pk$="":dt$=""
 6635 for x=4 to len(tg$)
 6640 z$=mid$(tg$,x,1)
 6650 if z$="," then pg=1 : z$=""
 6660 if pg=0 then pk$=pk$+z$
 6670 if pg=1 then dt$=dt$+z$
 6680 next
 6690 rem print "poke ";pk$;",";dt$
 6695 poke val(pk$),val(dt$)
 6700 return
 6800 rem --------------------------
 6810 rem multidata poke command mpk
 6820 rem --------------------------
 6830 pg=0:pk$="":dt$="":y=0
 6840 for x=5 to len(tg$)
 6850 z$=mid$(tg$,x,1)
 6860 if z$="," and pg=0 then pg=1 : z$="":y=y-1
 6865 if z$="," and pg=1 then pg=2 : z$="":y=y+1
 6870 if pg=0 then pk$=pk$+z$
 6880 if pg=1 then dt$=dt$+z$
 6885 if pg=2 then poke val(pk$)+y,val(dt$):dt$="":pg=1
 6900 next
 6905 poke val(pk$)+y+1,val(dt$)
 6910 return
