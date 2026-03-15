   10 rem -------------------------------
   20 rem    commodore markup language
   25 rem         by jeff ledger
   30 rem web fetch routines by c.garrett
   40 rem -------------------------------
   45 dim hl$(9)
   50 tg$="":tt=0 :rem flag for tracking html tags (1 = inside tag, 0 = outside)
   60 dr=56832 :rem data register (read = receive, write = transmit)
   70 sr=56833 :rem status register (flags: rx ready, tx ready, errors)
   80 cm=56834 :rem command register (parity, echo, dtr, interrupts)
   90 ct=56835 :rem control register (baud rate, word size, stop bits)
  100 rem clear screen
  102 poke 53281,0:poke 53280,0:poke53297,15: rem turbo
  104 print chr$(147);chr$(13);chr$(13)
  106 rem input "url: http://";ul$
  108 ul$="gunstar.one"
  110 print chr$(147);chr$(5);"parsing for cml...";:s=0
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
  280 ts$="atdt "+ul$+":80"+chr$(13)
  290 gosub 700
  291 rs$=""
  292 s=peek(sr): if (s and 8)=0 then goto 292
  293 c=peek(dr): rs$=rs$+chr$(c)
  294 if right$(rs$,7)<>"connect" and right$(rs$,7)<>"connect" then goto 292
  300 crlf$=chr$(13)+chr$(10)
  310 ts$="get / http/1.1"+crlf$+"host: "+ul$+crlf$+crlf$
  320 gosub 700
  325 ht=0: rem flag for if html started (1 = started, 0 = not started)
  330 s=peek(sr)
  335 if (s and 8)=0 then goto 330
  340 c=peek(dr)
  345 if c<>10 and c<>13 then cr=0: goto 330
  350 cr=cr+1
  355 if cr<4 then goto 330
  360 ht=1
  362 rt=0: rem search for <!-- tag
  364 s=peek(sr)
  366 if (s and 8)=0 then goto 364
  368 c=peek(dr)
  370 if c>=97 and c<=122 then c=c-32
  372 if c=asc("<") then rt=1 : goto 364
  374 if c=asc("!") and rt=1 then rt=2: goto 364
  376 if c=asc("-") and rt=2 then rt=3: goto 364
  378 if c=asc("-") and rt=3 then rt=0: rt=0 : goto 400
  380 if c=13 then rt=0: goto 364 : rem remark not detected
  382 print".";:goto 364
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
 1026 if tg$="/html" then print "eof" : gosub 3000: goto 5000
 1040 if tg$="fr" then print chr$(18);:tg$="": return
 1042 if tg$="br" then print chr$(13);: tg$=".": return
 1050 if tg$="fn" then print chr$(156):tg$="": return
 1055 if left$(tg$,2)="bc" then gosub 3300: poke53280,co:tg$="" : return
 1057 if left$(tg$,2)="tb" then gosub 3300: poke53297,co:tg$="" : return
 1060 if left$(tg$,2)="sc" then gosub 3300: poke53281,co:tg$="" : return
 1065 if left$(tg$,2)="fc" then gosub 3300: poke646,co : tg$="" : return
 1070 if left$(tg$,2)="hl" then gosub 3300: gosub3400 : tg$="" : return
 1075 rem
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
 5030 if hl=0 then end
 5040 if hl<>0 then get a$
 5050 if a$="" then goto 5040
 5060 if a$="q" then end
 5070 ul$=hl$(val(a$))
 5080 print "{clr}";:goto 110
