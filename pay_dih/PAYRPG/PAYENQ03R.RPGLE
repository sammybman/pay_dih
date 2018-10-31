000100950907     H* ********************************************************
000200950907     H*              B A N K S    D I H    L T D.              *
000300010808     H*                   PAYROLL SYSTEM                       *
000400950907     H*                                                        *
000500950907     H*                         BY                             *
000600950907     H*                                                        *
000700010808     H*                  DISERV SYSTEMS                        *
000800950907     H*                                                        *
000900950907     H*                                                        *
001000010808     H*       RPG PROGRAM NAME..: PAYENQ03R                    *
001100950907     H*                                                        *
001200010808     H*       PROGRAMMER NAME: SAMUEL BRITTON                  *
001300010808     H*       DESIGN DATE....: 2001/08/08                      *
001400950907     H*                                                        *
001500010808     H*  PROGRAM DESCRIPTION: HOLIDAY   ENQUIRY                *
001600950913     H*  MAINTENANCE                                           *
001700951220     H*  THIS PROGRAM IS DESIGNED TO DISPLAY DATES.            *
001800951220     H*                                                        *
001900950913     H*                                                        *
002000950907     H*                                                        *
002100950907     H*                                                        *
002200950907     H**********************************************************
002300111002     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
002400111002     H/define profoundui
002500010808     F* Holiday  Master File
002600010808     Fholl01    IF   e           k disk
002700941214     F*
002800010808     Fpayenq03d cf   e             workstn
002900010808     F                                     sfile(sflrec:rrn)
003000111002     F/IF DEFINED(WEB)
003100111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003200111002     F/ENDIF
003300010816     **
003400111002      /copy QCPYSRC,genpr
003500010808     **
003600010808     D arr             s             40    dim(1) ctdata perrcd(1)
003700010808     **
003800010808     D rrn             s              4  0
003900010808     **
004000010808     D                 ds
004100951122     D  STDAT                  1      8  0
004200951122     D  YER                    1      4  0
004300951130     D  MON                    5      6  0
004400951122     D  DAY                    7      8  0
004500010808     **
004600951122     D                 DS
004700951122     D  STDAT#                 1      8  0
004800951122     D  YER#                   1      4  0
004900951123     D  MON#                   5      6  0
005000951122     D  DAY#                   7      8  0
005100010808     **
005200941215     D                SDS
005300941215     D  @PGM             *PROC
005400111002     D  @USER                358    367
005500941215     **
005600951220     ** First initilisation routine
005700010808     C                   exsr      aainit
005800010808     *******************
005900010808     ** Program start **
006000010808     *******************
006100010808     C                   dow       0 = 0
006200010808
006300010808     C                   exfmt     fmt1
006400010808     **
006500010808     C                   clear                   errmsg
006600010808     ** End program (F3)
006700010808     C                   if        *in03
006800010808     C                   eval      *inlr = *on
006900010808     C                   return
007000010808     C                   endif
007100951122     **
007200951220     ** clear subfile
007300010808     C                   eval      rrn = *zeros
007400010808     C                   eval      day = *zeros
007500010808     C                   eval      *in33 = *on
007600010808     C                   write     sflctl
007700010808     C                   eval      *in33 = *off
007800010808     C                   eval      *in55 = *off
007900010816     ** type of month - Jan, Feb, Mar, etc.
008000010816     C                   eval      mtyp = get_month(mon)
008100941214     **
008200010808     C     stdat         setll     holfmt                                       START DATE
008300010808     C                   read      holfmt
008400941222     **
008500010808     C                   dow       not(%eof)
008600010808     C                   eval      stdat# = hodate
008700010808     **
008800010808     C                   if        yer = yer# and mon = mon#
008900951130     **
009000010808     ** Load subfile from holiday file
009100951122     **  Day of week
009200010816     C                   eval      dayno = dayofweek(hodate:daydesc)
009300951122     **
009400951130     **
009500010808     C                   eval      rrn = rrn + 1
009600010808     C                   write     sflrec
009700010808     C                   else
009800010808     C                   leave
009900010808     C                   endif
010000951201     **
010100010808     C                   read      holfmt
010200010808     C                   enddo
010300010808     C                   eval      *in55 = *on
010400941219     **
010500951201     ** No records found to display
010600010808     C                   if        rrn =  *zeros
010700010808     C                   call      'MESSAGE'
010800010808     C                   parm      'DIH0064'     MSGID             7
010900010808     C                   parm      *BLANKS       ERRMSG
011000010808     C                   iter
011100010808     C                   endif
011200010808     **
011300010808     C                   write     footer
011400010808     C                   leave
011500010808     C                   enddo
011600010808
011700010808     ***********************************
011800010808     **  Display Subfile of holidays  **
011900010808     ***********************************
012000010808     C                   dow       0 = 0
012100010808     C                   exfmt     sflctl
012200010808     ** F3 to exit program
012300010808     C                   if        *in03
012400010808     C                   eval      *inlr = *on
012500010808     C                   return
012600010808     C                   endif
012700010808     ** F2 to go back
012800010808     C                   if        *in02
012900010808     C                   leave
013000010808     C                   endif
013100941215     **
013200010808     C                   enddo
013300951220     ****************************************************
013400951220     **  1st initilisation routine                     **
013500951220     ****************************************************
013600010808     C     aainit        begsr
013700010816     C                   eval      coname = get_coname
013800010816     C                   eval      @date = get_sys_date
013900010808     **
014000950907     C                   TIME                    @TIME             6 0
014100010808     C                   eval      @title = arr(1)
014200010808     C                   eval      stdat = *zeros
014300950907     C                   ENDSR
014400941214     *********************************************
014500951130**
014600010808         Holiday Enquiry
