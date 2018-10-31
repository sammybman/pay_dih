000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300941003     H** PROGRAMMER - S.BRITTON                                           ABSENT
000400010612     Fpaymnt01d1cf   e             workstn
000500010612     F                                     sfile(Sflrec:rrn)
000600111002     F/IF DEFINED(WEB)
000700111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111002     F/ENDIF
000900010202     Fabstype   if   e           k disk
001000010612     Fpaypen    uf a e           k disk
001100010902     ** General PROTOTYPES
001200111002      /copy QCPYSRC,genPR
001300010202     **
001400010202     D                SDS
001500010202     D  @PGM             *PROC
001600111002     D  @USER                358    367
001700010202     **
001800010205     D arr             S             40    DIM(1) CTDATA PERRCD(1)
001900010202     D rrn             S              4  0
002000010612     **
002100010612     C     *entry        plist
002200010612     C                   parm                    paytype           2 0
002300010612     C                   parm                    pen_code          1
002400010612     C                   parm                    type_name        25
002500950802     **
002600010612     C     trn_key       klist
002700010612     C                   kfld                    paytype
002800010202     C                   kfld                    code#
002900010612     **
003000010612     C                   exsr      aainit
003100010612     C                   select
003200010612     ** Process Records for Payroll Type field PENALTIES = YES
003300010612     C                   when      pen_code = 'Y'
003400010202
003500010201     **
003600010202     **********************
003700010202     ** Load data        **
003800010202     **********************
003900010201
004000010202     C                   dow       0 = 0
004100010202     C     *loval        setll     abtfmt
004200010202     C                   read      abtfmt
004300010202     C                   dow       not(%eof)
004400010202     **
004500010202     C                   eval      rrn = rrn + 1
004600010202     C                   eval      code# = abttype
004700010202     C                   eval      desc# = abtdesc
004800010202     **
004900010612     C     trn_key       chain(n)  ppenfmt
005000010202     C                   if        %found
005100010612     C                   eval      sel = 'X'
005200010612     C                   eval      dft# = ppendft
005300010612     C                   eval      pen# = ppenper
005400010612     C                   else
005500010612     C                   eval      sel = ' '
005600010612     C                   eval      dft# = *blanks
005700010612     C                   eval      pen# = *zeros
005800010202     C                   endif
005900010202     **
006000010202     C                   write     sflrec
006100010202     C                   read      abtfmt
006200010202     C                   enddo
006300010202
006400010202     C                   leave
006500010202     C                   enddo
006600010201
006700010202     **********************
006800010202     ** change data      **
006900010202     **********************
007000010202
007100010202     C                   dow       0 = 0
007200010202     **
007300010202     c                   write     footer
007400010202     C                   exfmt     sflctl
007500010202     **
007600010202     ** F2 - Cancel
007700010202     **
007800010202     c                   if        *in02
007900010612     c                   eval      *inlr = '1'
008000010612     c                   return
008100010202     c                   endif
008200010202
008300010202
008400010612     C                   exsr      upd_subfile
008500010202
008600010202     ** Confirm transactions (update/Write records to data base file)
008700010612     C                   IF        *in15
008800010202     C                   exsr      Confrm
008900010612     C                   eval      *inlr = *on
009000010612     C                   return
009100010612     C                   Endif
009200010202     C                   enddo
009300010612
009400010612     ** Delete Records If Payroll Type Condition Changes
009500010612     C                   other
009600010612     c     paytype       chain     ppenfmt                            lr
009700010612     C                   dow       not(*inlr)
009800010612     c                   delete    ppenfmt
009900010612     c     paytype       reade     ppenfmt                                lr
010000010612     C                   enddo
010100010612     C                   return
010200010612     C                   endsl
010300010201     **
010400010201     ****************************************************************
010500010201     ***   RESET THE SUBFILE
010600010201     ****************************************************************
010700010201     C     RESET         BEGSR
010800010202     C                   eval      *in33 = *on
010900010202     C                   write     sflctl
011000010202     C                   eval      *in33 = *off
011100010201     **   Reset all fields
011200010202     C                   eval      desc# = *blanks
011300010612     C                   eval      pen# = *zeros
011400010612     C                   eval      pen# = *zeros
011500010202     C                   eval      rrn = *zeros
011600010201     C                   ENDSR
011700010201     ****************************************************************
011800010201     ***   Sub routine update and write to various master and Trans-
011900010201     ***   action files
012000010201     ****************************************************************
012100010201     C     CONFRM        BEGSR
012200010201     **
012300010201     ** Reading the subfile to get detail information for
012400010201     ** the data base file.
012500010201     **
012600010202     C                   z-add     rrn           rrx               4 0
012700010202     C     1             do        rrx           RR                4 0
012800010201     C     RR            chain     sflrec                             H6
012900010201     **
013000010201
013100010202     ** update relevant total in the absenteeism file
013200010612     c     trn_key       chain     ppenfmt
013300010202     C                   if        %found
013400010202
013500010612     C                   if        sel <> 'X'
013600010612     c                   delete    ppenfmt                              77
013700010612     C                   else
013800010613     C                   eval      ppenper = pen#
013900010613     C                   eval      ppendft = dft#
014000010612     c                   update    ppenfmt                              77
014100010612     C                   endif
014200010202
014300010202     C                   else
014400010202
014500010612     C                   if        sel = 'X'
014600010612     C                   eval      ppensts = 'A'
014700010612     C                   eval      ppentype = paytype
014800010612     C                   eval      ppenabs = code#
014900010612     C                   eval      ppenper = pen#
015000010613     C                   eval      ppendft = dft#
015100010612     C                   write     ppenfmt                              77
015200010202     C                   endif
015300010202
015400010202     C                   endif
015500010201     **
015600010201     C                   enddo
015700010201
015800010201     c
015900010201     C                   ENDSR
016000010612     ****************************************************************
016100010612     ***   UPDATE SUBFILE
016200010612     ****************************************************************
016300010612     C     upd_subfile   begsr
016400010612     **
016500010612     C                   z-add     rrn           rrx               4 0
016600010612     C                   do        rrx           RR                4 0
016700010612     C     RR            chain     sflrec                             H6
016800010612     **  Set Defaults for entries
016900010612     **  Clear percentage field if Use Default is selected
017000010612     C                   if        sel = 'X'
017100010612     C                   select
017200010612     C                   when      dft# = 'Y'
017300010612     C                   eval      pen# = *zeros
017400010612     **  Set Use Penalty default if Nothing is entered
017500010612     C                   when      dft# = *blanks and Pen# = *zeros
017600010612     C                   eval      dft# = 'Y'
017700010612     C                   endsl
017800010612     C                   endif
017900010612
018000010612     C                   update    sflrec
018100010612     C                   enddo
018200010612     C                   eval      rrn =  rrx
018300010612     C                   endsr
018400010301     **
018500010301     ***************************************************
018600010301     ** First initilisation routine for all programs  **
018700010301     ***************************************************
018800010301     C     AAINIT        BEGSR
018900010902     C                   eval      coname = get_coname
019000010902     C                   eval      @date = get_sys_date
019100010902     C                   eval      @title = arr(1)
019200010201     C                   TIME                    @TIME             6 0
019300010202     **
019400010202
019500010202     C                   ENDSR
019600010201     *****************************************************************
019700010201     ** Subroutine to undo any changes
019800010201     *****************************************************************
019900010201     c     roll_back     begsr
020000010201     c                   rolbk
020100010201     c                   endsr
020200010201     **
020300010202**  Program Title
020400011011      Payment Type Penalties
