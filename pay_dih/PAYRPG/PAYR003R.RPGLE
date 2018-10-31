000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200111002     H/define profoundui
000300010612     Fpayr003d  cf   e             workstn
000400010612     F                                     sfile(sflrec:rrn1)
000500111002     F/IF DEFINED(WEB)
000600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111002     F/ENDIF
000800111002     Fpaydepcon uf a e           k disk    usropn
000900111002     Femptypel02if   e           k disk    usropn
001000111002     Fdeptl05   if   e           k disk    usropn
001100110627      /copy QCPYSRC,genPR
001200110627      /copy QCPYSRC,autPR
001300110627      /copy QCPYSRC,emppr
001400981202     **
001500981120     D arr             s             40    dim(1) ctdata perrcd(1)
001600050525     D notfound        s               n
001700011015     d msg1            c                   const('This payroll type not valid f-
001800010904     D                                     or this Option')
001900010612     D msg2            c                   const('Invalid payroll Type')
002000010613     D msg3            c                   const('Invalid Employee Type')
002100981202     **
002200951001     D                SDS
002300951001     D  @PGM             *PROC
002400111002     D  @user                358    367
002500990403     **
002600990403
002700951001     **
002800010612     C     trn_key1      klist
002900010613     C                   kfld                    pdpttype#
003000010612     C                   kfld                    dept#
003100010613     C                   kfld                    pdptempt#
003200010612     **
003300010612     C     trn_key2      klist
003400010613     C                   kfld                    pdpttype#
003500010612     C                   kfld                    dept#
003600990403     **
003700040207      /FREE
003800111002         // authority check
003900111002         monitor;
004000111002           authority_check('PAY':@pgm);
004100111002         on-error;
004200111002           show_msg(get_err_msg('DIH0054'));
004300111002           *inlr = *on;
004400111002           return;
004500111002         endmon;
004600040207      /end-free
004700010430     **
004800010430     c                   exsr      aainit
004900951001     **
005000010430     ******************
005100010430     ** Program start *
005200010430     ******************
005300010430     C                   dow       0 = 0
005400981123     c                   exfmt     selopt
005500010612     C                   clear                   errmsg
005600981202     C
005700981123     c                   if        *in03
005800010430     c                   eval      *inlr = *on
005900981123     c                   return
006000981123     c                   endif
006100981202     C
006200010612     ** validate Payroll Type
006300050519      /free
006400050519          notfound =  get_payty_info('RD':pdpttype#:payty_struc);
006500050519      /end-free
006600050519
006700010612     C                   select
006800050519     C                   when      not notfound and ptybasdep <> 'Y'
006900010612     c                   eval      errmsg = msg1
007000010430     C                   iter
007100050519     C                   when      notfound
007200010612     c                   eval      errmsg = msg2
007300010613     C                   iter
007400010612     C                   endsl
007500010612     ** Use indicator 88 to protect entry field
007600010612     C                   if        ptyfactor = *zeros
007700010612     C                   eval      *in88 = *on
007800010612     C                   endif
007900010613     ** Validate Employee Type
008000010613     C                   if        pdptempt# <> *blanks
008100010613     C     pdptempt#     chain     empfmt
008200010613     C                   if        not(%found)
008300010613     c                   eval      errmsg = msg3
008400010613     C                   iter
008500010613     C                   endif
008600010613     C                   endif
008700981202     c
008800010504     **   Clear Sub_file
008900010430     C                   move      '1'           *in33
009000010612     c                   eval      rrn1 = *zeros
009100010430     C                   write     sflctl
009200010612     C                   move      '0'           *in33
009300010612     C**                 setoff                                       753376
009400010504     C                   leave
009500010504     C                   enddo
009600981120     **
009700010504     ***********************
009800010504     ** Get Lines         **
009900010504     ***********************
010000981120     c
010100010612     C     *loval        setll     dhrec
010200010612     C                   read      dhrec                                  55
010300010430     C                   dow       not(*in55)
010400010612     C                   eval      dept# = dept
010500010612     C                   eval      dept_name = dname
010600981126     C
010700030422     C*                  if        pdptempt# = *blanks
010800030422     C*    trn_key2      chain     pdptfmt
010900030422     C*                  else
011000010612     C     trn_key1      chain     pdptfmt
011100030422     C*                  endif
011200981120     **
011300010504     C                   if        %found
011400981127     C
011500981127     ** display records only for selected grade
011600010504
011700010612     C                   eval      pdptfac# = pdptfac
011800010612     C                   eval      pdptper# = pdptper
011900981127     **
012000010504     C                   else
012100010504
012200010612     C                   eval      pdptfac# = *zeros
012300010612     C                   eval      pdptper# = *zeros
012400010504     C                   endif
012500981120     **
012600010430     C                   add       1             rrn1              3 0
012700010430     C                   write     sflrec
012800010430
012900010612     C                   read      dhrec                                  55
013000010430     C                   enddo
013100981206     **
013200981120     c     rrn1          comp      0                                  75
013300981120     c
013400981120     c                   write     footer
013500981123     **
013600010430
013700010430     ******************
013800010430     ** Calcs Section *
013900010430     ******************
014000010430     c                   dow       0 = 0
014100010430     C                   exfmt     sflctl
014200981202     ** end program
014300981202     C                   if        *in03
014400981202     C                   seton                                        lr
014500981202     C                   return
014600981202     C                   endif
014700981202     C
014800981202     ** cancel changes made
014900981202     C                   if        *in02
015000010430     C                   leave
015100981202     C                   endif
015200981123
015300981207     C
015400010504     ** F15 = Confirm changes
015500981202     C                   if        *in15
015600981206     C                   exsr      confirm_sr
015700010430     C                   leave
015800981201     C                   endif
015900981202     C
016000010430     C                   enddo
016100111002      /free
016200111002
016300111002         //*****************************
016400111002         // Session Init
016500111002         //*****************************
016600111002
016700111002          begsr *inzsr;
016800111006            get_session_info('PAY':errmsg:@pgm);
016900111002            if errmsg <> *blanks;
017000111002              show_msg(errmsg);
017100111002              *inlr = *on;
017200111002              return;
017300111002            endif;
017400111002
017500111002            open(e) paydepcon;
017600111002            open(e) emptypel02;
017700111002            open(e) deptl05;
017800111002
017900111002          endsr;
018000111002
018100111002      /end-free
018200981202     C
018300010504     *******************************
018400010504     ** Confirm changes made
018500010504     *******************************
018600981126     c
018700010430     C     confirm_sr    begsr
018800981123     c                   z-add     rrn1          x                 3 0
018900981123     c                   do        x             y                 3 0
019000981123     c
019100010430     C     y             chain     sflrec                             07
019200981123
019300010504     **
019400030422     C*                  if        pdptempt# = *blanks
019500030422     C*    trn_key2      chain     pdptfmt
019600030422     C*                  else
019700010612     C     trn_key1      chain     pdptfmt
019800030422     C*                  endif
019900981206     **
020000010612     c                   eval      pdptfac = pdptfac#
020100010612     c                   eval      pdptper = pdptper#
020200981127     C
020300010612     C                   if        %found
020400010613     C                   if        pdptper <> *zeros or
020500010613     C                              pdptfac <> *zeros
020600010613
020700010612     C                   update    pdptfmt
020800010613     C                   else
020900010613     C                   delete    pdptfmt
021000010613     C                   endif
021100981208     **
021200981208     C                   Else
021300981123     **
021400981123     ** Prevent from writing a record with 0 value
021500990220     ** except for employees with special allowances
021600981126     C
021700010613     c                   if        pdptper <> *zeros or
021800010613     C                              pdptfac <> *zeros
021900010612     c                   eval      pdptsts = 'A'
022000010612     c                   eval      pdpttype = pdpttype#
022100010612     c                   eval      pdptempt = pdptempt#
022200010612     c                   eval      pdptdept = dept#
022300010612     C                   write     pdptfmt
022400981126     C                   endif
022500981208     C
022600010612     C                   endif
022700981126     C
022800981126     C
022900981208     C                   enddo
023000010430     C                   endsr
023100010504     c
023200010504     **************************************************************************
023300010504     ***subroutine to retrieve header information
023400010504     **************************************************************************
023500010504     C     AAINIT        begsr
023600010815     C                   eval      CONAME = get_coname
023700010815     C                   eval      @DATE = get_sys_date
023800010504     C                   time                    @TIME             6 0
023900010504     C                   MOVEL     ARR(1)        @TITLE
024000010504     C                   endsr
024100010504     c
024200981126**
024300011015    Setup Department Payment Defaults
