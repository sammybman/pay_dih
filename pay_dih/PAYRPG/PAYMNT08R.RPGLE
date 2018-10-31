000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200111002     H/define profoundui
000300010727     Fpaymnt08d cf   e             workstn
000400010727     F                                     sfile(sflrec:rrn1)
000500111002     F/IF DEFINED(WEB)
000600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111002     F/ENDIF
000800010727     Fpaysuspl01uf a e           K disk
000900010727     Fpayrtypl01if   e           k disk
001000010813     Fpayrresl01if   e           k disk
001100010727     Fpaycycd   if   e           k disk
001200010504     **
001300010504     ** Payroll PROTOTYPES
001400110627      /copy QCPYSRC,genPR
001500110627      /copy QCPYSRC,PAYPR
001600110627      /copy QCPYSRC,autPR
001700981202     **
001800981120     D arr             s             40    dim(1) ctdata perrcd(1)
001900010727     D qty             s              2  0
002000040224     D*dept            s              3  0
002100010813     D name#           s             40
002200010813     D list#           s             15
002300010813     D err#            s              7
002400100315     D HUNDRED         C                   100
002500040224     D*status          s              1
002600981202     **
002700951001     D                SDS
002800951001     D  @PGM             *PROC
002900111002     D  @user                358    367
003000951001     **
003100010727     C     key           klist
003200010727     C                   kfld                    susemp#
003300010727     C                   kfld                    susptype#
003400990403     **
003500040207      /free
003600111002         // authority check
003700111002         monitor;
003800111002           authority_check('PAY':@pgm);
003900111002         on-error;
004000111002           show_msg(get_err_msg('DIH0054'));
004100111002           *inlr = *on;
004200111002           return;
004300111002         endmon;
004400040207      /end-free
004500010430     **
004600010430     c                   exsr      aainit
004700951001     **
004800010430     ******************
004900010430     ** Program start *
005000010430     ******************
005100010430     C                   dow       0 = 0
005200981123     c                   exfmt     selopt
005300010504     c                   if        *in03
005400010504     c                   eval      *inlr = *on
005500010504     c                   return
005600010504     c                   endif
005700010504     ** validate employee
005800010504     ** get employee name
005900010813     c                   eval      status = get_emp_info(susemp#:dept:list#:
006000010813     c                                      name#:*omit:*omit:*omit:err#)
006100010504     **
006200010504     c                   if        err# <> *blanks
006300010504     ** Error message
006400040207      /free
006500040207         errmsg = get_err_msg(err#);
006600040207      /end-free
006700010504     c                   iter
006800010504     c                   endif
006900010504
007000010730     **   Clear Sub_file
007100010730     C                   move      '0'           *in
007200010730     C                   move      '1'           *in33
007300010730     c                   z-add     0             rrn1
007400010730     C                   write     sflctl
007500010730     C                   setoff                                       7533
007600981202     C
007700010504     C                   leave
007800010504     C                   enddo
007900981120     **
008000010504     ***********************
008100010504     ** Get Lines         **
008200010504     ***********************
008300981120     c
008400010727     C     *loval        setll     prtyfmt
008500010727     C                   read(n)   prtyfmt
008600010727     C                   dow       not(%eof)
008700010727     C                   eval      susptype# = prtyptype
008800010727     C                   eval      run_desc = prtydesc
008900010813     C     prtyptype     chain     prrefmt
009000010813     C                   if        %found
009100010813     C     status        scan      prrelist
009200010813     C                   if        %found
009300981126     C
009400010727     C     key           chain     susfmt
009500981120     **
009600010504     C                   if        %found
009700981127     C
009800010504     c                   movel(p)  'AMEND'       @mode
009900981127     ** display records only for selected grade
010000010504
010100100315     C                   eval      DELSEL = ' '
010200010727     C                   eval      susprd# = susprd
010300010727     C                   eval      sussdate# = sussdate
010400010727     C                   eval      susedate# = susedate
010500010727     C                   eval      susemp#   = susemp
010600100309     C                   eval      susperc#  = susperc
010700010727     C                   eval      *in99   = *off
010800981127     **
010900010504     C                   else
011000010504
011100010504     c                   movel(p)  'ENTRY'       @mode
011200010727     C                   eval      *in99   = *on
011300010727     C                   eval      susprd# = *zeros
011400100315     C                   eval      DELSEL = ' '
011500010727     C                   eval      sussdate# = *zeros
011600010727     C                   eval      susedate# = *zeros
011700100315     C                   eval      susperc# = HUNDRED
011800010504     C                   endif
011900010730     **
012000010730     C                   eval      protected = *in99
012100981120     **
012200010430     C                   add       1             rrn1              3 0
012300010430     C                   write     sflrec
012400010813     C                   endif
012500010813     C                   endif
012600010430
012700010727     C                   read(N)   prtyfmt
012800010430     C                   enddo
012900981206     **
013000010730     C                   eval      *in55 = *on
013100981120     c     rrn1          comp      0                                  75
013200981120     c
013300981120     c                   write     footer
013400981123     **
013500010430
013600010430     ******************
013700010430     ** Calcs Section *
013800010430     ******************
013900010504     C                   eval      emp_name = name#
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
015200981202     c
015300010504     ** UPdate Transactions
015400010504     C                   exsr      update_lines
015500951019     **
015600981207     C
015700010504     ** F15 = Confirm changes
015800981202     C                   if        *in15
015900981206     C                   exsr      confirm_sr
016000010430     C                   leave
016100981201     C                   endif
016200981202     C
016300010430     C                   enddo
016400111002
016500111002      /free
016600111002
016700111002         //*****************************
016800111002         // Session Init
016900111002         //*****************************
017000111002
017100111002          begsr *inzsr;
017200111006            get_session_info('PAY':errmsg:@pgm);
017300111002            if errmsg <> *blanks;
017400111002              show_msg(errmsg);
017500111002              *inlr = *on;
017600111002              return;
017700111002            endif;
017800111002
017900111002            open(e) paysuspl01;
018000111002            open(e) payrtypl01;
018100111002            open(e) payrresl01;
018200111002            open(e) paycycd;
018300111002
018400111002          endsr;
018500111002      /end-free
018600981202     C
018700010504     *******************************
018800010504     ** Confirm changes made
018900010504     *******************************
019000981126     c
019100010430     C     confirm_sr    begsr
019200981123     c                   z-add     rrn1          x                 3 0
019300981123     c                   do        x             y                 3 0
019400981123     c
019500010430     C     y             chain     sflrec                             07
019600010504     **
019700010727     C     key           chain     susfmt
019800981206     **
019900010727     C                   eval      susemp = susemp#
020000010727     C                   eval      susptype = susptype#
020100010727     C                   eval      sussdate = sussdate#
020200010727     C                   eval      susedate = susedate#
020300100309     C                   eval      susperc = susperc#
020400100315     C                   eval      susprd = susprd#
020500981127     C
020600010727     C                   if        %found
020700010727     c                   if        delsel <> 'D'
020800010727     C                   update    susfmt
020900010727     C                   Else
021000010727     C                   delete    susfmt
021100010727     C                   Endif
021200981208     **
021300981208     C                   Else
021400981123     **
021500981123     ** Prevent from writing a record with 0 value
021600981126     C
021700010727     c                   if        delsel <> 'D' and susprd <> *zeros
021800010727     C                   move      'A'           sussts
021900010727     C                   write     susfmt
022000981126     C                   endif
022100981208     C
022200981127     C                   endif
022300981126     C
022400981208     C                   enddo
022500010430     C                   endsr
022600010504     *******************************
022700010504     ** UPdate Lines
022800010504     *******************************
022900010504     c
023000010504     C     update_lines  begsr
023100010504     c                   z-add     rrn1          x                 3 0
023200010504     c                   do        x             y                 3 0
023300010504     c
023400010504     C     y             chain     sflrec                             07
023500010827     **
023600010730     C                   eval      *in99 = protected
023700010504
023800010727     C                   eval      qty = prtycycqty * susprd#
023900010727     C                   if        susprd# <> 0
024000010727     C     prtycycode    chain     cycfmt
024100010727     C                   if        sussdate# = 0
024200010727     C                   eval      susedate# = get_next_date(prtyndate:
024300010727     C                             prtycycode:qty:cycccode:
024400010727     C                             cycconvf)
024500010727     C                   eval      sussdate# = prtyndate
024600010727     C                   else
024700010727     C                   eval      susedate# = get_next_date(sussdate#:
024800010727     C                             prtycycode:qty:cycccode:
024900010727     C                             cycconvf)
025000010504
025100010727     C                   endif
025200010727     C                   else
025300010727     C                   eval      sussdate# = *zeros
025400100309     C                   eval      sussdate# = *zeros
025500100309     C                   eval      susperc# = *zeros
025600010504     C                   endif
025700010504     **
025800010504     C                   update    sflrec
025900010504     C
026000010504     C                   enddo
026100010504     C                   endsr
026200010504     **************************************************************************
026300010504     ***subroutine to retrieve header information
026400010504     **************************************************************************
026500010726     C     aainit        begsr
026600010815     C                   eval      CONAME = get_coname
026700010815     C                   eval      @DATE = get_sys_date
026800010726     C                   time                    @time             6 0
026900010726     C                   movel     arr(1)        @title
027000010730     **
027100010504     C                   endsr
027200010504     c
027300981126**
027400010726      Maintain Suspended Payments
