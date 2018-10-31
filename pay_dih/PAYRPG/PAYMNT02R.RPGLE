000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200111002     H/define profoundui
000300010410     Fpaymnt02d cf   e             workstn
000400971012     F                                     sfile(sflrec:rrn)
000500111002     F/IF DEFINED(WEB)
000600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111002     F/ENDIF
000800111002     Fpayrtypl01if   e           k disk    usropn
000900111002     Fpaytdefl01uf a e           k disk    usropn
001000971012     **
001100111002      /copy QCPYSRC,PAYPR
001200111002      /copy QCPYSRC,genpr
001300111002      /copy QCPYSRC,glpr
001400111002      /copy QCPYSRC,autpr
001500111002      /copy QCPYSRC,emppr
001600050524
001700010815     D@date            s             10
001800010411     Darr              s             40    dim(1) ctdata perrcd(1)
001900010828     Dname#            s             40
002000020126     Derrmsg           s             75
002100010828     Derr#             s              7
002200050525     Dcountr           s              5  0
002300040224     Ddept#            s              3s 0
002400050611     D ind             S              2  0
002500050524
002600971012     D                sds
002700971012     D  @pgm             *proc
002800111002     D  @user                358    367
002900010411
003000050524     Dlst_department   PR                  extpgm('HMRENQ02R')
003100050524     D                                3  0
003200971012     **
003300971012     C     trn_key       klist
003400010411     C                   kfld                    ptyptype
003500010425     C                   kfld                    pytrtype#
003600010411     C                   kfld                    pytdept#
003700010411     C                   kfld                    pytemp#
003800040207      /free
003900111002         // authority check
004000111002         monitor;
004100111002           authority_check('PAY':@pgm);
004200111002         on-error;
004300111002           show_msg(get_err_msg('DIH0054'));
004400111002           *inlr = *on;
004500111002           return;
004600111002         endmon;
004700050524
004800050524                exsr aainit;
004900050524
005000050524          //**************************
005100050524          //*** Start of Program   ***
005200050524          //**************************
005300050524                   dow 0 = 0;
005400050524
005500050524                   exfmt  fmt1;
005600050524
005700050524         // F3 = Program End
005800050524                 if  *in03;
005900050524                   *inlr = *on;
006000050524                    return;
006100050524                  endif;
006200050524
006300050524         // Indicators to set off
006400050611            // for ind = 96 to 99;
006500050611            //     *in(ind) = *off;
006600050611            // endfor;
006700050611                 *in96 = *off;
006800050611                 *in97 = *off;
006900050611                 *in98 = *off;
007000050611                 *in99 = *off;
007100050524
007200050524          // Prompt for list
007300050524                  if *in04;
007400050524                  select;
007500050524             // Display List of Departments
007600050524                     when fld = ('PYTDEPT#');
007700050524                     lst_department(pytdept#);
007800050524             // Display List of Payroll Run Types
007900050524                     when fld = ('PYTRTYPE#');
008000050524                     lst_runtype(pytrtype#);
008100050524                     endsl;
008200050524                     iter;
008300050524                  endif;
008400050524
008500050524          // Validate/Get department name for Screen
008600050524                    dept_name = get_dept_info(pytdept#:*in99);
008700050524
008800050524          //** Invalid Department entered
008900050524                    if *in99;
009000050524                      iter;
009100050524                    endif;
009200050524
009300050524      /end-free
009400010828     C                   if        pytrtype# <> *blank
009500010425     C     pytrtype#     chain     prtyfmt                            96
009600010425     ** Invalid Run Type
009700010425     C                   if        *in96
009800010425     C                   iter
009900010828     C                   endif
010000010828     C                   else
010100010828     C                   eval      *in96 = *off
010200010922     C                   eval      prtydesc = *blanks
010300010828     C                   endif
010400010425     ** Get Run type description for Screen
010500010425     C                   eval      run_name = prtydesc
010600010411     ** Invalid Employee Entered
010700010411     C                   if         pytemp# <> *zeros
010800010828     c                   callp     get_emp_info(pytemp#:dept#:'RE':name#
010900010828     c                                     :*omit:*omit:*omit:err#)
011000010411     ** Invalid Employee  entered
011100010411     c                   if        err# <> *blanks
011200010411     C                   eval      *in98 = *on
011300010411     C                   iter
011400010411     C                   else
011500010411     C                   eval      emp_name = name#
011600010411     ** Invalid Department/Employee combination entered
011700010411     C                   if        pytdept# <> dept#
011800010411     C                   eval      *in97 = *on
011900010411     C                   iter
012000010411     C                   endif
012100010411     C                   endif
012200010411     C                   else
012300010411     C                   eval      emp_name = *blanks
012400010411     C                   endif
012500960127     **
012600971012     c                   seton                                            33
012700971012     c                   z-add     0             rrn
012800971012     C                   write     sflctl
012900971012     c                   setoff                                           33
013000971012     *****************************
013100050524      /free
013200050525              *in68 = *off;
013300050525              countr = *zeros;
013400050525              dow not get_payty_info('RD':-1:payty_struc:countr);
013500050524
013600050524      /end-free
013700980606     **  Select transactions
013800010424     C     trn_key       chain     pytfmt                             66
013900010424     C                   if        not(*in66)
014000010411     c                   eval      pytacode# = pytacode
014100971012     C                   else
014200010411     c                   eval      pytacode# = ptydglcod
014300971012     C                   endif
014400010828     ** default account code
014500010828     c                   eval      pytacode## = ptydglcod
014600971012     c                   add       1             rrn               4 0
014700971012     c                   write     sflrec
014800980606     **
014900971012     c                   enddo
015000971012     **
015100050525      /free
015200050525              *in68 = *off;
015300050525      /end-free
015400010411     C                   leave
015500010411     C                   enddo
015600010411     **************************
015700010411     *** Body of Program    ***
015800010411     **************************
015900971012     C                   write     footer
016000010411     C                   dow       0 = 0
016100971012     C                   exfmt     sflctl
016200010411     **  Go back to start
016300010411     C                   if        *in02
016400010411     C                   leave
016500010411     C                   endif
016600010411     **  End Program
016700010411     C                   if        *in03
016800010411     C                   eval      *inlr = *on
016900010411     C                   return
017000010411     C                   endif
017100950822     **
017200951124     ******************************************************************
017300960418     *** CHECKING FOR INVALID ACCOUNT CODES /EMPLOYEES /KRONOS DATA ***
017400951124     ******************************************************************
017500971012     **  Validate account records
017600971012     c                   z-add     rrn           cnt               2 0
017700010411     c                   setoff                                       21
017800971012     c                   do        cnt           y                 2 0
017900010828     c     y             chain     sflrec
018000031001      /free
018100031001         get_gl_info(pytacode#:errmsg:'V');
018200031001      /end-free
018300010411     **
018400010411     C                   if        errmsg <> *blanks
018500010411     C                   seton                                        2021
018600010411     C                   else
018700010411     c                   setoff                                       20
018800010411     C                   endif
018900010411     **
019000010411     C                   update    sflrec
019100971012     c                   enddo
019200010411     ** If record has Invalid account code
019300010411     C                   if        *in21
019400010411     C                   iter
019500010411     C                   endif
019600951124     **
019700961028     ** IF RECORD WAS CHANGED REDISPLAY FORMAT
019800010411     C                   if        *in88
019900010411     C                   iter
020000010411     C                   endif
020100971012     **
020200971012     C                   exsr      trn_rtn
020300010411
020400010411     C                   leave
020500010411     C                   enddo
020600111002      /free
020700111002
020800111002         //*****************************
020900111002         // Session Init
021000111002         //*****************************
021100111002
021200111002          begsr *inzsr;
021300111006            get_session_info('PAY':errmsg:@pgm);
021400111002            if errmsg <> *blanks;
021500111002              show_msg(errmsg);
021600111002              *inlr = *on;
021700111002              return;
021800111002            endif;
021900111002
022000111002            open(e) payrtypl01;
022100111002            open(e) paytdefl01;
022200111002
022300111002          endsr;
022400111002      /end-free
022500111002
022600010411     ******************************************************************
022700010411     ***  Update Payroll GL. Type Definition File
022800010411     ******************************************************************
022900971012     **
023000971012     C     trn_rtn       begsr
023100971012     c                   z-add     rrn           cnt               2 0
023200971012     c                   do        cnt           y
023300010828     c     y             chain     sflrec
023400010828     C     trn_key       chain     pytfmt
023500010411     c                   eval      pytacode = pytacode#
023600010828     C                   if        not(%found)
023700010425     ** Write a record if default Gl is different from entry
023800010425     C                   if        pytacode# <> pytacode##
023900010425     c                   eval      pytrtype = pytrtype#
024000010411     c                   eval      pytdept = pytdept#
024100010411     c                   eval      pytemp  = pytemp#
024200010411     c                   eval      pytcode = ptyptype
024300010411     c                   eval      pytsts  = 'A'
024400010411     C                   write     pytfmt
024500010425     C                   endif
024600010425     **
024700971012     C                   else
024800010828     **
024900010828     C                   select
025000010828     C                   when      pytacode# = pytacode##
025100010828     C                   delete    pytfmt
025200010828     C                   other
025300010411     C                   update    pytfmt
025400010828     C                   endsl
025500010828     **
025600971012     C                   endif
025700971012     c                   enddo
025800971012     C                   endsr
025900010411     ***************************************************
026000010411     ** First initilisation routine for all programs  **
026100010411     ***************************************************
026200010411     C     AAINIT        BEGSR
026300010411     **
026400010411     **
026500010815     C                   eval      CONAME = get_coname
026600010815     C                   eval      @DATE = get_sys_date
026700010411     C                   time                    @TIME             6 0
026800010411     ** select header for program
026900010411     C                   eval      @title = arr(1)
027000010411     C                   eval      *in = *off
027100010411     **
027200010411     C                   ENDSR
027300971012     **
027400010411**  Program Title
027500010411      Payroll GL. Definition Maintenance
