000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300011205     Fpayrpt09d cf   e             workstn
000400111002     F/IF DEFINED(WEB)
000500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600111002     F/ENDIF
000700111002     Fpayrtypl01if   e           k disk    usropn
000800111002     Fpayrresl01if   e           k disk    usropn
000900010508     ** Payroll PROTOTYPES
001000111002      /copy QCPYSRC,genPR
001100111002      /copy QCPYSRC,PAYPR
001200111002      /copy QCPYSRC,autPR
001300010426     **
001400010815     Ddate             s              8  0
001500111002
001600010815     D                sds
001700010426     D  @pgm             *proc
001800111002     D  @user                358    367
001900111002
002000010724     D                 ds                  based(indptr)
002100010724     Dcancelled                2      2n
002200010724     Dend_program              3      3n
002300010724     Dconfirmed               15     15n
002400971012     **
002500010411     Darr              s             40    dim(1) ctdata perrcd(1)
002600010426     **
002700010724     Dindptr           s               *   inz(%addr(*in))
002800010426     d cnt             s              2  0
002900010426     d qry_select      s            200
003000010426     d type_list       s              1    dim(20)
003100020315     d error           s               n
003200010507     **
003300011205     d PayrollPgm      c                   const('PAYRPT09C ')
003400010724     **
003500011205     D msg1            c                   'Invalid Date Entered'
003600011205     D msg2            c                   'Invalid Payroll Run Type'
003700011205     D msg3            c                   'Mimic type cannot be Run type'
003800011205     D compmsg         c                   'Report Function Completed'
003900010228     **
004000010411
004100040207      /FREE
004200111002         // authority check
004300111002         monitor;
004400111002           authority_check('PAY':@pgm);
004500111002         on-error;
004600111002           show_msg(get_err_msg('DIH0054'));
004700111002           *inlr = *on;
004800111002           return;
004900111002         endmon;
005000040207      /end-free
005100010411     ** Initial Setup
005200111002      /copy QCPYSRC,aainit
005300951124     **
005400010411     **************************
005500010411     *** Start of Program   ***
005600010411     **************************
005700010411     C                   dow       0 = 0
005800011205
005900011205     C                   exfmt     fmt1
006000011205     ** F3 = End program
006100011205     C                   if        end_program
006200011205     C                   eval      *inlr = *on
006300011205     C                   return
006400011205     C                   endif
006500011205     **
006600011205     C                   clear                   errmsg
006700011205     ** Ensure valid date
006800011205     C     *iso          test(de)                run_date
006900011205     C                   if        %error
007000011205     c                   eval      errmsg = msg1
007100011205     C                   iter
007200011205     C                   endif
007300960127     **
007400011205     C     run_type      chain     prtyfmt
007500011205     ** Invalid Run type entered
007600011205     C                   if        not(%found)
007700011205     c                   eval      errmsg = msg2
007800011205     C                   iter
007900011205     C                   else
008000011205     C                   if        prtyprno = *zeros
008100011205     c                   eval      errmsg = msg3
008200011205     C                   endif
008300011205     C                   endif
008400011205     ** Ensure valid department number entered
008500011205     C                   if        dept# <> *zeros
008600020315     c                   callp     get_dept_info(dept#:error)
008700020315     C                   if        error
008800020315     c                   eval      errmsg = get_err_msg('DIH0008')
008900011205     C                   iter
009000011205     C                   endif
009100011205     C                   endif
009200011205     ** confirm processing
009300011205     C                   if        confirmed
009400011205     C                   exsr      paysetup
009500011205     c                   eval      errmsg = compmsg
009600011205     C                   leave
009700011205     C                   endif
009800010426
009900010411     C                   enddo
010000111002      /free
010100111002
010200111002         //*****************************
010300111002         // Session Init
010400111002         //*****************************
010500111002
010600111002          begsr *inzsr;
010700111006            get_session_info('PAY':errmsg:@pgm);
010800111002            if errmsg <> *blanks;
010900111002              show_msg(errmsg);
011000111002              *inlr = *on;
011100111002              return;
011200111002            endif;
011300111002
011400111002            open(e) payrtypl01;
011500111002            open(e) payrresl01;
011600111002
011700111002          endsr;
011800010725
011900111002      /end-free
012000011205     **************************************************
012100011205     ** Setup report Parameters for Processing       **
012200011205     **************************************************
012300010725     C     paysetup      begsr
012400011205     ** start and end dates
012500011205     C     run_type      chain     prrefmt
012600011205     **
012700011205     C                   select
012800011205     C                   when      emp# <> *zeros
012900011205     c                   eval      qry_select =
013000011205     C                                    ' emp *eq ' +  %char(emp#)
013100011205     C                   other
013200011205     C                   if        emp_type <> *blanks
013300011205     c                   eval      qry_select = ' status = ''' +
013400011205     C                                    emp_type  + ''''
013500011205     ** Add Department selection
013600011205     C                   exsr      add_dept_rtn
013700011205     C                   else
013800011205     **
013900011205     ** Move type field to array
014000011205     C                   movea     prrelist      type_list
014100011205
014200011205     C                   eval      cnt = 1
014300011205     ** Add employee type selection
014400011205     c                   eval      qry_select = ' status = ''' +
014500011205     C                                    type_list(cnt) + ''''
014600011205     ** Add Department selection
014700011205     C                   exsr      add_dept_rtn
014800011205     C                   eval      cnt = cnt + 1
014900011205     C                   dow       type_list(cnt) <> *blank
015000011205     **
015100011205     c                   eval      qry_select = %trim(qry_select) +
015200011205     C                                    ' *or status = ''' +
015300011205     C                                    type_list(cnt) + ''''
015400011205     ** Add Department selection
015500011205     C                   exsr      add_dept_rtn
015600011205     C                   eval      cnt = cnt + 1
015700011205     C                   enddo
015800011205     C                   endif
015900011205     C                   endsl
016000011205     **
016100011205     ** Generate Payslips
016200010725     **
016300010725     C                   callb     Payrollpgm
016400010725     C                   parm                    qry_select
016500011205     C                   parm                    run_type
016600011205     C                   parm                    run_date
016700010918     C                   parm                    prreseqcod
016800011126     C                   parm                    prrertflag
016900020117     C                   parm                    log_date
017000030228     C                   parm                    prrepstyle
017100030228     C                   parm                    prreytdopt
017200010725
017300010725     C                   endsr
017400010724
017500011205     ***************************************
017600011205     ** add department to query selection **
017700011205     ***************************************
017800011205     C     add_dept_rtn  begsr
017900011205     C                   if        dept# <> *zeros
018000011205     c                   eval      qry_select = %trim(qry_select) +
018100011205     C                                    ' & dept *eq ' +  %char(dept#)
018200011205     C                   endif
018300011205     C                   endsr
018400971012     **
018500010411**  Program Title
018600011205         Payslip Re-generation
