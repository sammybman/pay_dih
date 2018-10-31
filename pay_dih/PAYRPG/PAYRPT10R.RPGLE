000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300020129     Fpayrpt10d cf   e             workstn
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
001500010815     D                sds
001600010426     D  @pgm             *proc
001700111002     D  @user                358    367
001800111002
001900010724     D                 ds                  based(indptr)
002000010724     Dcancelled                2      2n
002100010724     Dend_program              3      3n
002200010724     Dconfirmed               15     15n
002300971012     **
002400010411     Darr              s             40    dim(1) ctdata perrcd(1)
002500010426     **
002600010724     Dindptr           s               *   inz(%addr(*in))
002700010426     d cnt             s              2  0
002800010426     d qry_select      s            200
002900020315     d error           s               n
003000010507     **
003100020129     d PayrollPgm      c                   const('PAYRPT10C ')
003200010724     **
003300011205     D msg1            c                   'Invalid Date Entered'
003400011205     D msg2            c                   'Invalid Payroll Run Type'
003500011205     D compmsg         c                   'Report Function Completed'
003600010228     **
003700170207
003701170207       // --------------------------------------------------
003702170207       // Prototype for procedure: PrintReport
003703170207       // --------------------------------------------------
003704170207       DCL-PR PrintReport EXTPGM('PAYRPT1002');
003705170207         runtype CHAR(1);
003706170207         sequence PACKED(1:0);
003707170207         option   char(1);
003708170207       END-PR ;
003709170207
003710170207
003713170207
003714170207       // --------------------------------------------------
003715170207       // Prototype for procedure: gen_sheetPrc
003716170207       // --------------------------------------------------
003717170207       DCL-PR gen_sheetPrc ;
003718170207       END-PR ;
003719170207
003720170207
003800040207      /FREE
003900111002         // authority check
004000111002         monitor;
004100111002           authority_check('PAY':@pgm);
004200111002         on-error;
004300111002           show_msg(get_err_msg('DIH0054'));
004400111002           *inlr = *on;
004500111002           return;
004600111002         endmon;
004700040207      /end-free
004800010411     ** Initial Setup
004900111002      /copy QCPYSRC,aainit
005000951124     **
005100010411     **************************
005200010411     *** Start of Program   ***
005300010411     **************************
005400010411     C                   dow       0 = 0
005500011205
005600011205     C                   exfmt     fmt1
005700011205     ** F3 = End program
005800011205     C                   if        end_program
005900011205     C                   eval      *inlr = *on
006000011205     C                   return
006100011205     C                   endif
006200011205     **
006300011205     C                   clear                   errmsg
006400960127     **
006500011205     C     run_type      chain     prtyfmt
006600011205     ** Invalid Run type entered
006700011205     C                   if        not(%found)
006800011205     c                   eval      errmsg = msg2
006900011205     C                   iter
007000011205     C                   endif
007100020129
007200011205     ** Ensure valid department number entered
007300011205     C                   if        dept# <> *zeros
007400020315     c                   callp     get_dept_info(dept#:error)
007500020315     C                   if        error
007600020315     c                   eval      errmsg = get_err_msg('DIH0008')
007700011205     C                   iter
007800011205     C                   endif
007900011205     C                   endif
008000011205     ** confirm processing
008100011205     C                   if        confirmed
008101170207          gen_sheetPrc();
008102170207          errmsg = compmsg;
008103170207          leave;
008200020129     C                   exsr      gen_sheet
008300011205     c                   eval      errmsg = compmsg
008400011205     C                   leave
008500011205     C                   endif
008600010426
008700010411     C                   enddo
008800010724
008900111002      /free
009000111002
009100111002         //*****************************
009200111002         // Session Init
009300111002         //*****************************
009400111002
009500111002          begsr *inzsr;
009600111006            get_session_info('PAY':errmsg:@pgm);
009700111002            if errmsg <> *blanks;
009800111002              show_msg(errmsg);
009900111002              *inlr = *on;
010000111002              return;
010100111002            endif;
010200111002
010300111002            open(e) payrtypl01;
010400111002            open(e) payrresl01;
010500111002
010600111002          endsr;
010700111002
010800111002      /end-free
010900010725
011000011205     **************************************************
011100011205     ** Setup report Parameters for Processing       **
011200011205     **************************************************
011300020129     C     gen_sheet     begsr
011400020129     ** payroll run type
011500020129     C                   if        run_type <> *blanks
011600020129     c                   eval      qry_select = ' timrtype = ''' +
011700020129     C                                    run_type  + ''''
011800020129     C                   endif
011900020129      ** select payroll types
012000020129     C                   if        pay_type <> *zeros
012100020129     c                   eval      qry_select = %trim(qry_select) +
012200020129     C                              ' & timtyp *eq ' +  %char(pay_type)
012300020129     C                   endif
012400011205     **
012500020129     ** Generate report
012501170207     **
012502170207
012503170207         leavesr;
012600010725     **
012700010725     C                   callb     Payrollpgm
012800010725     C                   parm                    qry_select
012900020129     C                   parm                    selhrs
013000020129     C                   parm                    dept#
013100020129     C                   parm                    seq#
013200010725
013300010725     C                   endsr
013400010724
013500971012     **
013600170207
013601170207       // --------------------------------------------------
013602170207       // Procedure name: gen_sheetPrc
013603170207       // Purpose:
013604170207       // Returns:
013605170207       // --------------------------------------------------
013606170207       DCL-PROC gen_sheetPrc ;
013607170207         // Your local fields go here
013608170207
013609170207         // Your calculation code goes here
013610170207
013620170207     ** Generate report
013621170207     **
013622170207         PrintReport(run_type:seq#:selhrs);
013623170207
013624170207
013630170207
013631170207         return ;
013632170207       END-PROC ;
013633170207**  Program Title
013700020129         Paysheet report
