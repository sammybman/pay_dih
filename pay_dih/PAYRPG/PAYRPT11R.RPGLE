000100010424     H DATEDIT(*ymd)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400021115     F* PAYROLL GL. Definition Report
000500010424     F* PROGRAMMER:-S. Britton & C. Williams
000600021120     Fpayrpt11d cf   e             workstn
000700111002     F/IF DEFINED(WEB)
000800111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900111002     F/ENDIF
001000010424     ** Payroll PROTOTYPES
001100110729      /copy qcpysrc,genpr
001200110729      /copy qcpysrc,PAYPR
001300110729      /copy qcpysrc,autpr
001400110729      /copy qcpysrc,emppr
001500990113     **
001600960130     D                SDS
001700960130     D  @PGM             *PROC
001800111002     D  @USER                358    367
001900021120     **
002000021120     D                 ds                  based(indptr)
002100021120     DType_reqd                1      1n
002200021120     Dcancelled                2      2n
002300021120     Dend_program              3      3n
002400021120     Dconfirmed               15     15n
002500010424     **
002600021115     D arr             s             40    dim(1) ctdata perrcd(1)
002700021115     D coname          s             30                                         TOTAL AMOUNT
002800021115     D @date           s             10                                         TOTAL AMOUNT
002900021115     D gl#             s             25                                         TOTAL AMOUNT
003000021115     D dft#            s             10                                         TOTAL AMOUNT
003100021120     d error           s               n
003200021120     d indptr          s               *   inz(%addr(*in))
003300021115
003400021120     d ReportPgm1      c                   'PAYRPT1101'
003500021120     d ReportPgm2      c                   'PAYRPT1102'
003600021120     d Type_enquiry    c                   'PAYENQ01R '
003700021120     D msg1            c                   'Invalid Payroll Type Entered'
003800021120     D compmsg         c                   'Report Function Completed'
003900010424     **
004000040207      /FREE
004100111002         // authority check
004200111002         monitor;
004300111002           authority_check('PAY':@pgm);
004400111002         on-error;
004500111002           show_msg(get_err_msg('DIH0054'));
004600111002           *inlr = *on;
004700111002           return;
004800111002         endmon;
004900040207      /end-free
005000021120     ** Initial Setup
005100111002      /copy QCPYSRC,aainit
005200021120
005300021120     **************************
005400021120     *** Start of Program   ***
005500021120     **************************
005600021120     C                   dow       0 = 0
005700021120
005800021120     C                   exfmt     fmt1
005900021120     ** F3 = End program
006000021120     C                   if        end_program
006100021120     C                   eval      *inlr = *on
006200021120     C                   return
006300021120     C                   endif
006400021120     **
006500021120     C                   clear                   errmsg
006600021120     **
006700021120     C                   if        pay_type <> *zeros
006800050519      /free
006900050519            // Invalid pay type entered
007000050519           if   get_payty_info('RD':pay_type:payty_struc);
007100050519
007200050519      /end-free
007300021120     c                   eval      errmsg = msg1
007400021120     C                   iter
007500021120     C                   endif
007600021120     C                   endif
007700021120     ** Display List
007800021120     C                   if        type_reqd
007900021120     C                   call      Type_enquiry
008000021120     C                   parm                    pay_type
008100021120     C                   iter
008200021120     C                   endif
008300021120
008400021120     ** Ensure valid department number entered
008500021120     C                   if        dept# <> *zeros
008600021120     c                   callp     get_dept_info(dept#:error)
008700021120     C                   if        error
008800021120     c                   eval      errmsg = get_err_msg('DIH0008')
008900021120     C                   iter
009000021120     C                   endif
009100021120     C                   endif
009200021120     ** confirm processing
009300021120     C                   if        confirmed
009400021120     C                   exsr      gen_report
009500021120     c                   eval      errmsg = compmsg
009600021120     C                   leave
009700021120     C                   endif
009800021120
009900021120     C                   enddo
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
011400111002          endsr;
011500021120
011600111002      /end-free
011700021120     **************************************************
011800021120     ** Setup report Parameters for Processing       **
011900021120     **************************************************
012000021120     C     gen_report    begsr
012100021120
012200021120     C                   select
012300021120     C                   when      seq# = 1
012400021120     C                   callb     ReportPgm1
012500021120     C                   parm                    dept#
012600021120     C                   parm                    pay_type
012700021120     C                   when      seq# = 2
012800021120     C                   callb     ReportPgm2
012900021120     C                   parm                    dept#
013000021120     C                   parm                    pay_type
013100021120     C                   endsl
013200021120
013300021120     C                   endsr
013400021115**
013500021115    Payroll Gl. Definition Report
