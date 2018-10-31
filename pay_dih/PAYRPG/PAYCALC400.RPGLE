000100950412     H DATEDIT(*DMY)
000200990208     F*
000300990216     **  Payded Logical - Update Deductions
000400060130     Fpaydedchg uf a e           K disk    commit
000500050627     Fpaycrosl02if   e           k disk
000600050627     Fpayrdefl03if   e           K DISK
000700050627     Fpayrresl01if   e           K DISK    usropn
000800020206     ** General/Payroll PROTOTYPES
000900150324      /copy qcpysrc,genPR
001000150324      /copy qcpysrc,PAYPR
001100150324      /copy qcpysrc,empPR
001200990212     **
001300050514     D countr          S              5  0
001400100902     D errorcode       S              7
001500050602
001600990216     **
001700950930     D                SDS
001800950930     D  @PGM             *PROC
001900150324     D  @USER                358    367
002000050627
002100050627     D  main_parm      PR                  extproc('PAYCALC400')
002200050627     D   run_type                     1
002300050627
002400050627     D  main_parm      PI
002500050627     D   run_type                     1
002600050530
002700040207      /free
002800050530
002900050530          exsr  aainit;
003000050627          exsr  delete_rtn;
003100050530
003200050530        //****************************************************
003300050627        //*** subfile subroutine - Delete DEDUCTION TYPE   ***
003400050530        //****************************************************
003500050627        begsr  delete_rtn;
003600050530
003700050530
003800050514          countr = *zeros;
003900050514          dow not get_ltype_info('RD':-1:ematf_struc:countr);
004000050530
004100050530            if  emautpyd = 'Y';
004200050621
004300050627              chain  emseltyp  pcrsfmt;
004400050627              if  %found;
004500050627                 chain (run_type:pcrsptype) prdffmt;
004600050627                 if  %found;
004700050627                   // Delete setup  deductions
004800050627                    setll emseltyp pcgfmt;
004900050627                    reade emseltyp pcgfmt;
005000050627
005100050627                    dow not %eof;
005200100902                      if %scan(get_emp_info(pcgemp:*omit:*blanks:*omit:
005300100902                              *omit:*omit:*omit:errorcode):prrelist) > 0
005400100902                              and errorcode = *blanks;
005500050627                         delete  pcgfmt;
005600050627                      endif;
005700050627                      reade emseltyp pcgfmt;
005800050627
005900050627                    enddo;
006000050627                 endif;
006100050627              endif;
006200050627
006300050530            endif;
006400050530
006500050530          Enddo;
006600050530
006700050530            *inlr  = *on;
006800050530            Return;
006900990216
007000050530        endsr;
007100050530
007200050530
007300050530        //***************************
007400050530        //** Initialize subroutine
007500050530        //***************************
007600050530         begsr  AAINIT;
007700050530
007800050627           open(e)  payrresl01;
007900050627           chain run_type  prrefmt;
008000050627           close(e)  payrresl01;
008100050530
008200050530         Endsr;
008300050530
008400050530      /end-free
008500050530
