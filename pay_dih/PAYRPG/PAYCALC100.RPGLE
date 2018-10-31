000100950412     H DATEDIT(*DMY)
000200990208     F*
000300990208     **  SUBFILE SELECTION DISPLAY FILE
000400050530     Fliaba_ytd if   e           K disk
000500990216     **  Payded Logical - Update Deductions
000600060130     Fpaydedchg uf a e           K disk    commit
000700050627     Fpaycrosl02if   e           k disk
000800050627     Fpayrdefl03if   e           K DISK
000900050627     Fpayrresl01if   e           K DISK    usropn
001000020206     ** General/Payroll PROTOTYPES
001100150324      /copy qcpysrc,genPR
001200150324      /copy qcpysrc,PAYPR
001300150324      /copy qcpysrc,empPR
001400990212     **
001500050530     D dedu            S             11  2
001600050530     D prno            S              6  0
001700021005     D date1           S              8  0
001800050514     D countr          S              5  0
001900050530     D cntr            s              5  0                                      TOTAL AMOUNT
002000050530     D curbal          s             11  2                                      TOTAL AMOUNT
002100050530     D class#          s              3                                         TOTAL AMOUNT
002200050602     D payrun_factor   s              7  3 inz(1)
002300050602     D payrun_qty      s              2  0 inz(1)
002400050602     D hrs_dummy       s              5  2
002500050602     D pay_amt         s             11  2
002600050602
002700050602     D YEAR_CONST      c                   12
002800990216     **
002900950930     D                SDS
003000950930     D  @PGM             *PROC
003100950930     D  @USER                254    263
003200050627
003300050627     D  main_parm      PR                  extproc('PAYCALC100')
003400050627     D   run_type                     1
003500050627
003600050627     D  main_parm      PI
003700050627     D   run_type                     1
003800050530
003900040207      /free
004000050530
004100050530          dedu = *zeros;
004200050530
004300050530          exsr  aainit;
004400050530          exsr  subfsubr;
004500050530
004600050530        //****************************************************
004700050530        //*** subfile subroutine - SELECT DEDUCTION TYPE   ***
004800050530        //****************************************************
004900050530        begsr  subfsubr;
005000050530
005100050514          countr = *zeros;
005200050514          dow not get_ltype_info('RD':-1:ematf_struc:countr);
005300050530
005400050530            if  emautpyd = 'Y';
005500050621
005600050627              chain  emseltyp  pcrsfmt;
005700050627              if  %found;
005800050627                 chain (run_type:pcrsptype) prdffmt;
005900050627                if  %found;
006000050627                  // generate deductions
006100050627                  exsr  gen_ded;
006200050627                endif;
006300050627              endif;
006400050627
006500050530            endif;
006600050530
006700050530          Enddo;
006800050530
006900050530            *inlr  = *on;
007000050530            Return;
007100990216
007200050530        endsr;
007300050530
007400050530        //***************************
007500050530        //** Initialize subroutine
007600050530        //***************************
007700050530         begsr  AAINIT;
007800050530
007900050627           open(e)  payrresl01;
008000050627           chain run_type  prrefmt;
008100050627           close(e)  payrresl01;
008200050627           get_sys_date(date1);
008300100814           // GET CURRENT PERIOD
008400050530           get_prd_info('*prd':*blanks:prno:date1);
008500050530
008600050530         Endsr;
008700050530
008800050530
008900050530        //***************************************                           **
009000050530        //*** Write Payroll deduction record  ***                           **
009100050530        //***************************************                           **
009200050530         begsr  write_deduction;
009300050530
009400050627              pcgsts = *blanks;
009500050621              pcgtype = emseltyp;
009600050530              pcgemp = EMP;
009700050530              pcgamt = dedu;
009800050530              write  pcgfmt;
009900050530
010000050530         Endsr;
010100990216
010200050530        //*******************************************                       **
010300050530        //*** GENERATE PAYROLL DEDUCTIONS & REPORTS                         **
010400050530        //*******************************************                       **
010500050530        begsr  gen_ded;
010600050530
010700050530          cntr = *zeros;
010800050530          dow not get_emp_info2('R':-1:emp_struc:cntr);
010900050530
011000050627           if  %scan(status:prrelist) > 0
011100050627              and  get_Cat(status) = 'A';
011200050627
011300050627           // remove any previous record found except
011400050627           // those manually setup
011500050627
011600050627            chain (emseltyp:emp) pcgfmt;
011700050627            if  %found and pcgsts <> 'A';
011800050627               delete pcgfmt;
011900050627             elseif %found and pcgsts = 'A';
012000050627               iter;
012100050627            endif;
012200050530
012300050530                class# = %xlate('P':' ':class);
012400050621                get_emplimbal(*zeros:emseltyp:
012500050530                        *zeros:emp:*zeros:prno:curbal);
012600990222
012700050621                if  curbal > *zeros;
012800050530
012900050530                    // Perform  deduction checks
013000050530
013100050602                   // Always deduct (if class record not found)
013200050621                  if  get_paded_info('RD':emseltyp:*zeros:
013300050530                               class#:paded_struc);
013400050530                      dedu = curbal;
013500050530                      exsr write_Deduction;
013600050530                    else;
013700050621                      chain (emseltyp:*zeros:emp) lafmt;
013800050530                      if not %found;
013900050530                         laamt = *zeros;
014000050530                      endif;
014100050530
014200050530                      select;
014300050602
014400050602                      // Above yearly balance
014500050531                        when lrul3ded = 1;
014600050602                           pay_amt = (get_emp_pay(emp:dept:empaytype:
014700050602                                       hrs_dummy:payrun_factor:
014800050602                                       payrun_qty:status));
014900050602
015000050602                           if curbal > pay_amt;
015100050602                              dedu = curbal - pay_amt;
015200050602                              exsr write_Deduction;
015300050602                           endif;
015400050602
015500050602                      // Do not deduct
015600050531                        when lrul3ded = 2;
015700050530                           dedu = *zeros;
015800050602
015900050602                      // Above Monthly allowance
016000050531                        when lrul3ded = 3;
016100050602                           pay_amt = (get_emp_pay(emp:dept:empaytype:
016200050602                                       hrs_dummy:payrun_factor:
016300050602                                       payrun_qty:status)) /
016400050602                                        YEAR_CONST;
016500050602                           if curbal > pay_amt;
016600050602                              dedu = curbal - pay_amt;
016700050530                              exsr write_Deduction;
016800050530                           endif;
016900050602
017000050602                      // Above Year-to-date allowance
017100050531                        when lrul3ded = 4;
017200050531                           if curbal > laamt;
017300050531                              dedu = curbal - laamt;
017400050531                              exsr write_Deduction;
017500050531                           endif;
017600050530                        other;
017700100814
017800050530                      endsl;
017900050530
018000050530                  endif;
018100050530
018200050530
018300050530                endif;
018400050530
018500050530              dedu = *zeros;
018600050530
018700050530            endif;
018800050530
018900050530          enddo;
019000050530
019100050530        endsr;
019200050530
019300050530      /end-free
019400050530
