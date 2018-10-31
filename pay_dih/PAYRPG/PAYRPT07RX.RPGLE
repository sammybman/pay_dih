000100010910     H DECEDIT('.') DATEDIT(*ymd/)
000200950513     F**
000300010911     F** redone by :- C.WILLIAMS  & S.BRITTON
000400950513     F**
000500020114     Fempmast   if   e           k disk
000600020107     Fpayhisl10 if   e           k disk
000700020107     Fpaytranl05if   e           k disk
000800010924     Fpayrpt07d cf   e             workstn
000900980228     Freport    o    f  132        printer oflind(*inof)
001000010908     **
001100010908      /copy genpr
001200020611      /copy *libl/payrpg,PAYPR
001300980228     **
001400010924     D arr             S             40    DIM(3) CTDATA PERRCD(1)
001500980228     **
001600010924     Dtax_type         s                   like(ptrntcode)inz(61)
001700010924     Dunion_type       s                   like(ptrntcode)inz(65)
001800010924     Dtran_type        s                   like(ptrntcode)
001900010924     **
002000010924     Dlast_emp         s                   like(phisemp)
002100020107     Dlast_type        s                   like(phistype)
002200010924     **
002300010924     Dstart_date       s                   like(phispdate)
002400010924     Dend_date         s                   like(phispdate)
002500010924     **
002600010924     Dtotal_ded        s                   like(phisgpay)
002700010924     Dgross_inc        s                   like(phisgpay)
002800010924     Dall_total        s                   like(phisgpay)
002900010912     **
003000010911     **
003100010910     Drec_err          c                   'No Payroll Found For this Date'
003200010910     Dnorec_err        c                   'No Record Was Found for The -
003300010910     d                                      Criteria Entered'
003400020107
003500020107     Dindptr           s               *   inz(%addr(*in))
003600020107     D                 ds                  based(indptr)
003700020107     Dend_program              3      3n
003800020107
003900950930     D                SDS
004000950930     D  @PGM             *PROC
004100950930     D  @USER                254    263
004200950930     **
004300010924     D emp_name        s             40
004400010910     D rep_title       s             40
004500950810     **
004600011001     Dlast_date        s                   like(phisndate)
004700011001     Dlast_run_date    s                   like(phisndate)
004800950812     **
004900010910
005000010910     C     tran_key      klist
005100011001     C                   kfld                    phisndate
005200010925     C                   kfld                    phistype
005300010924     C                   kfld                    tran_type
005400010925     C                   kfld                    phisemp
005500010910
005600970206     **
005700020107      /copy aainit
005800010908     C                   dow       0 = 0
005900010908     C                   exfmt     fmt1
006000010908
006100020107      /copy exit_copy
006200010913
006300010924     C     pay_prno      chain     phisfmt
006400010913     C                   if        not %found
006500010913     C                   eval      *in28 = '1'
006600010913     C                   eval      errmsg = norec_err
006700010913     C                   iter
006800010913     C                   endif
006900010913
007000010924     C                   select
007100010924     C                   when      rep_sel = 1
007200010924     C                   eval      rep_title = %trim(arr(2))
007300010924     C                   eval      tran_type = tax_type
007400010924     C                   eval      *in99 = *on
007500010924
007600010924     C                   when      rep_sel = 2
007700010924     C                   eval      rep_title = %trim(arr(3))
007800010924     C                   eval      tran_type = union_type
007900010924     C                   eval      *in99 = *off
008000010924     C                   other
008100010924     C                   iter
008200010924     C                   endsl
008300010924
008400020114     C                   callp     get_prd_info('*rng':*blanks:pay_prno:
008500020114     C                             start_date:end_date)
008600010908     C                   leave
008700010908     C                   enddo
008800980228     **
008900010911
009000010924     C                   except    hedr
009100010911
009200011001     ** start of pgm loop
009300011001
009400010924     C     pay_prno      chain     phisfmt
009500011001
009600011001     ** save first emp found
009700010924
009800020107     C                   dow       not(%eof(payhisl10))
009900040109     C                   if        phisndate = 20031120
010000020107     C                   eval      gross_inc = gross_inc + phisgpay
010100010910
010200011001     C                   if        phisemp = last_emp and
010300020107     C                             phisndate <> last_date
010400011001     C                             or phisemp <> last_emp
010500020107     C                             or phisemp = last_emp and
010600020107     C                             phisndate = last_date  and phistype <>
010700020107     C                             last_type
010800020107     **
010900011001     C     tran_key      setll     ptisfmt
011000020107     C     tran_key      reade     ptisfmt
011100020107     C                   dow       not (%eof(paytranl05))
011200010925     C                   exsr      summ
011300020107     C     tran_key      reade     ptisfmt
011400011001     C                   enddo
011500980228     **
011600010924     C                   if        *inof
011700010924     C                   except    hedr
011800010924     C                   endif
011900011001
012000011001     C                   eval      last_emp = phisemp
012100020107     C                   eval      last_type = phistype
012200011001     C                   eval      last_date = phisndate
012300011001     C                   eval      last_run_date = phisrdate
012400011001     C                   endif
012500030106     C                   endif
012600010924
012700010924     C     pay_prno      reade     phisfmt
012800010924     **
012900010924     C                   if        phisemp <> last_emp
013000010925
013100010924
013200010924     C                   if        total_ded >0
013300010924     C                   callp     get_emp_info(last_emp:*omit
013400010924     C                             :*blanks:emp_name)
013500020114     C     last_emp      chain     emprec
013600010925     C                   add       1             total             4 0
013700010925     C                   except    detail
013800010924     C                   endif
013900010924
014000010924     C                   eval      gross_inc = 0
014100010924     C                   eval      total_ded = 0
014200010924     C                   endif
014300010924
014400980228     C                   enddo
014500010924
014600010910     C                   eval      *inlr = '1'
014700950812     **
014800010911     C                   except    fin_subtot
014900010924     ******************************************
015000010924     **get total amount paid by each employee
015100010924     ******************************************
015200980228     C     summ          begsr
015300950812     **
015400020107
015500010924     C                   eval      total_ded = total_ded + ptrnamt
015600010924     C                   eval      all_total = all_total + ptrnamt
015700950810     **
015800980228     C                   endsr
015900010910     ***************************************************
016000950810     **
016100950812     OREPORT    E            HEDR           1 03
016200950930     O                       @PGM                10
016300010908     O                       coname              72
016400010912     O                       @date              132
016500950812     O          E            HEDR           1
016600010908     O                       @USER               10
016700010924     O                       rep_title           72
016800010924     O                                           73 '-'
016900010924     O                       end_date      Y     83
017000950513     O                                          127 'PAGE'
017100950513     O                       PAGE          Z    132
017200950812     O          E            HEDR           2
017300010925     O                                           11 'No.'
017400010924     O                                           29 'Employee Name'
017500010925     O               99                          75 'N.I.S.'
017600010925     O               99                          97 'Gross'
017700010924     O                                          118 'Deducted'
017800010913     O          e            DETAIL         2
017900010925     O                       last_emp      Z     11
018000010924     O                       emp_name       B    56
018100010924     O               99      nisnum         b    75
018200010925     O               99      gross_inc     1     97
018300010924     O                       total_ded     1    118
018400010924     O          e            fin_subtot  1  1
018500950513     O                                           33 'TOTAL'
018600950513     O                                           49 '--'
018700010925     O               99      total         2   +  2
018800010925     O                       all_total     2    118
018900010911     O          e            fin_subtot  1
019000950513     O                                           77 '**  END  OF  REPORT  **'
019100010908**
019200020107     Tax & Union Monthly Report Listing
019300020107TAX Monthly Listing For Period Ending
019400010924Union Monthly Listing For Period Ending
