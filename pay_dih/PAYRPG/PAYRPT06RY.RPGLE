000100010910     H DECEDIT('.') DATEDIT(*ymd/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400111002
000500080514      //* LISTING REGULARS EMPLOYEES N.I.S. REPORT.
000600080514      //*
000700080514      //* PROGRAMMER:- R.WILLIAMS  MOD. S.BRITTON
000800080514      //* redone by :- C.WILLIAMS  & S.BRITTON
000900080514      //*
001000111002     Fempmast   if   e           k disk    usropn
001100111002     Fpayhisl03 if   e           k disk    usropn
001200010913     Fpayhisl08 if   e           k disk    rename(phisfmt:phisprd)
001300111002     F                                     usropn
001400111002     Fpaynisctl if   e           k disk    usropn
001500111002     Fpaynisctl2if   e           k disk    usropn
001600111002     Fpayrtyp   if   e           k disk    usropn
001700111002     Fpaytranl05if   e           k disk    usropn
001800111002     Fnistotf   uf a e           k DISK    usropn
001900111002     Fpaynrdl01 uf a e           k DISK    commit  usropn
002000080221     Fpaynrdl02 if   e           k disk    rename(pnisfmt:pnisprd)
002100111002     F                                     usropn
002200010913     Fpayrpt06d cf   e             workstn
002300111002     F/IF DEFINED(WEB)
002400111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002500111002     F/ENDIF
002600040207     Fqsysprt   o    f  132        printer oflind(*inof)
002700111002
002800111002      /copy QCPYSRC,genpr
002900111002      /copy QCPYSRC,autpr
003000111002
003100010910     D arr             S             40    DIM(1) CTDATA PERRCD(1)
003200980414     D                 ds                  inz
003300970107     D  BIRTH                  1      8  0
003400950802     D  DAYBIR                 1      2  0
003500950802     D  MTHBIR                 3      4  0
003600970107     D  YRBIR                  5      8  0
003700010910     Dbir_date                         D   datfmt(*iso)
003800010910     Dcur_date                         D   datfmt(*iso)
003900111002
004000010910     Dtrn_type         s                   like(ptrntcode)inz(63)
004100011001     Dlast_date        s                   like(phisndate)
004200011001     Dlast_emp         s                   like(phisemp)
004300011022
004400011022     Dphisiearn#       s                   like(phisiearn)
004500011022     Dphisaearn#       s                   like(phisaearn)
004600080229
004700080229     Dinvoke_NISCpy    pr                  extpgm('FINBI13C')
004800080229     Dperiod                          6  0 const
004900081017     Dcycle                           2    const
005000131025     DCATFLAG                         1    const
005100080229
005200080221     Dsave_prd         s                   like(prd)
005300080221     Dpayroll_cnt      s              1  0 inz(0)
005400011022
005500011004     Dtotpercent       s              2  0
005600020109     D#num             s              2  0
005700010912     Dnumtotal         s              4  0
005800010911     Dnumltlmt         s              4  0
005900010912     Dnum6ltlmt        s              4  0
006000010911     Dnumgtlmt         s              4  0
006100010912     Dnum6gtlmt        s              4  0
006200010911     Dnis_chk          s              2
006300011022     Dptrnamt#         s                   like(ptrnamt)
006400060103     Dniscur           s             11  2
006500060103     Damtltlmt         s             11  2
006600060103     Damtgtlmt         s             11  2
006700060103     Damttotal         s             11  2
006800010913     Dpni2max#         s              9  2
006900020611     Dreportxt1        s             30
007000020611     Dreportxt2        s             30
007100111002
007200010911     Dinstotal         s             11  2
007300010911     Dinsltlmt         s             11  2
007400010911     Dinsgtlmt         s             11  2
007500080514      //
007600010912     Dtot6O            s             11  2
007700010913     Dtot6l            s             11  2
007800111002
007900060103     Dottot            s             11  2
008000060103     Dotact            s             11  2
008100060103     Dotins            s             11  2
008200010912     Dotecnt           s              4  0
008300111002
008400010911     Dcom_con          s              7  2
008500010911     Dcom_gtlmt        s              9  2
008600010911     Dall_gtlmt        s              9  2
008700010911     Dcom_ltlmt        s              9  2
008800010911     Dall_ltlmt        s              9  2
008900010911     Dcom_total        s              9  2
009000010910     Demp_con          s              7  2
009100010911     Demp_ltlmt        s              9  2
009200010911     Demp_gtlmt        s              9  2
009300010911     Demp_total        s              9  2
009400010911     Dall_total        s              9  2
009500010910     Dtotal_con        s              8  2
009600080514      //
009700010910     Ddate_err         c                   'Invalid Date Entered'
009800010910     Dtype_err         c                   'Invalid Payroll Type Entered'
009900010910     Drec_err          c                   'No Payroll Found For this Date'
010000010913     Dmult_choice      c                   'Cannot Select Both Pay Date & Perio-
010100010913     D                                     d'
010200010910     Dnorec_err        c                   'No Record Was Found for The -
010300010910     d                                      Criteria Entered'
010400111002
010500950930     D                SDS
010600950930     D  @PGM             *PROC
010700111002     D  @USER                358    367
010800111002
010900010910     D name            s             40
011000010910     D rep_name        s             25
011100010910     D rep_title       s             40
011200080221     D                 ds
011300080221     D pnrprno                        6  0
011400080221     D year                    1      4  0
011500080221     D month                   5      6  0
011600131025
011700131025     D*--------------------------------------------------
011800131025     D* Program name: GenerateNISReport
011900131025     D* Purpose:
012000131025     D* Returns:
012100131025     D* Parameter:      PRD => REPORT PERIOD
012200131025     D* Parameter:      PAY_DATE => RUN DATE OF PAYROLL
012300131025     D* Parameter:      CAT_FLAG => WAGE CATEGORY FLAG
012400131025     D*--------------------------------------------------
012500131025     D GenerateNISReport...
012600131025     D                 PR                  EXTPGM('PAYRPT0601')
012700131025     D  PRD                           6P 0
012800131025     D  PAY_DATE                      8P 0
012900131025     D  CAT_FLAG                      1A
013000131025
013100131103     D*GenerateNISReport...
013200131103     D*                PI
013300131103     D* PRD                           6P 0
013400131103     D* PAY_DATE                      8P 0
013500131103     D* CAT_FLAG                      1A
013600131028
013700131028     D*--------------------------------------------------
013800131028     D* Procedure name: OpenFilePrc
013900131028     D* Purpose:        open NIS alternate file cursor
014000131028     D* Returns:
014100131028     D*--------------------------------------------------
014200131028     D OpenFilePrc     PR
014300131028
014400131028     D*--------------------------------------------------
014500131028     D* Procedure name: ReadFilePrc
014600131028     D* Purpose:        Read File cursor
014700131028     D* Returns:
014800131028     D*--------------------------------------------------
014900131028     D ReadFilePrc     PR              N
015000131028
015100131028     D*--------------------------------------------------
015200131028     D* Procedure name: CloseFilePrc
015300131028     D* Purpose:        close all open cursors
015400131028     D* Returns:
015500131028     D*--------------------------------------------------
015600131028     D CloseFilePrc    PR
015700131028      *
015800131028     D NIS3DS        E DS                  EXTNAME('PAYNISCTL3')
015900131028      * holds the value of the wage category for the report
016000131028     D wage_cat_flag   S              1A   INZ(*BLANK)
016100131028
016200131025      //
016300111002
016400010908     C     nis_key       klist
016500010908     C                   kfld                    pay_date
016600011023     C                   kfld                    pay_type
016700011023     C                   kfld                    emp
016800010910
016900010910     C     tran_key      klist
017000010910     C                   kfld                    pay_date
017100010910     C                   kfld                    pay_type
017200010910     C                   kfld                    trn_type
017300011107     C                   kfld                    emp
017400010910
017500010908     C     key           klist
017600020109     C                   kfld                    pay_type
017700020109     C                   kfld                    pay_date
017800020109     C                   kfld                    #num
017900111002
018000980228     C                   move      *date         cur_date
018100040207      /FREE
018200111002         // authority check
018300111002         monitor;
018400111002           authority_check('PAY':@pgm);
018500111002         on-error;
018600111002           show_msg(get_err_msg('DIH0054'));
018700111002           *inlr = *on;
018800111002           return;
018900111002         endmon;
019000111002
019100040207      /end-free
019200080514      //generic initialisign subroutine
019300111002      /copy QCPYSRC,aainit
019400080514      /FREE
019500131028        // rep_title = %trim(arr(1));
019600011022
019700080514       dow 0 = 0;
019800080514         exfmt fmt1;
019900010908
020000080514         if *in03;
020100080514           *inlr = '1';
020200080514           return;
020300080514         endif;
020400080514
020500080514         //** F1 = Run Type enquiry
020600080514         if *in01;
020700080514      /END-FREE
020800011008     C                   call      'PAYENQ02R'
020900011008     C                   parm                    pay_type
021000080514      /FREE
021100080514           iter;
021200080514         endif;
021300080514
021400080514         //** F5 = Generate NIS soft copy
021500080514         if *in05;
021600080514           exsr NIS_SoftCpy;
021700080514           iter;
021800080514         endif;
021900080514
022000080514         //** F7 = Create/Modify Mailing List
022100080514         if *in07;
022200080514           email_list('PAY':@pgm);
022300080514           iter;
022400080514         endif;
022500010913
022600080514         if prd <> 0;
022700080514           save_prd = prd;
022800080514           chain prd phisprd;
022900080514           if not %found;
023000080514             *in28 = '1';
023100080514             errmsg = norec_err;
023200080514             iter;
023300080514           endif;
023400010913
023500080514      /END-FREE
023600010913     C                   call      'PAYENQ05R'
023700010920     C                   parm                    pay_type
023800010920     C                   parm                    prd
023900010913     C                   parm                    pay_date
024000080514      /FREE
024100080514           prd = 0;
024200080514           iter;
024300080514         endif;
024400080514         test(dE) pay_date;
024500080514         *IN25 = %ERROR;
024600080514         if *in25;
024700080514           errmsg = date_err;
024800080514           iter;
024900080514         endif;
025000010913
025100080514         if prd <> 0  and pay_date <> 0;
025200080514           errmsg = mult_choice;
025300080514           iter;
025400080514         endif;
025500010913
025600080514         chain pay_date phisfmt;
025700080514         if not %found;
025800080514           *in27 = '1';
025900080514           errmsg = rec_err;
026000080514           iter;
026100080514         endif;
026200010908
026300080514         // validating payroll type
026400080514         chain pay_type prtyfmt;
026500080514         if not %found;
026600080514           *in26 = '1';
026700080514           errmsg = type_err;
026800080514           iter;
026900080514         endif;
027000080514         leave;
027100080514       enddo;
027200131107
027300131107       // call report with default category value
027400131028        GenerateNISReport(PRD:PAY_DATE:wage_cat_flag);
027500131107
027600131107       //check file to get various categories to report on.
027700131028        OpenFileprc();
027800131028        dow not(ReadFileprc());
027900131025
028000131028           GenerateNISReport(PRD:PAY_DATE:wage_cat_flag);
028100131028        enddo;
028200131103
028300131107        closeFilePrc();
028400131107
028500131030       //*****************************
028600131030       // Session Init
028700131030       //*****************************
028800131030
028900131030        begsr *inzsr;
029000131030
029100131030
029200131030          open(e) empmast;
029300131030          open(e) payhisl03;
029400131107          open(e) payhisl08;
029500131030          open(e) paynisctl;
029600131030          open(e) paynisctl2;
029700131030
029800131030          open(e) payrtyp;
029900131030          open(e) paytranl05;
030000131030          open(e) nistotf;
030100131030          open(e) paynrdl01;
030200131030          open(e) paynrdl02;
030300131030
030400131030        endsr;
030500131028
030600131030      /end-FREE
030700131028     P*--------------------------------------------------
030800131028     P* Procedure name: OpenFilePrc
030900131028     P* Purpose:        open NIS alternate file cursor
031000131028     P* Returns:
031100131028     P*--------------------------------------------------
031200131028     P OpenFilePrc     B
031300131028     D OpenFilePrc     PI
031400131028      /FREE
031500131028
031600131028
031700131028
031800131028      /END-FREE
031900131028     P OpenFilePrc     E
032000131028
032100131028     P*--------------------------------------------------
032200131028     P* Procedure name: ReadFilePrc
032300131028     P* Purpose:        Read File cursor
032400131028     P* Returns:
032500131028     P*--------------------------------------------------
032600131028     P ReadFilePrc     B
032700131028     D ReadFilePrc     PI              N
032800131028
032900131028     D* Local fields
033000131028     D retField        S               N
033100131028
033200131028      /FREE
033300131028
033400131028
033500131028        RETURN retField;
033600131028
033700131028      /END-FREE
033800131028     P ReadFilePrc     E
033900131028
034000131028     P*--------------------------------------------------
034100131028     P* Procedure name: CloseFilePrc
034200131028     P* Purpose:        close all open cursors
034300131028     P* Returns:
034400131028     P*--------------------------------------------------
034500131028     P CloseFilePrc    B
034600131028     D CloseFilePrc    PI
034700131028
034800131028      /FREE
034900131028
035000131028
035100131028
035200131028      /END-FREE
035300131028     P CloseFilePrc    E
035400131028**
035500131028       NIS Monthly Report Listing
035600131025
