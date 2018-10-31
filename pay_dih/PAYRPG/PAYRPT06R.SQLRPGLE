000100010910     H DECEDIT('.') DATEDIT(*ymd/)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400111002
000500080514      //* LISTING REGULARS EMPLOYEES N.I.S. REPORT.
000600080514      //*
000700080514      //* PROGRAMMER:- R.WILLIAMS  MOD. S.BRITTON
000800080514      //* redone by :- C.WILLIAMS  & S.BRITTON
000900080514      //*
001100131112     Fpayhisl13 if   e           k disk    usropn
001200010913     Fpayhisl08 if   e           k disk    rename(phisfmt:phisprd)
001300111002     F                                     usropn
001600111002     Fpayrtyp   if   e           k disk    usropn
002200010913     Fpayrpt06d cf   e             workstn
002300111002     F/IF DEFINED(WEB)
002400111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002500111002     F/ENDIF
002700111002
002800111002      /copy QCPYSRC,genpr
002900111002      /copy QCPYSRC,autpr
003000111002
003100010910     D arr             S             40    DIM(1) CTDATA PERRCD(1)
003300131111
003700131111
003900111002
004100011001     Dlast_date        s                   like(phisndate)
004300011022
004600080229
004700080229     Dinvoke_NISCpy    pr                  extpgm('FINBI13C')
004800080229     Dperiod                          6  0 const
004900081017     Dcycle                           2    const
005000131025     DCATFLAG                         1    const
005100080229
005200080221     Dsave_prd         s                   like(prd)
005400011022
005500131111
006300131111
006400131111
007100111002
007200131111
007500080514      //
007600131111
008300111002
008400131111
008900131111     D PNI3DS        E DS                  EXTNAME('PAYNISCTL3')
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
011300131111
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
012900131112     D  pay_type#                     1A
012901131112     D  CAT_FLAG                      1A
012902131112     D  prt_code                      2A
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
015900131028      * holds the value of the wage category for the report
016000131028     D wage_cat_flag   S              1A   INZ(*BLANK)
016100131111
016101131111     D*--------------------------------------------------
016102131111     D* Procedure name: getNISSoftCopy
016103131111     D* Purpose:
016104131111     D* Returns:
016105131111     D*--------------------------------------------------
016106131111     D getNISSoftCopy  PR
016107131111
016108131111      *
016109131111
016110131111
016200131025      //
016300111002
016400131111
017900111002
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
021501131111
021502131111            getNISSoftCopy();
021503131111
021504131111       //check file to get various categories to report on.
021505131111
021600131111
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
027200131112
027300131107       // call report with default category value
027400131112        GenerateNISReport(PRD:PAY_DATE:pay_type:wage_cat_flag:prtycycode);
027500131107
027600131107       //check file to get various categories to report on.
027700131028        OpenFileprc();
027800131028        dow not(ReadFileprc());
027900131111            wage_cat_flag = PN3WCAT;
028000131112           GenerateNISReport(PRD:PAY_DATE:pay_type:wage_cat_flag:prtycycode);
028100131028        enddo;
028200131103
028300131107        closeFilePrc();
028301131112        *inlr = *on;
028302131112        return;
028400131107
028500131030       //*****************************
028600131030       // Session Init
028700131030       //*****************************
028800131030
028900131030        begsr *inzsr;
029000131030
029100131030
029200131111
029300131112          open(e) payhisl13;
029400131107          open(e) payhisl08;
029500131111
029700131030
029800131030          open(e) payrtyp;
029900131111
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
031500131111         exec sql
031501131111          declare pni3file cursor for select * from paynisctl3 where
031502131111           pni3sts = 'A';
031503131111           exec sql
031504131111           open   pni3file;
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
033300131111              exec sql
033301131111              fetch next from pni3file into :pni3ds;
033302131111              if sqlstt <> '00000';
033303131111               return *on;
033304131111                 else;
033305131111                return *off;
033306131111              ENDIF;
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
034900131111            exec sql
035000131111             close   pni3file;
035100131028
035200131028      /END-FREE
035300131028     P CloseFilePrc    E
035400131111
035401131111     P*--------------------------------------------------
035402131111     P* Procedure name: getNISSoftCopy
035403131111     P* Purpose:
035404131111     P* Returns:
035405131111     P*--------------------------------------------------
035406131111     P getNISSoftCopy  B
035407131111     D getNISSoftCopy  PI
035408131111
035409131111
035410131111      /FREE
035411131111
035412131111         //*********************************************
035413131111       //  enable the user to generate NIS Soft-Copy *
035414131111       //*********************************************
035415131111
035416131111
035417131111           dow  0 = 0;
035418131111           exfmt winnisfmt1;
035419131111           if *in02;
035420131111           return;
035421131111           endif;
035422131111           if nisprd# <= 0;
035423131111           Dsply  'Invalid Period Entered';
035424131111           iter;
035425131111           endif;
035426131111           leave;
035427131111           enddo;
035428131111             wage_cat_flag = ' ';
035429131111            invoke_NISCpy(nisprd#:prtycycod#:wage_cat_flag);
035430131111              OpenFileprc();
035431131111            dow not(ReadFileprc());
035432131111             wage_cat_flag = PN3WCAT;
035433131111             invoke_NISCpy(nisprd#:prtycycod#:wage_cat_flag);
035434131111           enddo;
035435131111           wage_cat_flag = ' ';
035436131111
035437131111
035438131111
035439131111      /END-FREE
035440131111     P getNISSoftCopy  E
035441131111**
035500131028       NIS Monthly Report Listing
