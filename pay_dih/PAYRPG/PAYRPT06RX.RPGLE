000100010910     H DECEDIT('.') DATEDIT(*ymd/)
000200080514      //*
000300080514      //* LISTING REGULARS EMPLOYEES N.I.S. REPORT.
000400080514      //*
000500080514      //* PROGRAMMER:- R.WILLIAMS  MOD. S.BRITTON
000600080514      //* redone by :- C.WILLIAMS  & S.BRITTON
000700080514      //*
000800010910     Fempmast   if   e           k disk
000900010908     Fpayhisl03 if   e           k disk
001000010913     Fpayhisl08 if   e           k disk    rename(phisfmt:phisprd)
001100010910     Fpaynisctl if   e           k disk
001200010911     Fpaynisctl2if   e           k disk
001300010910     Fpayrtyp   if   e           k disk
001400011001     Fpaytranl05if   e           k disk
001500980228     Fnistotf   uf a e           k DISK
001600080221     Fpaynrdl01 uf a e           k DISK    commit
001700080221     Fpaynrdl02 if   e           k disk    rename(pnisfmt:pnisprd)
001800010913     Fpayrpt06d cf   e             workstn
001900040207     Fqsysprt   o    f  132        printer oflind(*inof)
002000080514      //
002100010908      /copy genpr
002200040207      /copy autpr
002300080514      //
002400010910     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002500980414     D                 ds                  inz
002600970107     D  BIRTH                  1      8  0
002700950802     D  DAYBIR                 1      2  0
002800950802     D  MTHBIR                 3      4  0
002900970107     D  YRBIR                  5      8  0
003000010910     Dbir_date                         D   datfmt(*iso)
003100010910     Dcur_date                         D   datfmt(*iso)
003200080514      //
003300010910     Dtrn_type         s                   like(ptrntcode)inz(63)
003400011001     Dlast_date        s                   like(phisndate)
003500011001     Dlast_emp         s                   like(phisemp)
003600011022
003700011022     Dphisiearn#       s                   like(phisiearn)
003800011022     Dphisaearn#       s                   like(phisaearn)
003900080229
004000080229     Dinvoke_NISCpy    pr                  extpgm('FINBI13C')
004100080229     Dperiod                          6  0 const
004200080229
004300080221     Dsave_prd         s                   like(prd)
004400080221     Dpayroll_cnt      s              1  0 inz(0)
004500011022
004600011004     Dtotpercent       s              2  0
004700020109     D#num             s              2  0
004800010912     Dnumtotal         s              4  0
004900010911     Dnumltlmt         s              4  0
005000010912     Dnum6ltlmt        s              4  0
005100010911     Dnumgtlmt         s              4  0
005200010912     Dnum6gtlmt        s              4  0
005300010911     Dnis_chk          s              2
005400011022     Dptrnamt#         s                   like(ptrnamt)
005500060103     Dniscur           s             11  2
005600060103     Damtltlmt         s             11  2
005700060103     Damtgtlmt         s             11  2
005800060103     Damttotal         s             11  2
005900010913     Dpni2max#         s              9  2
006000020611     Dreportxt1        s             30
006100020611     Dreportxt2        s             30
006200080514      //
006300010911     Dinstotal         s             11  2
006400010911     Dinsltlmt         s             11  2
006500010911     Dinsgtlmt         s             11  2
006600080514      //
006700010912     Dtot6O            s             11  2
006800010913     Dtot6l            s             11  2
006900080514      //
007000060103     Dottot            s             11  2
007100060103     Dotact            s             11  2
007200060103     Dotins            s             11  2
007300010912     Dotecnt           s              4  0
007400080514      //
007500010911     Dcom_con          s              7  2
007600010911     Dcom_gtlmt        s              9  2
007700010911     Dall_gtlmt        s              9  2
007800010911     Dcom_ltlmt        s              9  2
007900010911     Dall_ltlmt        s              9  2
008000010911     Dcom_total        s              9  2
008100010910     Demp_con          s              7  2
008200010911     Demp_ltlmt        s              9  2
008300010911     Demp_gtlmt        s              9  2
008400010911     Demp_total        s              9  2
008500010911     Dall_total        s              9  2
008600010910     Dtotal_con        s              8  2
008700080514      //
008800010910     Ddate_err         c                   'Invalid Date Entered'
008900010910     Dtype_err         c                   'Invalid Payroll Type Entered'
009000010910     Drec_err          c                   'No Payroll Found For this Date'
009100010913     Dmult_choice      c                   'Cannot Select Both Pay Date & Perio-
009200010913     D                                     d'
009300010910     Dnorec_err        c                   'No Record Was Found for The -
009400010910     d                                      Criteria Entered'
009500080514      //
009600950930     D                SDS
009700950930     D  @PGM             *PROC
009800950930     D  @USER                254    263
009900080514      //
010000010910     D name            s             40
010100010910     D rep_name        s             25
010200010910     D rep_title       s             40
010300080221     D                 ds
010400080221     D pnrprno                        6  0
010500080221     D year                    1      4  0
010600080221     D month                   5      6  0
010700080514      //
010800080514      //
010900010908     C     nis_key       klist
011000010908     C                   kfld                    pay_date
011100011023     C                   kfld                    pay_type
011200011023     C                   kfld                    emp
011300010910
011400010910     C     tran_key      klist
011500010910     C                   kfld                    pay_date
011600010910     C                   kfld                    pay_type
011700010910     C                   kfld                    trn_type
011800011107     C                   kfld                    emp
011900010910
012000010908     C     key           klist
012100020109     C                   kfld                    pay_type
012200020109     C                   kfld                    pay_date
012300020109     C                   kfld                    #num
012400080514      //
012500980228     C                   move      *date         cur_date
012600080514      // authority check
012700040207      /FREE
012800040207          authority_check('PAY':@pgm);
012900040207      /end-free
013000080514      //generic initialisign subroutine
013100011022      /copy *libl/payrpg,aainit
013200080514      /FREE
013300080514       rep_title = %trim(arr(1));
013400011022
013500080514       dow 0 = 0;
013600080514         exfmt fmt1;
013700010908
013800080514         if *in03;
013900080514           *inlr = '1';
014000080514           return;
014100080514         endif;
014200080514
014300080514         //** F1 = Run Type enquiry
014400080514         if *in01;
014500080514      /END-FREE
014600011008     C                   call      'PAYENQ02R'
014700011008     C                   parm                    pay_type
014800080514      /FREE
014900080514           iter;
015000080514         endif;
015100080514
015200080514         //** F5 = Generate NIS soft copy
015300080514         if *in05;
015400080514           exsr NIS_SoftCpy;
015500080514           iter;
015600080514         endif;
015700080514
015800080514         //** F7 = Create/Modify Mailing List
015900080514         if *in07;
016000080514           email_list('PAY':@pgm);
016100080514           iter;
016200080514         endif;
016300010913
016400080514         if prd <> 0;
016500080514           save_prd = prd;
016600080514           chain prd phisprd;
016700080514           if not %found;
016800080514             *in28 = '1';
016900080514             errmsg = norec_err;
017000080514             iter;
017100080514           endif;
017200010913
017300080514      /END-FREE
017400010913     C                   call      'PAYENQ05R'
017500010920     C                   parm                    pay_type
017600010920     C                   parm                    prd
017700010913     C                   parm                    pay_date
017800080514      /FREE
017900080514           prd = 0;
018000080514           iter;
018100080514         endif;
018200080514         test(dE) pay_date;
018300080514         *IN25 = %ERROR;
018400080514         if *in25;
018500080514           errmsg = date_err;
018600080514           iter;
018700080514         endif;
018800010913
018900080514         if prd <> 0  and pay_date <> 0;
019000080514           errmsg = mult_choice;
019100080514           iter;
019200080514         endif;
019300010913
019400080514         chain pay_date phisfmt;
019500080514         if not %found;
019600080514           *in27 = '1';
019700080514           errmsg = rec_err;
019800080514           iter;
019900080514         endif;
020000010908
020100080514         // validating payroll type
020200080514         chain pay_type prtyfmt;
020300080514         if not %found;
020400080514           *in26 = '1';
020500080514           errmsg = type_err;
020600080514           iter;
020700080514         endif;
020800080514         leave;
020900080514       enddo;
021000080514       //
021100010911
021200080514       chain prtycycode pni2fmt;
021300080514       setll *loval emprec;
021400080514       read emprec;
021500080514       dow not(%eof(empmast));
021600080514         setll nis_key phisfmt;
021700080514         reade nis_key phisfmt;
021800010910
021900080514         read pnifmt;
022000080514         totpercent = pnieper + pnicper;
022100011023
022200080514         //* read next record
022300011023
022400080514         dow not(%eof(payhisl03));
022500011023
022600080514           phisiearn# = phisiearn# + phisiearn;
022700080514           phisaearn# = phisaearn# + phisaearn;
022800011023
022900080514           //
023000080514           //
023100080514           last_emp = phisemp;
023200080514           last_date = phisndate;
023300080514           //
023400080514           //
023500080514           reade nis_key phisfmt;
023600080514         enddo;
023700011023
023800080514         setll tran_key ptisfmt;
023900080514         reade tran_key ptisfmt;
024000080514         *IN55 = %EOF;
024100080514         dow not (*in55);
024200080514           // get total amount paid by each employee
024300080514           ptrnamt# = ptrnamt# + ptrnamt;
024400080514           // write NIS data to file
024500080514
024600080221          if ptrntcode = 63;
024700080221          // if record does not exist, add to file
024800080221          // if record does not exist, add to file
024900080221           chain (emp:save_prd:pay_type) pnisprd;
025000080514
025100080221           if %found;
025200080221           dow not(%eof(paynrdl02));
025300080221           reade (emp:save_prd:pay_type) pnisprd;
025400080221           enddo;
025500080221           payroll_cnt = pnrboxno;
025600080221           else;
025700080221           payroll_cnt = 0;
025800080221           endif;
025900080221          chain nis_key pnisfmt;
026000080221          if not(%found(paynrdl01));
026100080221             pnrsts = 'A';
026200080221             pnrtype = ptrntype;
026300080221             pnrncode = ptrncode;
026400080221             pnrtcode = ptrntcode;
026500080221             pnremp = ptrnemp;
026600080221             pnrrdate = ptrnrdate;
026700080221             pnrpdate = ptrnpdate;
026800080221             pnrndate = ptrnndate;
026900080221             pnreamt  = ptrnamt;
027000080221             // to populate below
027100080221             pnrprno  = *zeros;
027200080221             pnryear  = *zeros;
027300080221             pnrmonth = *zeros;
027400080221             pnriamt  = *zeros;
027500080221             pnrcamt  = *zeros;
027600080221             pnrboxno = payroll_cnt +1;
027700080221             // Write Correct run type for pay transaction file
027800080221             write pnisfmt;
027900080221             commit;
028000080221           endif;
028100080221           endif;
028200080221      /end-free
028300080514      /FREE
028400080514           reade tran_key ptisfmt;
028500080514           *IN55 = %EOF;
028600080514         enddo;
028700011023
028800080514         if phisiearn# > 0;
028900011023
029000080514           rep_name = %trim(christ) + ' ' +
029100080514               %trim(surnam);
029200011023
029300080514           // Is NIS field is blank move DATE of BIRTH to NIS number Field
029400080514           if birth > 0;
029500080514      /END-FREE
029600011023     C     *eur          move      birth         bir_date
029700080514      /FREE
029800080514             if nisnum = *blanks;
029900080514      /END-FREE
030000011023     C                   move      bir_date      nisnum
030100080514      /FREE
030200080514             endif;
030300080514             //
030400080514      /END-FREE
030500011023     C     cur_date      subdur    bir_date      emp_age:*y        4 0
030600080514      /FREE
030700080514             *in11 = emp_age >= pnimxage;
030800080514      /END-FREE
030900040113      /free
031000040113         if *in11;
031100040113           *in11 = ptrnamt# = 0;
031200040113         endif;
031300040113      /end-free
031400080514      //
031500080514      //
031600080514      /FREE
031700080514             if not *in80 or *inof;
031800080514               except hedr;
031900080514             endif;
032000080514             //
032100080514      /END-FREE
032200011023     C  n80              seton                                        80
032300011023
032400080514      /FREE
032500080514             if *in11;
032600080514               com_con = phisiearn# * (pninper/100);
032700080514               emp_con = *zeros;
032800080514             else;
032900080514               com_con = phisiearn# * (pnicper/100);
033000080514               emp_con = ptrnamt#;
033100080514             endif;
033200080514             ptrnamt# = *zeros;
033300011023
033400080514             com_con = round_up(com_con:'A');
033500080514             total_con = com_con + emp_con;
033600080514             #num  = phisfnight;
033700080514             exsr summ;
033800080514      /END-FREE
033900080221      /free
034000080221          // adding the company contribution to file
034100080221          chain (pay_date:pay_type:emp) pnisfmt;
034200080221          if %found;
034300080221          chain nis_key phisfmt;
034400080221          if %found(payhisl03);
034500080221             pnrprno  = phisprno;
034600080221             pnryear  = year;
034700080221             pnrmonth = month;
034800080221             endif;
034900080221             pnriamt  = phisiearn#;
035000080221             pnrcamt  = com_con;
035100080221             pnrreg#  = pnireg;
035200080221             update pnisfmt;
035300080221          endif;
035400080221      /end-free
035500080514      /FREE
035600080514             except detail;
035700080514             //
035800080514             if yrbir = 0;
035900080514               *in11 = '0';
036000080514             endif;
036100080514           endif;
036200080514         endif;
036300080514         //
036400080514         read emprec;
036500080514       enddo;
036600011023
036700080514       *inlr = '1';
036800080514       //
036900080514       exsr fsumm;
037000080514       // GET Monthly Figure
037100080514       nis_chk = '*M';
037200080514       chain Nis_chk pni2fmt;
037300080514       pni2max# = pni2max;
037400080514       reportxt1 = 'LESS THAN' + ' $' +
037500080514           %char(pni2max#) + ' MONTHLY';
037600080514       reportxt2 = %char(pni2max#) + ' AND OVER'
037700080514           + ' MONTHLY';
037800080514       except fin_subtot;
037900080514       //***************************************
038000080514       //                                    ***
038100080514       //***************************************
038200080514       begsr summ;
038300080514      /END-FREE
038400011022     C     phisaearn#    COMP      pni2max                              04
038500080514      //
038600080514      /FREE
038700080514         select;
038800080514         when not *in11;
038900080514           if *in04;
039000080514             com_ltlmt = com_con + com_ltlmt;
039100080514             emp_ltlmt = emp_con + emp_ltlmt;
039200080514             numltlmt = numltlmt + 1;
039300080514             amtltlmt = amtltlmt + phisaearn#;
039400080514             insltlmt  = insltlmt + phisiearn#;
039500080514             all_ltlmt = emp_ltlmt + com_ltlmt;
039600080514           ELSE;
039700080514             emp_gtlmt = emp_con + emp_gtlmt;
039800080514             com_gtlmt = com_con + com_gtlmt;
039900080514             all_gtlmt = emp_gtlmt + com_gtlmt;
040000080514             numgtlmt = numgtlmt + 1;
040100080514             amtgtlmt = amtgtlmt + phisaearn#;
040200080514             insgtlmt  = insgtlmt + phisiearn#;
040300080514      /END-FREE
040400011022     C                   ADD       phisaearn#    ACTSO            11 2
040500010910     C                   ADD       TOTAL_con     TOTSO            11 2
040600080514      /FREE
040700080514           Endif;
040800080514           //
040900080514         other;
041000080514           //
041100080514           IF *in04;
041200080514             num6ltlmt = num6ltlmt + 1;
041300080514      /END-FREE
041400011022     C                   ADD       phisaearn#    ACT6L            11 2
041500011022     C                   ADD       phisiearn#    INS6L            11 2
041600080514      /FREE
041700080514             tot6l   = tot6l + phisiearn# * (pninper/100);
041800080514             tot6l = round_up(tot6l:'A');
041900080514           ELSE;
042000080514             num6gtlmt = num6gtlmt + 1;
042100080514      /END-FREE
042200011022     C                   ADD       phisaearn#    ACT6O            11 2
042300011022     C                   ADD       phisiearn#    INS6O            11 2
042400080514      /FREE
042500080514             tot6o   = tot6o + phisiearn# * (pninper/100);
042600080514             tot6o = round_up(tot6o:'A');
042700080514             //
042800080514           ENDIF;
042900080514         Endsl;
043000080514         otecnt = num6gtlmt + num6ltlmt;
043100080514         //
043200080514         com_total = com_ltlmt + com_gtlmt;
043300080514         emp_total = emp_ltlmt + emp_gtlmt;
043400080514         all_total = all_ltlmt + all_gtlmt;
043500080514         //                  if        otecnt  <> *zeros
043600080514         //                  except    detail
043700080514         //                  endif
043800080514         *IN01 = *OFF;
043900080514         //
044000080514       Endsr;
044100080514       //
044200080514       //***************************************
044300080514       //                                    ***
044400080514       //***************************************
044500080514       BEGSR FSUMM;
044600080514         amttotal = amtltlmt + amtgtlmt;
044700080514         numtotal = numgtlmt + numltlmt;
044800080514         instotal = insltlmt + insgtlmt;
044900080514         otact = act6O + act6l;
045000080514         otins = ins6O + ins6l;
045100080514         ottot = TOT6O + TOT6l;
045200080514      /END-FREE
045300010913     C                   ADD       all_total     NISMTH           10 2
045400020121     C                   ADD       OTTOT         NISMTH
045500080514      //
045600080514      /FREE
045700080514         setll key nisfmt;
045800080514         readp nisfmt;
045900080514         //
046000080514         if %eof or nisfn > #num
046100080514               or nistyp <> pay_type;
046200080514           nis#mt = *zeros;
046300080514         endif;
046400080514         //
046500080514         niscur = nis#mt + nismth;
046600010928
046700080514         chain key nisfmt;
046800080514         nis#mt   =    niscur;
046900080514         if %found;
047000080514           update nisfmt;
047100080514         else;
047200080514           nisfn = #num;
047300080514           nistyp = pay_type;
047400080514           nisdate = pay_date;
047500080514           write nisfmt;
047600080514         endif;
047700080514         //
047800080514       endsr;
047900080514       //*********************************************
048000080514       //  enable the user to generate NIS Soft-Copy *
048100080514       //*********************************************
048200080514      /END-FREE
048300080229      /free
048400080229           BEGSR NIS_SoftCpy;
048500080229           dow  0 = 0;
048600080229           exfmt winnisfmt1;
048700080229           if *in02;
048800080229           leaveSR;
048900080229           endif;
049000080229           if nisprd# <= 0;
049100080229           Dsply  'Invalid Period Entered';
049200080229           iter;
049300080229           endif;
049400080229           leave;
049500080229           enddo;
049600080229           invoke_NISCpy(nisprd#);
049700080229           endsr;
049800080229      /end-free
049900080514      //*************************************************
050000080514      //
050100080514      //*************************************************
050200040207     Oqsysprt   E            HEDR           1 03
050300950930     O                       @PGM                10
050400010908     O                       coname              72
050500010912     O                       @date              132
050600950812     O          E            HEDR           1
050700020611     O                       @user               10
050800020611     O                       prtydesc            40
050900950513     O                                           65 'EMPLOYEES N.I.S. REPORT'
051000020611     O                                           87 'FOR PAY PERIOD ENDING'
051100020611     O                       pay_date      Y   +  1
051200950513     O                                          127 'PAGE'
051300950513     O                       PAGE          Z    132
051400950812     O          E            HEDR           2
051500950513     O                                           23 'EMPLOYER"S REGISTRATION'
051600950513     O                                           32 'NUMBER:-'
051700011004     O                       pnireg              42
051800010910     O          E            HEDR           1
051900980805     O                                           10 'EMP.'
052000980805     O                                           25 'NAME'
052100950513     O                                           48 'N.I.S.'
052200010912     O                                           61 ' ACTUAL'
052300010912     O                                           76 'INSURABLE'
052400010910     O                                           97 'COMPANY  EMPLOYEE'
052500010910     O                                          112 'TOTAL'
052600020611     O                                          127 'PAY PERIOD No. '
052700950812     O          E            HEDR           2
052800980805     O                                            9 'NO.'
052900010911     O                                           25 'EMPLOYEE NAME'
053000950513     O                                           62 'NUMBER      EARNINGS'
053100010912     O                                           76 'EARNINGS'
053200010912     O                       pnicper       1     86
053300010912     O                       pnieper       1     97
053400011004     O                       totpercent         110
053500011004     O                                          112 '%'
053600010913     O          e            DETAIL         2
053700980805     O                     11                     4 '***'
053800010910     O                       phisemp       ZB    10
053900010910     O                       rep_name       B    36
054000010910     O                       nisnum         b    48
054100011022     O                       phisaearn#    1B    62
054200011022     O                       phisiearn#    1B    76
054300010910     O                       com_con       1B    88
054400010910     O                       emp_con       1B    97
054500010910     O                       total_con     1B   112
054600010910     O                       #num          Z    125
054700010910     O          e            fin_subtot  2  1
054800950513     O                                           74 'S   U   M   M   A   R'
054900950513     O                                           78 'Y'
055000010910     O          e            fin_subtot     2
055100950513     O                                           77 '========================'
055200950513     O                                           78 '='
055300010910     O          e            fin_subtot     1
055400950513     O                                           70 'T O T A L'
055500950513     O                                           89 'E A R N I N G S'
055600950513     O                                          125 'C O N T R I B U T I O N'
055700950513     O                                          127 'S'
055800010910     O          e            fin_subtot     1
055900950513     O                                           85 '------------------------'
056000950513     O                                           89 '----'
056100950513     O                                          126 '------------------------'
056200950513     O                                          127 '-'
056300010911     O          e            fin_subtot     1
056400950513     O                                           59 'EMPLOYEES'
056500950513     O                                           73 'ACTUAL'
056600950513     O                                           89 'INSURABLE'
056700970606     O                                          104 'COMPANY'
056800970207     O                                          117 'EMPLOYEE'
056900011004     O                                          124 'TOTAL '
057000011004     O                       totpercent    z    130
057100011004     O                                          132 ' %'
057200010911     O          e            fin_subtot     1
057300950513     O                                           59 '---------'
057400950513     O                                           73 '------'
057500950513     O                                           89 '---------'
057600970606     O                                          104 '-------'
057700970207     O                                          118 '--------'
057800950513     O                                          132 '----------'
057900010911     O          e            fin_subtot     2
058000020611     O                       reportxt1           30
058100020611     O                                           49 '--'
058200010911     O                       numltlmt      2B    57
058300010911     O                       amtltlmt      2B    73
058400010911     O                       insltlmt      2B    89
058500010911     O                       com_ltlmt     2B   104
058600010911     O                       emp_ltlmt     2B   118
058700010911     O                       all_ltlmt     2B   132
058800010911     O          e            fin_subtot     2
058900010913     O                       pni2max#      1     12
059000020611     O                                           27 ' & OVER MONTHLY'
059100980414     O                                           49 '-'
059200010911     O                       numgtlmt      2B    57
059300010911     O                       amtgtlmt      2B    73
059400010911     O                       insgtlmt      2B    89
059500010911     O                       com_gtlmt     2B   104
059600010911     O                       emp_gtlmt     2B   118
059700010911     O                       all_gtlmt     2B   132
059800010911     O          e            fin_subtot     3
059900950513     O                                           33 'TOTAL'
060000950513     O                                           49 '--'
060100010911     O                       numtotal      2B    57
060200010911     O                       amttotal      2B    73
060300010911     O                       instotal      2B    89
060400010911     O                       com_total     2B   104
060500010911     O                       emp_total     2B   118
060600010911     O                       all_total     2B   132
060700010911     O          e            fin_subtot  1  1
060800020611     O                                           52 'EMPLOYEES'
060900020611     O                       pnimxage            55
061000020611     O                                           65 'YEARS AND'
061100020611     O                                           79 'OVER OR UNDER'
061200020611     O                       pnimiage            82
061300020611     O                                           88 'YEARS'
061400010911     O          e            fin_subtot     3
061500950513     O                                           67 '------------------------'
061600950513     O                                           87 '------------------------'
061700950513     O                                           88 '-'
061800010911     O          e            fin_subtot  1  1
061900950513     O                                           59 'EMPLOYEES'
062000950513     O                                           73 'ACTUAL'
062100950513     O                                           89 'INSURABLE'
062200010912     O                                          126 'TOTAL '
062300010912     O                       pninper       1    132
062400010911     O          e            fin_subtot     2
062500950513     O                                           59 '---------'
062600950513     O                                           73 '------'
062700950513     O                                           89 '---------'
062800950513     O                                          132 '-----------'
062900010911     O          e            fin_subtot     2
063000020611     O                       reportxt1           30
063100950513     O                                           49 '--'
063200010912     O                       num6ltlmt     2B    57
063300950513     O                       ACT6L         1B    73
063400950513     O                       INS6L         1B    89
063500010913     O                       com_total     2B   104
063600010913     O                       emp_total     2B   118
063700010913     O                       TOT6L         1B   132
063800010911     O          e            fin_subtot     2
063900020611     O                       reportxt2           30
064000980414     O                                           49 '-'
064100010912     O                       num6gtlmt     2B    57
064200950513     O                       ACT6O         2B    73
064300950513     O                       INS6O         2B    89
064400950513     O                       TOT6O         2B   132
064500010911     O          e            fin_subtot     3
064600950513     O                                           33 'TOTAL'
064700950513     O                                           49 '--'
064800950513     O                       OTECNT        2B    57
064900950513     O                       OTACT         2B    73
065000950513     O                       OTINS         2B    89
065100950513     O                       OTTOT         2B   132
065200020121     O          e            fin_subtot  1  2
065300950513     O                                           52 'TOTAL REMITTANCE FOR THE'
065400020611     O                                           67 'PAY PERIOD   -'
065500010913     O                                           75 '$'
065600950513     O                       NISMTH        2B    89
065700010911     O          e            fin_subtot     2
065800950513     O                                           49 'TOTAL REMITTANCES FOR'
065900950513     O                                           67 'THE YEAR IS    --'
066000950513     O                       NISCUR        2B    89
066100950513     O                                           75 '$'
066200010911     O          e            fin_subtot     3
066300950513     O                                           52 'N.B. - *** DENOTE EMPLOY'
066400020611     O                                           55 'EES'
066500020611     O                       pnimxage            58
066600020611     O                                           73 'YEARS AND OVER'
066700010911     O          e            fin_subtot  1
066800950513     O                                           77 '**  END  OF  REPORT  **'
066900010908**
067000011008       NIS Monthly Report Listing
