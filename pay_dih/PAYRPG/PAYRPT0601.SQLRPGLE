000100131112
000200170302     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR') alwnull(*usrctl)
000300111002     H/define profoundui
000400111002
000500080514      //* LISTING REGULARS EMPLOYEES N.I.S. REPORT.
000600080514      //*
000700080514      //* PROGRAMMER:- R.WILLIAMS  MOD. S.BRITTON
000800080514      //* redone by :- C.WILLIAMS  & S.BRITTON
000900080514      //*
001000140922     Fempmast   if   e           k disk    usropn
001100131112     Fpayhisl13 if   e           k disk    usropn
001200010913     Fpayhisl08 if   e           k disk    rename(phisfmt:phisprd)
001300111002     F                                     usropn
001400111002     Fpaynisctl if   e           k disk    usropn
001500111002     Fpaynisctl2if   e           k disk    usropn
001600111002     Fpayrtyp   if   e           k disk    usropn
001700111002     Fpaytranl05if   e           k disk    usropn
001800131210     FnistotF   uf a e           k DISK    usropn
001900111002     Fpaynrdl01 uf a e           k DISK    commit  usropn
002000080221     Fpaynrdl02 if   e           k disk    rename(pnisfmt:pnisprd)
002100111002     F                                     usropn
002200131112     F*payrpt06d cf   e             workstn
002300131112     F*/IF DEFINED(WEB)
002400131112     F*                                     HANDLER('PROFOUNDUI(HANDLER)')
002500131112     F*/ENDIF
002600040207     Fqsysprt   o    f  132        printer oflind(*inof)
002700111002
002800111002      /copy QCPYSRC,genpr
002900111002      /copy QCPYSRC,autpr
003000131112      /copy QCPYSRC,PaYpr
003100111002
003200010910     D arr             S             40    DIM(1) CTDATA PERRCD(1)
003300980414     D                 ds                  inz
003400131112     D  BIRTH#                 1      8  0
003500950802     D  DAYBIR                 1      2  0
003600950802     D  MTHBIR                 3      4  0
003700970107     D  YRBIR                  5      8  0
003800010910     Dbir_date                         D   datfmt(*iso)
003900010910     Dcur_date                         D   datfmt(*iso)
004000111002
004100010910     Dtrn_type         s                   like(ptrntcode)inz(63)
004200011001     Dlast_date        s                   like(phisndate)
004300011001     Dlast_emp         s                   like(phisemp)
004400011022
004500011022     Dphisiearn#       s                   like(phisiearn)
004600011022     Dphisaearn#       s                   like(phisaearn)
004700080229
004800080229     Dinvoke_NISCpy    pr                  extpgm('FINBI13C')
004900080229     Dperiod                          6  0 const
005000081017     Dcycle                           2    const
005100131025     Dcatflag                         1    const
005200131112     D BASICONLY       S              2A   INZ('*B')
005300131112     D STDEARN         S              2A   INZ('*S')
005400080229
005500080221     Dsave_prd         s                   like(prd)
005600080221     Dpayroll_cnt      s              1  0 inz(0)
005700011022
005800011004     Dtotpercent       s              2  0
005900020109     D#num             s              2  0
006000010912     Dnumtotal         s              4  0
006100010911     Dnumltlmt         s              4  0
006200010912     Dnum6ltlmt        s              4  0
006300010911     Dnumgtlmt         s              4  0
006400010912     Dnum6gtlmt        s              4  0
006500010911     Dnis_chk          s              2
006600011022     Dptrnamt#         s                   like(ptrnamt)
006700131112     Dpnieper#         s                   like(pnieper)
006800060103     Dniscur           s             11  2
006900060103     Damtltlmt         s             11  2
007000060103     Damtgtlmt         s             11  2
007100060103     Damttotal         s             11  2
007200010913     Dpni2max#         s              9  2
007300020611     Dreportxt1        s             30
007400131112     Derrmsg           s             75
007500020611     Dreportxt2        s             30
007600111002
007700010911     Dinstotal         s             11  2
007800010911     Dinsltlmt         s             11  2
007900010911     Dinsgtlmt         s             11  2
008000080514      //
008100010912     Dtot6O            s             11  2
008200010913     Dtot6l            s             11  2
008300111002
008400060103     Dottot            s             11  2
008500060103     Dotact            s             11  2
008600060103     Dotins            s             11  2
008700010912     Dotecnt           s              4  0
008800111002
008900131210     Dcnt              s              4  0  inz(0)
009000010911     Dcom_con          s              7  2
009100010911     Dcom_gtlmt        s              9  2
009200010911     Dall_gtlmt        s              9  2
009300010911     Dcom_ltlmt        s              9  2
009400010911     Dall_ltlmt        s              9  2
009500171103     Dcom_total        s             10  2
009600010910     Demp_con          s              7  2
009700010911     Demp_ltlmt        s              9  2
009800010911     Demp_gtlmt        s              9  2
009900171103     Demp_total        s             10  2
010000171103     Dall_total        s             10  2
010100010910     Dtotal_con        s              8  2
010200171103
010300131210     DStd_Emp          c                   'Workers Above $50,000.00 Per Month'
010400131210     Ddate_err         c                   'Invalid Date Entered'
010500010910     Dtype_err         c                   'Invalid Payroll Type Entered'
010600010910     Drec_err          c                   'No Payroll Found For this Date'
010700010913     Dmult_choice      c                   'Cannot Select Both Pay Date & Perio-
010800010913     D                                     d'
010900010910     Dnorec_err        c                   'No Record Was Found for The -
011000010910     d                                      Criteria Entered'
011100111002
011200950930     D                SDS
011300950930     D  @PGM             *PROC
011400111002     D  @USER                358    367
011500111002
011600010910     D name            s             40
011700010910     D rep_name        s             25
011800010910     D rep_title       s             40
011900080221     D                 ds
012000080221     D pnrprno                        6  0
012100080221     D year                    1      4  0
012200080221     D month                   5      6  0
012300131025
012400131025     D*--------------------------------------------------
012500131025     D* Procedure name: MAINPGM
012600131025     D* Purpose:        NIS REPORT HANDLER
012700131025     D* Returns:
012800131025     D* Parameter:      PERIOD => REPORT PERIOD
012900131025     D* Parameter:      RUNDATE => PAYROLL RUN DATE
013000131025     D* Parameter:      CAT_FLAG => WAGE CATEGORY FLAG
013100131025     D*--------------------------------------------------
013200131112     D MAINPGM         PR                  EXTPGM('PAYRPT0601')
013300131025     D                                6P 0
013400131025     D                                8P 0
013500131112     D                                1A
013600131025     D                                1A
013700131112     D                                2A
013800131025
013900131025     D MAINPGM         PI
014000131025     D  PRD                           6P 0
014100131025     D  PAY_DATE                      8P 0
014200131112     D  PAY_TYPE                      1A
014300131112     D  CAT_FLAG                      1A
014400131112     D  prt_code                      2A
014500131112      *
014600131112     D @DATE           S             10A
014700131112      *
014800131112     D CONAME          S             30A
014900131112
015000131112     D*--------------------------------------------------
015100131112     D* Procedure name: isDefault
015200131112     D* Purpose:
015300131112     D* Returns:
015400131112     D* Parameter:      emp##
015500131112     D*--------------------------------------------------
015600131112     D isDefault       PR              N
015700131112     D  emp##                         6P 0
015800131112
015900131112     D*--------------------------------------------------
016000131112     D* Procedure name: openFileprc
016100131112     D* Purpose:
016200131112     D* Returns:
016300131112     D*--------------------------------------------------
016400131112     D openFileprc     PR
016500131112
016600131112     D*--------------------------------------------------
016700131112     D* Procedure name: readFileprc
016800131112     D* Purpose:
016900131112     D* Returns:
017000131112     D*--------------------------------------------------
017100131112     D readFileprc     PR              N
017200131112
017300131112     D*--------------------------------------------------
017400131112     D* Procedure name: closePrc
017500131112     D* Purpose:
017600131112     D* Returns:
017700131112     D*--------------------------------------------------
017800131112     D closePrc        PR
017900131112      *
018000131112     D pn3DS         E DS                  EXTNAME('PAYNISCTL3')
018100131210      *
018200131210     D pni3desc#       S             40A   INZ(*BLANK)
018300131210      *
018400131210     D FNIGHT#         S              2S 0 INZ(0)
018500140922       DCL-S pnicper# LIKE(pnicper) INZ(*zeros);
018600140922
018700131210
018800111002
018900010908     C     nis_key       klist
019000010908     C                   kfld                    pay_date
019100011023     C                   kfld                    pay_type
019200011023     C                   kfld                    emp
019300131112     C                   kfld                    cat_flag
019400010910
019500131112     C     nis_key2      klist
019600131112     C                   kfld                    pay_date
019700131112     C                   kfld                    pay_type
019800131112     C                   kfld                    emp
019900010910     C     tran_key      klist
020000010910     C                   kfld                    pay_date
020100010910     C                   kfld                    pay_type
020200010910     C                   kfld                    trn_type
020300011107     C                   kfld                    emp
020400131028     C                   kfld                    cat_flag
020500010910
020600140211
020700131210     C*                   kfld                    cat_flag
020800131210     C     key2          klist
020900131210     C                   kfld                    pay_type
021000131210     C                   kfld                    pay_date
021100131210     C                   kfld                    #num
021200131210     C                   kfld                    cat_flag
021300111002
021400980228     C                   move      *date         cur_date
021500040207      /FREE
021600131025
021700080514       rep_title = %trim(arr(1));
021800131113           chain pay_type prtyfmt;
021900131112
022000131112         read pnifmt;
022100131112
022200131112        exec sql
022300131112          select * into :pn3DS from paynisctl3
022400131112            where pn3wcat = :cat_flag;
022500131112
022600131112        // if alternate nis recpord found,over-ride defaults
022700131112
022800131112        if sqlstt = '00000';
022900140922            pnicper# =  pni3cper;
023000131112            pnieper# = pni3eper;
023100140922            totpercent = pnieper# + pnicper#;
023200140922            pni3desc# = pni3desc;
023300140922               if pni3cyc = '*M';
023400140922                  pni2max# = pni3max;
023500140922               endif;
023600140922
023700140922
023800131112          else;
023900140922
024000131210            pni3desc# = std_emp;
024100131112            pnieper# = pnieper;
024200140922            pnicper# =  pnicper;
024300140922            totpercent = pnieper# + pnicper#;
024400131112        ENDIF;
024500131112
024600131112
024700131112
024800131112       chain prt_code pni2fmt;
024900080514       setll *loval emprec;
025000080514       read emprec;
025100080514       dow not(%eof(empmast));
025200080514         setll nis_key phisfmt;
025300080514         reade nis_key phisfmt;
025400011023
025500080514         //* read next record
025600131112         dow not(%eof(payhisl13));
025700011023
025800080514           phisiearn# = phisiearn# + phisiearn;
025900080514           phisaearn# = phisaearn# + phisaearn;
026000011023
026100080514           last_emp = phisemp;
026200080514           last_date = phisndate;
026300171103
026400080514           reade nis_key phisfmt;
026500080514         enddo;
026600011023
026700080514         setll tran_key ptisfmt;
026800080514         reade tran_key ptisfmt;
026900080514         *IN55 = %EOF;
027000080514         dow not (*in55);
027100080514           // get total amount paid by each employee
027200080514           ptrnamt# = ptrnamt# + ptrnamt;
027300080514           // write NIS data to file
027400080514
027500080221          if ptrntcode = 63;
027600080221          // if record does not exist, add to file
027700080221          // if record does not exist, add to file
027800080221           chain (emp:save_prd:pay_type) pnisprd;
027900080514
028000080221           if %found;
028100080221           dow not(%eof(paynrdl02));
028200080221           reade (emp:save_prd:pay_type) pnisprd;
028300080221           enddo;
028400080221           payroll_cnt = pnrboxno;
028500080221           else;
028600080221           payroll_cnt = 0;
028700080221           endif;
028800131112          chain nis_key2 pnisfmt;
028900080221          if not(%found(paynrdl01));
029000080221             pnrsts = 'A';
029100080221             pnrtype = ptrntype;
029200080221             pnrncode = ptrncode;
029300080221             pnrtcode = ptrntcode;
029400080221             pnremp = ptrnemp;
029500080221             pnrrdate = ptrnrdate;
029600080221             pnrpdate = ptrnpdate;
029700080221             pnrndate = ptrnndate;
029800080221             pnreamt  = ptrnamt;
029900080221             // to populate below
030000080221             pnrprno  = *zeros;
030100080221             pnryear  = *zeros;
030200080221             pnrmonth = *zeros;
030300080221             pnriamt  = *zeros;
030400080221             pnrcamt  = *zeros;
030500081017             pnrfcode = prtycycode;
030600080221             pnrboxno = payroll_cnt +1;
030700131211             PNRCATFLG = cat_flag;
030800080221             // Write Correct run type for pay transaction file
030900081020               write pnisfmt;
031000080221             commit;
031100080221           endif;
031200080221           endif;
031300131028
031400080514           reade tran_key ptisfmt;
031500080514           *IN55 = %EOF;
031600080514         enddo;
031700080514         if phisiearn# > 0;
031800011023
031900080514           rep_name = %trim(christ) + ' ' +
032000080514               %trim(surnam);
032100011023
032200131112            BIRTH# = BIRTH;
032300080514           // Is NIS field is blank move DATE of BIRTH to NIS number Field
032400131112           if birth# > 0;
032500140211
032600131112
032700131112     C     *eur          move      birth#        bir_date
032800140211
032900080514             if nisnum = *blanks;
033000140211
033100011023     C                   move      bir_date      nisnum
033200140211
033300080514             endif;
033400080514             //
033500140211
033600011023     C     cur_date      subdur    bir_date      emp_age:*y        4 0
033700140211
033800080514             *in11 = emp_age >= pnimxage;
033900131028
034000040113         if *in11;
034100040113           *in11 = ptrnamt# = 0;
034200040113         endif;
034300131112
034400080514             if not *in80 or *inof;
034500080514               except hedr;
034600080514             endif;
034700080514             //
034800140211                    if not *in80;
034900140211                      *in80 = *on;
035000140211                    ENDIF;
035100011023
035200140211
035300080514             if *in11;
035400080514               com_con = phisiearn# * (pninper/100);
035500080514               emp_con = *zeros;
035600080514             else;
035700140922               com_con = phisiearn# * (pnicper#/100);
035800080514               emp_con = ptrnamt#;
035900080514             endif;
036000080514             ptrnamt# = *zeros;
036100011023
036200080514             com_con = round_up(com_con:'A');
036300080514             total_con = com_con + emp_con;
036400080514             #num  = phisfnight;
036500080514             exsr summ;
036600131025
036700080221          // adding the company contribution to file
036800080221          chain (pay_date:pay_type:emp) pnisfmt;
036900080221          if %found;
037000080221          chain nis_key phisfmt;
037100131112          if %found(payhisl13);
037200080221             pnrprno  = phisprno;
037300080221             pnryear  = year;
037400080221             pnrmonth = month;
037500080221             endif;
037600080221             pnriamt  = phisiearn#;
037700080221             pnrcamt  = com_con;
037800080221             pnrreg#  = pnireg;
037900081020             update pnisfmt;
038000080221          endif;
038100131025
038200080514             except detail;
038300080514             //
038400080514             if yrbir = 0;
038500080514               *in11 = '0';
038600080514             endif;
038700080514           endif;
038800080514         endif;
038900080514         //
039000080514         read emprec;
039100080514       enddo;
039200011023
039300080514       *inlr = '1';
039400080514       //
039500080514       exsr fsumm;
039600080514       // GET Monthly Figure
039700080514       nis_chk = '*M';
039800131112       if cat_flag = ' ';
039900131112           chain Nis_chk pni2fmt;
040000131112           pni2max# = pni2max;
040100131112        endif;
040200131112           reportxt1 = 'LESS THAN' + ' $' +
040300080514           %char(pni2max#) + ' MONTHLY';
040400131112           reportxt2 = %char(pni2max#) + ' AND OVER'
040500080514           + ' MONTHLY';
040600080514       except fin_subtot;
040700131210       monitor;
040800131210          commit;
040900131210          on-error;
041000131210          endmon;
041100131210
041200111002         //*****************************
041300111002         // Session Init
041400111002         //*****************************
041500111002
041600111002          begsr *inzsr;
041700131112       //     get_session_info('PAY':errmsg:@pgm);
041800131112       //     if errmsg <> *blanks;
041900131112       //       show_msg(errmsg);
042000131112       //       *inlr = *on;
042100131112       //       return;
042200131112       //     endif;
042300131112         coname = get_coname;
042400131112           @date = get_sys_date();
042500111002            open(e) empmast;
042600131112            open(e) payhisl13;
042700111002            open(e) payhisl08;
042800111002            open(e) paynisctl;
042900111002            open(e) paynisctl2;
043000111002            open(e) payrtyp;
043100111002            open(e) paytranl05;
043200131210            open(e) nistotF;
043300111002            open(e) paynrdl01;
043400111002            open(e) paynrdl02;
043500111002
043600111002          endsr;
043700080514       //***************************************
043800080514       //                                    ***
043900080514       //***************************************
044000080514       begsr summ;
044100080514      /END-FREE
044200011022     C     phisaearn#    COMP      pni2max                              04
044300080514      //
044400080514      /FREE
044500080514         select;
044600080514         when not *in11;
044700080514           if *in04;
044800080514             com_ltlmt = com_con + com_ltlmt;
044900080514             emp_ltlmt = emp_con + emp_ltlmt;
045000080514             numltlmt = numltlmt + 1;
045100080514             amtltlmt = amtltlmt + phisaearn#;
045200080514             insltlmt  = insltlmt + phisiearn#;
045300080514             all_ltlmt = emp_ltlmt + com_ltlmt;
045400080514           ELSE;
045500080514             emp_gtlmt = emp_con + emp_gtlmt;
045600080514             com_gtlmt = com_con + com_gtlmt;
045700080514             all_gtlmt = emp_gtlmt + com_gtlmt;
045800080514             numgtlmt = numgtlmt + 1;
045900080514             amtgtlmt = amtgtlmt + phisaearn#;
046000080514             insgtlmt  = insgtlmt + phisiearn#;
046100080514      /END-FREE
046200011022     C                   ADD       phisaearn#    ACTSO            11 2
046300010910     C                   ADD       TOTAL_con     TOTSO            11 2
046400080514      /FREE
046500080514           Endif;
046600080514           //
046700080514         other;
046800080514           //
046900080514           IF *in04;
047000080514             num6ltlmt = num6ltlmt + 1;
047100080514      /END-FREE
047200011022     C                   ADD       phisaearn#    ACT6L            11 2
047300011022     C                   ADD       phisiearn#    INS6L            11 2
047400080514      /FREE
047500080514             tot6l   = tot6l + phisiearn# * (pninper/100);
047600080514             tot6l = round_up(tot6l:'A');
047700080514           ELSE;
047800080514             num6gtlmt = num6gtlmt + 1;
047900080514      /END-FREE
048000011022     C                   ADD       phisaearn#    ACT6O            11 2
048100011022     C                   ADD       phisiearn#    INS6O            11 2
048200171103
048300080514             tot6o   = tot6o + phisiearn# * (pninper/100);
048400080514             tot6o = round_up(tot6o:'A');
048500171103
048600080514           ENDIF;
048700080514         Endsl;
048800171103
048900080514         otecnt = num6gtlmt + num6ltlmt;
049000080514         com_total = com_ltlmt + com_gtlmt;
049100080514         emp_total = emp_ltlmt + emp_gtlmt;
049200080514         all_total = all_ltlmt + all_gtlmt;
049300080514         //                  if        otecnt  <> *zeros
049400080514         //                  except    detail
049500080514         //                  endif
049600080514         *IN01 = *OFF;
049700171103
049800080514       Endsr;
049900171103
050000080514       //***************************************
050100080514       //                                    ***
050200080514       //***************************************
050300080514       BEGSR FSUMM;
050400080514         amttotal = amtltlmt + amtgtlmt;
050500080514         numtotal = numgtlmt + numltlmt;
050600080514         instotal = insltlmt + insgtlmt;
050700080514         otact = act6O + act6l;
050800080514         otins = ins6O + ins6l;
050900080514         ottot = TOT6O + TOT6l;
051000140211
051100010913     C                   ADD       all_total     NISMTH           10 2
051200020121     C                   ADD       OTTOT         NISMTH
051300140211     C     key           klist
051400140211     C                   kfld                    pay_type
051500140211     C                   kfld                    pay_date
051600140211     C                   kfld                    #num
051700080514      //
051800140211
051900131210
052000131210       cnt = 0;
052100080514         setll key nisfmt;
052200131210         Dow cnt < 2;
052300131210          cnt += 1;
052400131210         readp nisfmt;
052500080514         //
052600140211         if %eof or nisfn > #num and niswcate = cat_flag and #num > 0
052700080514               or nistyp <> pay_type;
052800080514           nis#mt = *zeros;
052900131210           leave;
053000080514         endif;
053100131210         IF CNT = 2 And  niswcate <> cat_flag;
053200131210          nis#mt = *zeros;
053300131210          leave;
053400131210         ENDIF;
053500131210         if niswcate = cat_flag;
053600131210           leave;
053700131210         ENDIF;
053800131210         enddo;
053900080514         //
054000131112
054100080514         niscur = nis#mt + nismth;
054200131210
054300131210         chain key2 nisfmt;
054400080514         nis#mt   =    niscur;
054500131210
054600080514         if %found;
054700081020           update nisfmt;
054800080514         else;
054900080514           nisfn = #num;
055000131210           niswcate = cat_flag;
055100080514           nistyp = pay_type;
055200080514           nisdate = pay_date;
055300081020            write nisfmt;
055400080514         endif;
055500080514         //
055600080514       endsr;
055700131112
055800131112          //*************************************************
055900131112          //
056000131112          //*************************************************
056100131112      /END-FREE
056200040207     Oqsysprt   E            HEDR           1 03
056300950930     O                       @PGM                10
056400010908     O                       coname              72
056500010912     O                       @date              132
056600950812     O          E            HEDR           1
056700020611     O                       @user               10
056800020611     O                       prtydesc            40
056900950513     O                                           65 'EMPLOYEES N.I.S. REPORT'
057000020611     O                                           87 'FOR PAY PERIOD ENDING'
057100131112     O                       pay_date          +  1 '    /  /  '
057200131112     O                                          127 'PAGE'
057300950513     O                       PAGE          Z    132
057400950812     O          E            HEDR           2
057500950513     O                                           23 'EMPLOYER"S REGISTRATION'
057600950513     O                                           32 'NUMBER:-'
057700011004     O                       pnireg              42
057800131210     O                       pni3desc#           90
057900010910     O          E            HEDR           1
058000980805     O                                           10 'EMP.'
058100980805     O                                           25 'NAME'
058200950513     O                                           48 'N.I.S.'
058300010912     O                                           61 ' ACTUAL'
058400010912     O                                           76 'INSURABLE'
058500010910     O                                           97 'COMPANY  EMPLOYEE'
058600010910     O                                          112 'TOTAL'
058700020611     O                                          127 'PAY PERIOD No. '
058800950812     O          E            HEDR           2
058900980805     O                                            9 'NO.'
059000010911     O                                           25 'EMPLOYEE NAME'
059100950513     O                                           62 'NUMBER      EARNINGS'
059200010912     O                                           76 'EARNINGS'
059300140922     O                       pnicper#      1     86
059400131112     O                       pnieper#      1     97
059500011004     O                       totpercent         110
059600011004     O                                          112 '%'
059700010913     O          e            DETAIL         2
059800980805     O                     11                     4 '***'
059900010910     O                       phisemp       ZB    10
060000010910     O                       rep_name       B    36
060100010910     O                       nisnum         b    48
060200011022     O                       phisaearn#    1B    62
060300011022     O                       phisiearn#    1B    76
060400010910     O                       com_con       1B    88
060500010910     O                       emp_con       1B    97
060600010910     O                       total_con     1B   112
060700010910     O                       #num          Z    125
060800010910     O          e            fin_subtot  2  1
060900950513     O                                           74 'S   U   M   M   A   R'
061000950513     O                                           78 'Y'
061100010910     O          e            fin_subtot     2
061200950513     O                                           77 '========================'
061300950513     O                                           78 '='
061400010910     O          e            fin_subtot     1
061500950513     O                                           70 'T O T A L'
061600950513     O                                           89 'E A R N I N G S'
061700950513     O                                          125 'C O N T R I B U T I O N'
061800950513     O                                          127 'S'
061900010910     O          e            fin_subtot     1
062000950513     O                                           85 '------------------------'
062100950513     O                                           89 '----'
062200950513     O                                          126 '------------------------'
062300950513     O                                          127 '-'
062400010911     O          e            fin_subtot     1
062500950513     O                                           59 'EMPLOYEES'
062600950513     O                                           73 'ACTUAL'
062700950513     O                                           89 'INSURABLE'
062800970606     O                                          104 'COMPANY'
062900970207     O                                          117 'EMPLOYEE'
063000011004     O                                          124 'TOTAL '
063100011004     O                       totpercent    z    130
063200011004     O                                          132 ' %'
063300010911     O          e            fin_subtot     1
063400950513     O                                           59 '---------'
063500950513     O                                           73 '------'
063600950513     O                                           89 '---------'
063700970606     O                                          104 '-------'
063800970207     O                                          118 '--------'
063900950513     O                                          132 '----------'
064000010911     O          e            fin_subtot     2
064100020611     O                       reportxt1           30
064200020611     O                                           49 '--'
064300010911     O                       numltlmt      2B    57
064400010911     O                       amtltlmt      2B    73
064500010911     O                       insltlmt      2B    89
064600010911     O                       com_ltlmt     2B   104
064700010911     O                       emp_ltlmt     2B   118
064800010911     O                       all_ltlmt     2B   132
064900010911     O          e            fin_subtot     2
065000010913     O                       pni2max#      1     12
065100020611     O                                           27 ' & OVER MONTHLY'
065200980414     O                                           49 '-'
065300010911     O                       numgtlmt      2B    57
065400010911     O                       amtgtlmt      2B    73
065500010911     O                       insgtlmt      2B    89
065600010911     O                       com_gtlmt     2B   104
065700010911     O                       emp_gtlmt     2B   118
065800010911     O                       all_gtlmt     2B   132
065900010911     O          e            fin_subtot     3
066000950513     O                                           33 'TOTAL'
066100950513     O                                           49 '--'
066200010911     O                       numtotal      2B    57
066300010911     O                       amttotal      2B    73
066400010911     O                       instotal      2B    89
066500010911     O                       com_total     2B   104
066600010911     O                       emp_total     2B   118
066700010911     O                       all_total     2B   132
066800010911     O          e            fin_subtot  1  1
066900020611     O                                           52 'EMPLOYEES'
067000020611     O                       pnimxage            55
067100020611     O                                           65 'YEARS AND'
067200020611     O                                           79 'OVER OR UNDER'
067300020611     O                       pnimiage            82
067400020611     O                                           88 'YEARS'
067500010911     O          e            fin_subtot     3
067600950513     O                                           67 '------------------------'
067700950513     O                                           87 '------------------------'
067800950513     O                                           88 '-'
067900010911     O          e            fin_subtot  1  1
068000950513     O                                           59 'EMPLOYEES'
068100950513     O                                           73 'ACTUAL'
068200950513     O                                           89 'INSURABLE'
068300010912     O                                          126 'TOTAL '
068400010912     O                       pninper       1    132
068500010911     O          e            fin_subtot     2
068600950513     O                                           59 '---------'
068700950513     O                                           73 '------'
068800950513     O                                           89 '---------'
068900950513     O                                          132 '-----------'
069000010911     O          e            fin_subtot     2
069100020611     O                       reportxt1           30
069200950513     O                                           49 '--'
069300010912     O                       num6ltlmt     2B    57
069400950513     O                       ACT6L         1B    73
069500950513     O                       INS6L         1B    89
069600010913     O                       com_total     2B   104
069700010913     O                       emp_total     2B   118
069800010913     O                       TOT6L         1B   132
069900010911     O          e            fin_subtot     2
070000020611     O                       reportxt2           30
070100980414     O                                           49 '-'
070200010912     O                       num6gtlmt     2B    57
070300950513     O                       ACT6O         2B    73
070400950513     O                       INS6O         2B    89
070500950513     O                       TOT6O         2B   132
070600010911     O          e            fin_subtot     3
070700950513     O                                           33 'TOTAL'
070800950513     O                                           49 '--'
070900950513     O                       OTECNT        2B    57
071000950513     O                       OTACT         2B    73
071100950513     O                       OTINS         2B    89
071200950513     O                       OTTOT         2B   132
071300020121     O          e            fin_subtot  1  2
071400950513     O                                           52 'TOTAL REMITTANCE FOR THE'
071500020611     O                                           67 'PAY PERIOD   -'
071600010913     O                                           75 '$'
071700950513     O                       NISMTH        2B    89
071800010911     O          e            fin_subtot     2
071900950513     O                                           49 'TOTAL REMITTANCES FOR'
072000950513     O                                           67 'THE YEAR IS    --'
072100950513     O                       NISCUR        2B    89
072200950513     O                                           75 '$'
072300010911     O          e            fin_subtot     3
072400950513     O                                           52 'N.B. - *** DENOTE EMPLOY'
072500020611     O                                           55 'EES'
072600020611     O                       pnimxage            58
072700020611     O                                           73 'YEARS AND OVER'
072800010911     O          e            fin_subtot  1
072900950513     O                                           77 '**  END  OF  REPORT  **'
073000131112
073100131112
073200131112
073300131112
073400131112
073500131112     P*--------------------------------------------------
073600131112     P* Procedure name: isDefault
073700131112     P* Purpose:        checking to see if the nis percentage to use is the...
073800131112     P*                           default one
073900131112     P* Returns:
074000131112     P* Parameter:      emp_no => employee whose salary is being tested
074100131112     P*--------------------------------------------------
074200131112     P isDefault       B
074300131112     D isDefault       PI              N
074400131112     D  emp_no                        6P 0
074500131112
074600131112     D  in_yearf       s              7P 3
074700131112     D  wrkyearf       s              7P 3
074800131112     D  basic_wage     S             13P 2
074900131112     D  max_wage       s             13P 2
075000131112     D  min_wage       s             13P 2
075100131112     D
075200131112
075300131112
075400131112     D* Local fields
075500131112     D retField        S               N   inz(*on)
075600131112
075700131112      /FREE
075800131112          //get year factor of the cycle code sent to program
075900131112              exec sql
076000131112                select cycyearf into :in_yearf from paycycd where
076100131112                  cyccode = :prt_code;
076200131112
076300131112          basic_wage = (get_basic_wage(emp_no)/in_yearf);
076400131112
076500131112          //get data from alternate NIS config file
076600131112            openFileprc();
076700131112            dow not (readFilePrc());
076800131112
076900131112          //get year factor of the cycle code in alternate NIS config file
077000131112
077100131112                exec sql
077200131112                select cycyearf into :wrkyearf from paycycd where
077300131112                  cyccode = :pni3cyc;
077400131112
077500131112                min_wage = round_up((pni3min *
077600131112                               wrkyearf/in_yearf):'A');
077700131112                max_wage = round_up((pni3Max *
077800131112                                              wrkyearf/in_yearf):'A');
077900131112
078000131112                Select;
078100131112                  when Pni3ecat = BASICONLY;
078200131112
078300131112                  when Pni3ecat = STDEARN;
078400131112                    basic_wage = ptrnamt;
078500131112                endsl;
078600131112
078700131112               //checking each record to see if the
078800131112               if basic_wage <= max_wage and
078900131112                 basic_wage >= min_wage;
079000131112                 cat_flag = pn3wcat;
079100131112                 retfield = *off;
079200131112                 leave;
079300131112               endif;
079400131112
079500131112            enddo;
079600131112
079700131112            closePrc();
079800131112
079900131112          //Return calc flag
080000131112
080100131112        RETURN retField;
080200131112
080300131112      /END-FREE
080400131112     P isDefault       E
080500131112
080600131112     P*--------------------------------------------------
080700131112     P* Procedure name: openFileprc
080800131112     P* Purpose:
080900131112     P* Returns:
081000131112     P*--------------------------------------------------
081100131112     P openFileprc     B
081200131112     D openFileprc     PI
081300131112
081400131112
081500131112      /FREE
081600131112
081700131210
081800131112      /END-FREE
081900131112     P openFileprc     E
082000131112
082100131112     P*--------------------------------------------------
082200131112     P* Procedure name: readFileprc
082300131112     P* Purpose:
082400131112     P* Returns:
082500131112     P*--------------------------------------------------
082600131112     P readFileprc     B
082700131112     D readFileprc     PI              N
082800131112
082900131112
083000131112     D* Local fields
083100131112     D retField        S               N
083200131112
083300131112      /FREE
083400131112
083500131112           EXEC SQL
083600131112                  FETCH NEXT FROM NISALTFILE INTO
083700131112
083800131112                   :pn3DS;
083900131112
084000131112            if sqlstt = '02000';
084100131112               retField = *on;
084200131112              else;
084300131112               retField = *off;
084400131112            endif;
084500131112
084600131112
084700131112            RETURN retField;
084800131112
084900131112
085000131112        RETURN retField;
085100131112
085200131112      /END-FREE
085300131112     P readFileprc     E
085400131112
085500131112     P*--------------------------------------------------
085600131112     P* Procedure name: closePrc
085700131112     P* Purpose:
085800131112     P* Returns:
085900131112     P*--------------------------------------------------
086000131112     P closePrc        B
086100131112     D closePrc        PI
086200131112
086300131112
086400131112      /FREE
086500131112             EXEC SQL
086600131112                close NISALTFILE;
086700131112
086800131112
086900131112
087000131112      /END-FREE
087100131112     P closePrc        E
087200131210
087300131210
087400131210**
087500011008       NIS Monthly Report Listing
