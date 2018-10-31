000100980210     H DATEDIT(*ymd)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111013     H/define profoundui
000400141114      // ********************************************************
000500141114      //              B A N K S    D I H    L T D.              *
000600141114      //                   CENTS ELIMINATION                    *
000700141114      //                                                        *
000800141114      //                         BY                             *
000900141114      //                                                        *
001000141114      //             Banks DIH I.S. department                  *
001100141114      //             P.O BOX 10194, GEORGETOWN,GUYANA           *
001200141114      //                                                        *
001300141114      //                                                        *
001400141114      //       RPG PROGRAM NAME..: EMP004R                      *
001500141114      //                                                        *
001600141114      //       UPDATED BY.....: WINSTON ROBINSON                *
001700141114      //       Amended By.....: Chris Williams                  *
001800141114      //       Date ..........: 03/03/1999                      *
001900141114      //       DESIGN DATE....: 26/11/1998                      *
002000141114      //       MODULES........: *NONE                           *
002100141114      //       INDICATORS.....: 40:toggle- type field on/off.   *
002200141114      //                                 - type enquiry text    *
002300141114      //                                   on/off.              *
002400141114      //                                 - process all records  *
002500141114      //                                   text on/off          *
002600141114      //                        41:toggle- write off balance    *
002700141114      //                                   field on/off         *
002800141114      //                                   debit/credit field   *
002900141114      //                                   on/off               *
003000141114      //                                                        *
003100141114      //  THIS PROGRAM IS DESIGNED TO HANDLE CENTS ELIMINATION  *
003200141114      //  AND SMALL BALANCE WRITE-OFF IN THE LIABILITY FILES    *
003300141114      //  FILES.: LIABM   (master file)                         *
003400141114      //          LIABT   (transaction file)                    *
003500141114      //*********************************************************
003600111013     Fliabm     iF   E           K DISK    usropn
003700111013     fliabt     o    e           k disk    commit  usropn
003800141114      //
003900020206     femp004d   cf   e             workstn
004000111013     F/IF DEFINED(WEB)
004100111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
004200111013     F/ENDIF
004300141114      // General/Payroll PROTOTYPES
004400111013      /copy QCPYSRC,genPR
004500111013      /copy QCPYSRC,glPR
004600111013      /copy QCPYSRC,appPR
004700111013      /copy QCPYSRC,autPR
004800111013      /copy QCPYSRC,empPR
004900111013
005000980206     d                sds
005100980206     d  @pgm             *proc
005200111013     d  @user                358    367
005300141114      //
005400031001     d la              s                   dim(12) like(liamt01) based(lptr)
005500980206     d mth             s              2  0
005600981030     d cents#          s             11  2
005700981102     d bal1            s             11  2
005800981102     d bal2            s             11  2
005900020204     D prstrd          S              8  0
006000020204     D prendd          S              8  0
006100020206     D gl_out          S             25
006200020712     D glarr           s             25    dim(100)                             TOTAL AMOUNT
006300020206     D xcnt            s              4  0 inz(0)                               TOTAL AMOUNT
006400020712     D ycnt            s              4  0                                      TOTAL AMOUNT
006500031001     d lptr            s               *   inz(%addr(liamt01))
006600141114      //
006700031001     d  liab_struc   e ds                  extname(liabm)
006800141114      //
006900020712     Dgl_accumd        ds                  occurs(100) inz
007000020206     D gl_prd                         6  0
007100020206     D gl_account                    25
007200020206     D gl_desc                       30
007300020206     D gl_total                      13  2
007400030703     D gl_refno                      15
007500141114      //
007600980210     d                 ds
007700980216     d  balance                1     11  2
007800981029     d  cents                 10     11  2
007900141114      //
008000981030     d
008100980206     d  #text1         c                   const('Cents Elimination')
008200980210     d  #text2         c                   const('Writing off Small Balances')
008300981030     d  #text3         c                   const('Cents Elimination for')
008400981027     D
008500020204     D ARR             S             40    dim(2) CTDATA PERRCD(1)
008600020204     D date@           S              8  0
008700111013     D esysceacct      S             25
008800141114      //
008900141114      //   CHECK USER AUTHORITY
009000040207      /free
009100111013         // authority check
009200141126         monitor;
009300141126           authority_check('EMT':@pgm);
009400141126         on-error;
009500141126           show_msg(get_err_msg('DIH0054'));
009600141126           *inlr = *on;
009700141126           return;
009800141126         endmon;
009900111013
010000111013         // get clearing account
010100111013         exec sql
010200111013            select esysceacct into :esysceacct from empcon;
010300040207      /end-free
010400981027     C
010500981027     C     *like         define    ltamt         ltamt#
010600981027     C     start         tag
010700141114      // display first screen
010701141114      /FREE
010800141114       exsr aainit;
010900141114       exfmt fmt1;
011000141114
011100141114       // in03 = End**
011200141114       IF *in03 = '1';
011300141114         *INlr = *ON;
011400141114         return;
011500141114       endif;
011600141114
011700141114       if choice  = ' ';
011800141114         *IN42 = *ON;
011801141114      /END-FREE
011900981027     C                   goto      start
011901141114      /FREE
012000141114       endif;
012100141114
012200141114       // arrange fields in format2
012300141114       select;
012400141114       WHEN choice = '1';
012500141114         *IN40 = *ON;
012600141114         *IN41 = *OFF;
012601141114      /END-FREE
012700981027     C                   goto      top
012701141114      /FREE
012800141114       WHEN choice = '2';
012900141114         *IN41 = *ON;
013000141114         *IN40 = *OFF;
013001141114      /END-FREE
013100981103     C                   move      'D'           act
013200981027     C                   goto      top
013300981027     C
013301141114      /FREE
013400141114       endsl;
013500141114       //
013501141114      /END-FREE
013600981027     C     top           tag
013601141114      /FREE
013700141114       exfmt fmt2;
013800141114       *IN88 = *OFF;
013801141114       *IN47 = *OFF;
013802141114       *IN54 = *OFF;
013900141114       *IN89 = *OFF;
013901141114      /END-FREE
014000990213     C                   move      *blanks       errmsg
014100981027     C
014101141114      /FREE
014200141114       IF *in02 = '1';
014201141114      /END-FREE
014300990213     C                   move      *blanks       choice
014400990213     C                   goto      start
014401141114      /FREE
014500141114       endif;
014600141114       //*in03 = End**
014700141114       IF *in03 = '1';
014800141114         *INlr = *ON;
014900141114         return;
015000141114       endif;
015100141114
015200141114       // validate period entered
015300141114       errmsg = validate_prd(prno:'EMT');
015400141114       // if error write message to screen
015500141114       if errmsg <> ' ';
015501141118         show_msg(errmsg);
015600141114         *IN89 = *ON;
015700141114       endif;
015701141114      /END-FREE
015800990213     c                   movel     errmsg        msg              10
015801141114      /FREE
015900141114       if *in40 or *in89;
015901141114      /END-FREE
016000990213     c                   goto      top
016001141114      /FREE
016100141114       endif;
016200141114       // Get period end date
016400141114       get_prd_info('*rng':*blanks:prno:
016500141114           prstrd:prendd);
016600141114
016700141114       //*in01 = account types enquiry
016900141114       IF *in01 = '1';
017000141114         type = lst_ltype();
017001141114      /END-FREE
017100981027     C                   goto      top
017101141114      /FREE
017200141114       endif;
017300141114
017400141119       //process all records (confirmation window)
017600141114       if *in04;
017700141114         exsr all_recs;
017800141114       endif;
017900141114
018000141119       //validate type
018200050514           *in47 = get_ltype_info('RD':type:ematf_struc);
018300050514      /end-free
018400981030     C     *in47         cabeq     '1'           top
018500981028     C
018600141114      //  check period # for zeros
018601141114      /FREE
018700141114       if prno = *zeros;
018800141114         *IN54 = *ON;
018801141114      /END-FREE
018900981027     C                   goto      top
018901141114      /FREE
019000141114       endif;
019100141114
019200141114
019300141114       //  check type for zeros
019400141114       if choice = '1' and type = 0;
019500141114         *IN43 = *ON;
019501141114      /END-FREE
019600981028     c                   goto      top
019601141114      /FREE
019700141114       endif;
019800141114
019900141114       //  check write-off balance for zeros
020000141114       if choice = '2' and wbal = 0;
020100141114         *IN44 = *ON;
020101141114      /END-FREE
020200981028     c                   goto      top
020201141114      /FREE
020300141114       endif;
020500141114
020600141114       exsr win_sr;
020700141114       //
020800141114       // process selected choice
020900141114
020901141114      /END-FREE
021000981103     C     choice        caseq     '1'           cnts_elim
021100981103     C     choice        caseq     '2'           bal_elim
021200981103     C                   endcs
021300111013      /free
021400111013
021500111013         //*****************************
021600111013         // Session Init
021700111013         //*****************************
021800111013
021900111013          begsr *inzsr;
022000111013            get_session_info('EMT':errmsg:@pgm);
022100111013            if errmsg <> *blanks;
022200111013              show_msg(errmsg);
022300111013              *inlr = *on;
022400111013              return;
022500111013            endif;
022600111013
022700111013            open(e) liabm;
022800111013            open(e) liabt;
022900111013
023000111013          endsr;
023100111013      /end-free
023200141114      //**********************************************************************
023300141114      // subroutine to write details to gl file                             **
023400141114      //**********************************************************************
023401141114      /FREE
023500141114       begsr cents_gl;
023501141120
023700141120         if not get_ltype_info('RD':litype:ematf_struc);
023701141120
023900141120            gl_out = emacct;
024000141120            exsr accum_gl_tot;
024100141114
024200141114         if choice = '1';
024300141114           // cents elimination (gl)
024400141114           gl_total = gl_total + (cents * -1);
024500141114           gl_desc = #text1;
024600141114         else;
024700141114           // small balance write-of (gl)
024800141114           gl_total = balance * -1;
024900141114           gl_desc = #text2;
025000141114         endif;
025100141114
025200141114       endif;
025300141114       endsr;
025400141114
025500141114       //**********************************************************************
025600141114       // subroutine to write details to gl file (nett of all transactions)  **
025700141114       //************ *********************************************************
025800141114       begsr nett_gl;
025900141114         gl_out  = esysceacct;
026000141114         exsr accum_gl_tot;
026100141114         gl_total = cents#;
026200141114         gl_desc = #text3 + ' ' + %char(prno);
026300141114
026400141114         //  update JL number
026500141114         //
026600141114         ltnumb = get_next_no('JL':'U');
026700141114
026800020206
026900141114         // call pgm to write to g/l files
027100050519          %occur(gl_accumd) = 1;
027200031001          errmsg = *blanks;
027300031001          write_to_gl(%addr(gl_accumd):xcnt:'EMT':errmsg);
027301141120
027700141114         reset xcnt;
027800141114         clear *all gl_accumd;
027900141114       endsr;
028000141114
028100141114       //**********************************************************************
028200141114       // Cents elimination sub-routine  (by type)                           **
028300141114       //**********************************************************************
028400141114       begsr cnts_elim;
028500141114
028501141114      /END-FREE
028600981030     C                   z-add     *zeros        cents#
028700141114      // checks to determine if all records have been selected to be processed
028701141114      /FREE
028800141114         if not(*in04);
028900141114           chain type lifmt;
028901141114           *IN45 = NOT %FOUND;
029000141114         else;
029100141114           SETLL *LOVAL lifmt;
029200141114           READ lifmt;
029201141114           *IN45 = %EOF;
029300141114         endif;
029400141114
029500141114         dow not(*in45);
029501141114      /END-FREE
029600981029     C                   move      prno          mth
029700981029     C                   z-add     la(mth)       balance
029701141114      /FREE
029703141119
029800141114           if cents <> 0;
029801141119
030000141119           // writing record to liability transaction file
030101141114      /END-FREE
030200981029     C                   move      'X'           ltsts
030300981029     C                   z-add     date@         ltdate
030400981029     C                   movel     #text1        lttext
030500981029     C                   z-add     liemp         ltemp
030600981029     C                   z-add     litype        lttype
030800981029     C
030900981029     C                   z-sub     cents         ltamt
031000981029     C
031100981029     C                   z-add     prno          ltprno
031300141114             ltnumb = get_next_no('JL');
031400020206     C                   move      'SYS'         ltorg
031500981029     C                   z-add     *zeros        ltstyp
031600981029     C                   move      *blanks       ltref
031700981029     C
031701141114      /FREE
031800141114             write ltfmt;
031900141114
032100141120             update_liab(ltnumb:ltorg:'I':ltprno);
032101141120
032300141120      //writes g/ledger records
032400141120        exsr cents_gl;
032401141120
032500141120      //add current total to total balance
032501141114      /END-FREE
032600981030     C                   add       cents         cents#
032601141114      /FREE
032700141114           endif;
032800141114
032900141114           // checks to ensure that all records have not been selected to be processed
033000141114           if not(*in04);
033100141114             reade type lifmt;
033101141114             *IN45 = %EOF;
033200141114           else;
033300141114             READ lifmt;
033301141114             *IN45 = %EOF;
033400141114           endif;
033500141114         enddo;
033600141114
033700141114         //  write nett to gl
033800141114         exsr nett_gl;
033900141114         // If update  did not complete successfully
034000141114         if errmsg <> *blanks;
034001141118            show_msg(errmsg);
034100141120           rolbk;
034200021218
034300141114         else;
034400141114           commit;
034500141114         endif;
034600141114
034700141114         // end of cents elimination sub-routine
034800141114       endsr;
034900141114
035000141114       //****************************************************************************
035100141114       // subroutine to write off small balances                                   **
035200141114       //****************************************************************************
035300141114       begsr bal_elim;
035400141114         chain type lifmt;
035401141114         *IN45 = NOT %FOUND;
035500141114
035501141114      /END-FREE
035600001004     C                   z-add     *zeros        cents#
035700001004     C
035701141114      /FREE
035800141114         dow not(*in45);
035801141114      /END-FREE
035900981102     C                   move      prno          mth
036000981102     C                   z-add     la(mth)       balance
036100981102     C
036101141114      /FREE
036200141114           eval bal1 = %abs(wbal);
036300141114           eval bal2 = %abs(balance);
036400141114
036500141114           // if only debit records are selected
036600141114           IF act = 'D'
036700141114           AND bal2 <= bal1
036800141114           AND balance > *zeros;
036900141114             exsr bal_elim2;
037000141114             // writes g/ledger records
037100141114             exsr cents_gl;
037200141114           endif;
037300141114
037400141114           // if only credit records are selected
037500141114           IF act = 'C'
037600141114           AND bal2 <= bal1
037700141114           AND balance < *zeros;
037800141114             exsr bal_elim2;
037900141114             // writes g/ledger records
038000141114             exsr cents_gl;
038100141114           endif;
038200141114
038300141114           // if both debit and credit records are selected
038400141114           IF act = 'B'
038500141114           AND bal2 <= bal1
038600141114           AND bal2 <> *zeros;
038700141114             exsr bal_elim2;
038800141114             // writes g/ledger records
038900141114             exsr cents_gl;
039000141114           endif;
039100141114
039200141114
039300141114           READE type lifmt;
039301141114           *IN45 = %EOF;
039400141114         enddo;
039500141114
039600141114         //  write nett to gl
039700141114         exsr nett_gl;
039800141114         // If update  did not complete successfully
039900141114         if errmsg <> *blanks;
040000141114           rolbk;
040100021218
040200141114         else;
040300141114           commit;
040400141114         endif;
040500141114
040600141114         // end of small balance write-off sub-routine
040700141114       endsr;
040800141114
040900141114       //***************************************************************************************
041000141114       //  subroutine to write to the liability transaction file for small balance write-off  **
041100141114       //***************************************************************************************
041200141114       begsr bal_elim2;
041300141114         // writing record to liability transaction file
041400141114
041401141114      /END-FREE
041500981103     C                   move      'X'           ltsts
041600981103     C                   z-add     date@         ltdate
041700981103     C                   movel     #text2        lttext
041800981103     C                   z-add     liemp         ltemp
041900981103     C                   z-add     litype        lttype
042000981103     C
042100981103     C
042200981103     C                   z-sub     balance       ltamt
042300141114      // add current total to total balance
042400990309     C                   add       balance       cents#
042500981103     C
042600981103     C                   z-add     prno          ltprno
042700141114      //
042701141114      /FREE
042800141114         ltnumb = get_next_no('JL');
042801141114      /END-FREE
042900020206     C                   move      'SYS'         ltorg
043000981103     C                   z-add     *zeros        ltstyp
043100981103     C                   move      *blanks       ltref
043200981103     C
043201141114      /FREE
043300141114         write ltfmt;
043400141114
043600141120         update_liab(ltnumb:ltorg:'I':ltprno);
043700031001      /end-free
043800981103     C
043801141114      /FREE
043900141114       endsr;
044000141114
044100141114       //***************************************
044200141114       //  subroutine to process all records  **
044300141114       //***************************************
044400141114       begsr all_recs;
044500141114         if prno = *zeros;
044600141114           *IN54 = *ON;
044601141114      /END-FREE
044700981103     C                   goto      top
044701141114      /FREE
044800141114         endif;
044900141114
044901141114      /END-FREE
045000981103     C                   move      'N'           ans
045001141114      /FREE
045100141114         exfmt fmt3;
045200141114
045300141114         if ans = 'Y';
045400141114           exsr cnts_elim;
045401141114      /END-FREE
045500981103     C                   goto      start
045501141114      /FREE
045600141114         else;
045601141114      /END-FREE
045700981103     C                   goto      top
045701141114      /FREE
045800141114         endif;
045900141114       endsr;
046000141114
046100141114
046200141114       //***************************************
046300141114       // confirm subroutine                  **
046400141114       //***************************************
046500141114       begsr win_sr;
046600141114         exfmt winrec;
046700141114         //enter YES to confirm
046800141114         IF answer <> 'YES';
046801141114      /END-FREE
046900980216     C                   GOTO      START
046901141114      /FREE
047000141114         endif;
047100141114       endsr;
047200141114       //*********************************************************************
047300141114       // Search for gl.code in array . if found set occurance level and add
047400141114       // total ,if not found add new array record and Data Struct. total
047500141114       //*********************************************************************
047600141114       BEGSR accum_gl_tot;
047700141114         //
047701141114      /END-FREE
047800020712     C                   z-add     1             ycnt
047900020712     C     gl_out        lookup    glarr(ycnt)                            12
047901141114      /FREE
048000141114         if not(*in12);
048001141114      /END-FREE
048100020712     C                   add       1             xcnt
048200020712     C                   move      gl_out        glarr(xcnt)
048300020712     C     xcnt          occur     gl_accumd
048301141114      /FREE
048400141114           gl_account = gl_out;
048500141114           gl_prd = prno;
048600141114           //
048700141114         else;
048800141114           //
048801141114      /END-FREE
048900020712     C     ycnt          occur     gl_accumd
048901141114      /FREE
049000141114         endif;
049100141114         //
049200020206
049300141114       ENDSR;
049400141114       //***************************************************************
049500141114       // Begin subroutine to print header                            **
049600141114       //***************************************************************
049700141114       BEGSR AAINIT;
049800141114         coname = get_coname;
049900141114         @date = get_sys_date(date@);
050000141114         @title = arr(1);
050100141114
050300050524             get_prd_info('*prd':*blanks:prno:date@);
050500141114       ENDSR;
050600141114       //***************************************************************
050700141114       //
050701141114      /END-FREE
050800981027**   Header to be displayed
050900981027           Cents Elimination
051000981027
