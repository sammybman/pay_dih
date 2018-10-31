000100141114      // Liability detail master file
000200091121     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111013     H/define profoundui
000400111013     Fliabt     if a E           K Disk    commit  usropn
000500111013     Fjrnml01   uF   E           K Disk    commit  usropn
000600020206     Femp006d   CF   E             WORKSTN
000700111013     F                                     sfile(sflrec:rrn)
000800111013     F                                     sfile(sflrec2:rrn2)
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200160712
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,glpr
001500111013      /copy QCPYSRC,apppr
001600111013      /copy QCPYSRC,paypr
001700111013      /copy QCPYSRC,autpr
001800111013      /copy QCPYSRC,emppr
001900160712
002000980106     D                sds
002100980106     D  @pgm             *proc
002200111013     D  @user                358    367
002300160712
002400040131     D errmsg          s             75
002500021014     D #date           s              8  0
002600021014     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002700010404     D xcnt            s              4  0 inz(0)                               TOTAL AMOUNT
002800020204     D glname#         s             75                                         TOTAL AMOUNT
002900020204     D name#           s             40                                         TOTAL AMOUNT
003000020204     D prnumb          s              6  0                                      TOTAL AMOUNT
003100021014     D rrn             s              4  0                                      TOTAL AMOUNT
003200021014     D rrn2            s              4  0                                      TOTAL AMOUNT
003300020204     D gl_out          s             25
003400141114      //
003500010404     Dgl_accumd        ds                  occurs(500) inz
003600010404     D gl_prd                         6  0
003700010404     D gl_account                    25
003800010404     D gl_desc                       30
003900010404     D gl_total                      13  2
004000030703     D gl_refno                      15
004100141114
004200141114      //   CHECK USER AUTHORITY
004300111013         // authority check
004400141127         monitor;
004500141127           authority_check('EMT':@pgm);
004600141127         on-error;
004700141127           show_msg(get_err_msg('DIH0054'));
004800141127           *inlr = *on;
004900141127           return;
005000141127         endmon;
005100111013
005200990215     C
005300980218     C     top           tag
005400160712
005500141114      //** get sys date
005600160712
005700141114       EXSR AAINIT;
005800141114
005900141114       *IN33 = *ON;
006000980218     c                   z-add     0             rrn
006100141114       write sflctl;
006200141114       *IN33 = *OFF;
006300160712
006400141114       exsr first;
006500141114
006600141114       if rrn = 0;
006700141114         *inlr = *on;
006800160712           show_msg(get_err_msg('DIH0125'));
006900160712           return;
007000141114       endif;
007100160712
007200141114       write footer;
007300141114       exfmt sflctl;
007400160712
007500141114       if *in03;
007600141114         *inlr = *on;
007700141114         return;
007800141114       endif;
007900160712
008000141114       readc sflrec;
008100141114       *IN57 = %EOF;
008200141114       dow not(*in57);
008300160712
008400980218     c     sel#          caseq     'A'           auth_sr
008500980220     c     sel#          caseq     'C'           canc_sr
008600990226     c     sel#          caseq     'X'           view_sr
008700980218     c                   endcs
008800160712
008900141114         readc sflrec;
009000141114         *IN57 = %EOF;
009100141114       enddo;
009200111013
009300111013         //*****************************
009400111013         // Session Init
009500111013         //*****************************
009600111013
009700111013          begsr *inzsr;
009800111013            get_session_info('EMT':errmsg:@pgm);
009900111013            if errmsg <> *blanks;
010000111013              show_msg(errmsg);
010100111013              *inlr = *on;
010200111013              return;
010300111013            endif;
010400111013
010500111013            open(e) jrnml01;
010600111013            open(e) liabt;
010700111013
010800111013          endsr;
010900990226
011000141114      //****************************************************************
011100141114
011200141114       begsr view_sr;
011300141114
011400141114         //
011500141114         *in33 = *on;
011600141114         rrn2 = *zeros;
011700141114         write sflctl2;
011800141114         *in33 = *off;
011900141114         //
012000141114         chain jrnumb# jrfmt;
012100141114         *IN56 = NOT %FOUND;
012200141114         dow not(*in56);
012300141114      /END-FREE
012400021014     C                   move      *blanks       emp_name
012500021014     C                   move      *blanks       typename
012600141114      //
012700141114      /FREE
012800141114           if jrtype <> *zeros;
012900141114
013000050514           get_ltype_info('RD':jrtype:ematf_struc);
013100050514      /end-free
013200980203     C
013300141114      /FREE
013400141114             typename = emdsp;
013500141114
013600141114             if emsubtyp = 'Y';
013700141114               //
013800050514           get_lstype_info('RD':jrtype:jrstyp:eastf_struc);
013900141114
014000141114               typename = %trim(typename) + '-' + eadsp;
014100141114
014200141114             endif;
014300141114
014400141114             //* get employee name ***
014500141114             get_emp_info(jremp:*omit:*blanks:name#);
014600141114
014700141114             emp_name = name#;
014800990301
014900141114           endif;
015000141114           //* When account number is entered (DEBIT) ***
015100141114           //  GET GL. Description
015200031001           get_gl_info(jracct:glname#:'D');
015300141114
015400141114           glname = glname#;
015500021014
015600141114           rrn2 = rrn2 + 1;
015700141114           *in99 = not(*in99);
015800141114           write sflrec2;
015900141114           //
016000141114           reade jrnumb# jrfmt;
016100141114           *IN56 = %EOF;
016200141114         enddo;
016300141114
016400141114         //
016500141114         write footer2;
016600141114         exfmt sflctl2;
016700021014
016800141114      /END-FREE
016900990301     c     *in02         cabeq     '1'           top
017000990301     c     *in15         caseq     '1'           auth_sr
017100990301     c                   endcs
017200141114      //* Program termination ***
017300141114      /FREE
017400141114         if *in03;
017500141114           *inlr = *on;
017600141114           return;
017700141114         endif;
017800990301
017900141114       endsr;
018000141114
018100141114       //****************************************************************
018200141114       begsr auth_sr;
018300141114         //
018400141114         chain jrnumb# jrfmt;
018500141114         *IN56 = NOT %FOUND;
018600141114         dow not(*in56);
018700141114           //
018800141114           // First Signature
018900141114           //*                 if        jrausr1 = *blanks
019000141114           //*                 dow       not(*in56)
019100141114           //*                 move      @user         jrausr1
019200141114           //**                update    jrfmt
019300141114           //**  jrnumb#       reade     jrfmt                                  56
019400141114           //**                enddo
019500141114           //**                commit
019600141114           //**                leavesr
019700141114           //**                endif
019800141114           //
019900141114           // Check transaction period to ensure transaction is processed
020000141114           // to a valid period.
020100141114           change_prd(jrprno:'EMT');
020200141114           //
020300141114           ltsts = 'X';
020400141114           //
020500141114           gl_out = jracct;
020600141114           ltdate = #date;
020700141114           lttext = jrtext;
020800141114           ltemp = jremp;
020900141114           lttype = jrtype;
021000141114           ltstyp = jrstyp;
021100141114           ltamt = jramt;
021200141114           ltprno = jrprno;
021300141114           ltnumb = jrnumb;
021400141114           ltorg = 'EMT';
021500141114
021600141114           prnumb = jrprno;
021700141114           //
021800141114           exsr accum_gl_tot;
021900141114
022000141114           gl_refno = %editc(jrnumb:'4');
022100141114           gl_desc = jrtext;
022200141114           gl_total = jramt;
022300141114           //
022400990407
022500141114           if lttype <> *zeros;
022600141114             write ltfmt;
022700141114             //
022800050127             update_liab(ltnumb:ltorg:'I':ltprno);
022900141114           endif;
023000141114           //
023100141114           jrsts = 'A';
023200141114           jrausr2 = @user;
023300141114           jra1date = #date;
023400141114           //
023500141114           update jrfmt;
023600141114           reade jrnumb# jrfmt;
023700141114           *IN56 = %EOF;
023800141114         enddo;
023900141114         //
024000141114         // call pgm to write to g/l files
024100031001          errmsg = *blanks;
024200040131          %occur(gl_accumd) = 1;
024300031001          write_to_gl(%addr(gl_accumd):xcnt:'EMT':errmsg);
024400031001      /end-free
024500141114      //
024600141114      // If update  did not complete successfully
024700141114      /FREE
024800141114         if errmsg <> *blanks;
024900141114
025000141114           show_msg(errmsg);
025100141114           rolbk;
025200141114
025300141114         else;
025400141114
025500141114           reset xcnt;
025600141114           //
025700141114           commit;
025800141114         endif;
025900141114       endsr;
026000141114       //****************************************************************
026100141114       //* Cancelling Subroutine
026200141114       //****************************************************************
026300141114       begsr canc_sr;
026400141114         setll jrnumb# jrfmt;
026500141114         reade jrnumb# jrfmt;
026600141114         dow not(%eof);
026700141114           jrsts = 'C';
026800141114           update jrfmt;
026900141114           reade jrnumb# jrfmt;
027000141114         enddo;
027100141114         commit;
027200141114       endsr;
027300141114       //****************************************************************
027400141114       begsr First;
027500021014
027600141114         setll *loval jrfmt;
027700141114         read jrfmt;
027800141114
027900141114         *IN55 = %EOF;
028000141114         //
028100141114         dow not(*in55);
028200020718
028300141114      /END-FREE
028400021014     c                   move      ' '           sel#
028500980218     c                   z-add     jramt         jramt#
028600980218     c                   movel(p)  jrtext        jrtext#
028700021014     c                   z-add     jrnumb        jrnumb#
028800141114      //
028900141114      /FREE
029000141114           if jrausr1 <> @user;
029100141114             //
029200141114             if jrausr1 <> *blanks;
029300141114               *in44 =  *on;
029400141114             else;
029500141114               *in44 =  *off;
029600141114             endif;
029700141114             //
029800141114             rrn = rrn + 1;
029900141114             write sflrec;
030000141114           endif;
030100021014
030200141114           setgt jrnumb# jrfmt;
030300141114           *IN55 = NOT %FOUND;
030400141114           read jrfmt;
030500141114           *IN55 = %EOF;
030600141114         enddo;
030700141114       endsr;
030800141114       //*********************************************************************
030900141114       // Search for gl.code in array . if found set occurance level and add
031000141114       // total ,if not found add new array record and Data Struct. total
031100141114       //*********************************************************************
031200141114       BEGSR accum_gl_tot;
031300141114         //
031400141114      /END-FREE
031500010404     C                   add       1             xcnt
031600010404     C     xcnt          occur     gl_accumd
031700141114      /FREE
031800141114         gl_account = gl_out;
031900141114         gl_prd = prnumb;
032000141114         //
032100010404
032200141114       ENDSR;
032300141114       //****************************************************************
032400141114       //Begain subroutine AAINIT to print header
032500141114       //****************************************************************
032600141114       BEGSR AAINIT;
032700141114         CONAME = get_coname;
032800141114         @date = get_sys_date(#date);
032900141114         @title = arr(1);
033000141114       ENDSR;
033100141114       //**********************************************
033200141114       //
033300141114      /END-FREE
033400980218**
033500990217  Authorise Employee Transactions
