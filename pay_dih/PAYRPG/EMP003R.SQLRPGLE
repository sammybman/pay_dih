000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000200111013     H/define profoundui
000300141112      // Liability detail master file
000400111013     Fjrnml01   iF A E           K Disk    commit usropn
000500020206     Femp003d   CF   E             WORKSTN
000600111013     F/IF DEFINED(WEB)
000700111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111013     F/ENDIF
000900141112      // General/Payroll PROTOTYPES
001000111013      /copy QCPYSRC,PAYPR
001100111013      /copy QCPYSRC,genPR
001200111013      /copy QCPYSRC,glPR
001300111013      /copy QCPYSRC,autpr
001400111013      /copy QCPYSRC,emppr
001500021005
001600980106     D                sds
001700980106     D  @pgm             *proc
001800980106     D  @n_prm           *parms
001900111013     D  @user                358    367
002000141112      //
002100020204     D arr             s             40    dim(2) ctdata perrcd(1)
002200020204     D date@           s              8  0
002300020208     D name#           s             40
002400040224     D dept#1          s              3s 0
002500040224     D dept#2          s              3s 0
002600020208     D err#            s              7
002700020208     D prno            s              6  0
002800050514     D type#1          s              2  0
002900050514     D type#2          s              3  0
003000141112      //
003100040207      /free
003200141120         //authority check
003300141120         monitor;
003400141120           authority_check('EMT':@pgm);
003500141120         on-error;
003600141120           show_msg(get_err_msg('DIH0054'));
003700141120           *inlr = *on;
003800141120           return;
003900141120         endmon;
004000111013
004100040207      /end-free
004200990318
004300141112      //
004400020208     C     sub_key1      klist
004500020208     C                   Kfld                    lttype#1
004600020208     C                   Kfld                    easubtyp#1
004700020208     C
004800020208     C     sub_key2      klist
004900020208     C                   Kfld                    lttype#2
005000020208     C                   Kfld                    easubtyp#2
005100981021     C
005200981023     C
005300981023     C
005301141112      /FREE
005400141112       exsr aainit;
005500141112       //********************
005600141112       // OPtion selection **
005700141112       //********************
005800141112       dow 0 = 0;
005801141112
005900141112         exfmt fmt0;
006000020208
006100141112         // End program (F3)
006200141112         if *in03;
006300141112           *inlr = *on;
006400141112           return;
006500141112         endif;
006501141112
006700141112         if  *in07;
006800141112            maint_EmpCB_Rec();
006900141112             *inlr = *on;
007000141112            return;
007100141112         endif;
007300020208
007400141112      // Process Selection
007500141112         select;
007600141112           //
007700141112         when opt = 1;
007800141112           exsr single_trans;
007900141112           //
008000141112         when opt = 2;
008001141112      /END-FREE
008100111013     C                   call      'EMP003R01'
008101141112      /FREE
008200141112         endsl;
008300020208
008400141112       enddo;
008600111013
008700111013         //*****************************
008800111013         // Session Init
008900111013         //*****************************
009000111013
009100111013          begsr *inzsr;
009200111013            get_session_info('EMT':errmsg:@pgm);
009300111013            if errmsg <> *blanks;
009400111013              show_msg(errmsg);
009500111013              *inlr = *on;
009600111013              return;
009700111013            endif;
009800111013
009900111013            open(e) jrnml01;
010000111013
010100111013          endsr;
010200111013
010500141112       begsr single_trans;
010600141112
010700141112         //
010800141112         // GET NEXT EMPLOYEE TRANSACTION NUMBER FROM CONTROL FILE
010900141112         //
011000141112         ltnumb#1 = get_next_no('ET');
011100141112         //
011101141112      /END-FREE
011200980131     C                   move      *blanks       emsubtyp
011300981022     C
011400141112      // set period number to current period number
011500141112      //
011600020208     C                   z-add     prno          prdnumb#
011700141112      //**********************************
011800141112      // Single transaction Processing  **
011900141112      //**********************************
011901141112      /FREE
012000141112         dow 0 = 0;
012100141112           exfmt fmt1;
012101141112      /END-FREE
012200990213     c                   move      *blanks       errmsg
012300981020     C
012301141112      /FREE
012400141112           *IN20 = *OFF;
012401141112           *IN33 = *OFF;
012402141112           *IN34 = *OFF;
012500141112           *IN35 = *OFF;
012501141112           *IN36 = *OFF;
012502141112           *IN37 = *OFF;
012600141112           *IN39 = *OFF;
012601141112           *IN77 = *OFF;
012602141112           *IN81 = *OFF;
012700141112
012800141112           // setoff from 41-55
012900141112
012901141112      /END-FREE
013000981020     C                   z-add     41            y                 2 0
013001141112      /FREE
013100141112           dow y <> 56;
013101141112      /END-FREE
013200981020     C                   move      '0'           *in(y)
013300981020     C                   add       1             y
013301141112      /FREE
013400141112           enddo;
013500141112           //
013600141112           // Display the first screen format selecting & validating period
013700141112           // F3 - Exit
013800141112           //
013900141112           // validate prno - allow for 1 prno back & 1 forward
014000141112           errmsg = validate_prd(prdnumb#);
014100141112           // if error write message to screen
014200141112           if errmsg <> ' ';
014300141112             *IN56 = *ON;
014301141114             show_msg(errmsg);
014400141112             iter;
014500141112           endif;
014600141112
014700141112           // cancel opt.
014800141112           if *in02;
014900141112             leave;
015000141112           endif;
015001141112
015100141112           // Program termination **
015200141112           if *in03;
015300141112             *inlr = *on;
015400141112             return;
015500141112           endif;
015501141112
015600141112           // Type enquiry
015700141112           IF *in01 = '1';
015800141112             type#1 = lst_ltype();
015900141112
016000141112             if lttype#1  = 0;
016001141112      /END-FREE
016100981021     C                   z-add     type#1        lttype#1
016101141112      /FREE
016200141112             else;
016201141112      /END-FREE
016300981021     C                   z-add     type#1        lttype#2
016301141112      /FREE
016400141112             endif;
016500141112
016600141112             iter;
016700141112           endif;
016800141112
016900141112           // Sub_Type enquiry
017000141112           if *in05;
017100141112             type#2 = lst_lstype(type#1);
017200141112
017300141112             if easubtyp#1 = 0;
017301141112      /END-FREE
017400981021     C                   z-add     type#2        easubtyp#1
017401141112      /FREE
017500141112             else;
017501141112      /END-FREE
017600981021     C                   z-add     type#2        easubtyp#2
017601141112      /FREE
017700141112             endif;
017800141112
017900141112             iter;
018000141112           endif;
018100141112
018200141112           //* Automatically moving employee number  ***
018300141112           IF ltemp#2 = *zeros
018400141112           AND lttype#2 <> *zeros;
018401141112      /END-FREE
018500980206     C                   z-add     ltemp#1       ltemp#2
018501141112      /FREE
018600141112           endif;
018700141112
018800141112           // check to ensure that both account fields are not used
018900141112           if dacct1 <> *blanks and cacct1 <> *blanks;
019000141112             *IN81 = *ON;
019100141112             iter;
019200141112           endif;
019300141112
019400141112           //* Validate period number ***
019500141112           IF prdnumb# = *zeros;
019600141112             *IN42 = *ON;
019700141112             iter;
019800141112           Else;
019900981023
020000141112             // Use Period Validation pgm to validate period
020100981023
020200141112             errmsg = validate_prd(prdnumb#);
020300020204
020400141112             if errmsg <> ' ';
020500141112               *IN54 = *ON;
020501141114               show_msg(errmsg);
020600141112               iter;
020700141112
020800141112             endif;
020900141112           endif;
021000141112
021100141112           //* When amount field is blank ***
021200141112           IF ltamt# = *zeros;
021300141112             *IN33 = *ON;
021400141112             iter;
021500141112           endif;
021600141112
021700141112           //* When particulars field is blank ***
021800141112           IF lttext# = *blanks;
021900141112             *IN34 = *ON;
022000141112             iter;
022100141112           endif;
022200141112
022300141112           //* When transaction type, account and employee number fileds are - ***
022400141112           //* Blank (DEBIT) ***
022500141112           IF ltemp#1 = *zeros
022600141112           AND lttype#1 = *zeros
022700141112           AND dacct1 = *blanks;
022800141112             *IN35 = *ON;
022900141112             iter;
023000141112           endif;
023100141112
023200141112           //* When transaction type, account and employee number fileds are - ***
023300141112           //* Blank (CREDIT) ***
023400141112           IF ltemp#2 = *zeros
023500141112           AND lttype#2 = *zeros
023600141112           AND cacct1 = *blanks;
023700141112             *IN36 = *ON;
023800141112             iter;
023900141112           endif;
024000141112
024100141112
024200141112           //* When an invalid combination is entered (DEBIT) ***
024300141112           IF ltemp#1 <> *zeros
024400141112           AND dacct1 <> *blanks
024500141112           OR lttype#1 <> *zeros
024600141112           AND dacct1 <> *blanks
024700141112           OR easubtyp#1 <> *zeros
024800141112           AND dacct1 <> *blanks;
024900141112             *IN44 = *ON;
025000141112             iter;
025100141112           endif;
025200141112
025300141112           //* When an invalid combination is entered (CREDIT) ***
025400141112           IF ltemp#2 <> *zeros
025500141112           AND cacct1 <> *blanks
025600141112           OR lttype#2 <> *zeros
025700141112           AND cacct1 <> *blanks
025800141112           OR easubtyp#2 <> *zeros
025900141112           AND cacct1 <> *blanks;
026000141112             *IN45 = *ON;
026100141112             iter;
026200141112           endif;
026300141112
026400141112           //* Check for duplication on data entered ***
026500141112           IF ltemp#1 <> *zeros
026600141112           AND ltemp#2 <> *zeros;
026700141112
026800141112             IF ltemp#1 = ltemp#2
026900141112             AND lttype#1 = lttype#2
027000141112             AND easubtyp#1 = easubtyp#2;
027100141112               *IN51 = *ON;
027200141112               iter;
027300141112
027400141112             endif;
027500141112           endif;
027600141112
027700141112           IF dacct1 <> *blanks
027800141112           AND cacct1 <> *blanks;
027900141112
028000141112             IF dacct1 = cacct1;
028100141112               *IN52 = *ON;
028200141112               iter;
028300141112             endif;
028400141112           endif;
028500141112
028600141112           //* Verify that employee num is valid ***
028700141112           if ltemp#1 <> *zeros;
028800141112             // retrieve employee name
028900141112             status = get_emp_info(ltemp#1:dept#1:
029000141112                 *blanks:name#:*omit:*omit:*omit:err#);
029100141112             emp_name1 = name#;
029200141112             if err# <> *blanks;
029300141112               *IN47 = *ON;
029400141112               iter;
029500141112             endif;
029600141112
029700141212             //* When Transaction type is entered (DEBIT) ***
029800141112             if lttype#1 <> *zeros;
029900141112
030100050514           get_ltype_info('RD':lttype#1:ematf_struc);
030101141112
030300141112               if emchgtyp <> *blanks and
030400141112                     %scan(status:emchgtyp) = *zeros;
030500141112                 *IN47 = *ON;
030600141112                 iter;
030700141112               endif;
030800141112
030900141112               select;
031000141112               when emsubtyp = 'Y';
031100141112                 if easubtyp#1 = *zeros;
031200141112                   *IN30 = *ON;
031300141112                   iter;
031400141112                 Else;
031500141112                   *IN30 = *OFF;
031501141112                   *IN31 = *OFF;
031600980131     C                   move      *blanks       dbname
031700980130     C                   cat(p)    emdsp:0       dbname
031800980130     C
032000050514            get_lstype_info('RD':lttype#1:easubtyp#1:eastf_struc);
032200980130     C                   cat(p)    '-':0         dbname
032300980130     C                   cat(p)    eadsp:0       dbname
032400141112                 endif;
032500141112
032600141112               other;
032700141112                 if easubtyp#1 <> *zeros;
032800141112                   *IN31 = *ON;
032900141112                 endif;
033000141112               endsl;
033100141112             Else;
033200141112
033300141112               *IN43 = *ON;
033400141112               iter;
033500141112             endif;
033600141112
033700141112           Else;
033800141112
033900141112             if lttype#1 <> *zeros;
034000141112               *IN43 = *ON;
034100141112               iter;
034200141112             endif;
034300141112           endif;
034400141112
034500141112           //* Verify that employee num is valid ***
034600141112           if ltemp#2 <> *zeros;
034700141112             // retrieve employee name
034800141112             status = get_emp_info(ltemp#2:dept#2:
034900141112                 *blanks:name#:*omit:*omit:*omit:err#);
035000141112             emp_name2 = name#;
035100141112             if err# <> *blanks;
035200141112               *IN47 = *ON;
035300141112               iter;
035400141112             endif;
035500141112
035600141112             //* When Transaction type is entered  (CREDIT) ***
035700141112             if lttype#2 <> *zeros;
035800141112
036000050514           get_ltype_info('RD':lttype#2:ematf_struc);
036001141212
036200141112               if emchgtyp <> *blanks and
036300141112                     %scan(status:emchgtyp) = *zeros;
036400141112                 *IN49 = *ON;
036500141112                 iter;
036600141112               endif;
036700141112
036800141112               select;
036900141112               when emsubtyp = 'Y';
037000141112                 if easubtyp#2 = *zeros;
037100141112                   *IN38 = *ON;
037200141112                   iter;
037300141112                 Else;
037400141112                   *IN38 = *OFF;
037401141112                   *IN40 = *OFF;
037500980203     C                   move      *blanks       crname
037600980203     C                   cat(p)    emdsp:0       crname
037700980203     C
037900050514            get_lstype_info('RD':lttype#2:easubtyp#2:eastf_struc);
038100980203     C                   cat(p)    '-':0         crname
038200980203     C                   cat(p)    eadsp:0       crname
038300141112                 endif;
038400141112
038500141112               other;
038600141112                 if easubtyp#2 <> *zeros;
038700141112                   *IN40 = *ON;
038800141112                 endif;
038900141112               endsl;
039000141112             Else;
039100141112
039200141112               *IN46 = *ON;
039300141112               iter;
039400141112             endif;
039500141112
039600141112           Else;
039700141112             if lttype#2 <> *zeros;
039800141112               *IN46 = *ON;
039900141112               iter;
040000141112             endif;
040100141112           endif;
040200141112
040300141112
040400141112           //* When account number is entered (DEBIT) ***
040500141112           IF lttype#1 = *zeros
040600141112           AND ltemp#1 = *zeros
040700141112           AND dacct1 <> *blanks;
040800141112
040900141112             //  validate gl.
041100031001         errmsg = *blanks;
041200031001         get_gl_info(dacct1:errmsg:'V');
041201141114
041400141112             if errmsg <> *blanks;
041401141114               show_msg(errmsg);
041500141112               *IN77 = *ON;
041600141112               *IN32 = *ON;
041700141112               iter;
041800141112             else;
041900141112               *IN77 = *OFF;
042000141112               *IN32 = *OFF;
042100141112             endif;
042200141112             //
042300980131     C                   move      *blanks       dbname
042400141112           endif;
042500141112
042600141112           //* When account number is entered (CREDIT) ***
042700141112           IF lttype#2 = *zeros
042800141112           AND easubtyp#2 = *zeros
042900141112           AND cacct1 <> *blanks;
043000141112
043100981023     C                   move      *blanks       emp_name2
043200141112      //  validate gl.
043400031001         errmsg = *blanks;
043500031001         get_gl_info(cacct1:errmsg:'V');
043501141114
043800141112             if errmsg <> *blanks;
043801141114               show_msg(errmsg);
043900141112               *IN77 = *ON;
044000141112               *IN41 = *ON;
044100141112               iter;
044200141112             else;
044300141112               *IN77 = *OFF;
044400141112               *IN41 = *OFF;
044500141112             endif;
044600141112             //
044601141112      /END-FREE
044700980131     C                   move      *blanks       crname
044701141112      /FREE
044800141112           endif;
044900141112
045000141112           // Confirm
045100141112           if *in15;
045200141112             exsr confirm;
045300141112           endif;
045400141112         enddo;
045500141112       endsr;
045600141112
045700141112       //************************
045800141112       // Confirm transactions **
045900141112       //************************
046000141112       begsr confirm;
046100141112         //
046200141112
046300141112         // GET NEXT EMPLOYEE TRANSACTION NUMBER FROM CONTROL FILE
046500141112         ltnumb#1 = get_next_no('ET':'U');
046600141112         //
046700020208     C                   move      'O'           jrsts
046800141112         if lttype#1 > 0;
046801141212
047000050514           get_ltype_info('RD':lttype#1:ematf_struc);
047100050514           jracct = emacct;
047200050514           //Get correct gl
047300050514           jracct =  crossref_gl(ltemp#1:dept#1:lttype#1:
047400050514                                           easubtyp#1:jracct);
047600141112         else;
047700141112           jracct = dacct1;
047800141112         endif;
047900141112         //
047901141112      /END-FREE
048000020208     C                   z-add     date@         jrdate
048100020208     C                   move(p)   lttext#       jrtext
048200020208     C                   move(p)   lttext#2      jrtext2
048300020208     C                   z-add     ltemp#1       jremp
048400020208     C                   z-add     lttype#1      jrtype
048500020208     C                   z-add     easubtyp#1    jrstyp
048600020208     C                   z-add     ltamt#        jramt
048700020208     C                   z-add     prdnumb#      jrprno
048800020208     C                   z-add     ltnumb#1      jrnumb
048900020208     C                   move      @user         jrusr
049000020208     C                   move      '1'           jrtag
049001141112      /FREE
049100141112         write jrfmt;
049101141112      /END-FREE
049200020208     C                   move      'JRN'         ltorg             3
049300141112      //
049400020208     C                   move      'O'           jrsts
049500141112      //
049501141112      /FREE
049600141112         if lttype#2 > 0;
049800050514           get_ltype_info('RD':lttype#2:ematf_struc);
049900050514           jracct = emacct;
050000050514           //Get correct gl
050100050514           jracct =  crossref_gl(ltemp#2:dept#2:lttype#2:
050200050514                                           easubtyp#2:jracct);
050400141112         else;
050500141112           jracct = cacct1;
050600141112         endif;
050700141112         //
050701141112      /END-FREE
050800020208     C                   z-add     date@         jrdate
050900020208     C                   move(p)   lttext#       jrtext
051000020208     C                   move(p)   lttext#2      jrtext2
051100020208     C                   z-add     ltemp#2       jremp
051200020208     C                   z-add     lttype#2      jrtype
051300020208     C                   z-add     easubtyp#2    jrstyp
051400020208     C                   z-sub     ltamt#        jramt
051500020208     C                   z-add     prdnumb#      jrprno
051600020208     C                   z-add     ltnumb#1      jrnumb
051700020208     C                   move      @user         jrusr
051800020208     C                   move      '1'           jrtag
051801141112      /FREE
051900141112         write jrfmt;
052000141112
052100141112         commit;
052200050706
052400050706          ltamt# = *zeros;
052500050706          ltnumb#1 = *zeros;
052600050706          lttype#1 = *zeros;
052700050706          lttype#2 = *zeros;
052800050706          ltemp#1 = *zeros;
052900050706          ltemp#2 = *zeros;
053000050706          lttext# = *blanks;
053100050706          lttext#2 = *blanks;
053200050706          dbname = *blanks;
053300050706          crname = *blanks;
053400050706          easubtyp#1 = *zeros;
053500050706          easubtyp#2 = *zeros;
053600050706          emp_name1 = *blanks;
053700050706          emp_name2 = *blanks;
053800050706          dacct1 = *blanks;
053900050706          cacct1 = *blanks;
054200141112         prdnumb# = jrprno;
054300141112         //
054400141112       endsr;
054500141112       //*********************************************************
054600141112       BEGSR AAINIT;
054700141112         coname = get_coname;
054800141112         @title = arr(1);
054900141112         @date = get_sys_date(date@);
055000141112         get_prd_info('*prd':*blanks:prno
055100141112             :date@);
055200141112         //
055300141112       ENDSR;
055400141112
055500141112       //
055501141112      /END-FREE
055600981021**   Header to be displayed
055700020208   Employees Transaction Maintenance
055800981021     **
