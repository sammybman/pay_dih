000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200141112      // ********************************************************
000300141112      //              B A N K S    D I H    L T D.              *
000400141112      //                 EMPLOYEE CREDIT CONTROL                *
000500141112      //                                                        *
000600141112      //                         BY                             *
000700141112      //                                                        *
000800141112      //             Banks DIH I.S. department                  *
000900141112      //             P.O BOX 10194, GEORGETOWN,GUYANA           *
001000141112      //                                                        *
001100141112      //                                                        *
001200141112      //       RPG PROGRAM NAME..: EMP001R
001300141112      //                                                        *
001400141112      //       PROGRAMMER NAME: WINSTON ROBINSON                *
001500141112      //       DESIGN DATE....: 1998/10/16                      *
001600141112      //       MODULES........: *NONE                           *
001700141112      //                                                        *
001800141112      //       INDICATORS.....:                                 *
001900141112      //                                                        *
002000141112      //  THIS PROGRAM IS DESIGNED TO MAINTAIN THE CUSTOMER     *
002100141112      //  REGION AND DISTRICT MASTER FILES                      *
002200141112      //                                                        *
002300141112      //*********************************************************
002400951017     H DATEDIT(*YMD)
002500111013     H/define profoundui
002600141112      //
002700111013     Fliabm     uf   e           K Disk    usropn
002800020206     Femp001d   cf   e             workstn
002900111013     F/IF DEFINED(WEB)
003000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003100111013     F/ENDIF
003200141112      // General/Payroll PROTOTYPES
003300111013      /copy QCPYSRC,genPR
003400111013      /copy QCPYSRC,PAYPR
003500111013      /copy QCPYSRC,autPR
003600111013      /copy QCPYSRC,empPR
003700141112      //
003800981016     D
003900981016     D ARR             S             40    DIM(2) CTDATA PERRCD(1)
004000020206     D name#           S             40
004100020206     D err#            S              7
004200111013     D errmsg          S             75
004300141112      //
004400941215     D                SDS
004500941215     D  @PGM             *PROC
004600111013     D  @USER                358    367
004700981016     D
004800141112      //
004900040207      /free
005000111013
005100111013         // authority check
005200141127         monitor;
005300141127           authority_check('EMT':@pgm);
005400141127         on-error;
005500141127           show_msg(get_err_msg('DIH0054'));
005600141127           *inlr = *on;
005700141127           return;
005800141127         endmon;
005900111013
006000040207      /end-free
006100141112      //
006200981016     C     rec_key       klist
006300981016     C                   kfld                    litype#
006400981016     C                   kfld                    listyp#
006500981016     C                   kfld                    liemp#
006600981016     C
006700981016     C     rec_key2      klist
006800981016     C                   kfld                    litype#
006900981016     C                   kfld                    listyp#
007000141112      //
007001141112      /FREE
007100141112       exsr aainit;
007200141112       //************************
007300141112       // display first screen
007400141112       //************************
007500141112       dow 0 = 0;
007501141112
007600141112         exfmt fmt1;
007700141112         *IN25 = *OFF;
007701141112         *IN28 = *OFF;
007702141112         *IN29 = *OFF;
007800141112         *IN27 = *OFF;
007900141112
008000141112         //end program
008100141112         IF *in03 = '1';
008200141112           *INlr = *ON;
008300141112           return;
008400141112         endif;
008500141112
008600141112         if *in01;
008700141112           listyp# = lst_lstype(litype#);
008800141112           iter;
008900141112         endif;
009000141112
009100141112         if *in04;
009200141112           //eval listyp# = lst_lstype('RD')
009300141126           litype# = lst_ltype();
009400141112           iter;
009500141112         endif;
009501141127
009502141127      /if defined(web)
009503141127          if hidBtn = *on;
009504141127            iter;
009505141127          EndIf;
009506141127      /endif
009700141112
009800141112         //validate employee number
009900141112         status = get_emp_info(liemp#:*omit:
010000141112             *blanks:name#:*omit:*omit:*omit:err#);
010100141112         if err# <> *blanks;
010200141112           *in27 = *on;
010300141112           iter;
010400141112         else;
010500141112           *in27 = *off;
010600141112           emp_nam = name#;
010700141112         endif;
010800020206
010900141112         // validate transaction type
011000141112         if emsubtyp = 'Y';
011001141112
011200050514          *in28 = get_lstype_info('RD':litype#:listyp#:eastf_struc);
011201141112
011400141112           if *in28;
011500141112             iter;
011600141112           endif;
011700141112         endif;
011800141112
011900141112         // validate deduction type
012100050514          *in29 = get_ltype_info('RD':litype#:ematf_struc);
012101141112
012300141112         if *in29;
012400141112           iter;
012500141112         endif;
012600021005
012700141112         if emchgtyp  <> *blanks and
012800141112               %scan(status:emchgtyp) = 0;
012900141112           *in24 = *on;
013000141112         else;
013100141112           *in24 = *off;
013200141112         endif;
013300141112
013400141112
013500141112         chain rec_key lifmt;
013501141112         *IN26 = NOT %FOUND;
013600141112         if *in26;
013700141112           iter;
013800141112         endif;
013900141112
014000141112
014001141112      /END-FREE
014100981016     C                   z-add     liemp         liemp#
014200981016     C                   z-add     listyp        listyp#
014300981016     c                   z-add     litype        litype#
014400981016     C                   move      licode        licode#
014500981016     C                   z-add     lifix         lifix#
014600141112      //
014601141112      /FREE
014700141112         leave;
014800141112       enddo;
014900141112
015000141112       //************************
015100141112       // Main display
015200141112       //************************
015300141112       dow 0 = 0;
015400141112         exfmt fmt2;
015500141112
015600141112         // end program
015700141112         if *in03;
015800141112           *inlr = *on;
015900141112           return;
016000141112         endif;
016100141112
016200141112         // f2 =  cancel
016300141112         if *in02;
016400141112           leave;
016500141112         endif;
016600141112
016700141112         if licode#  =  'N';
016701141112      /END-FREE
016800981016     C                   z-add     *zeros        lifix#
016801141112      /FREE
016900141112         endif;
017000141112
017100141112         if *in15;
017101141112      /END-FREE
017200981016     C                   z-add     lifix#        lifix
017300981016     C                   move      licode#       licode
017301141112      /FREE
017400141112           update lifmt;
017500141112           //
017501141112      /END-FREE
017600981016     C                   z-add     *zeros        liemp#
017700981016     C                   z-add     *zeros        listyp#
017800981016     C                   z-add     *zeros        litype#
017801141112      /FREE
017900141112           leave;
018000141112         endif;
018100141112
018200141112       enddo;
018300141112
018301141112      /END-FREE
018400111013      /free
018500111013
018600111013         //*****************************
018700111013         // Session Init
018800111013         //*****************************
018900111013
019000111013          begsr *inzsr;
019100111013            get_session_info('EMT':errmsg:@pgm);
019200111013            if errmsg <> *blanks;
019300111013              show_msg(errmsg);
019400111013              *inlr = *on;
019500111013              return;
019600111013            endif;
019700111013
019800111013            open(e) liabm;
019900111013
020000111013          endsr;
020100111013
020200111013      /end-free
020300141112      //***************************************************************
020400141112      // Begin subroutine to print header
020500141112      //***************************************************************
020501141112      /FREE
020600141112       begsr aainit;
020700141112         coname = get_coname;
020800141112         @date = get_sys_date;
020900141112         @title = arr(1);
021000141112
021100141112       endsr;
021200141112
021300141112       //
021301141112      /END-FREE
021400981016**   Header to be displayed
021500981016    Payroll  Deduction  Maintenance
021600970925     **
