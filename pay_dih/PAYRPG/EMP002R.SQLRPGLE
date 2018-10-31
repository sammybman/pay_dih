000100951017     H DATEDIT(*YMD)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111013     H/define profoundui
000400141112      // ********************************************************
000500141112      //              B A N K S    D I H    L T D.              *
000600141112      //                                                        *
000700141112      //       RPG PROGRAM NAME..: EMP002R                      *
000800141112      //                                                        *
000900141112      //       PROGRAMMER NAME: Samuel Birkett                  *
001000141112      //       DESIGN DATE....: 1998/05/04                      *
001100141112      //                                                        *
001200141112      //       PROGRAM DESCRIPTION:                             *
001300141112      //                                                        *
001400141112      //  THIS PROGRAM IS DESIGNED TO UPDATE PAYROLL            *
001500141112      //  DEDUCTION FILES.                                      *
001600141112      //                                                        *
001700141112      //*********************************************************
001800141112      // employees
001900111013     Fpaydedchg uf a E           K DISK    usropn
002000141112      //
002100020206     Femp002d   CF   E             WORKSTN
002200941215     F                                     SFILE(SFLREC:RRN)
002300111013     F/IF DEFINED(WEB)
002400111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002500111013     F/ENDIF
002600141112      // General/Payroll PROTOTYPES
002700111013      /copy QCPYSRC,genPR
002800111013      /copy QCPYSRC,PAYPR
002900111013      /copy QCPYSRC,autPR
003000111013      /copy QCPYSRC,empPR
003100141112      //
003200950909     D ARR             S             40    DIM(2) CTDATA PERRCD(1)
003300020206     D #date           S              8  0
003400020206     D name#           S             40
003500020206     D err#            S              7
003600050514     D countr          S              5  0
003700111013     D errmsg          S             75
003800141112      //
003900950909     D                SDS
004000950909     D  @PGM             *PROC
004100111013     D  @USER                358    367
004200141112      //
004300980504     C     pcglist       klist
004400980504     C                   kfld                    pcgtype
004500980504     C                   kfld                    pcgemp
004600141112      //
004700040207      /free
004800111013         // authority check
004900141117         monitor;
005000141117           authority_check('EMT':@pgm);
005100141117         on-error;
005200141117           show_msg(get_err_msg('DIH0054'));
005300141117           *inlr = *on;
005400141117           return;
005500141117         endmon;
005600111013
005700040207      /end-free
005800990403
005900141112      //
006000141112      //
006100141112      // FIRST INITILISATION ROUTINE
006200141112      //
006201141112      /FREE
006300141112       EXSR AAINIT;
006400141112       EXSR option;
006500141112       EXSR subfsubr;
006600141112       //
006700141112       //*******************************************************
006800141112       // Add  ROUTINE
006900141112       //*******************************************************
007000141112       BEGSR add;
007100141112         dow 0 = 0;
007101141112      /END-FREE
007200980520     C                   z-add     *zeros        emp#
007300980520     C                   move      *blank        empnam
007400980520     C                   z-add     *zeros        dedamt
007500981017     C                   movel(P)  'ADD'         mode
007501141112      /FREE
007600141112           EXFMT fmt1;
007700141112           //
007800141112           IF *in03;
007900141112             *INLR = *ON;
008000141112             Return;
008100141112           Endif;
008200141112           //
008300141112           if *in02;
008400141112             exsr subfsubr;
008500141112           Endif;
008600141112           //
008700141112           //  validate employee number
008800141112           status = get_emp_info(emp#:*omit:
008900141112               *blanks:name#:*omit:*omit:*omit:err#);
009000141112           if err# <> *blanks or
009100141112                 get_cat(status) <> 'A';
009200141112             *in27 = *on;
009300141112             iter;
009400141112           else;
009500141112             *in27 = *off;
009600141112             empnam = name#;
009700141112           endif;
009800141112           //
009801141112      /END-FREE
009900980520     C                   z-add     emp#          pcgemp
010000980520     C                   z-add     seltyp        pcgtype
010001141112      /FREE
010100141112           chain pcglist pcgfmt;
010101141112           *IN28 = NOT %FOUND;
010200141112           if *in28 = *off;
010300141112             iter;
010400141112           endif;
010500141112           //
010600141112           EXFMT fmt2;
010700141112           //
010800141112           IF *in02;
010900141112             iter;
011000141112           Endif;
011100141112           //
011200141112           IF *in03;
011300141112             *inlr = *on;
011400141112             Return;
011500141112           Endif;
011600141112           //
011800141112             if dedamt <> 0;
011900141112                pcgsts = 'A';
012000141112                pcgemp = emp#;
012100050627                pcgtype = seltyp;
012200050627                pcgamt = dedamt;
012300141112                write pcgfmt;
012400050627             endif;
012700141112         enddo;
012900141112
013000141112       Endsr;
013001141112      /END-FREE
013100111013      /free
013200111013
013300111013         //*****************************
013400111013         // Session Init
013500111013         //*****************************
013600111013
013700111013          begsr *inzsr;
013800111013            get_session_info('EMT':errmsg:@pgm);
013900111013            if errmsg <> *blanks;
014000111013              show_msg(errmsg);
014100111013              *inlr = *on;
014200111013              return;
014300111013            endif;
014400111013
014500111013            open(e) paydedchg;
014600111013
014700111013          endsr;
014800111013
014900111013      /end-free
015000980520     C
015100141112      //***************************************************************
015200141112      // Change display - fmtsel
015300141112      //*******************************************************
015301141112      /FREE
015400141112       BEGSR change;
015401141112      /END-FREE
015500980520     C                   z-add     *zeros        seltyp
015501141112      /FREE
015600141112         exsr subfsubr;
015601141112      /END-FREE
015700980520     C                   move(p)   'CHANGE'      mode
015701141112      /FREE
015800141112         dow 0 = 0;
015801141112      /END-FREE
015900980520     C                   z-add     *zeros        emp#
015901141112      /FREE
016000141112           exfmt fmtchg;
016100141112           //
016200141112           IF *IN02 = *ON;
016300141112             exsr option;
016400141112           endif;
016500141112           //
016600141112           IF *IN03 = *ON;
016700141112             *INLR = *ON;
016800141112             Return;
016900141112           Endif;
017000141112           //
017001141112      /END-FREE
017100980520     C                   z-add     seltyp        pcgtype
017200980520     C                   z-add     emp#          pcgemp
017300141112      //
017301141112      /FREE
017400141112           chain pcglist pcgfmt;
017401141112           *IN30 = NOT %FOUND;
017500141112           if *in30;
017600141112             iter;
017700141112           endif;
017701141112
017900050514          get_ltype_info('RD':pcgtype:ematf_Struc);
017901141112
018000050514      /end-free
018100980520     c                   movel     emdsp         dednam
018200980520     c                   z-add     pcgamt        dedamt
018201141112      /FREE
018300141112           exfmt fmtchg2;
018400141112           //
018500141112           if *in02;
018600141112             iter;
018700141112           Endif;
018800141112           //
018900141112           if *in03;
019000141112             *inlr = *on;
019100141112             Return;
019200141112           Endif;
019300141112           //
019301141112
019500141112             if dedamt <> 0;
019600050627                pcgsts = 'A';
019700050627                pcgemp = emp#;
019800050627                pcgamt = dedamt;
019900050627                update  pcgfmt;
020000050627             endif;
020001141112
020200141112         enddo;
020300141112       Endsr;
020400141112       //***************************************************************
020500141112       // Delete display - fmtsel
020600141112       //*******************************************************
020700141112       BEGSR delete;
020800141112         exsr subfsubr;
020900141112         dow 0 = 0;
020901141112      /END-FREE
021000980520     C                   move(p)   'DELETE'      mode
021100980520     C                   z-add     *zeros        emp#
021101141112      /FREE
021200141112           exfmt fmtchg;
021300141112           //
021400141112           IF *IN02 = *ON;
021500141112             exsr option;
021600141112           endif;
021700141112           //
021800141112           IF *IN03 = *ON;
021900141112             *INLR = *ON;
022000141112             Return;
022100141112           Endif;
022200141112           //
022201141112      /END-FREE
022300980520     C                   z-add     seltyp        pcgtype
022400980520     C                   z-add     emp#          pcgemp
022401141112      /FREE
022500141112           chain pcglist pcgfmt;
022501141112           *IN30 = NOT %FOUND;
022600141112           if *in30;
022700141112             iter;
022800141112           endif;
022801141112
023000050514          get_ltype_info('RD':pcgtype:ematf_Struc);
023001141112
023100050514      /end-free
023200980520     c                   movel     emdsp         dednam
023300980520     c                   z-add     pcgamt        dedamt
023301141112      /FREE
023400141112           exfmt fmtdel;
023500141112           //
023600141112           if *in02;
023700141112             iter;
023800141112           Endif;
023900141112           //
024000141112           if *in03;
024100141112             *inlr = *on;
024200141112             Return;
024300141112           Endif;
024400141112           //
024500141112           IF *in20 = '1';
024600141112             delete pcgfmt;
024700141112           endif;
024800141112         enddo;
024900141112       Endsr;
025000141112       //*******************************************************
025100141112       // subfile subroutine
025200141112       //*******************************************************
025300141112       BEGSR subfsubr;
025301141112      /END-FREE
025400980520     C                   Z-ADD     *ZERO         RRN               4 0
025500141112      //
025501141112      /FREE
025600141112         reset sflrec;
025700141112         *IN33 = *ON;
025800141112         WRITE SFLCTL;
025900141112         *IN33 = *OFF;
026000141112         //
026200050514          *in55 = *off;
026300050514          countr = *zeros;
026400050514          dow not get_ltype_info('RD':-1:ematf_Struc:countr);
026401141112
026600141112         if emalwpyd = 'Y';
026601141112      /END-FREE
026700980520     C                   MOVEl     emdsp         descript#
026701141112      /FREE
026800141112           acct# = emacct;
026801141112      /END-FREE
026900980520     C                   Z-ADD     emseltyp      type
027000980520     C                   move(P)   emchgtyp      ctype             2
027100980520     C                   move(p)   emsubtyp      sub_tp
027200141112      //
027300980520     C                   ADD       1             RRN               4 0
027301141112      /FREE
027400141112           Write Sflrec;
027500141112         endif;
027600141112         //
027700141112       Enddo;
027701141112
027900050514          *in55 = *on;
027901141112
028200141112       dow 0 = 0;
028300020417
028400141112         EXFMT SFLCTL;
028401141117
028600141112         if *in03;
028700141112           *inlr = *on;
028800141112           Return;
028900141112         Endif;
028901141117
029000141112         //
029100141112         // CHECK ANY SELECTION MADE
029200141112         //
029300141112         READC SFLREC;
029301141112         *IN22 = %EOF;
029400141112         if not(*in22);
029500141112           //
029600141112           if sel# = 'X';
029601141112      /END-FREE
029700980520     C                   z-add     type          seltyp
029800980520     c                   movel     descript#     dednam
029801141112      /FREE
029900141112             leave;
030000141112           else;
030100141112             iter;
030200141112           endif;
030300141117
030400141112         endif;
030500141112       enddo;
030600141112       endsr;
030700141112
030800141112       //*******************************************************
030900141112       // FIRST screen display - fmtsel
031000141112       //*******************************************************
031100141112       BEGSR option;
031200141112         exfmt fmtsel;
031300980520
031400141112         if *in03;
031500141112           *inlr = *on;
031600141112           return;
031700141112         endif;
031800141112         //
031900141112         select;
032000141112         when opt = 1;
032100141112           exsr subfsubr;
032200141112           exsr add;
032300980520
032400141112         when opt = 2;
032500141112           exsr change;
032600980520
032700141112         when opt = 3;
032800141112           exsr delete;
032900141112         endsl;
033000141112       Endsr;
033100141112       //*******************************************************
033200141112       // FIRST INITILISATION ROUTINE FOR ALL PROGRAMS
033300141112       //*******************************************************
033400141112       BEGSR AAINIT;
033500141112         coname = get_coname;
033600141112         @date = get_sys_date(#date);
033700141112         @title = arr(1);
033800141112       Endsr;
033900980520
033901141112      /END-FREE
034000950909**
034100971011          Employees Charges
034200950909XXXXXXXXXX0123456789XXXXXXXXXX0123456789
