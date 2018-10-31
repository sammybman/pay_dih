000100951006     H DATEDIT(*YMD)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400141113      // ********************************************************
000500141113      //                     DISERV SYSTEMS                     *
000600141113      //                Employee Credit Control                 *
000700141113      //                                                        *
000800141113      //                         BY                             *
000900141113      //                                                        *
001000141113      //             DISERV  APPLICATION DEVELOPMENT TEAM       *
001100141113      //                                                        *
001200141113      //                                                        *
001300141113      //       RPG PROGRAM NAME..: EMP003R01                    *
001400141113      //                                                        *
001500141113      //       PROGRAMMER NAME: chris williams                  *
001600141113      //       DESIGN DATE....: 1998/06/01                      *
001700141113      //       Modified.......: 2002/02/08  (Sammy Britton)     *
001800141113      //                                                        *
001900141113      //  PROGRAM DESCRIPTION:  CREATE A Emp. Liab. Transaction *
002000141113      //                                                        *
002100141113      //                                                        *
002200141113      //*********************************************************
002300141113      // Liability transaction  file
002400141117     Fjrnml01   iF A E           K Disk    commit usropn
002500020208     Femp003d01 cf   e             workstn
002600020208     F                                     sfile(sflrec:rrn)
002700111013     F/IF DEFINED(WEB)
002800111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900111013     F/ENDIF
003000141113      //
003100141113      // General/Payroll PROTOTYPES
003200111013      /copy QCPYSRC,PAYPR
003300111013      /copy QCPYSRC,genPR
003400111013      /copy QCPYSRC,glPR
003500111013      /copy QCPYSRC,empPR
003600141113      //
003700020208     D ENT             S              6  0 dim(50)
003800020208     D ARR             S             40    dim(1) ctdata perrcd(1)
003900020123     d #time           s              6  0
004000001213     d #date           s              8  0
004100141113      //status          s              1
004200001214     d rrn             s              4  0 inz(0)
004300001214     d line#           s              3  0 inz(0)
004400020208     d prno            s              6  0
004500001227     d cnt             s              2  0 inz(0)
004600001228     D save            S              4  0 inz(0)
004700020122     D name#           S             40
004800141113      //dept            S              3  0
004900020201     D totcnt          S              3  0
005000020122     D err#            S              7
005100141113      //
005200020126     d employee_enq    c                   'HMRENQ09R '
005300020208     Dmsg1             c                   'Invalid Liability Type'
005400020208     Dmsg2             c                   'Type is not valid for this emp-
005500020208     D                                     loyee'
005600020208     Dmsg3             c                   'Valid Sub-type required '
005700020208     Dmsg4             c                   'No Sub-type required for this type'
005800020208     Dmsg5             c                   'Invalid amount entered '
005900020208     Dmsg6             c                   'Transaction totals do not match'
006000141113      //
006100950909     D                SDS
006200950909     D  @PGM             *PROC
006300111013     D  @USER                358    367
006400141113      //
006500111013      /copy QCPYSRC,named_ind
006600001228     D can_entry               2      2n
006700020125     D override_trn           21     21n
006800001228     D line_protected         30     30n
006900001228     D clear_subfile          33     33n
007000001228     D line_retrieved         40     40n
007100020125     D limit_exceeded         99     99n
007200141113      //
007300141113      // First initilisation routine
007400141113      /FREE
007500141117
007600141113       // Clear the subfile at the end of the cycle
007700141117       exsr reset;
007800141117
007900141117       exsr aainit;
008000020123
008100141113       //********************
008200141113       // Program Start   ***
008300141113       //********************
008400141113       dow 0 = 0;
008500141113
008600141113         // Display the first screen format
008700141113         exfmt fmt1;
008800141113         // End program
008900111013      /copy QCPYSRC,exit_copy
009000141113      // Clear error message field
009100141113         errmsg = *blanks;
009200141117
009300141113         // Validate GL. number
009400031001         get_gl_info(gl#:errmsg:'V');
009500141113
009600141113         if errmsg <> *blanks;
009700141117           show_msg(errmsg);
009800141113           iter;
009900141113         endif;
010000141113         //
010100141113         // Error message - blank description
010200141113         if desc# = *blanks;
010300141113           errmsg = get_err_msg('DIH0022');
010400141117           show_msg(errmsg);
010500141113           iter;
010600141113         Endif;
010700141113         //
010800141113         // Error message - negative amount
010900141113         if amt# <= *zeros;
011000141113           errmsg = msg5;
011100141117           show_msg(errmsg);
011200141113           iter;
011300141113         Endif;
011400020208
011500141113         // validate Period Number
011600141113         errmsg = validate_prd(prdnumb#);
011700141113         if errmsg <> *blanks;
011800141117           show_msg(errmsg);
011900141113           iter;
012000141113         endif;
012100020208
012200141113         //
012300141113         leave;
012400141113       enddo;
012500141117
012600001214
012700141113       // main calcs
012800141113       //**********************
012900141113       // BODY OF PROGRAM    **
013000141113       //**********************
013100141113       //
013200141113       dow 0 = 0;
013300141113         //  Display the format
013400150615      /IF not DEFINED(WEB)
013500150615         //  Write footer record
013600150615         // if greenscreen
013700150615         write SFLCTL;
013800150615         exfmt footer;
013900150615      /else
014000150615         exfmt SFLCTL;
014100150615      /endif
014200141113      /END-FREE
014300020123     C                   move      *blanks       errmsg
014400141113      // CANCEL ENTRY
014500141113      /FREE
014600150615         if *in02;
014700141113           leave;
014800141113         Endif;
014900141113         // END PROGRAM
015000141113      /END-FREE
015100111013      /copy QCPYSRC,exit_copy
015200020123
015300141113      // Employee enquiry
015400141113         if *in06;
015500020208     C                   call      employee_enq
015600020208     C                   parm                    ltemp##
015700141113      /FREE
015800141113           iter;
015900141113         endif;
016000141113         // Type enquiry
016100141113         if *in07;
016200141113           lttype## = lst_ltype();
016300141113           iter;
016400141113         endif;
016500141113         // Sub-Type enquiry
016600141113         if *in08;
016700141113           ltstyp## = lst_lstype(lttype##);
016800141113           iter;
016900141113         endif;
017000020123
017100141113         // Check IF A LINE NUMBER IS ENTERED
017200141113         if not line_retrieved;
017300141113           //
017400141113           if lin > 0 and lin <= save;
017500141113             chain lin sflrec;
017600141113             *IN55 = NOT %FOUND;
017700020123
017800141113             // move sub-file fields to footer
017900141113             ltemp## = ltemp#;
018000141113             lttype## = lttype#;
018100141113             ltstyp## = ltstyp#;
018200141113             ltamt## = ltamt#;
018300141113             outtot = outtot - calamt#;
018400141113             //
018500020123
018600141113             // VALIDATE NUMBER ENTERED
018700141113             //
018800141113             line_protected = *on;
018900141113             line_retrieved = *on;
019000141113             //
019100141113             iter;
019200141113           endif;
019300141113           //
019400141113         endif;
019500141113         //**********************************************************
019600141113         // End trn. creation and write records to data base file  **
019700141113         //**********************************************************
019800141113         // F15 = confirmed (indicator 15)
019900141113         if confirmed;
020000020208
020100141113           if (amt# - outtot) <> *zeros;
020200141113             errmsg = msg6;
020300141117             show_msg(errmsg);
020400141113             iter;
020500141113           endif;
020600020208
020700141113           exsr CONFRM;
020800141113           leave;
020900141113         endif;
021000141113         //
021100141113         status = get_emp_info(ltemp##:dept:
021200141113             *blanks:name#:*omit:*omit:*omit:err#);
021300141113         if err# <> *blanks;
021400141113           errmsg = get_err_msg('DIH0035');
021500141117           show_msg(errmsg);
021600141113           iter;
021700141113         endif;
021800020208
021900141117       if get_ltype_info('RD':lttype##:ematf_struc);
022000141113
022100141113         errmsg = msg1;
022200141117         show_msg(errmsg);
022300141113         iter;
022400141113       endif;
022500020208
022600141117       if emchgtyp <> *blanks and %scan(status:emchgtyp) = *zeros;
022700141117
022800141113         errmsg = msg2;
022900141117         show_msg(errmsg);
023000141113         iter;
023100141113       endif;
023200141113       //
023300141113       select;
023400141113       when emsubtyp = 'Y';
023500141113
023600141113         if get_lstype_info('RD':lttype##:ltstyp##:eastf_struc);
023700141113
023800141113         errmsg = msg3;
023900141117         show_msg(errmsg);
024000141113         iter;
024100141113       endif;
024200141113
024300141113       other;
024400141113       if ltstyp## <> *zeros;
024500141113         errmsg = msg4;
024600141117         show_msg(errmsg);
024700141113         iter;
024800141113       endif;
024900141113
025000141113       endsl;
025100141113       //
025200141113       if ltamt## = *zeros;
025300141113         errmsg = msg5;
025400141117         show_msg(errmsg);
025500141113         iter;
025600141113       endif;
025700051009
025800141113       //
025900141113      /END-FREE
026000020123     C                   MOVE      *BLANKS       ERRMSG
026100141113      // Seton indicators from 22 to 28
026200020125     C                   MOVEA     '0000000'     *in(22)
026300020123
026400141113      // CHECK IF ITEM ALREADY ON THIS REQ ORDER
026500141113      //* 55item          lookup    ent                                    22
026600141113      //*                 if        *in22
026700141113      //*                 eval      errmsg = get_err_msg('DIH0074')
026800141113      //*                 iter
026900141113      //*                 Endif
027000141113      //
027100141113      /FREE
027200141113       *IN26 = *OFF;
027300020123
027400141113       //
027500141113       if rrn = 0;
027600141113         *in55 = *on;
027700141113       else;
027800141113         chain lin sflrec;
027900141113         *IN55 = NOT %FOUND;
028000141113       endif;
028100141113       // clear sub-file fields
028200141113       //                  clear                   sflrec
028300141113       //
028400141113       //
028500141113       ltemp# = ltemp##;
028600141113       lttype# = lttype##;
028700141113       ltstyp# = ltstyp##;
028800141113       ltamt# = ltamt##;
028900141113       name = name#;
029000141113       dept# = dept;
029100141113       calamt# = ltamt##;
029200141113       outtot = outtot + ltamt##;
029300141113       //
029400141113       if *in55;
029500141113         save = save + 1;
029600141113         rrn = save;
029700141113         line# = rrn;
029800141113         ent(line#) = ltemp##;
029900141113         write sflrec;
030000141113       else;
030100141113         line# = lin;
030200141113         update sflrec;
030300141113       endif;
030400141113       // Clear input fields
030500141113       clear ltemp##;
030600141113       clear lttype##;
030700141113       clear ltstyp##;
030800141113       clear ltamt##;
030900141113       reset cnt;
031000141113       limit_exceeded = *off;
031100141113       *in10 = *on;
031200141113       lin = save + 1;
031300141113       line_retrieved = *off;
031400141113       //
031500141113       enddo;
031600141117
031700141117        //*****************************
031800141117         // Session Init
031900141117         //*****************************
032000141117
032100141117          begsr *inzsr;
032200141117            get_session_info('EMT':errmsg:@pgm);
032300141117            if errmsg <> *blanks;
032400141117              show_msg(errmsg);
032500141117              *inlr = *on;
032600141117              return;
032700141117            endif;
032800141117
032900141117            open(e) jrnml01;
033000141117
033100141117          endsr;
033200141113       //
033300001222
033400141113       //**************************************************************
033500141113       //*   RESET THE SUBFILE
033600141113       //**************************************************************
033700141113       BEGSR RESET;
033800141113         // Clear Sub_file
033900141113         clear_subfile = *on;
034000141113         write sflctl;
034100141113         clear_subfile = *off;
034200141113         // Clear entry fields
034300141113         clear footer;
034400141113         clear ent;
034500141117         // Reset all entry fields
034600141113         reset rrn;
034700141113         reset line#;
034800141113         reset save;
034900141113         limit_exceeded = *off;
035000141113         amt# = *zeros;
035100141113         gl# = *blanks;
035200141113         desc# = *blanks;
035300141113         desc2# = *blanks;
035400141113       ENDSR;
035500141113       //**************************************************************
035600141113       //*   Sub routine to display the last part of a purchase order
035700141113       //*   creation.
035800141113       //**************************************************************
035900141113       BEGSR CONFRM;
036000020123
036100141113         //
036200141113         // Get system date/time
036300141113         get_sys_date(#date:#time);
036400141113         // Get next transaction  number
036500141113         ltnumb# = get_next_no('ET':'U');
036600141113         //  clear fields on output record
036700141113         clear jrfmt;
036800141113         // Write Control Record first
036900141113         jrsts = 'O';
037000141113         jrdate = #date;
037100141113         jremp = *zeros;
037200141113         jrtype = *zeros;
037300141113         jrstyp = *zeros;
037400141113         jrtext = desc#;
037500141113         jrtext2 = desc2#;
037600141113         jracct = gl#;
037700141113         jrnumb = ltnumb#;
037800141113         jrprno = prdnumb#;
037900141113         jrusr = @user;
038000141113         jrtag = '2';
038100141113         if c_type = 'D';
038200141113           jramt  = %abs(amt#);
038300141113         else;
038400141113           jramt  = %abs(amt#) * -1;
038500141113         endif;
038600021014
038700141113         write jrfmt;
038800141113         // Reading the subfile to get detail information for
038900141113         // the data base file.
039000141113         //
039100141113      /END-FREE
039200021014     C                   Z-ADD     save          RRX               4 0
039300020125     C     1             do        rrx           RR                4 0
039400141113      /FREE
039500141113           chain RR sflrec;
039600141113           *INH6 = NOT %FOUND;
039700141113           //  Write detail records
039800050514           get_ltype_info('RD':lttype#:ematf_struc);
039900050514           jracct = emacct;
040000050514           //Get correct gl
040100050514           jracct =  crossref_gl(ltemp#:dept#:lttype#:
040200050514                                           ltstyp#:jracct);
040300141113           jremp = ltemp#;
040400141113           jrtype = lttype#;
040500141113           jrstyp = ltstyp#;
040600141113           if c_type = 'D';
040700141113             jramt  = - ltamt#;
040800141113           else;
040900141113             jramt  = ltamt#;
041000141113           endif;
041100020208
041200141113           write jrfmt;
041300020201
041400141113         enddo;
041500020208
041600141113         commit;
041700141113         EXSR RESET;
041800141113       ENDSR;
041900141113       //
042000141113       //*******************************************************
042100141113       // Startup routine for program
042200141113       //*******************************************************
042300141113       BEGSR AAINIT;
042400141113         // Get Company Name
042500141113         CONAME = get_coname;
042600141113         // Get system date
042700141117         @DATE = get_sys_date(#date);
042800141117         get_prd_info('*prd':*blanks:prno:#date);
042900141117
043000141113         ltnumb# = get_next_no('ET');
043100141113
043200141113         // set period number to current period number
043300141113         prdnumb# = prno;
043400141113         // select header for program
043500141113         @title = arr(1);
043600141113       ENDSR;
043700141113      /END-FREE
043800960219**
043900020208   Employees Transaction Maintenance
