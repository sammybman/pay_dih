000100071018     H bnddir('DISDIR') dftactgrp(*no) actgrp(*caller)
000200111013     H/define profoundui
000300071011     F**PROGRAMMER --       NASOYA CHAPMAN       -- 19.09.2007
000400071018     Fempmnt02d CF   E             WORKSTN
000500111013     F/IF DEFINED(WEB)
000600111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111013     F/ENDIF
000800111013
000900071017     D ARR             S             40    DIM(2) CTDATA PERRCD (1)
001000071011     D datal           S              8  0 inz(0)
001100071011     D ests            S              1    inz(' ')
001200071011     D list            S              3    inz('EAR')
001300070919     D aainit          pr
001400070919     D clr_screen      pr
001500070919     D update_fields   pr
001600071011     D write_to_file   pr
001700070919     D load_data       pr
001800111013
001900070919     D                SDS
002000070919     D  @PGM             *proc
002100111013     D@user                  358    367
002200111013
002300111013      /copy QCPYSRC,paypr
002400111013      /copy QCPYSRC,genpr
002401141203      /copy QCPYSRC,autpr
002500070919      /free
002600111013
002700111013         // authority check
002800111013         monitor;
002900111013           authority_check('EMT':@pgm);
003000111013         on-error;
003100111013           show_msg(get_err_msg('DIH0054'));
003200111013           *inlr = *on;
003300111013           return;
003400111013         endmon;
003500111013
003600070919        DOW 0 = 0;
003700071017       //executing the procedure to clear the fields on the screen//
003800070919         clr_screen();
003900071017       //executing the procedure to initialise the main variables//
004000070919         aainit();
004100070919         DOW 0 = 0;
004200070919       //displaying the first screen//
004300070919       exfmt fmt1;
004400071015       //setting the indicator off//
004500071009       *in26 = *off;
004600070919       //clearing the error message field//
004700070919        errmsg = *blanks;
004800070919       //checking to see if f3 is pressed//
004900070919       if *in03;
005000070919         *inlr = *on;
005100070919         return;
005200070919       ENDIF;
005300070919       //validating data being entered//
005400071017       //checking to see if Employee number was entered//
005500071011       if empno <= *Zeros;
005600071011         errmsg = get_err_msg('DIH0022');
005700071009         show_msg(errmsg);
005800071009         *in26 = *on;
005900070919         iter;
006000070919       ENDIF;
006100071017       //using sql statement to check for records in the file//
006200070919         exec sql
006300071016         select bklemp, bklsdate, bkledate into
006400111013         :empno, :entdat, :compldat from empbkln
006500071016         where bklemp = :empno and bklsts = 'A';
006600070919
006700070919         if sqlstt <> '00000';
006800071017       //if record not found enter loan entry date if record found enter loan co
006900071017       //setting mode to ADD because the requested record was not found//
007000071016           @mode = '  ADD  ';
007100071017       //out put the Employee Number and Employee Name to the screen//
007200071011           ests = get_emp_info(empno:*omit:' ':EMPname);
007300071011           if %scan(ests:list) = 0;
007400071011             errmsg = get_err_msg('DIH0035');
007500071011             show_msg(errmsg);
007600071017             *in26 = *on;
007700071011             iter;
007800071011           ENDIF;
007900071017       //setting indicator on because the requested record was not found//
008000070919           *in44 = *on;
008100070919            clr_screen();
008200070919            else;
008300071015       //setting mode to AMEND because the requested record was found//
008400070919           @mode = ' AMEND ';
008500071017       //setting the indicator off because the record was found//
008600071015           *in44 = *off;
008700071017       //output the Employee Number and Employee Name to the screen//
008800071011           get_emp_info(empno:*omit:' ':EMPname);
008900070919        ENDIF;
009000071017       //leave the loop to get to the next screen
009100070919         leave;
009200070919         ENDDO;
009300070919       Dow 0 = 0;
009400071017       //displaying the second screen//
009500070919       exfmt FMT2;
009600071015       //setting the indicators off//
009700071009       *in27 = *off;
009800071009       *in28 = *off;
009900071017       //clearing the error message field//
010000070919        errmsg = *blanks;
010100071017       //checking to see if f3 is pressed//
010200070919       if *in03;
010300070919         *inlr = *on;
010400070919         return;
010500070919       ENDIF;
010600071017       //checking to see if f2 is pressed//
010700070919       if *in02;
010800070919         leave;
010900071017       //executing the procedure to clear the fields on the screen//
011000070919         clr_screen();
011100070919       ENDIF;
011200070919       //validating data being entered//
011300071017       if %trim(@mode) = 'AMEND';
011400071017       //checking to see if Loan Completion date was entered//
011500071017        if compldat = *zeros;
011600071017         errmsg = 'please enter Loan Completion date';
011700071017         show_msg(errmsg);
011800071017         *in28 = *on;
011900071017         iter;
012000071017       ENDIF;
012100071017       //testing Loan Completion date iso compliancy//
012200071017       test(de) *iso compldat;
012300071017       if %error;
012400071017         errmsg = 'Invalid Loan Completion date format';
012500071017         show_msg(errmsg);
012600071017        *in28 = *on;
012700071017         iter;
012800071017       ENDIF;
012900071017       //checking to ensure persons enter the correct date//
013000071017       if compldat <= entdat;
013100071017         errmsg = 'Loan Completion date must be greater than';
013200071017         errmsg = %trim(errmsg) + ' Loan Entry date';
013300071017         show_msg(errmsg);
013400071017        *in28 = *on;
013500071017         iter;
013600071017       ENDIF;
013700071017       //checking to see if the Completion date is greater than the Current date
013800071017       if compldat > datal;
013900071017       errmsg = 'Loan Completion date must not be greater than';
014000071017       errmsg = %trim(errmsg) + ' the Current date';
014100071017       show_msg(errmsg);
014200071017       *in28 = *on;
014300071017       iter;
014400071017       endif;
014500071017       ELSE;
014600071017       //checking to see if Loan Entry date was entered//
014700071015        if entdat = *zeros;
014800071017        errmsg = 'please enter Loan Entry date';
014900071015        show_msg(errmsg);
015000071015        *in27 = *on;
015100070919         iter;
015200070919       ENDIF;
015300071017       //testing Loan Entry date iso compliancy//
015400071009       test(de) *iso entdat;
015500071009       if %error;
015600071017         errmsg = 'Invalid Loan Entry date format';
015700071009         show_msg(errmsg);
015800071009        *in27 = *on;
015900071009         iter;
016000071009       ENDIF;
016100071017       //checking to see if Loan Entry date is greater than the Current date//
016200071009       if entdat > datal;
016300071011        errmsg = 'Loan Entry date must not be greater than the Current date';
016400071011        show_msg(errmsg);
016500071011        *in27 = *on;
016600071011        iter;
016700071009       ENDIF;
016800071017       ENDIF;
016900070919       //if F15 is pressed, write data from screen to file fields//
017000070919       if *in15;
017100070919       //execute procedure to load data from screen to file//
017200070919         load_data();
017300071017       //return to the first screen//
017400071018         commit;
017500070919         leave;
017600070919       ENDIF;
017700070919       enddo;
017800070919       ENDDO;
017900111013
018000111013         //*****************************
018100111013         // Session Init
018200111013         //*****************************
018300111013
018400111013          begsr *inzsr;
018500111013            get_session_info('EMT':errmsg:@pgm);
018600111013            if errmsg <> *blanks;
018700111013              show_msg(errmsg);
018800111013              *inlr = *on;
018900111013              return;
019000111013            endif;
019100111013
019200111013          endsr;
019300111013
019400070919      /end-free
019500071017       //..........................................................//
019600070919       //if add record, writing record, else update existing record//
019700070919       //.........................................................//
019800070919     p load_data       B
019900070919     D load_data       PI
020000070919      /free
020100071017       //checking text of @mode field to decide if to update or add record.
020200070919        if %trim(@mode) = 'AMEND';
020300070919       //executing procedure to update file//
020400070919        update_fields();
020500070919        else;
020600070919       //execute procedure to write data from screen to file fields//
020700070919        write_to_file();
020800070919        ENDIF;
020900070919      /end-free
021000070919     p load_data       E
021100070919       //.............................................................//
021200070919       //procedure to write data from screen to file fields using sql//
021300070919       //............................................................//
021400070919     p write_to_file   B
021500070919     D write_to_file   PI
021600070919      /free
021700070919        exec sql
021800111013        insert into empbkln(bkleuser, bklsts, bklemp,
021900071016        bklsdate, bkledate)
022000071016        values (:@user, 'A', :empno, :entdat, :compldat);
022100070919      /end-free
022200070919     p write_to_file   E
022300070919       //....................................................//
022400070919       //procedure to update data from screen to file fields//
022500070919       //..................................................//
022600070919     p update_fields   B
022700070919     D update_fields   PI
022800070919      /free
022900071017       //executing the sql//
023000070919         exec sql
023100071017       //using sql to update empbkln//
023200111013         update empbkln
023300070919         set
023400071017        bklcuser = :@user,
023500070919        bkledate = :compldat,
023600070919        bklsts = 'C'
023700071015        where bklemp = :empno;
023800070919      /end-free
023900070919     p update_fields   E
024000071002       //..............................//
024100071002       //procedure to clear the screen//
024200071002       //............................//
024300070919     p clr_screen      B
024400070919     D clr_screen      PI
024500070919      /free
024600070919        entdat = *zeros;
024700070919        compldat = *zeros;
024800070919      /end-free
024900070919     p clr_screen      E
025000071002       //............................................//
025100071002       //procedure to initialsie the main variables //
025200071002       //..........................................//
025300070919     p aainit          B
025400070919     D aainit          PI
025500070919      /free
025600071008         coname = get_coname;
025700071009         @date = get_sys_date(datal);
025800070919         @title = Arr(1);
025900070919      /end-free
026000070919     p aainit          E
026100070919**
026200071017  Employee Bank Loan Record Maintenance
