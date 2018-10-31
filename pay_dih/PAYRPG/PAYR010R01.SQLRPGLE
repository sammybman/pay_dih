000100111027     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111027     H/define profoundui
000300090105     F**PROGRAMMER --       NASOYA CHAPMAN       -- 06.01.2009
000400090128     Fpayr010d01CF   E             WORKSTN
000500090105     F                                     sfile(sflrec:rrn)
000600111027     F/IF DEFINED(WEB)
000700111027     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111027     F/ENDIF
000900090105     D ARR             S             40    DIM(2) CTDATA PERRCD (1)
001000090105     D datal           S              8  0 inz(0)
001100090105     D date            S              4  0 inz(0)
001200090126     D ok              S              1     inz('N')
001300090129     D temp            S             60
001400090105     D aainit          pr
001500090105     D clr_screen      pr
001600090122     D new_line        pr
001700090105     D load_data       pr
001800090105     D write_to_file   pr
001900090105     D del_record      pr
002000090105     D write_sub       pr
002100090128     D performdelete   pr
002200090128     D clr_subfile     pr
002300090105     D update_screen   pr              n
002400090126     D ok                             1
002500090105     D open_filesr     pr
002600090105     D read_datasr     pr              n
002700090127     D close_filesr    pr
002800090105     D count           S                   LIKE(rrn)
002900090128     D trans#          S              8  0
003000090129     D line#           S              2  0
003100090105     D                SDS
003200090105     D  @PGM             *proc
003300090126     D
003400090105     Drrn              S              4  0 inz(0)
003500090120     DncsDS          e DS                  extname(rbgrange)
003600090128     D*--------------------------------------------------
003700090128     P* Program name: GET_PAYR_ENQ
003800090128     D* Purpose:
003900090128     D* Returns:
004000090128     D* Parameter:      trans#
004100090128     D*--------------------------------------------------
004200111027     D GET_PAYR_ENQ    PR                  EXTPGM('PAYR010R03')
004300090129     D  trans#                        8  0
004400090128
004500111027      /copy QCPYSRC,genpr
004600090105      /free
004700090105       //executing the procedure to initialise the main variables//
004800090105       aainit();
004900090105       DOW 0 = 0;
005000090128        //clearing the error message field//
005100090128        errmsg = *blanks;
005200090105       //displaying the first screen//
005300090105       exfmt fmt1;
005400090105       //setting the indicators off//
005500090105       *in26 = *off;
005600090105       //if F3 is pressed, close files and exit program
005700090105       if *in03;
005800090105         *inlr = *on;
005900090105         return;
006000090105       ENDIF;
006100090105
006200090128       //call external program if F4 is pressed//
006300090128       if *in04;
006400090128         get_payr_enq(trans#);
006500090128         iter;
006600090128       ENDIF;
006700090128
006800090105       //validating the first screen//
006900090105       //checking to see if transit number was entered//
007000090105       if trans# = *zeros;
007100090128         errmsg = get_err_msg('DIH0022');
007200090128         show_msg(errmsg);
007300090105         *in26 = *on;
007400090105         iter;
007500090128       ENDIF;
007600090122
007700090122       //checking to see if transit number exist in the master file//
007800090122       exec sql
007900090129       select rtrngl into :temp from rbgtrnmas where rtrnkey = :trans#
008000090122       and rtrnsts = 'A';
008100090122
008200090122       //checking if record found in temp//
008300090122       if sqlstt <> '00000';
008400090128        errmsg = get_err_msg('DIH0064');
008500090128        show_msg(errmsg);
008600090122        *in26 = *on;
008700090122        iter;
008800090122       ENDIF;
008900090122
009000090127       DOW 0 = 0;
009100090128         //executing procedure to clear the subfile//
009200090128         clr_subfile();
009300090128
009400090105        //executing pocedure to set up file position cursor//
009500090105         open_filesr();
009600090119
009700090105         dow not read_datasr();
009800090105         //write data to subfile//
009900090129         write_sub();
010000090105         enddo;
010100090105         close_filesr();
010200090126
010300090105         //checking to see if any records found//
010400090119         if rrn = 0;
010500090119
010600090128         @mode = '  ADD  ';
010700090128
010800090128        //writing description from master file to the screen//
010900090128        exec sql
011000090129        select rtrndesc into :desc## from rbgtrnmas
011100090128        where rtrnkey = :trans# and rtrnsts = 'A';
011200090128
011300090123           new_line();
011400090119           *in60 = *on;
011500090119          else;
011600090105          //setting mode to AMEND because the requested record was found//
011700090119          @mode = ' AMEND ';
011800090119         ENDIF;
011900090122
012000090105       DOW 0 = 0;
012100090105       write footer;
012200090105       //Displaying the subfile//
012300090105       exfmt sflctl;
012400090105       //setting the indicators off//
012500090105       *in27 = *off;
012600090105       *in28 = *off;
012700090105       //clearing the error message field//
012800090105       errmsg = *blanks;
012900090105       //F3 = end program//
013000090105       if *in03;
013100090105         *inlr = *on;
013200090105         return;
013300090105       ENDIF;
013400090123
013500090123       //F10 = new line//
013600090123       if *in10;
013700090123       //calling procedure to create new line//
013800090123        new_line();
013900090123        iter;
014000090123       endif;
014100090123
014200090128       //Deleting individual line from a record//
014300090126       readc sflrec;
014400090128        if not(%eof);
014500090127         if sel = 'D';
014600090127          performdelete();
014700090128          sel = ' ';
014800090127          leave;
014900090127         ENDIF;
015000090128
015100090123       //calling procedure to validate fields on the screen//
015200090123       if update_screen(ok);
015300090123         iter;
015400090123       endif;
015500090123
015600090123         iter;
015700090126        endif;
015800090129
015900090129        if not *in15 and not *in20 and not *in02;
016000090129         iter;
016100090129        ENDIF;
016200090127       leave;
016300090127       ENDDO;
016400090129
016500090129       if *in02;
016600090129         //clear screen//
016700090129         clr_screen();
016800090129         leave;
016900090129       ENDIF;
017000090123
017100090105       //checking to see if f15 is pressed//
017200090105       if *in15;
017300090105       //if f15 is pressed execute procedure to load data//
017400090105       load_data();
017500090105       commit;
017600090128       trans# = *zeros;
017700090105       //return to first screen//
017800090105       leave;
017900090105       ENDIF;
018000090105
018100090105       if *in20;
018200090105       //execute procedure to delete records//
018300090105       del_record();
018400090129       trans# = *zeros;
018500090105       //return to first screen//
018600090105       leave;
018700090105       ENDIF;
018800090126       enddo;
018900090123       enddo;
019000090105      /end-free
019100090105      //.........................................//
019200090105       //Procedure to set up file position cursor//
019300090105       //.......................................//
019400090105     p open_filesr     B
019500090105     D open_filesr     PI
019600090105      /free
019700090105
019800090106        exec sql
019900090129        declare ncsfile cursor for select * from rbgrange
020000090121        where rbrtrans# = :trans# and rbrsts = 'A';
020100090105
020200090105        exec sql
020300090105        open ncsfile;
020400090105
020500090105      /end-free
020600090105     p open_filesr     E
020700090105       //................................//
020800090105       //Procedure to get data from file//
020900090105       //........................ .....//
021000090105     p read_datasr     B
021100090105     D read_datasr     PI              n
021200090105      /free
021300090105        exec sql
021400090105        fetch next from ncsfile into :ncsds;
021500090105        if sqlstt <> '00000';
021600090105          return *on;
021700090105          else;
021800090105            return *off;
021900090105        ENDIF;
022000090105      /end-free
022100090105     p read_datasr     E
022200090105      //............................//
022300090105      //procedure to close the file//
022400090105      //..........................//
022500090105     p close_filesr    B
022600090105     D close_filesr    PI
022700090105      /free
022800090105
022900090105       exec sql
023000090105       close ncsfile;
023100090105
023200090105      /end-free
023300090105     p close_filesr    E
023400090105       //...........................................//
023500090105       //Procedure to writre records to the subfile//
023600090105       //.........................................//
023700090105     p write_sub       B
023800090105     D write_sub       PI
023900090105      /free
024000090105        FRANGE = RBRFRANGE;
024100090105        TRANGE = RBRTRANGE;
024200090121
024300090122        //writing description from master file to the screen//
024400090128        exec sql
024500090129        select rtrndesc into :desc## from rbgtrnmas
024600090128        where rtrnkey = :trans# and rtrnsts = 'A';
024700090105
024800090128        line# += 1;
024900090128        rrn = line#;
025000090105        write sflrec;
025100090106        *in60 = *on;
025200090105
025300090105      /end-free
025400090105     p write_sub       E
025500090105       //...........................................................//
025600090105       //if add record, writing record, else update existing record//
025700090105       //.........................................................//
025800090105     p load_data       B
025900090105     D load_data       PI
026000090105      /free
026100090105       //executing procedure to write data to file//
026200090105        write_to_file();
026300090105      /end-free
026400090105     p load_data       E
026500090105       //..............................................//
026600090105       //Procedure to write transaction to file fields//
026700090105       //............................................//
026800090105     p write_to_file   B
026900090105     D write_to_file   PI
027000090105     D hold            S              3  0
027100090114     D counter         S                   LIKE(rrn)
027200090105      /free
027300090105
027400090105       EXEC SQL
027500090129       DELETE FROM RBGRANGE WHERE RBRTRANS# = :TRANS#;
027600090105
027700090105       hold = rrn;
027800090114       for counter = 1 to hold;
027900090105       chain counter sflrec;
028000090105
028100090105       exec sql
028200090129       insert into rbgrange(rbrtrans#, rbrfrange, rbrtrange, rbrsts)
028300090121       values (:trans#, :frange, :trange, 'A');
028400090105
028500090105       ENDFOR;
028600090105      /end-free
028700090105     p write_to_file   E
028800090105      //...........................................//
028900090105      //Procedure to validate fields on the screen//
029000090105      //.........................................//
029100090105     p update_screen   B
029200090105     D update_screen   PI              n
029300090126     D ok                             1
029400090106     D hold            S              3  0
029500090114     D counter         S                   LIKE(rrn)
029600090105     D retfield        S               N
029700090105      /free
029800090105       retfield = *off;
029900090105       hold = rrn;
030000090105       for counter = 1 to hold;
030100090105       chain counter sflrec;
030200090105       if frange = *zeros;
030300090128         errmsg = get_err_msg('DIH0022');
030400090128         show_msg(errmsg);
030500090105         *in27 = *on;
030600090105         retfield = *on;
030700090105         update sflrec;
030800090105         return retfield;
030900090105       ENDIF;
031000090106
031100090105       if trange = *zeros;
031200090105         errmsg = 'please enter to range';
031300090128         show_msg(errmsg);
031400090105         *in28 = *on;
031500090105          retfield = *on;
031600090105         update sflrec;
031700090105         return retfield;
031800090105       ENDIF;
031900090106
032000090106       //checking if from range already exist within a range//
032100090106       exec sql
032200090129       select rbrgl into :temp from rbgrange where :frange >= rbrfrange
032300090128       and :frange <= rbrtrange and rbrsts = 'A';
032400090106
032500090106       if sqlstt = '00000';
032600090115        exec sql
032700090129        select rbrgl into :temp from rbgrange where :frange = rbrfrange
032800090128        and :trange = rbrtrange and rbrsts = 'A';
032900090115
033000090115         if sqlstt <> '00000';
033100090115
033200090106         errmsg = 'from range is already within a range';
033300090128         show_msg(errmsg);
033400090106         *in27 = *on;
033500090106         retfield = *on;
033600090106         update sflrec;
033700090106         return retfield;
033800090115         ENDIF;
033900090106       ENDIF;
034000090106
034100090106       if trange < frange;
034200090106         errmsg = 'to range must not be less than from range';
034300090128         show_msg(errmsg);
034400090106         *in28 = *on;
034500090106         retfield = *on;
034600090106         update sflrec;
034700090106         return retfield;
034800090106       ENDIF;
034900090106
035000090106       //checking if to range already exist within a range//
035100090106       exec sql
035200090129       select rbrgl into :temp from rbgrange where :trange <= rbrtrange
035300090128       and :trange >= rbrtrange and rbrsts = 'A';
035400090106
035500090106       if sqlstt = '00000';
035600090115        exec sql
035700090129        select rbrgl into :temp from rbgrange where :frange = rbrfrange
035800090128        and :trange = rbrtrange and rbrsts = 'A';
035900090115
036000090115         if sqlstt <> '00000';
036100090115
036200090106         errmsg = 'to range is already within a range';
036300090128         show_msg(errmsg);
036400090106         *in28 = *on;
036500090106         retfield = *on;
036600090106         update sflrec;
036700090106         return retfield;
036800090115         ENDIF;
036900090106       ENDIF;
037000090106
037100090105       endfor;
037200090105       update sflrec;
037300090123       ok = 'Y';
037400090105       return retfield;
037500090105      /end-free
037600090105     p update_screen   E
037700090105       //...........................//
037800090105       //procedure to delete record//
037900090105       //.........................//
038000090105     p del_record      B
038100090105     D del_record      PI
038200090105      /free
038300090105
038400090105       //using sql to delete record from file//
038500090105        exec sql
038600090129        update rbgrange
038700090105        set
038800090105        rbrsts = 'D'
038900090121        where rbrtrans# = :trans# and rbrsts = 'A';
039000090105        commit;
039100090105
039200090105      /end-free
039300090105     p del_record      E
039400090122      //..............................//
039500090122      //procedure to clear the screen//
039600090122      //............................//
039700090122     p new_line        B
039800090122     D new_line        PI
039900090122      /free
040000090122
040100090122        FRANGE = 0;
040200090122        TRANGE = 0;
040300090122
040400090128        line# += 1;
040500090128        rrn = line#;
040600090122        write sflrec;
040700090122
040800090122      /end-free
040900090122     p new_line        E
041000090105       //..............................//
041100090105       //procedure to clear the screen//
041200090105       //............................//
041300090105     p clr_screen      B
041400090105     D clr_screen      PI
041500090105      /free
041600090123        trans# = *zeros;
041700090128        desc## = *blanks;
041800090105        frange = *zeros;
041900090105        trange = *zeros;
042000090121        sel = *blanks;
042100090105      /end-free
042200090105     p clr_screen      E
042300090128       //.............................//
042400090128       //procedure to perform delete //
042500090128       //...........................//
042600090128     p performdelete   B
042700090128     D performdelete   PI
042800090128      /free
042900090128
043000090128        EXEC SQL
043100090129        DELETE FROM RBGRANGE WHERE RBRFRANGE = :FRANGE AND RBRSTS = 'A';
043200090128
043300090128        //close the file//
043400090128         close_filesr();
043500090128      /end-free
043600090128     p performdelete   E
043700090128       //................................//
043800090128       //procedure to clear the subfile //
043900090128       //..............................//
044000090128     p clr_subfile     B
044100090128     D clr_subfile     PI
044200090128      /free
044300090128
044400090128        *in33 = *on;
044500090128         write sflctl;
044600090128         *in33 = *off;
044700090128         reset rrn;
044800090128         line# = 0;
044900090128      /end-free
045000090128     p clr_subfile     E
045100090105       //............................................//
045200090105       //procedure to initialsie the main variables //
045300090105       //..........................................//
045400090105     p aainit          B
045500090105     D aainit          PI
045600090105      /free
045700090105         coname = get_coname;
045800090105         @date = get_sys_date(datal);
045900090105         @title = Arr(1);
046000090105         //clearing the subfile//
046100090105         *in33 = *on;
046200090105         write sflctl;
046300090105         *in33 = *off;
046400090105         reset rrn;
046500090105      /end-free
046600090105     p aainit          E
046700090105**
046800090129      Transit Account Range Maint
