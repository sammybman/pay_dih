000100111027     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111027     H/define profoundui
000300090121     F**PROGRAMMER --       NASOYA CHAPMAN       -- 21.01.2009
000400090128     Fpayr010d02CF   E             WORKSTN
000500111027     F/IF DEFINED(WEB)
000600111027     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111027     F/ENDIF
000800090121     D ARR             S             40    DIM(2) CTDATA PERRCD (1)
000900090121     D aainit          pr
001000090121     D clr_screen      pr
001100090121     D load_data       pr
001200090121     D write_to_file   pr
001300090121     D update_fields   pr
001400090121     D del_record      pr
001500090121     D key             S              8  0
001600090121     D                SDS
001700090121     D  @PGM             *proc
001800090121     D*--------------------------------------------------
001900090128     P* Program name: GET_PAYR_ENQ
002000090121     D* Purpose:
002100090121     D* Returns:
002200090121     D* Parameter:      key
002300090121     D*--------------------------------------------------
002400111027     D GET_PAYR_ENQ    PR                  EXTPGM('PAYR010R03')
002500090121     D  key                           8  0
002600090121     D
002700111027      /copy QCPYSRC,genpr
002800111027      /copy QCPYSRC,glpr
002900090121      /free
003000090121       DOW 0 = 0;
003100090121       //executing the procedure to clear the fields on the screen//
003200090121       clr_screen();
003300090121       //executing the procedure to initialise the main variables//
003400090121       aainit();
003500090121
003600090121       DOW 0 = 0;
003700090121       //displaying the first screen//
003800090121       exfmt fmt1;
003900090121       //setting the indicators off//
004000090121       *in26 = *off;
004100090121       //clearing the error message field//
004200090121        errmsg = *blanks;
004300090121       //if F3 is pressed, close files and exit program
004400090121       if *in03;
004500090121         *inlr = *on;
004600090121         return;
004700090121       ENDIF;
004800090121        //call external program if F4 is pressed//
004900090121       if *in04;
005000090128         get_payr_enq(key);
005100090128
005200090121         iter;
005300090121       ENDIF;
005400090121       //validating the first screen//
005500090121       //checking to see if skill code was entered//
005600090121       if key = *zeros;
005700090121         errmsg = 'Please enter Key';
005800090121         *in26 = *on;
005900090121         iter;
006000090121       ENDIF;
006100090121       //using sql statement to check for record in the file//
006200090121       //checking to see if record exist in the file//
006300090121       exec sql
006400090121       select rtrnkey, rtrndesc, rtrngl, rtrnmain into :key, :desc, :gl, :main
006500090129       from rbgtrnmas where rtrnkey = :key and rtrnsts = 'A';
006600090121
006700090121       //Checking to see if record found//
006800090121       if sqlstt <> '00000';
006900090121       //setting mode to ADD because the requested record does not exist//
007000090121         @mode = '  ADD  ';
007100090121        else;
007200090121         //setting mode to AMEND because the requested record was found//
007300090121         @mode = ' AMEND ';
007400090121       ENDIF;
007500090121       //leaves the loop to go to the next screen
007600090121       leave;
007700090121       enddo;
007800090121       DOW 0 = 0;
007900090121       //displaying the second screen//
008000090121       exfmt fmt2;
008100090121       *in27 = *off;
008200090121       *in28 = *off;
008300090121       *in29 = *off;
008400090121       //clearing the error message field//
008500090121        errmsg = *blanks;
008600090121       //F3 = end program//
008700090121       if *in03;
008800090121         *inlr = *on;
008900090121         return;
009000090121       ENDIF;
009100090121       if *in02;
009200090121         leave;
009300090121       ENDIF;
009400090121       //Validating the second screen//
009500090121       //checking to see if description was entered//
009600090121       if desc = *blanks;
009700090121         errmsg = 'Please enter the Description';
009800090121         *in27 = *on;
009900090121         iter;
010000090121       ENDIF;
010100090121       //checking to see if general ledger was entered//
010200090121       if gl = *zeros;
010300090121         errmsg = 'Please enter General ledger';
010400090121         *in28 = *on;
010500090121         iter;
010600090121       ENDIF;
010700090121       //checking to see if main was entered//
010800090121       if main = *zeros;
010900090121         errmsg = 'Please enter main';
011000090121         *in29 = *on;
011100090121         iter;
011200090121       ENDIF;
011300090121       //checking to see if f15 is pressed//
011400090121       if *in15;
011500090121       //if f15 is pressed execute procedure to load data from screen to file fi
011600090121       load_data();
011700090121       commit;
011800090121       //return to first screen//
011900090121       leave;
012000090121       ENDIF;
012100090121       if *in20;
012200090121       //execute procedure to delete records//
012300090121       del_record();
012400090121       //return to first screen//
012500090121       leave;
012600090121       ENDIF;
012700090121       enddo;
012800090121       ENDDO;
012900090121      /end-free
013000090121       //...........................................................//
013100090121       //if add record, writing record, else update existing record//
013200090121       //.........................................................//
013300090121     p load_data       B
013400090121     D load_data       PI
013500090121      /free
013600090121       //checking text of @mode field to decide if to update or add record.
013700090121        if %trim(@mode) = 'AMEND';
013800090121       //executing procedure to update file//
013900090121       update_fields();
014000090121       else;
014100090121       //executing procedure to write to file//
014200090121        write_to_file();
014300090121       ENDIF;
014400090121      /end-free
014500090121     p load_data       E
014600090121       //........................................//
014700090121       //procedure to update data to file fields//
014800090121       //......................................//
014900090121     p update_fields   B
015000090121     D update_fields   PI
015100090121      /free
015200090121       //executing sql to update skillarea//
015300090121       exec sql
015400090129        update rbgtrnmas
015500090121       set
015600090121       rtrndesc = :desc,
015700090121       rtrngl = :gl,
015800090121       rtrnmain = :main
015900090121       where rtrnkey = :key and rtrnsts = 'A';
016000090121      /end-free
016100090121     p update_fields   E
016200090121       //..............................................//
016300090121       //Procedure to write transaction to file fields//
016400090121       //............................................//
016500090121     p write_to_file   B
016600090121     D write_to_file   PI
016700090121      /free
016800090121       exec sql
016900090129       insert into rbgtrnmas(rtrnkey, rtrndesc, rtrngl, rtrnmain,
017000090121       rtrnsts) values (:key, :desc, :gl, :main, 'A');
017100090121
017200090121      /end-free
017300090121     p write_to_file   E
017400090121       //...........................//
017500090121       //procedure to delete record//
017600090121       //.........................//
017700090121     p del_record      B
017800090121     D del_record      PI
017900090121      /free
018000090121       //using sql to delete record from file//
018100090121        exec sql
018200090129        update rbgtrnmas
018300090121        set
018400090121        rtrnsts = 'D'
018500090121        where rtrnkey = :key and rtrnsts = 'A';
018600090121        commit;
018700090121      /end-free
018800090121     p del_record      E
018900090121       //..............................//
019000090121       //procedure to clear the screen//
019100090121       //............................//
019200090121     p clr_screen      B
019300090121     D clr_screen      PI
019400090121      /free
019500090121        key = *zeros;
019600090121        desc = *blanks;
019700090121        gl = *zeros;
019800090121        main = *zeros;
019900090121      /end-free
020000090121     p clr_screen      E
020100090121       //............................................//
020200090121       //procedure to initialsie the main variables //
020300090121       //..........................................//
020400090121     p aainit          B
020500090121     D aainit          PI
020600090121      /free
020700090121         coname = get_coname;
020800090121         @date = get_sys_date();
020900090121         @title = Arr(1);
021000090121      /end-free
021100090121     p aainit          E
021200090121**
021300090129       Bank Transit Number Maint
