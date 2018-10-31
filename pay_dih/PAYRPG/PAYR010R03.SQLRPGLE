000100111027     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111027     H/define profoundui
000300090121     F**PROGRAMMER --         NASOYA CHAPMAN        --  21.01.2009
000400090128     Fpayr010d03CF   E             WORKSTN
000500090120     F                                     sfile(sflrec:rrn)
000600111027     F/IF DEFINED(WEB)
000700111027     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111027     F/ENDIF
000900090120     DARR              S             40    DIM(2) CTDATA PERRCD(1)
001000090120     Daainit           pr
001100090120     Dwrite_sub        pr
001200090120     Dopen_filesr      pr
001300090120     Dread_datasr      pr              n
001400090120     Dclose_filesr     pr
001500090120     D                SDS
001600090120     D@PGM               *PROC
001700090120     D
001800090120     D@parm              *parms
001900090120     Drrn              s              4  0 inz(0)
002000090121     DncsDS          e DS                  extname(rbgtrnmas)
002100090120     D*--------------------------------------------------
002200090129     P* Procedure name: PAYR010R03
002300090120     D* Purpose:
002400090120     D* Returns:
002500090121     D* Parameter:      KEY
002600090128     D* Parameter:      TRANS#
002700090120     D*--------------------------------------------------
002800090128     D PAYR010R03      PR
002900090121     D  KEY                           8  0
003000090128     D  TRANS#                        8  0
003100090120     D
003200090128     D PAYR010R03      PI
003300090121     D  KEY                           8  0
003400090128     D  TRANS#                        8  0
003500090128
003600111027      /copy QCPYSRC,genpr
003700090120      /free
003800090120       //executing procedure to set uo file position cursor//
003900090120       open_filesr();
004000090120       dow 0 = 0;
004100090120       //executing procedure to initialise main variables//
004200090120       aainit();
004300090120       //executing the procedure to read the data//
004400090120       dow not read_datasr();
004500090120       //executing  procedure to write to the file//
004600090121       write_sub();
004700090120       ENDDO;
004800090120       close_filesr();
004900090120       //Checking to see if any records found//
005000090120       if rrn = 0;
005100090120       //If no record found display 'no record found'//
005200090120       errmsg = 'No records Found';
005300090120       rrn +=1;
005400090120       write sflrec;
005500090120       endif;
005600090120       //Display bottom after all records have been written to screen//
005700090120        *in60 = *on;
005800090120       //leaves the loop//
005900090120       leave;
006000090120       enddo;
006100090120
006200090120       DOW 0 = 0;
006300090120        write footer;
006400090120        //Displaying the Subfile//
006500090120        exfmt sflctl;
006600090120        //check if f3 is pressed//
006700090120        if *in03;
006800090120          *inlr = *on;
006900090120          return;
007000090120        ENDIF;
007100090120
007200090120        readc sflrec;
007300090120        dow not(%eof);
007400090120        if sel = 'X';
007500090121
007600090121          KEY = KEY#;
007700090121
007800090120          return;
007900090120          else;
008000090120          sel = '';
008100090120        endif;
008200090120
008300090120        readc sflrec;
008400090120        enddo;
008500090120        leave;
008600090120        ENDDO;
008700090120      /end-free
008800090120       //...........................................//
008900090120       //Procedure to writre records to the subfile//
009000090120       //.........................................//
009100090120     p write_sub       B
009200090120     D write_sub       PI
009300090120      /free
009400090120        if @parm > 0;
009500090120          *in30 = *off;
009600090120        else;
009700090120          *in30 = *on;
009800090120        ENDIF;
009900090120
010000090121        key# = rtrnkey;
010100090121        desc# = rtrndesc;
010200090121        gl# = rtrngl;
010300090121        main# = rtrnmain;
010400090129        SEL = '';
010500090121
010600090120        rrn +=1;
010700090120        write sflrec;
010800090120      /end-free
010900090120     p write_sub       E
011000090120       //.........................................//
011100090120       //Procedure to set up file position cursor//
011200090120       //.......................................//
011300090120     p open_filesr     B
011400090120     D open_filesr     PI
011500090120      /free
011600090120
011700090120        exec sql
011800090121        declare ncsfile cursor for select * from rbgtrnmas
011900090121        where rtrnsts = 'A';
012000090120        exec sql
012100090120        open ncsfile;
012200090120
012300090120      /end-free
012400090120     p open_filesr     E
012500090120       //................................//
012600090120       //Procedure to get data from file//
012700090120       //........................ .....//
012800090120     p read_datasr     B
012900090120     D read_datasr     PI              n
013000090120      /free
013100090120        exec sql
013200090120        fetch next from ncsfile into :ncsds;
013300090120        if sqlstt <> '00000';
013400090120          return *on;
013500090120          else;
013600090120            return *off;
013700090120        ENDIF;
013800090120      /end-free
013900090120     p read_datasr     E
014000090120      //............................//
014100090120      //procedure to close the file//
014200090120      //..........................//
014300090120     p close_filesr    B
014400090120     D close_filesr    PI
014500090120      /free
014600090120
014700090120       exec sql
014800090120       close ncsfile;
014900090120
015000090120      /end-free
015100090120     p close_filesr    E
015200090120       //.......................................//
015300090120       //Procedure to initialise main variables//
015400090120       //.....................................//
015500090120     p aainit          B
015600090120     D aainit          PI
015700090120      /free
015800090120         coname = get_coname;
015900090120         @date = get_sys_date;
016000090120         @title = Arr(1);
016100090120         *in33 = *on;
016200090120         write sflctl;
016300090120         *in33 = *off;
016400090120         reset rrn;
016500090120      /end-free
016600090120     p aainit          E
016700090120**
016800090129        Transit Number Enquiry
