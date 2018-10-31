000100080313     H** PROGRAMMER - C.Cwilliams
000200131025     H bnddir('DISDIR':'PAYDIR') dftactgrp(*no) actgrp(*CALLER)
000300080313     dDDate            s               D   DATFMT(*ISO)
000400080313     dDay_No           s              1s 0 inz(0)
000500080313     ddated            s              8  0 inz(0)
000600080313     dperiod           s              6  0
000700081017     dcycle            s              2
000800131025     dcatflag          s              1
000900080313     dold_emp          s              6  0 inz(0)
001000080313     dold_date         s              8  0 inz(0)
001100080313     dNew_Date         s              8  0 inz(0)
001200080313     D open_filesr     pr
001300080313     D read_datasr     pr              n
001400080313     D close_filesr    pr
001500080313     Dpayds          E DS                  extname(paynisrdat)
001600131025      /COPY qcpysrc,GENPR
001700010305     **
001800080313     C     *ENTRY        PLIST
001900080313     C                   PARM                    PERIOD
002000081017     C                   PARM                    cycle
002100131025     C                   PARM                    catflag
002200951215     **
002300080313      /FREE
002400080313          open_filesr();
002500080313          dow not read_datasr();
002600080313          Dated = pnrndate;
002700080313          Day_No = DayofWeek(Dated);
002800080313          if Day_No <> 1;
002900080313          DDate = %date(pnrndate);
003000080313         //if not monday, change date to monday's date
003100080313          select;
003200080313          when Day_no = 2;
003300080313      /end-free
003400080313     c                   adddur    -1:*D         DDate
003500080313      /FREE
003600080313          when Day_no = 3;
003700080313      /end-free
003800080313     c                   adddur    -2:*D         DDate
003900080313      /FREE
004000080313          when Day_no = 4;
004100080313      /end-free
004200080313     c                   adddur    -3:*D         DDate
004300080313      /FREE
004400080313          when Day_no = 5;
004500080313      /end-free
004600080313     c                   adddur    -4:*D         DDate
004700080313      /FREE
004800080313          when Day_no = 6;
004900080313      /end-free
005000080313     c                   adddur    -5:*D         DDate
005100080313      /FREE
005200080313          when Day_no = 7;
005300080313          endsl;
005400080313          // convert date field to decimal for updating
005500080313          New_date = %dec(DDate:*ISO);
005600080313          // update to monday date
005700080313          EXEC SQL
005800080313          UPDATE PAYNISRDAT SET PNRNDATE = :NEW_DATE
005900080313          WHERE pnrprno = :period and pnrndate = :old_date
006000080313          and pnremp = :old_emp;
006100080313          endif;
006200080313          enddo;
006300080313          commit;
006400080313         close_filesr();
006500080313         return;
006600080313        //.........................................//
006700080313        //Procedure to set up file position cursor//
006800080313        //.......................................//
006900080313      /end-free
007000080313     p open_filesr     B
007100080313     D open_filesr     PI
007200080313      /free
007300080313
007400080313        exec sql
007500080313        declare paycsr cursor for select * from paynisrdat
007600080313        //checking to see if dept# matches req#//
007700131025        where pnrprno = :period and  pnrfcode = :cycle and
007800131025        pnrcatflg = :catflag;
007900080313        exec sql
008000080313        open paycsr;
008100080313
008200080313      /end-free
008300080313     p open_filesr     E
008400080313       //................................//
008500080313       //Procedure to get data from file//
008600080313       //..............................//
008700080313     p read_datasr     B
008800080313     D read_datasr     PI              n
008900080313      /free
009000080313        exec sql
009100080313        fetch next from paycsr into :payds;
009200080313        if sqlstt <> '00000';
009300080313          return *on;
009400080313          else;
009500080313          old_emp = pnremp;
009600080313          old_date = pnrndate;
009700080313            return *off;
009800080313        ENDIF;
009900080313      /end-free
010000080313     p read_datasr     E
010100080313       //............................//
010200080313       //Procedure to close the file//
010300080313       //..........................//
010400080313     p close_filesr    B
010500080313     D close_filesr    PI
010600080313      /free
010700080313       exec sql
010800080313       close paycsr;
010900080313      /end-free
011000080313     p close_filesr    E
