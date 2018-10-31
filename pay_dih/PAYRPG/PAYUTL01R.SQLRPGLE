000100151002       Ctl-Opt DFTACTGRP(*NO) bnddir('DISDIR') actgrp(*NEW);
000200150930       Ctl-Opt DATFMT(*ISO);
000300150930     F** Payroll Utility #1 - Adj. taxable gross figures
000400960713     F** Programmer S.BRITTON
000500151002     **
000600151002     **    ChgPgm      Pgm( PAYUTL01R )
000700151002     **                RmvObs( *ALL )
000800151002     **
000900150930
001000150930      // Payroll PROTOTYPES
001100111002      /copy QCPYSRC,genPR
001200150930
001300150930
001400150930      // Entry parameter definition
001500150930       Dcl-Pr  payutl01r   EXTPGM('PAYUTL01R');
001600151002         YearOfIncome    Packed(4)       const;
001700150930         Paytype         Packed(2)       const;
001800151002         StartDate       Char(7)         const;
001900151002         EndDate         Char(7)         const;
002000150930       End-Pr;
002100150930
002200150930       Dcl-Pi  payutl01r;
002300151002         YearOfIncome    Packed(4)       const;
002400150930         Paytype         Packed(2)       const;
002500151002         StartDate       Char(7)         const;
002600151002         EndDate         Char(7)         const;
002700150930       End-Pi;
002800150930
002900150930
003000150930
003100150930       Dcl-Ds *n PSDS;
003200150930         @pgm            *proc;
003300150930         @user           Char(10)        Pos(358);
003400150930         @parms          *parms;
003500150930       End-Ds;
003600150930
003700150930
003800010109     Dyrend_date       c                   const('31.12.')
003900010109     Dyrstr_date       c                   const('01.01.')
004000150930
004100150930
004200021101     D arr             S             40    dim(1) ctdata perrcd(1)
004300150930     D amt             s                   like(ytdamt)
004400151002     D gro_type        s              2  0 inz(00)
004500151002     D StartPayDate    s              8  0
004600151002     D EndPayDate      s              8  0
004700150930
004800150930     D ytdDS         e DS                  extname(payeytd)
004900150930
005000151001       //// authority check
005100151001       //monitor;
005200151001       //  authority_check('PAY':@pgm);
005300151001       //on-error;
005400151001       //  show_msg(get_err_msg('DIH0054'));
005500151001       //  *inlr = *on;
005600151001       //  return;
005700151001       //endmon;
005800150930
005900150930
006000151002         StartPayDate = %dec( %date(startdate:*CYMD0) : *iso);
006100151002         EndPayDate = %dec( %date(enddate:*CYMD0) : *iso);
006200151003
006300150930         open_file();
006400150930
006500150930         dow not read_data();
006600150930
006700151002           amt = sum_PAYTRANS(emp : Paytype : StartPayDate : EndPayDate );
006800150930
006900150930           exec sql
007000150930             update payeytd set ytdamt = (ytdamt - :amt)
007100151003              where CURRENT of YTDFILE;
007200150930
007300150930         enddo;
007400150930
007500151011         exec sql
007600151011            insert into payutl01LG values (:YearOfIncome, :Paytype,
007700151011                    :Startdate, :Enddate, :@user, current timestamp);
007800151011
007900150930         commit;
008000150930
008100150930         close_file();
008200151002         *inlr = *on;
008300150930
008400041107
008500041107     C     *inzsr        begsr
008600150930
008700151001       //   get_session_info('PAY':errmsg:@pgm);
008800151001       //   if errmsg <> *blanks;
008900151001       //     show_msg(errmsg);
009000151001       //     *inlr = *on;
009100151001       //     return;
009200151001       //   endif;
009300111002
009400041107     C                   endsr
009500020326
009600050422
009700151002       //.........................................//
009800151002       //Procedure to set up file position cursor//
009900151002       //.......................................//
010000150930     P open_file       B
010100150930     D open_file       PI
010200150930
010300150930
010400150930       exec sql
010500151003         declare ytdfile cursor for select * from payeytd
010600151002           where  year = :YearofIncome
010700151002            and ytdtype = :Gro_type  and ytdorg = 'PAY';
010800150930
010900150930       exec sql
011000151003         open ytdfile;
011100150930
011200150930     P open_file       E
011300150930
011400150930      //................................//
011500150930      //Procedure to get data from file//
011600150930      //..............................//
011700150930     P read_data       B
011800150930     D read_data       PI              n
011900150930
012000150930        exec sql
012100151003          fetch next from ytdfile into :ytdDS;
012200150930
012300150930        if sqlstt = '02000';
012400150930          return *on;
012500150930         else;
012600150930          return *off;
012700150930        endif;
012800150930
012900150930     P read_data       E
013000150930
013100150930
013200150930      //............................//
013300150930      //Procedure to close the file//
013400150930      //..........................//
013500150930     p close_file      B
013600150930     D close_file      PI
013700150930
013800150930        exec sql
013900151003          close ytdfile;
014000150930
014100150930     P close_file      E
014200150930
014300151002
014400151002      //......................................//
014500151002      //Procedure to Sum payroll Transactions //
014600151002      //......................................//
014700151002     p Sum_PAYTRANS    B
014800151002     D Sum_PAYTRANS    PI            11  2
014900151002     D  Empno                         6  0 Const
015000151002     D  Paytype                       2  0 Const
015100151002     D  StrPaydate                    8  0 Const
015200151002     D  EndPaydate                    8  0 Const
015300151002
015400151002       DCL-S Total      Packed(11:2);
015500151002
015600151002           exec sql
015700151002             select sum(ptrnamt) into :Total from paytrans
015800151002                where ptrnemp = :Empno and ptrnndate >= :StrPayDate
015900151002                  and ptrnndate <= :EndPaydate and ptrntcode = :Paytype;
016000151002
016100151002           return  Total;
016200151002
016300151002     P Sum_PAYTRANS    E
016400151002
016500011008**
016600150930       Adjust Payroll YTD. Taxable gross amount
