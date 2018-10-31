000100100215     H  dftactgrp(*no) actgrp(*CALLER)  bnddir('DISDIR')
000200131008      //****** TRANSFER TO BANK ACCOUNT  --- C.Williams******
000300000000      //
000400110126     Fgenpysuml9IF   E           K DISK
000500000000      // SubProcedure PROTOTYPE
000600131008      /COPY qcpysrc,GENPR
000700000000     D                SDS
000800000000     D  @PGM             *PROC
000900140910     D  @USER                358    367
001000090106
001100090107     D payelcDS      e ds                  extname(payelctrn) inz
001200091012     D                                     qualified  prefix('P':1)
001300090107
001400091012     D shrelcDS      e ds                  extname(shrelctrn) inz
001500091012     D                                     qualified  prefix('P':1)
001600100209     D cuselcDS      e ds                  extname(cuselctrn) inz
001700100209     D                                     qualified  prefix('P':1)
001800000000      //
001900140910     D errmsg#         S             40
002000100927     D cbtype          s              2  0 inz(4)
002100100215     D crddate         S               D
002200100927     D bnktype         s              2  0 inz(9)
002300091012     D amount          s             12  2
002400000000     D batch           s              4  0
002500000000     D count           s              6  0
002600031210     D procesInd       s               n
002700000000     D totpay          s             11  2
002800090106     **
002900090106     D from            S             50
003000090106     D to              S            200
003100090106     D replyTo         S             50
003200090106     D subject         S             50
003300090106     D filename        S            100
003400100927     D filenamer       S             30
003500090905     D filenameOUT     S            100
003600090106     D text            S           9999
003700090107     D pos             S              3  0
003800090107     D start           S              3  0  inz(1)
003900090107     D length          S              3  0
004000090106
004100090106     D maillistDS    E DS                  extname(dismaillst)
004200091010
004300091010     D*-----------------------------------------
004400091010     D* Prototype for procedure: Main_pgm
004500091010     D*-----------------------------------------
004600100927     D  main_pgm       pr                  extpgm('DB0002R')
004700091010     D   bnk_type                     2  0
004800091010     D   countin                      6  0
004900091010     D   eof                           n
005000100212     D   credtdate                    8  0
005100091010     D   err_ind                      1
005200091010     D   btchlower                    6  0
005300091010     D   btchupper                    6  0
005400091010     D   bankaccount                 25
005500091010     D   origin                       3
005600091010
005700091010     D  main_pgm       pi
005800091010     D   bnk_type                     2  0
005900091010     D   countin                      6  0
006000091010     D   eof                           n
006100100212     D   credtdate                    8  0
006200100215     D   err_ind                      1
006300100215     D   btchlower                    6  0
006400100215     D   btchupper                    6  0
006500100215     D   bankaccount                 25
006600100215     D   origin                       3
006700100215     D* Prototype for procedure: get_credit_date
006800100215     D*------------------------------------------
006900100215     D get_credit_date...
007000100215     D                 PR             6  0
007100100215     D dateCrd                        6  0 const
007200100215     D dateAcc                        6  0 const
007300091010
007400000000
007500000000      /FREE
007600100212           //if countin < tot_Recs;
007700100212           //return;
007800100212           //endif;
007900090106
008000090107         //using sql statement to check for record in file//
008100091010         select;
008200091010           // payroll
008300091010           when origin = 'PAY';
008400091010             exec sql
008500091010                select * into :payelcDS from payelctrn
008600100215                  where petype = :cbtype;
008700100927
008800091010           // share dividend
008900091010           when origin = 'SHR';
009000091010             exec sql
009100091012                select * into :shrelcDS from shrelctrn
009200100215                  where setype = :cbtype;
009300091012
009400091012             eval-corr payelcDS = shrelcDS;
009500100209           // customer discounts
009600100209           when origin = 'CRD';
009700100209             exec sql
009800100209                select * into :cuselcDS from cuselctrn
009900100215                  where cetype = :cbtype;
010000100209
010100100209             eval-corr payelcDS = cuselcDS;
010200091010         endsl;
010300091010
010400110126            setll (origin:bnktype) payfmt;
010500110126            reade (origin:bnktype) payfmt;
010600110126
010700110126               paycrd = get_credit_date(paycrd:paydate);
010800110126               crddate = %date(paycrd:*dmy);
010900091010         // get mailing information
011000090107         exec sql
011100090107              select * into :maillistds from dismaillst
011200090107                where dmailorg =:origin
011300140605                     and dmailname = :payelcDS.pevpgmid;
011400090107
011500090107         if sqlstt <> '00000';
011600140910           dsply payelcDS.pevpgmid;
011700140910           dsply origin;
011800090107         endif;
011900090106
012000090106
012100090107         from = 'SysAdmin@' + 'Banksdih.com';
012200090107         //to   = %trimr(%xlate(';':' ':dmaillist));
012300090107         replyTo = *blanks;
012400100215         subject = payelcDS.pesubject + ' credit date :' + %char(crddate);
012500090106
012600091012         text = payelcDS.petext;
012700090106
012800091012         filename = payelcDS.pefilenm;
012900100927
013000100927              filenamer =  %subst(filename:1:27);
013100090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
013200090905                         'zip';
013300090905
013400090905         // Zip and encrypt attachment file
013500100215          Zip_files(filename:filenameOUT:subject);
013600090905
013700090106
013800090107         DOW 0 = 0;
013900090107            pos = %scan(';':%trim(dmaillist):START);
014000090107
014100090107            if pos > 0;
014200090107
014300090107               length = pos-start;
014400090107
014500090107               //returns from the starting position of dmaillist to the
014600090107               //position just before the semi colon
014700090107               to = %subst(dmaillist:start:length);
014800090107               start = pos + 1;
014900090107
015000090107               //send email //
015100090824               Email_handler(from:to:replyto:subject:text:'1':
015200091016                      filename);
015300090107
015400090107               if start >= %len(dmaillist);
015500090107                 leave;
015600090107               Endif;
015700090107
015800090107              else; // search code not found
015900090107               leave;
016000090107            endif;
016100090107         enddo;
016200090106
016300090107
016400090824         // update payroll transaction file
016500091010             exec sql
016600110126               update genpysum
016700091010                  set paysts = 'P' where paydiv = :bnk_type
016800110126                    and paysts = 'V' and payorg = :origin;
016900090107
017000090107         commit;
017100090106
017200090107         // terminate program
017300090107         count = *zeros;
017400090107         eof = *on;
017500090107         *inlr = *ON;
017600090107         return;
017700100215
017800100215      /end-free
017900100215     P*-----------------------------------------
018000100215     P* Procedure name: get_credit_date
018100100215     P* Purpose:        get_credit_date
018200100215     P* Returns:        0
018300100215     P* Parameter:      0
018400100215     P*-----------------------------------------
018500100215     P get_credit_date...
018600100215     P                 B
018700100215     D get_credit_date...
018800100215     D                 PI             6  0
018900100215     D dateIn                         6  0 const
019000100215     D dateAcc                        6  0 const
019100100215
019200100215     D olddateISO      s              8  0
019300100215     D newdate         s              6  0
019400100215      /free
019500100215
019600100215             // DO NOT allow Credit date falling on a weekend
019700100215             //.... day 6 or 7
019800100215             //olddateISO = %dec(%date(dateIn:*dmy):*iso);
019900100215               olddateISO = %dec(%date(dateIn:*dmy):*iso);
020000100215             if dayofweek(olddateISO) = 6
020100100215               or dayofweek(olddateISO) = 7;
020200100215             //... etc.
020300100215             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
020400100215               newdate = dateAcc;
020500100215              else;
020600100215               newdate = dateIn;
020700100215             endif;
020800100215
020900100215          return newdate;
021000100215
021100100215      /end-free
021200100215     P get_credit_date...
021300100215     P                 E
021400100215
