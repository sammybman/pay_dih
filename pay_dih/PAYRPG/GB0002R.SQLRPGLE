000100100215     H  dftactgrp(*no) actgrp(*CALLER)  bnddir('DISDIR')
000200000000      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300000000      //
000400110126     Fgenpysuml9IF   E           K DISK
000500000000      // SubProcedure PROTOTYPE
000600121120      /COPY qcpysrc,GENPR
000700000000     D                SDS
000800000000     D  @PGM             *PROC
000900121120     D  @USER                358    367
001000090106
001100090107     D payelcDS      e ds                  extname(payelctrn) inz
001200091012     D                                     qualified  prefix('P':1)
001300090107
001400091012     D shrelcDS      e ds                  extname(shrelctrn) inz
001500091012     D                                     qualified  prefix('P':1)
001600100209     D cuselcDS      e ds                  extname(cuselctrn) inz
001700100209     D                                     qualified  prefix('P':1)
001800000000      //
001900121120     D gbtype          s              2  0 inz(5)
002000100215     D crddate         S               D
002100121120     D bnktype         s              2  0 inz(5)
002200091012     D amount          s             12  2
002300000000     D batch           s              4  0
002400000000     D count           s              6  0
002500031210     D procesInd       s               n
002600000000     D totpay          s             11  2
002700090106     **
002800090106     D from            S             50
002900090106     D to              S            200
003000090106     D replyTo         S             50
003100090106     D subject         S             50
003200090106     D filename        S            100
003300100927     D filenamer       S             30
003400090905     D filenameOUT     S            100
003500090106     D text            S           9999
003600090107     D pos             S              3  0
003700090107     D start           S              3  0  inz(1)
003800090107     D length          S              3  0
003900090106
004000090106     D maillistDS    E DS                  extname(dismaillst)
004100091010
004200091010     D*-----------------------------------------
004300091010     D* Prototype for procedure: Main_pgm
004400091010     D*-----------------------------------------
004500121120     D  main_pgm       pr                  extpgm('GB0002R')
004600091010     D   bnk_type                     2  0
004700091010     D   countin                      6  0
004800091010     D   eof                           n
004900100212     D   credtdate                    8  0
005000091010     D   err_ind                      1
005100091010     D   btchlower                    6  0
005200091010     D   btchupper                    6  0
005300091010     D   bankaccount                 25
005400091010     D   origin                       3
005500091010
005600091010     D  main_pgm       pi
005700091010     D   bnk_type                     2  0
005800091010     D   countin                      6  0
005900091010     D   eof                           n
006000100212     D   credtdate                    8  0
006100100215     D   err_ind                      1
006200100215     D   btchlower                    6  0
006300100215     D   btchupper                    6  0
006400100215     D   bankaccount                 25
006500100215     D   origin                       3
006600100215     D* Prototype for procedure: get_credit_date
006700100215     D*------------------------------------------
006800100215     D get_credit_date...
006900100215     D                 PR             6  0
007000100215     D dateCrd                        6  0 const
007100100215     D dateAcc                        6  0 const
007200091010
007300000000
007400000000      /FREE
007500100212           //if countin < tot_Recs;
007600100212           //return;
007700100212           //endif;
007800090106
007900090107         //using sql statement to check for record in file//
008000091010         select;
008100091010           // payroll
008200091010           when origin = 'PAY';
008300091010             exec sql
008400091010                select * into :payelcDS from payelctrn
008500121120                  where petype = :gbtype;
008600100927
008700091010           // share dividend
008800091010           when origin = 'SHR';
008900091010             exec sql
009000091012                select * into :shrelcDS from shrelctrn
009100121120                  where setype = :gbtype;
009200091012
009300091012             eval-corr payelcDS = shrelcDS;
009400100209           // customer discounts
009500100209           when origin = 'CRD';
009600100209             exec sql
009700100209                select * into :cuselcDS from cuselctrn
009800121120                  where cetype = :gbtype;
009900100209
010000100209             eval-corr payelcDS = cuselcDS;
010100091010         endsl;
010200091010
010300110126            setll (origin:bnktype) payfmt;
010400110126            reade (origin:bnktype) payfmt;
010500110126
010600110126               paycrd = get_credit_date(paycrd:paydate);
010700110126               crddate = %date(paycrd:*dmy);
010800091010         // get mailing information
010900090107         exec sql
011000090107              select * into :maillistds from dismaillst
011100090107                where dmailorg =:origin
011200140605                     and dmailname = :payelcDS.pevpgmid;
011300090107
011400090107         if sqlstt <> '00000';
011500090107         endif;
011600090106
011700090106
011800090107         from = 'SysAdmin@' + 'Banksdih.com';
011900090107         //to   = %trimr(%xlate(';':' ':dmaillist));
012000090107         replyTo = *blanks;
012100100215         subject = payelcDS.pesubject + ' credit date :' + %char(crddate);
012200090106
012300091012         text = payelcDS.petext;
012400090106
012500091012         filename = payelcDS.pefilenm;
012600100927
012700100927              filenamer =  %subst(filename:1:27);
012800090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
012900090905                         'zip';
013000090905
013100090905         // Zip and encrypt attachment file
013200100215          Zip_files(filename:filenameOUT:subject);
013300090905
013400090106
013500090107         DOW 0 = 0;
013600090107            pos = %scan(';':%trim(dmaillist):START);
013700090107
013800090107            if pos > 0;
013900090107
014000090107               length = pos-start;
014100090107
014200090107               //returns from the starting position of dmaillist to the
014300090107               //position just before the semi colon
014400090107               to = %subst(dmaillist:start:length);
014500090107               start = pos + 1;
014600090107
014700090107               //send email //
014800090824               Email_handler(from:to:replyto:subject:text:'1':
014900091016                      filename);
015000090107
015100090107               if start >= %len(dmaillist);
015200090107                 leave;
015300090107               Endif;
015400090107
015500090107              else; // search code not found
015600090107               leave;
015700090107            endif;
015800090107         enddo;
015900090106
016000090107
016100090824         // update payroll transaction file
016200091010             exec sql
016300110126               update genpysum
016400091010                  set paysts = 'P' where paydiv = :bnk_type
016500110126                    and paysts = 'V' and payorg = :origin;
016600090107
016700090107         commit;
016800090106
016900090107         // terminate program
017000090107         count = *zeros;
017100090107         eof = *on;
017200090107         *inlr = *ON;
017300090107         return;
017400100215
017500100215      /end-free
017600100215     P*-----------------------------------------
017700100215     P* Procedure name: get_credit_date
017800100215     P* Purpose:        get_credit_date
017900100215     P* Returns:        0
018000100215     P* Parameter:      0
018100100215     P*-----------------------------------------
018200100215     P get_credit_date...
018300100215     P                 B
018400100215     D get_credit_date...
018500100215     D                 PI             6  0
018600100215     D dateIn                         6  0 const
018700100215     D dateAcc                        6  0 const
018800100215
018900100215     D olddateISO      s              8  0
019000100215     D newdate         s              6  0
019100100215      /free
019200100215
019300100215             // DO NOT allow Credit date falling on a weekend
019400100215             //.... day 6 or 7
019500100215             //olddateISO = %dec(%date(dateIn:*dmy):*iso);
019600100215               olddateISO = %dec(%date(dateIn:*dmy):*iso);
019700100215             if dayofweek(olddateISO) = 6
019800100215               or dayofweek(olddateISO) = 7;
019900100215             //... etc.
020000100215             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
020100100215               newdate = dateAcc;
020200100215              else;
020300100215               newdate = dateIn;
020400100215             endif;
020500100215
020600100215          return newdate;
020700100215
020800100215      /end-free
020900100215     P get_credit_date...
021000100215     P                 E
021100100215
