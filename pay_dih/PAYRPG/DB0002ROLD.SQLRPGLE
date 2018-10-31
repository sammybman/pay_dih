000100100215     H  dftactgrp(*no) actgrp(*CALLER)  bnddir('DISDIR')
000200000000      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300000000      //
000400100215     Fbnkpysuml9IF   E           K DISK
000500100215     Fshrpysuml9IF   E           K DISK
000600100215     Fcuspysuml9IF   E           K DISK
000700000000      // SubProcedure PROTOTYPE
000800090824      /COPY GENPR
000900000000     D                SDS
001000000000     D  @PGM             *PROC
001100000000     D  @USER                254    263
001200090106
001300090107     D payelcDS      e ds                  extname(payelctrn) inz
001400091012     D                                     qualified  prefix('P':1)
001500090107
001600091012     D shrelcDS      e ds                  extname(shrelctrn) inz
001700091012     D                                     qualified  prefix('P':1)
001800100209     D cuselcDS      e ds                  extname(cuselctrn) inz
001900100209     D                                     qualified  prefix('P':1)
002000000000      //
002100100927     D cbtype          s              2  0 inz(4)
002200100215     D crddate         S               D
002300100927     D bnktype         s              2  0 inz(9)
002400091012     D amount          s             12  2
002500000000     D batch           s              4  0
002600000000     D count           s              6  0
002700031210     D procesInd       s               n
002800000000     D totpay          s             11  2
002900090106     **
003000090106     D from            S             50
003100090106     D to              S            200
003200090106     D replyTo         S             50
003300090106     D subject         S             50
003400090106     D filename        S            100
003500100927     D filenamer       S             30
003600090905     D filenameOUT     S            100
003700090106     D text            S           9999
003800090107     D pos             S              3  0
003900090107     D start           S              3  0  inz(1)
004000090107     D length          S              3  0
004100090106
004200090106     D maillistDS    E DS                  extname(dismaillst)
004300091010
004400091010     D*-----------------------------------------
004500091010     D* Prototype for procedure: Main_pgm
004600091010     D*-----------------------------------------
004700100927     D  main_pgm       pr                  extpgm('DB0002R')
004800091010     D   bnk_type                     2  0
004900091010     D   countin                      6  0
005000091010     D   eof                           n
005100100212     D   credtdate                    8  0
005200091010     D   err_ind                      1
005300091010     D   btchlower                    6  0
005400091010     D   btchupper                    6  0
005500091010     D   bankaccount                 25
005600091010     D   origin                       3
005700091010
005800091010     D  main_pgm       pi
005900091010     D   bnk_type                     2  0
006000091010     D   countin                      6  0
006100091010     D   eof                           n
006200100212     D   credtdate                    8  0
006300100215     D   err_ind                      1
006400100215     D   btchlower                    6  0
006500100215     D   btchupper                    6  0
006600100215     D   bankaccount                 25
006700100215     D   origin                       3
006800100215     D* Prototype for procedure: get_credit_date
006900100215     D*------------------------------------------
007000100215     D get_credit_date...
007100100215     D                 PR             6  0
007200100215     D dateCrd                        6  0 const
007300100215     D dateAcc                        6  0 const
007400091010
007500000000
007600000000      /FREE
007700100212           //if countin < tot_Recs;
007800100212           //return;
007900100212           //endif;
008000090106
008100090107         //using sql statement to check for record in file//
008200091010         select;
008300091010           // payroll
008400091010           when origin = 'PAY';
008500091010             exec sql
008600091010                select * into :payelcDS from payelctrn
008700100215                  where petype = :cbtype;
008800100927
008900100215            setll bnktype payfmt;
009000100215            reade bnktype payfmt;
009100100215
009200100215               paycrd = get_credit_date(paycrd:paydate);
009300100215               crddate = %date(paycrd:*dmy);
009400091010           // share dividend
009500091010           when origin = 'SHR';
009600091010             exec sql
009700091012                select * into :shrelcDS from shrelctrn
009800100215                  where setype = :cbtype;
009900091012
010000091012             eval-corr payelcDS = shrelcDS;
010100100215            setll bnktype sbkfmt;
010200100215            reade bnktype sbkfmt;
010300100215
010400100629               sbkcrd = get_credit_date(sbkcrd:sbkdate);
010500100215               crddate = %date(sbkcrd:*dmy);
010600100209           // customer discounts
010700100209           when origin = 'CRD';
010800100209             exec sql
010900100209                select * into :cuselcDS from cuselctrn
011000100215                  where cetype = :cbtype;
011100100209
011200100209             eval-corr payelcDS = cuselcDS;
011300100215            setll bnktype cbkfmt;
011400100215            reade bnktype cbkfmt;
011500100215
011600100629               cbkcrd = get_credit_date(cbkcrd:cbkdate);
011700100215               crddate = %date(cbkcrd:*dmy);
011800091010         endsl;
011900091010
012000091010         // get mailing information
012100090107         exec sql
012200090107              select * into :maillistds from dismaillst
012300090107                where dmailorg =:origin
012400091012                     and dmailobj = :payelcDS.pevpgmid;
012500090107
012600090107         if sqlstt <> '00000';
012700090107         endif;
012800090106
012900090106
013000090107         from = 'SysAdmin@' + 'Banksdih.com';
013100090107         //to   = %trimr(%xlate(';':' ':dmaillist));
013200090107         replyTo = *blanks;
013300100215         subject = payelcDS.pesubject + ' credit date :' + %char(crddate);
013400090106
013500091012         text = payelcDS.petext;
013600090106
013700091012         filename = payelcDS.pefilenm;
013800100927
013900100927              filenamer =  %subst(filename:1:27);
014000090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
014100090905                         'zip';
014200090905
014300090905         // Zip and encrypt attachment file
014400100215          Zip_files(filename:filenameOUT:subject);
014500090905
014600090106
014700090107         DOW 0 = 0;
014800090107            pos = %scan(';':%trim(dmaillist):START);
014900090107
015000090107            if pos > 0;
015100090107
015200090107               length = pos-start;
015300090107
015400090107               //returns from the starting position of dmaillist to the
015500090107               //position just before the semi colon
015600090107               to = %subst(dmaillist:start:length);
015700090107               start = pos + 1;
015800090107
015900090107               //send email //
016000090824               Email_handler(from:to:replyto:subject:text:'1':
016100091016                      filename);
016200090107
016300090107               if start >= %len(dmaillist);
016400090107                 leave;
016500090107               Endif;
016600090107
016700090107              else; // search code not found
016800090107               leave;
016900090107            endif;
017000090107         enddo;
017100090106
017200090107
017300090824         // update payroll transaction file
017400091010         select;
017500091010           // payroll
017600091010           when origin = 'PAY';
017700091010             exec sql
017800091010               update bnkpysum
017900091010                  set paysts = 'P' where paydiv = :bnk_type
018000091010                    and paysts = 'V';
018100091010           // Share Dividend
018200091010           when origin = 'SHR';
018300091010             exec sql
018400091010               update shrpysum
018500091010                  set sbksts = 'P' where sbkdiv = :bnk_type
018600091010                    and sbksts = 'V';
018700100209           when origin = 'CRD';
018800100209             exec sql
018900100209               update cuspysum
019000100209                  set cbksts = 'P' where cbkdiv = :bnk_type
019100100209                    and cbksts = 'V';
019200091010         endsl;
019300090107
019400090107         commit;
019500090106
019600090107         // terminate program
019700090107         count = *zeros;
019800090107         eof = *on;
019900090107         *inlr = *ON;
020000090107         return;
020100100215
020200100215      /end-free
020300100215     P*-----------------------------------------
020400100215     P* Procedure name: get_credit_date
020500100215     P* Purpose:        get_credit_date
020600100215     P* Returns:        0
020700100215     P* Parameter:      0
020800100215     P*-----------------------------------------
020900100215     P get_credit_date...
021000100215     P                 B
021100100215     D get_credit_date...
021200100215     D                 PI             6  0
021300100215     D dateIn                         6  0 const
021400100215     D dateAcc                        6  0 const
021500100215
021600100215     D olddateISO      s              8  0
021700100215     D newdate         s              6  0
021800100215      /free
021900100215
022000100215             // DO NOT allow Credit date falling on a weekend
022100100215             //.... day 6 or 7
022200100215             //olddateISO = %dec(%date(dateIn:*dmy):*iso);
022300100215               olddateISO = %dec(%date(dateIn:*dmy):*iso);
022400100215             if dayofweek(olddateISO) = 6
022500100215               or dayofweek(olddateISO) = 7;
022600100215             //... etc.
022700100215             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
022800100215               newdate = dateAcc;
022900100215              else;
023000100215               newdate = dateIn;
023100100215             endif;
023200100215
023300100215          return newdate;
023400100215
023500100215      /end-free
023600100215     P get_credit_date...
023700100215     P                 E
023800100215
