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
002100100209     D cbtype          s              2  0 inz(1)
002200100215     D crddate         S               D
002300100215     D bnktype         s              2  0 inz(6)
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
004600100209     D  main_pgm       pr                  extpgm('CB0002R')
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
008700100215            setll bnktype payfmt;
008800100215            reade bnktype payfmt;
008900100215
009000100215               paycrd = get_credit_date(paycrd:paydate);
009100100215               crddate = %date(paycrd:*dmy);
009200091010           // share dividend
009300091010           when origin = 'SHR';
009400091010             exec sql
009500091012                select * into :shrelcDS from shrelctrn
009600100215                  where setype = :cbtype;
009700091012
009800091012             eval-corr payelcDS = shrelcDS;
009900100215            setll bnktype sbkfmt;
010000100215            reade bnktype sbkfmt;
010100100215
010200100629               sbkcrd = get_credit_date(sbkcrd:sbkdate);
010300100215               crddate = %date(sbkcrd:*dmy);
010400100209           // customer discounts
010500100209           when origin = 'CRD';
010600100209             exec sql
010700100209                select * into :cuselcDS from cuselctrn
010800100215                  where cetype = :cbtype;
010900100209
011000100209             eval-corr payelcDS = cuselcDS;
011100100215            setll bnktype cbkfmt;
011200100215            reade bnktype cbkfmt;
011300100215
011400100629               cbkcrd = get_credit_date(cbkcrd:cbkdate);
011500100215               crddate = %date(cbkcrd:*dmy);
011600091010         endsl;
011700091010
011800091010         // get mailing information
011900090107         exec sql
012000090107              select * into :maillistds from dismaillst
012100090107                where dmailorg =:origin
012200140605                     and dmailname = :payelcDS.pevpgmid;
012300090107
012400090107         if sqlstt <> '00000';
012500090107         endif;
012600090106
012700090106
012800090107         from = 'SysAdmin@' + 'Banksdih.com';
012900090107         //to   = %trimr(%xlate(';':' ':dmaillist));
013000090107         replyTo = *blanks;
013100100215         subject = payelcDS.pesubject + ' credit date :' + %char(crddate);
013200090106
013300091012         text = payelcDS.petext;
013400090106
013500091012         filename = payelcDS.pefilenm;
013600090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
013700090905                         'zip';
013800090905
013900090905         // Zip and encrypt attachment file
014000100215          Zip_files(filename:filenameOUT:subject);
014100090905
014200090106
014300090107         DOW 0 = 0;
014400090107            pos = %scan(';':%trim(dmaillist):START);
014500090107
014600090107            if pos > 0;
014700090107
014800090107               length = pos-start;
014900090107
015000090107               //returns from the starting position of dmaillist to the
015100090107               //position just before the semi colon
015200090107               to = %subst(dmaillist:start:length);
015300090107               start = pos + 1;
015400090107
015500090107               //send email //
015600090824               Email_handler(from:to:replyto:subject:text:'1':
015700091016                      filename);
015800090107
015900090107               if start >= %len(dmaillist);
016000090107                 leave;
016100090107               Endif;
016200090107
016300090107              else; // search code not found
016400090107               leave;
016500090107            endif;
016600090107         enddo;
016700090106
016800090107
016900090824         // update payroll transaction file
017000091010         select;
017100091010           // payroll
017200091010           when origin = 'PAY';
017300091010             exec sql
017400091010               update bnkpysum
017500091010                  set paysts = 'P' where paydiv = :bnk_type
017600091010                    and paysts = 'V';
017700091010           // Share Dividend
017800091010           when origin = 'SHR';
017900091010             exec sql
018000091010               update shrpysum
018100091010                  set sbksts = 'P' where sbkdiv = :bnk_type
018200091010                    and sbksts = 'V';
018300100209           when origin = 'CRD';
018400100209             exec sql
018500100209               update cuspysum
018600100209                  set cbksts = 'P' where cbkdiv = :bnk_type
018700100209                    and cbksts = 'V';
018800091010         endsl;
018900090107
019000090107         commit;
019100090106
019200090107         // terminate program
019300090107         count = *zeros;
019400090107         eof = *on;
019500090107         *inlr = *ON;
019600090107         return;
019700100215
019800100215      /end-free
019900100215     P*-----------------------------------------
020000100215     P* Procedure name: get_credit_date
020100100215     P* Purpose:        get_credit_date
020200100215     P* Returns:        0
020300100215     P* Parameter:      0
020400100215     P*-----------------------------------------
020500100215     P get_credit_date...
020600100215     P                 B
020700100215     D get_credit_date...
020800100215     D                 PI             6  0
020900100215     D dateIn                         6  0 const
021000100215     D dateAcc                        6  0 const
021100100215
021200100215     D olddateISO      s              8  0
021300100215     D newdate         s              6  0
021400100215      /free
021500100215
021600100215             // DO NOT allow Credit date falling on a weekend
021700100215             //.... day 6 or 7
021800100215             //olddateISO = %dec(%date(dateIn:*dmy):*iso);
021900100215               olddateISO = %dec(%date(dateIn:*dmy):*iso);
022000100215             if dayofweek(olddateISO) = 6
022100100215               or dayofweek(olddateISO) = 7;
022200100215             //... etc.
022300100215             //newdate = %dec(%date(olddateISO:*dmy)+ %days(1));
022400100215               newdate = dateAcc;
022500100215              else;
022600100215               newdate = dateIn;
022700100215             endif;
022800100215
022900100215          return newdate;
023000100215
023100100215      /end-free
023200100215     P get_credit_date...
023300100215     P                 E
023400100215
