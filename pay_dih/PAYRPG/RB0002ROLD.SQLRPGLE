000100090106     H DATEDIT(*DMY) dftactgrp(*no) actgrp(*CALLER)
000200000000      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300000000      //
000400000000
000500000000      // SubProcedure PROTOTYPE
000600090824      /COPY GENPR
000700000000     D                SDS
000800000000     D  @PGM             *PROC
000900000000     D  @USER                254    263
001000090106
001100090107     D payelcDS      e ds                  extname(payelctrn) inz
001200091012     D                                     qualified  prefix('P':1)
001300090107
001400091012     D shrelcDS      e ds                  extname(shrelctrn) inz
001500091012     D                                     qualified  prefix('P':1)
001501100212     D cuselcDS      e ds                  extname(cuselctrn) inz
001502100212     D                                     qualified  prefix('P':1)
001600000000      //
001700090107     D rbtype          s              2  0 inz(2)
001800091012     D amount          s             12  2
001900000000     D batch           s              4  0
002000000000     D count           s              6  0
002100031210     D procesInd       s               n
002200000000     D totpay          s             11  2
002300090106     **
002400090106     D from            S             50
002500090106     D to              S            200
002600090106     D replyTo         S             50
002700090106     D subject         S             50
002800090106     D filename        S            100
002900090905     D filenameOUT     S            100
003000090106     D text            S           9999
003100090107     D pos             S              3  0
003200090107     D start           S              3  0  inz(1)
003300090107     D length          S              3  0
003400090106
003500090106     D maillistDS    E DS                  extname(dismaillst)
003600091010
003700091010     D*-----------------------------------------
003800091010     D* Prototype for procedure: Main_pgm
003900091010     D*-----------------------------------------
004000091010     D  main_pgm       pr                  extpgm('RB0002R')
004100091010     D   bnk_type                     2  0
004200091010     D   countin                      6  0
004300091010     D   eof                           n
004400091010     D   tot_recs                     6  0
004500091010     D   err_ind                      1
004600091010     D   btchlower                    6  0
004700091010     D   btchupper                    6  0
004800091010     D   bankaccount                 25
004900091010     D   origin                       3
005000091010
005100091010     D  main_pgm       pi
005200091010     D   bnk_type                     2  0
005300091010     D   countin                      6  0
005400091010     D   eof                           n
005500091010     D   tot_recs                     6  0
005600091010     D   err_ind                      1
005700091010     D   btchlower                    6  0
005800091010     D   btchupper                    6  0
005900091010     D   bankaccount                 25
006000091010     D   origin                       3
006100091010
006200000000
006300000000      /FREE
006400090106         if countin < tot_Recs;
006500090106           return;
006600090106         endif;
006700090106
006800090107         //using sql statement to check for record in file//
006900091010         select;
007000091010           // payroll
007100091010           when origin = 'PAY';
007200091010             exec sql
007300091010                select * into :payelcDS from payelctrn
007400091010                  where petype = :rbtype;
007500091010
007600091010           // share dividend
007700091010           when origin = 'SHR';
007800091010             exec sql
007900091012                select * into :shrelcDS from shrelctrn
008000091010                  where setype = :rbtype;
008100091012
008200091012             eval-corr payelcDS = shrelcDS;
008201100908           // customer discount
008202100908           when origin = 'CRD';
008203100908             exec sql
008204100908                select * into :cuselcDS from cuselctrn
008205100908                  where cetype = :rbtype;
008206100908
008207100908             eval-corr payelcDS = cuselcDS;
008208100908
008300091010         endsl;
008400091010
008500091010         // get mailing information
008600091012
008700090107         exec sql
008800090107              select * into :maillistds from dismaillst
008900090107                where dmailorg =:origin
009000091012                     and dmailobj = :payelcDS.pevpgmid;
009100090107
009200090107         if sqlstt <> '00000';
009300090107         endif;
009400090106
009500090106
009600090107         from = 'SysAdmin@' + 'Banksdih.com';
009700090107         //to   = %trimr(%xlate(';':' ':dmaillist));
009800090107         replyTo = *blanks;
009900091012         subject = payelcDS.pesubject + ' as at ' + %char(%date());
010000090106
010100091012         text = payelcDS.petext;
010200090106
010300091012         filename = payelcDS.pefilenm;
010400090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
010500090905                         'zip';
010600090905
010700090905         // Zip and encrypt attachment file
010800090905         Zip_files(filename:filenameOUT:subject);
010900090905
011000090106
011100090107         DOW 0 = 0;
011200090107            pos = %scan(';':%trim(dmaillist):START);
011300090107
011400090107            if pos > 0;
011500090107
011600090107               length = pos-start;
011700090107
011800090107               //returns from the starting position of dmaillist to the
011900090107               //position just before the semi colon
012000090107               to = %subst(dmaillist:start:length);
012100090107               start = pos + 1;
012200090107
012300090107               //send email //
012400090824               Email_handler(from:to:replyto:subject:text:'1':
012500091016                      filename);
012600090107
012700090107               if start >= %len(dmaillist);
012800090107                 leave;
012900090107               Endif;
013000090107
013100090107              else; // search code not found
013200090107               leave;
013300090107            endif;
013400090107         enddo;
013500090106
013600090107
013700090824         // update payroll transaction file
013800091010         select;
013900091010           // payroll
014000091010           when origin = 'PAY';
014100091010             exec sql
014200091010               update bnkpysum
014300091010                  set paysts = 'P' where paydiv = :bnk_type
014400091010                    and paysts = 'V';
014500091010           // Share Dividend
014600091010           when origin = 'SHR';
014700091010             exec sql
014800091010               update shrpysum
014900091010                  set sbksts = 'P' where sbkdiv = :bnk_type
015000091010                    and sbksts = 'V';
015001100908           when origin = 'CRD';
015002100908             exec sql
015003100908               update cuspysum
015004100908                  set cbksts = 'P' where cbkdiv = :bnk_type
015005100908                    and cbksts = 'V';
015100091010         endsl;
015200090107
015300090107         commit;
015400090106
015500090107         // terminate program
015600090107         count = *zeros;
015700090107         eof = *on;
015800090107         *inlr = *ON;
015900090107         return;
016000090106
