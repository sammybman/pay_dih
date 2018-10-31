000100090106     H DATEDIT(*DMY) dftactgrp(*no) actgrp(*CALLER)
000200000000      //****** TRANSFER TO BANK ACCOUNT  --- S.BRITTON ******
000300000000      //
000400000000
000500000000      // SubProcedure PROTOTYPE
000600120418      /COPY QCPYSRC,GENPR
000700000000     D                SDS
000800000000     D  @PGM             *PROC
000900120418     D  @USER                358    367
001000090106
001100090107     D payelcDS      e ds                  extname(payelctrn) inz
001200091012     D                                     qualified  prefix('P':1)
001300090107
001400091012     D shrelcDS      e ds                  extname(shrelctrn) inz
001500091012     D                                     qualified  prefix('P':1)
001600100212     D cuselcDS      e ds                  extname(cuselctrn) inz
001700100212     D                                     qualified  prefix('P':1)
001800000000      //
001900090107     D rbtype          s              2  0 inz(2)
002000091012     D amount          s             12  2
002100000000     D batch           s              4  0
002200000000     D count           s              6  0
002300031210     D procesInd       s               n
002400000000     D totpay          s             11  2
002500090106     **
002600140910     D errmsg#         S             40
002700140908     D from            S             50
002800090106     D to              S            200
002900090106     D replyTo         S             50
003000090106     D subject         S             50
003100090106     D filename        S            100
003200090905     D filenameOUT     S            100
003300090106     D text            S           9999
003400090107     D pos             S              3  0
003500090107     D start           S              3  0  inz(1)
003600090107     D length          S              3  0
003700090106
003800090106     D maillistDS    E DS                  extname(dismaillst)
003900091010
004000091010     D*-----------------------------------------
004100091010     D* Prototype for procedure: Main_pgm
004200091010     D*-----------------------------------------
004300091010     D  main_pgm       pr                  extpgm('RB0002R')
004400091010     D   bnk_type                     2  0
004500091010     D   countin                      6  0
004600091010     D   eof                           n
004700091010     D   tot_recs                     6  0
004800091010     D   err_ind                      1
004900091010     D   btchlower                    6  0
005000091010     D   btchupper                    6  0
005100091010     D   bankaccount                 25
005200091010     D   origin                       3
005300091010
005400091010     D  main_pgm       pi
005500091010     D   bnk_type                     2  0
005600091010     D   countin                      6  0
005700091010     D   eof                           n
005800091010     D   tot_recs                     6  0
005900091010     D   err_ind                      1
006000091010     D   btchlower                    6  0
006100091010     D   btchupper                    6  0
006200091010     D   bankaccount                 25
006300091010     D   origin                       3
006400091010
006500000000
006600000000      /FREE
006700090106         if countin < tot_Recs;
006800090106           return;
006900090106         endif;
007000090106
007100090107         //using sql statement to check for record in file//
007200091010         select;
007300091010           // payroll
007400091010           when origin = 'PAY';
007500091010             exec sql
007600091010                select * into :payelcDS from payelctrn
007700091010                  where petype = :rbtype;
007800091010
007900091010           // share dividend
008000091010           when origin = 'SHR';
008100091010             exec sql
008200091012                select * into :shrelcDS from shrelctrn
008300091010                  where setype = :rbtype;
008400091012
008500091012             eval-corr payelcDS = shrelcDS;
008600100908           // customer discount
008700100908           when origin = 'CRD';
008800100908             exec sql
008900100908                select * into :cuselcDS from cuselctrn
009000100908                  where cetype = :rbtype;
009100100908
009200100908             eval-corr payelcDS = cuselcDS;
009300100908
009400091010         endsl;
009500091010
009600091010         // get mailing information
009700091012
009800090107         exec sql
009900090107              select * into :maillistds from dismaillst
010000090107                where dmailorg =:origin
010100140605                     and dmailname = :payelcDS.pevpgmid;
010200090107
010300090107         if sqlstt <> '00000';
010400140910           dsply payelcDS.pevpgmid;
010500140910           dsply origin;
010600090107         endif;
010700090106
010800090106
010900090107         from = 'SysAdmin@' + 'Banksdih.com';
011000090107         //to   = %trimr(%xlate(';':' ':dmaillist));
011100090107         replyTo = *blanks;
011200091012         subject = payelcDS.pesubject + ' as at ' + %char(%date());
011300090106
011400091012         text = payelcDS.petext;
011500090106
011600091012         filename = payelcDS.pefilenm;
011700090905         filenameOUT = %subst(filename:1:%scan('.':filename)) +
011800090905                         'zip';
011900090905
012000090905         // Zip and encrypt attachment file
012100090905         Zip_files(filename:filenameOUT:subject);
012200090905
012300090106
012400090107         DOW 0 = 0;
012500140908
012600090107            pos = %scan(';':%trim(dmaillist):START);
012700090107
012800090107            if pos > 0;
012900090107
013000090107               length = pos-start;
013100090107
013200090107               //returns from the starting position of dmaillist to the
013300090107               //position just before the semi colon
013400090107               to = %subst(dmaillist:start:length);
013500090107               start = pos + 1;
013600090107
013700090107               //send email //
013800090824               Email_handler(from:to:replyto:subject:text:'1':
013900091016                      filename);
014000090107
014100090107               if start >= %len(dmaillist);
014200090107                 leave;
014300090107               Endif;
014400090107
014500090107              else; // search code not found
014600090107               leave;
014700090107            endif;
014800090107         enddo;
014900090106
015000090107
015100090824         // update payroll transaction file
015200091010             exec sql
015300110121               update genpysum
015400091010                  set paysts = 'P' where paydiv = :bnk_type
015500110121                    and paysts = 'V' and payorg =:origin;
015600090107
015700090107         commit;
015800090106
015900090107         // terminate program
016000090107         count = *zeros;
016100090107         eof = *on;
016200090107         *inlr = *ON;
016300090107         return;
016400090106
