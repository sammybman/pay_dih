000100090106     H DATEDIT(*DMY) dftactgrp(*no) actgrp(*CALLER)
000200120418      //****** TRANSFER TO BANK ACCOUNT  --- C.WILLIAMS *****
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
001900120418     D Sbtype          s              2  0 inz(3)
002000091012     D amount          s             12  2
002100000000     D batch           s              4  0
002200000000     D count           s              6  0
002300031210     D procesInd       s               n
002400000000     D totpay          s             11  2
002500090106     **
002600090106     D from            S             50
002700090106     D to              S            200
002800090106     D replyTo         S             50
002900090106     D subject         S             50
003000090106     D filename        S            100
003100090905     D filenameOUT     S            100
003200090106     D text            S           9999
003300090107     D pos             S              3  0
003400090107     D start           S              3  0  inz(1)
003500090107     D length          S              3  0
003600090106
003700090106     D maillistDS    E DS                  extname(dismaillst)
003800091010
003900091010     D*-----------------------------------------
004000091010     D* Prototype for procedure: Main_pgm
004100091010     D*-----------------------------------------
004200101109     D  main_pgm       pr                  extpgm('SB0002R')
004300091010     D   bnk_type                     2  0
004400091010     D   countin                      6  0
004500091010     D   eof                           n
004600091010     D   tot_recs                     6  0
004700091010     D   err_ind                      1
004800091010     D   btchlower                    6  0
004900091010     D   btchupper                    6  0
005000091010     D   bankaccount                 25
005100091010     D   origin                       3
005200091010
005300091010     D  main_pgm       pi
005400091010     D   bnk_type                     2  0
005500091010     D   countin                      6  0
005600091010     D   eof                           n
005700091010     D   tot_recs                     6  0
005800091010     D   err_ind                      1
005900091010     D   btchlower                    6  0
006000091010     D   btchupper                    6  0
006100091010     D   bankaccount                 25
006200091010     D   origin                       3
006300091010
006400000000
006500000000      /FREE
006600090106         if countin < tot_Recs;
006700090106           return;
006800090106         endif;
006900090106
007000090107         //using sql statement to check for record in file//
007100091010         select;
007200091010           // payroll
007300091010           when origin = 'PAY';
007400091010             exec sql
007500091010                select * into :payelcDS from payelctrn
007600120418                  where petype = :SBTYPE;
007700091010
007800091010           // share dividend
007900091010           when origin = 'SHR';
008000091010             exec sql
008100091012                select * into :shrelcDS from shrelctrn
008200120418                  where setype = :SBTYPE;
008300091012
008400091012             eval-corr payelcDS = shrelcDS;
008500100908           // customer discount
008600100908           when origin = 'CRD';
008700100908             exec sql
008800100908                select * into :cuselcDS from cuselctrn
008900120418                  where cetype = :SBTYPE;
009000100908
009100100908             eval-corr payelcDS = cuselcDS;
009200100908
009300091010         endsl;
009400091010
009500091010         // get mailing information
009600091012
009700090107         exec sql
009800090107              select * into :maillistds from dismaillst
009900090107                where dmailorg =:origin
010000140605                     and dmailname = :payelcDS.pevpgmid;
010100090107
010200140911         if sqlstt <> '00000';
010300140911           dsply payelcDS.pevpgmid;
010400140911           dsply origin;
010500140911           dsply sqlstt;
010600140911         endif;
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
012500090107            pos = %scan(';':%trim(dmaillist):START);
012600090107
012700090107            if pos > 0;
012800090107
012900090107               length = pos-start;
013000090107
013100090107               //returns from the starting position of dmaillist to the
013200090107               //position just before the semi colon
013300090107               to = %subst(dmaillist:start:length);
013400090107               start = pos + 1;
013500090107
013600090107               //send email //
013700090824               Email_handler(from:to:replyto:subject:text:'1':
013800091016                      filename);
013900090107
014000090107               if start >= %len(dmaillist);
014100090107                 leave;
014200090107               Endif;
014300090107
014400090107              else; // search code not found
014500090107               leave;
014600090107            endif;
014700090107         enddo;
014800090106
014900090107
015000090824         // update payroll transaction file
015100110120         //select;
015200091010           // payroll
015300110120         //when origin = 'PAY';
015400091010             exec sql
015500110121               update genpysum
015600091010                  set paysts = 'P' where paydiv = :bnk_type
015700110121                    and paysts = 'V' and payorg = :origin;
015800090107
015900090106
016000090107         // terminate program
016100090107         count = *zeros;
016200090107         eof = *on;
016300090107         *inlr = *ON;
016400090107         return;
016500090106
