000100090805     H DATEDIT(*DMY)
000200090805      //****** Email NIS Soft Copy  ******
000300090805      //****** PGMRS:  S.BRITTON, CWILLIAMS
000400000000      //
000500000000
000600000000      // SubProcedure PROTOTYPE
000700000000      //
000800000000     D                SDS
000900000000     D  @PGM             *PROC
001000131025     D  @USER                358  367
001100090106
001200090107     D payelcDS      e ds                  extname(payelctrn) inz
001300090107
001400090106     D
001500000000      //
001600090805     D rbtype          s              2  0 inz(5)
001700000000     Deof              s               n
001800090805     Dcmonth           s              9
001900090805     D yr              s              4  0
002000090805     Dmth              s              2  0
002100000000     D batch           s              4  0
002200000000     D count           s              6  0
002300031210     D procesInd       s               n
002400000000     D totpay          s             11  2
002500090805     D desc            s             20
002600090106     **
002700090805      /COPY GENPR
002800090106     D origin          S              3    inz('PAY')
002900090106     D from            S             50
003000090106     D to              S            200
003100090106     D replyTo         S             50
003200090106     D subject         S             50
003300090805     D pgmid           S             10
003400090106     D filename        S            100
003500090904     D filenameOUT     S            100
003600090106     D text            S           9999
003700090106     D rtnCode         S              3
003800090106     D rtnText         S             50
003900090107     D pos             S              3  0
004000090107     D start           S              3  0  inz(1)
004100090107     D length          S              3  0
004200090106
004300090106     D maillistDS    E DS                  extname(dismaillst)
004400090805      * disecting period to get month
004500090805      * month
004600090805
004700090805
004800000000     C     *entry        plist
004900090805     C                   parm                    pay_type          2
005000090805     C                   parm                    period            6 0
005100000000
005200000000      /FREE
005300090805          // if countin < tot_Recs;
005400090805         //  return;
005500090805         //endif;
005600090106
005700090805            mth = %int(%subst(%char(period):5));
005800090805            yr = %int(%subst(%char(period):1:4));
005900090107         //using sql statement to check for record in file//
006000090107         exec sql
006100090107              select * into :payelcDS from payelctrn
006200090107                where petype = :rbtype;
006300090107
006400090107         exec sql
006500090107              select * into :maillistds from dismaillst
006600090107                where dmailorg =:origin
006700140605                     and dmailname = :pevpgmid;
006800090107
006900090107         if sqlstt <> '00000';
007000090107         endif;
007100090106
007200090106
007300090107         from = 'SysAdmin@' + 'Banksdih.com';
007400090107         //to   = %trimr(%xlate(';':' ':dmaillist));
007500090805         exec sql
007600090805              select cycdesc into :desc from paycycd
007700090805                where cyccode =:pay_type;
007800090805
007900090107         replyTo = *blanks;
008000090805
008100090904         cmonth = get_month(mth);
008200090805         subject = %trim(desc) + ' '+%trim(pesubject)+' for ' +%trim(cmonth);
008300090904         subject = %trim(subject) + %editc(yr:'3');
008400090106
008500090107         text = petext;
008600090106
008700090904         filename = pefilenm;
008800090904         filenameOUT = %subst(filename:1:%scan('.':filename)) +
008900090904                         'zip';
009000090904
009100090904         // Zip and encrypt attachment file
009200090905         Zip_files(filename:filenameOUT:subject);
009300090106
009400090107         DOW 0 = 0;
009500090107            pos = %scan(';':%trim(dmaillist):START);
009600090107
009700090107            if pos > 0;
009800090107
009900090107               length = pos-start;
010000090107
010100090107               //returns from the starting position of dmaillist to the
010200090107               //position just before the semi colon
010300090107               to = %subst(dmaillist:start:length);
010400090107               start = pos + 1;
010500090107
010600090107               //send email //
010700090824               Email_handler(from:to:replyto:subject:text:'1':
010800090904                      filenameOUT);
010900090107
011000090107               if start >= %len(dmaillist);
011100090107                 leave;
011200090107               Endif;
011300090107
011400090107              else; // search code not found
011500090107               leave;
011600090107            endif;
011700090107         enddo;
011800090106
011900090107
012000090107         commit;
012100090106
012200090107         // terminate program
012300090107         *inlr = *ON;
012400090107         return;
012500090106
