000100090824     H DATEDIT(*DMY)  dftactgrp(*no) bnddir('DISDIR')
000200090805      //****** Email NIS Soft Copy  ******
000300090805      //****** PGMRS:  S.BRITTON, CWILLIAMS
000400000000      //
000500000000
000600000000      // SubProcedure PROTOTYPE
000700000000      //
000800000000     D                SDS
000900000000     D  @PGM             *PROC
001000000000     D  @USER                254    263
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
003500090106     D text            S           9999
003600090106     D rtnCode         S              3
003700090106     D rtnText         S             50
003800090107     D pos             S              3  0
003900090107     D start           S              3  0  inz(1)
004000090107     D length          S              3  0
004100090824     D pay_type        S              2     inz('FN')
004200090824     D period          S              6  0  inz(200906)
004300090824
004400090824     D file2           c                    '/diserviq/payroll/banksdih.txt'
004500090106
004600090106     D maillistDS    E DS                  extname(dismaillst)
004700090805      * disecting period to get month
004800090805      * month
004900090805
005000000000
005100000000      /FREE
005200090106
005300090805            mth = %int(%subst(%char(period):5));
005400090805            yr = %int(%subst(%char(period):1:4));
005500090107         //using sql statement to check for record in file//
005600090107         exec sql
005700090107              select * into :payelcDS from payelctrn
005800090107                where petype = :rbtype;
005900090107
006000090107         exec sql
006100090107              select * into :maillistds from dismaillst
006200090107                where dmailorg =:origin
006300090805                     and dmailobj = :pevpgmid;
006400090107
006500090107         if sqlstt <> '00000';
006600090107         endif;
006700090106
006800090107         from = 'SysAdmin@' + 'Banksdih.com';
006900090805         exec sql
007000090805              select cycdesc into :desc from paycycd
007100090805                where cyccode =:pay_type;
007200090805
007300090107         replyTo = *blanks;
007400090805
007500090805          cmonth = get_month(mth);
007600090805         subject = %trim(desc) + ' '+%trim(pesubject)+' for ' +%trim(cmonth);
007700090805         subject = %trim(subject)+ %editc(yr:'3');
007800090106
007900090107         text = petext;
008000090106
008100090824         filename= %trim(pefilenm) + ',' + file2;
008200090824       //filename= pefilenm;
008300090106
008400090107
008500090107               length = pos-start;
008600090107
008700090107               //returns from the starting position of dmaillist to the
008800090107               //position just before the semi colon
008900090824               to = 'sbritton@banksdih.com';
009000090107               start = pos + 1;
009100090107
009200090107               //send email //
009300090824               Email_handler(from:to:replyto:subject:text:'1':
009400090824                      filename);
009500090106
009600090106
009700090107         // terminate program
009800090107         *inlr = *ON;
009900090107         return;
010000090106
