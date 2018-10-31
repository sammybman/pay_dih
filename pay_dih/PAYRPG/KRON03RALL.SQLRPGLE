000100110307     H DATEDIT(*DMY) dftactgrp(*no) bnddir('DISDIR')
000200950429     H******  PROGRAMMER - SAMUEL BRITTON
000300110803      /copy QCPYSRC,PAYPR
000400110803      /copy QCPYSRC,empPR
000500110307       //----------------------------------------------------------
000600110307       // this program should only be used for the initial load
000700110307       // of the kronos iseries central employee database
000800110307       //----------------------------------------------------------
000900960704     Dtxt1             c                   CONST('RESTAURANT-98')
001000110307
001100110309     D cntr            s              5  0                                      counter
001200110309     D badge           s              9  0                                      badge number
001300110309     D tmp_flag        s              1                                         badge number
001400110307
001500110307     Dkronos_int       pr                  extpgm('KRON03RINT')
001600110309     D                                6  0 const
001700110309     D                                9  0 const
001800110307
001900950516     D                SDS
002000960415     D  @PGM             *PROC
002100960415     D  @USER                254    263
002200960307     D  @PRM             *PARMS
002300110309
002400110309      /free
002500110307         //***************************************************************
002600110307         //***  UPDATE EMPLOYEE RECORD  (KRONOS) ***
002700110307         //***************************************************************
002800110309
002900110307          cntr = *zeros;
003000110307          dow not get_emp_info2('R':-1:emp_struc:cntr);
003100110307
003200110307            exec sql
003300110307              select kbadge into :badge from kempm
003400110307                  where kemp = :emp;
003500110309
003600110309            if  get_Cat(status:*omit:*omit:tmp_flag) = 'A'
003700110309                 and tmp_flag  <> 'T';
003800110309
003900110309                 kronos_int(emp:badge);
004000110307              else;
004100110309                if tmp_flag  <> 'T';
004200110309                  exec sql
004300110309                    delete kempm
004400110309                      where kemp = :emp and kbadge = :badge;
004500110309                endif;
004600110307            endif;
004700110307
004800110307          enddo;
004900110307
005000110309          commit;
005100110307          *inlr = *on;
005200110307          return;
