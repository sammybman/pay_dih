000100110920     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*no)
000200110920     H/define profoundui
000300000313     F**********************************************************
000400010607     F*                   DISERV  SYSTEM                       *
000500111013     F*                    Liability  Control                  *
000600980616     F*                                                        *
000700980616     F*                         BY                             *
000800980616     F*                                                        *
000900030310     F*             Diserv Systems                             *
001000000313     F*
001100000313     F*                                                        *
001200980616     F*                                                        *
001300111013     F*       RPG PROGRAM NAME..: EMPRPTSR                     *
001400980616     F*                                                        *
001500010607     F*       PROGRAMMER NAME: Samuel Britton                  *
001600010607     F*                        Christopher Williams            *
001700110920     F*       DESIGN DATE....: 2011/09/21                      *
001800110920     F*       MODULES........:                                 *
001900980917     F*       INDICATORS.....:                                 *
002000980917     F*                                                        *
002100980616     F*                                                        *
002200010716     F*  THIS PROGRAM IS DESIGNED  to configure  system        *
002300010716     F*                            parameters                  *
002400980616     F*                                                        *
002500980616     F**********************************************************
002600111013     Femprptsd  cf   e             workstn
002700110920     F/IF DEFINED(WEB)
002800110920     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900110920     F/ENDIF
003000010607     **
003100110920      /copy QCPYSRC,genPR
003200110920      /copy QCPYSRC,autPR
003300111013      /copy QCPYSRC,empPR
003400020226     **
003500110920     D arr             s             40    DIM(2) CTDATA PERRCD(1)
003600110920     D origin          s              3
003700110921     D opt             s              1  0
003800111013     D type            s              2  0
003900110920     D errmsg          s             75
004000150615     D @DATE           s             10
004100110920
004200111013     D Liab_det        pr                  extpgm('LI003R')
004300111013     D period                         6  0 const
004400111013     D ltype                          2  0 const
004500111013     D flag                           1    const
004600111013
004700111013     D emp_ded_setup   pr                  extpgm('EMPFRPT03')
004800111013     D period                         6  0 const
004900111013     D flag                           1    const
005000111013
005100111013     D Liab_deb_crd    pr                  extpgm('LI013R')
005200111013     D period                         6  0 const
005300111013     D ltype                          2  0 const
005400111013     D flag                           1    const
005500111013
005600111013     D emp_summary     pr                  extpgm('LI007R')
005700111013     D period                         6  0 const
005800111013     D ltype                          2  0 const
005900111013
006000111013     D emp_allowances  pr                  extpgm('LI008R')
006100111013     D period                         6  0 const
006200111013     D flag                           1    const
006300111013
006400111013     D emp_credit_sts  pr                  extpgm('LI010R')
006500111013     D period                         6  0 const
006600111013     D flag                           1    const
006700111013
006800111013     D emp_liab_summ   pr                  extpgm('LI004R')
006900111013     D period                         6  0 const
007000111013     D ltype                          2  0 const
007100111013
007200111013     D emp_drinks      pr                  extpgm('LI016R')
007300111013     D period                         6  0 const
007400111013
007500111013     D saving_withdrw  pr                  extpgm('SAVRPT01R')
007600111013     D period                         6  0 const
007700111013
007800111013     D saving_mntend   pr                  extpgm('SAVRPT02R')
007900111013     D period                         6  0 const
008000111013
008100111013     D saving_yrend    pr                  extpgm('SAVRPT03R')
008200111013     D period                         6  0 const
008300111013     D flag                           1    const
008400111013
008500111013     D saving_letter   pr                  extpgm('SAVRPT04R')
008600111013     D period                         6  0 const
008700111013     D flag                           1    const
008800111013
008900111013     D saving_mntupd   pr                  extpgm('EMPFRPT05')
009000111013     D period                         6  0 const
009100111013
009200110920
009300980319     D                sds
009400980917     D  @pgm             *proc
009500110920     D  @user                358    367
009600980603     **
009700020226
009800020226     *****************************************************************
009900111013     ** checking authority to access reports
010000020226     *****************************************************************
010100040207      /free
010200110921
010300110921          // authority check
010400110921         monitor;
010500111013            authority_check('EMT':@pgm);
010600110921          on-error;
010700110921            show_msg(get_err_msg('DIH0054'));
010800110921            *inlr = *on;
010900110921            return;
011000110921         endmon;
011100110921
011200110920
011300110920         exsr  aainit;
011400110920         //*******************
011500110920         //** Program Start
011600110920         //*******************
011700110920         dow   0 = 0;
011800110921            exfmt  fmt1;
011900110921
012000110920            // end of program
012100110920            if  *in03;
012200110920               *inlr = *on;
012300110920               return;
012400110920            endif;
012500110921
012600111013            // select liability Types
012700111013            if  *in01;
012800111013               type = lst_ltype();
012900111013               iter;
013000111013            endif;
013100141216
013200141216            //If submit button clicked
013300150615         // if subBtn = *on;
013400150615           if prno <= 0;
013500150615           show_msg(get_err_msg('DIH0130'));
013600150615           iter;
013700150615           endif;
013800141216            select;
013900141216              when  sel# = 1;
014000141216                liab_det(prno:type:*blank);
014100141216              when  sel# = 2;
014200141216                emp_ded_setup(prno:*blank);
014300141216              when  sel# = 3;
014400141216                liab_deb_crd(prno:type:'C');
014500141216                liab_deb_crd(prno:type:'D');
014600141216              when  sel# = 4;
014700141216                emp_summary(prno:type);
014800141216              when  sel# = 5;
014900141216                emp_allowances(prno:*blanks);
015000141216              when  sel# = 6;
015100141216                emp_credit_sts(prno:*blanks);
015200141216              when  sel# = 7;
015300141216                emp_liab_summ(prno:type);
015400141216              when  sel# = 8;
015500141216              when  sel# = 9;
015600141216                emp_drinks(prno);
015700141216              when  sel# = 10;
015800141216                saving_withdrw(prno);
015900141216              when  sel# = 11;
016000141216                saving_mntupd(prno);
016100141216                commit;
016200141216                saving_mntend(prno);
016300141216              when  sel# = 12;
016400141216                saving_yrend(prno:'A');  // active employees
016500141216                saving_yrend(prno:' ');  // inactive employees
016600141216              when  sel# = 13;
016700141216                saving_letter(prno:'A');
016800141216            endsl;
016900141216
017000150615         // EndIf;
017100110920
017200110920         enddo;
017300110920
017400110920       //*****************************
017500110920       // Session Init
017600110920       //*****************************
017700110920
017800110920        begsr *inzsr;
017900111013          get_session_info('EMT':errmsg:@pgm);
018000110920          if errmsg <> *blanks;
018100110920            show_msg(errmsg);
018200110920            *inlr = *on;
018300110920            return;
018400110920          endif;
018500110920
018600110920        endsr;
018700110920
018800110920
018900110920        //*****************************************************
019000110920        //** Begin subroutine to print header
019100110920        //*****************************************************
019200110920        begsr  AAINIT;
019300111013           CONAME = get_coname;
019400141204           @DATE = get_sys_date;
019500111013           @title = arr(1);
019600110920        endsr;
019700110920
019800110920      /end-free
019900980917     **
020000980917**   Header to be displayed
020100111013      Employee Liability Reports Menu
