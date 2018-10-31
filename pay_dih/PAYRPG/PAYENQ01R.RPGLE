000100050520     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
000200111002     H/define profoundui
000300950911     H* ********************************************************
000400950911     H*              B A N K S    D I H    L T D.              *
000500010412     H*                Payroll Control System                  *
000600950911     H*                                                        *
000700950911     H*                         BY                             *
000800950911     H*                                                        *
000900010412     H*       DISERV APPLICATION DEVELOPMENT TEAM              *
001000970115     H*             P.O BOX 10140, GEORGETOWN,GUYANA           *
001100050520     H*        freed & function added     20.5.2005            *
001200950911     H*                                                        *
001300010412     H*       RPG PROGRAM NAME..: PAYENQ01R                    *
001400950911     H*                                                        *
001500970724     H*       PROGRAMMER NAME: CHRIS WILLIAMS                  *
001600010412     H*       DESIGN DATE....: 2001/04/12                      *
001700950911     H*                                                        *
001800010412     H*  PROGRAM DESCRIPTION: Payroll Code Enquiry             *
001900950911     H*                                                        *
002000010412     H*  THIS PROGRAM IS DESIGNED TO DSPLAY THE PAYROLL        *
002100010412     H*  CODE FILE( From The PAYTYP  Master)                   *
002200950911     H*                                                        *
002300950911     H**********************************************************
002400050520     Fpayenq01d cf   e             workstn sfile(winrec:rec) usropn
002500111002     F/IF DEFINED(WEB)
002600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002700111002     F/ENDIF
002800050520     Fqsysprt   o    f   80        printer oflind(*inof) usropn
002900050520     D
003000111002      /copy QCPYSRC,genpr
003100111002      /copy QCPYSRC,autpr
003200111002      /copy QCPYSRC,emppr
003300050520     D
003400050520     D ARR             s             40    dim(1) ctdata perrcd(1)
003500970117     D                sds
003600970117     D  @pgm             *proc
003700111002     D  @user                358    367
003800010412     D  @parm            *parms
003900050520
004000111002     D  main_pgm       pr                  extpgm('PAYENQ01R')
004100050520     D                                2  0 options(*nopass)
004200050520     D                                1    options(*nopass)
004300050520     D  main_pgm       pi
004400050520     D  in_code                       2  0 options(*nopass)
004500050520     D  in_flag                       1    options(*nopass)
004600050520     d
004700050520     d rec             s              4  0
004800050520     d counter         s              5  0
004900050520     d @title          s             40
005000050520     d coname          s             30
005100050520     d @date           s             10
005200050520     d @time           s              8
005300050520
005400050520      /free
005500050520        // Execute subroutine to show header
005600050520          exsr aainit;
005700050520
005800050520            if  %parms = 2
005900050520               and in_flag = 'Y';
006000050520             exsr printrpt;
006100050520            else;
006200050520             exsr enquiry;
006300050520           endif;
006400050520
006500050520       //--------------------------
006600050520       // ENQUIRY
006700050520       //--------------------------
006800050520         begsr enquiry;
006900050520           open payenq01d;
007000050520             if @parm > 0;
007100050520               *in45 = '1';
007200050520             endif;
007300050520
007400050520             dow not *inlr;
007500050520
007600050520            *in13 = *on;
007700050520            *in55 = *off;
007800050520            counter = *zeros;
007900050520
008000050520             write window;
008100050520               write wdwctl;
008200050520
008300050520                 *in13 = *off;
008400050520                  rec = *zeros;
008500050520
008600050525            dow not get_payty_info('RD0':-1:payty_struc:counter);
008700010412
008800050520           //  Get code from paytyp Master
008900050520            //   sel# = ' ';
009000050520
009100050520            // if searchnam = *blanks
009200050520             //   or searchnam <> *blanks and
009300050520             //    %scan(%trim(searchnam):ptydesc) <> 0;
009400050520
009500050520                   rec +=  1;
009600050520                  write winrec;
009700050520
009800050520            //  endif;
009900050520            enddo;
010000050520
010100050520              *in15 = rec > 0;
010200050520                *in55 = *on;
010300050520
010400050520               write window;
010500050520               exfmt wdwctl;
010600010412
010700050520           // F3 = End Program
010800050520              if *in03;
010900050520               *inlr = '1';
011000050520                return;
011100050520              endif;
011200050520
011300050520           // F5 = Print list
011400050520              if *in05;
011500050520               exsr printrpt;
011600050520                iter;
011700050520              endif;
011800050520
011900050520              readc winrec;
012000050520
012100050520              dow not(%eof);
012200050520
012300050520         // If Selection is made, Return To Calling Pgm
012400050520             if sel# = 'X';
012500090622              if @parm <> 0;
012600050520                in_code = ptyptype;
012700090622              endif;
012800050520                close payenq01d;
012900070731                *inlr = '1';
013000070731                return;
013100050520              endif;
013200050520
013300050520             readc winrec;
013400050520             enddo;
013500050520
013600050520             enddo;
013700050520          endsr;
013800050520
013900050520       //--------------------------
014000050520       // REPORT
014100050520       //--------------------------
014200050520            begsr printrpt;
014300050520            open qsysprt;
014400050520
014500050520            counter = *zeros;
014600050520            except rep_hedr;
014700050525            dow not get_payty_info('RD0':-1:payty_struc:counter);
014800050520
014900050520              if *inof;
015000050520                except rep_hedr;
015100050520              endif;
015200050520
015300050520            except detail;
015400050520
015500050520            enddo;
015600050520            except final;
015700050520
015800050520          endsr;
015900050520
016000050520       //***************************************************
016100050520       //** First initilisation routine for all programs  **
016200050520       //***************************************************
016300050520              begsr aainit;
016400050520                coname = get_coname;
016500050520                  @date = get_sys_date;
016600050520                   @title = arr(1);
016700050520             endsr;
016800050520
016900050520      /end-free
017000050520     Oqsysprt   e            rep_hedr       1 01
017100050520     O                       @pgm                10
017200050520     O                       coname              55
017300050520     O                       @date               79
017400050520     O
017500050520     O          e            rep_hedr       2
017600050520     O                       @user               10
017700050520     O                       @title              65
017800050520     O                                           74 'PAGE -'
017900050520     O                       page          Z     79
018000050520     ** PRINT HIGHLIGHTED INFORMATION
018100050520     O          e            rep_hedr       2
018200050520     O                                           14 'PAY TYPE'
018300050520     O                                           31 'DESCRIPTION'
018400050520     O          e            detail      1
018500050520     O                       ptyptype      z     10
018600050520     O                       ptydesc             50
018700050520     O          e            final       2
018800050520     O                                           75 '** END OF REPORT **'
018900050520**
019000050520        PAY TYPE LISTING
