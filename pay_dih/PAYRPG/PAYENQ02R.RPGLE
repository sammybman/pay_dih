000100050524     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
000200111002     H/define profoundui
000300950911     H* ********************************************************
000400950911     H*              B A N K S    D I H    L T D.              *
000500010412     H*                Payroll Control System                  *
000600950911     H*                                                        *
000700950911     H*                         BY                             *
000800950911     H*                                                        *
000900010412     H*       DISERV APPLICATION DEVELOPMENT TEAM              *
001000970115     H*             P.O BOX 10140, GEORGETOWN,GUYANA           *
001100950911     H*                                                        *
001200950911     H*                                                        *
001300010412     H*       RPG PROGRAM NAME..: PAYENQ01R                    *
001400950911     H*                                                        *
001500970724     H*       PROGRAMMER NAME: CHRIS WILLIAMS                  *
001600010412     H*       DESIGN DATE....: 2001/04/12                      *
001700950911     H*                                                        *
001800010412     H*  PROGRAM DESCRIPTION: Payroll Code Enquiry             *
001900950911     H*                                                        *
002000010412     H*  THIS PROGRAM IS DESIGNED TO DSPLAY THE PAYROLL        *
002100050524     H*  CODE FILE( From The PAYRTYP  Master)                  *
002200950911     H*                                                        *
002300950911     H**********************************************************
002400050524     fpayrtypl01if   e           k disk
002500050525     Fpayenq02d cf   e             workstn sfile(winrec:rec) usropn
002600111002     F/IF DEFINED(WEB)
002700111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002800111002     F/ENDIF
002900050524     Fqsysprt   o    f   80        printer oflind(*inof) usropn
003000950911     D*
003100111002      /copy QCPYSRC,genpr
003200111002      /copy QCPYSRC,autpr
003300111002      /copy QCPYSRC,emppr
003400050524     D
003500050524     D ARR             s             40    dim(1) ctdata perrcd(1)
003600050524     D
003700970117     D                sds
003800970117     D  @pgm             *proc
003900111002     D  @user                358    367
004000010412     D  @parm            *parms
004100050524     d
004200111002     D  main_pgm       pr                  extpgm('PAYENQ02R')
004300050524     D                                1    options(*nopass)
004400050524     D                                1    options(*nopass)
004500050524     D  main_pgm       pi
004600050524     D  in_code                       1    options(*nopass)
004700050524     D  in_flag                       1    options(*nopass)
004800050524     d
004900050524     **
005000050524     d rec             s              4  0
005100050524     d counter         s              5  0
005200050524     d @title          s             40
005300050524     d coname          s             30
005400050524     d @date           s             10
005500050524     d @time           s              8
005600050524
005700050524      /free
005800050524        // Execute subroutine to show header
005900050524            exsr aainit;
006000050524
006100050524            if  %parms = 2
006200050524               and in_flag = 'Y';
006300050524             exsr printrpt;
006400050524             else;
006500050524             exsr enquiry;
006600050524           endif;
006700010412
006800050524       //--------------------------
006900050524       // ENQUIRY
007000050524       //--------------------------
007100050524              begsr enquiry;
007200050524              open payenq02d;
007300050524
007400050524                  if  @parm > 0;
007500050524                     *in45 = '1';
007600050524                  endif;
007700050524
007800050524             dow not *inlr;
007900050524
008000050524            *in13 = *on;
008100050524            *in55 = *off;
008200050524            counter = *zeros;
008300050524
008400050524             write window;
008500050524               write wdwctl;
008600050524
008700050524                 *in13 = *off;
008800050524                  rec = *zeros;
008900050524
009000050524               setll *loval prtyfmt;
009100050524               read prtyfmt;
009200050524               dow not(%eof);
009300050524
009400050524                rec += 1;
009500070731                prtyptype# = prtyptype;
009600050524                write winrec;
009700050524
009800050524                read prtyfmt;
009900050524                enddo;
010000050524
010100050524                *in15 = rec > 0;
010200050524                *in55 = *on;
010300050524
010400050524                write window;
010500050524                exfmt wdwctl;
010600050524
010700050524           //** F3 = End Program
010800050524                   if  *in03;
010900050524                      *inlr = '1';
011000050524                       return;
011100050524                   endif;
011200010412
011300050524           // F5 = Print list
011400050524              if *in05;
011500050524               exsr printrpt;
011600050524                iter;
011700050524              endif;
011800050524
011900050524               readc winrec;
012000050524               dow not(%eof);
012100050524
012200050524           // If Selection is made, Return To Calling Pgm
012300050524                  if sel# = 'X';
012400050524                     in_code = prtyptype#;
012500070731                     close payenq02d;
012600070731                     *inlr = '1';
012700050524                     return;
012800050524                  endif;
012900050524
013000050524                  readc winrec;
013100050524                  enddo;
013200050524
013300050524                  enddo;
013400050524          endsr;
013500050524       //--------------------------
013600050524       // REPORT
013700050524       //--------------------------
013800050524            begsr printrpt;
013900050524            open qsysprt;
014000050524
014100050524            counter = *zeros;
014200050524            except rep_hedr;
014300050524
014400050524            dow not(%eof);
014500050524            setll *loval prtyfmt;
014600050524            read prtyfmt;
014700050524
014800050524              if *inof;
014900050524                except rep_hedr;
015000050524              endif;
015100050524
015200050524            except detail;
015300050524
015400050524            except final;
015500050524
015600050524             enddo;
015700050524          endsr;
015800050524
015900050524       //***************************************************
016000050524       //** First initilisation routine for all programs  **
016100050524       //***************************************************
016200050524              begsr aainit;
016300050524                coname = get_coname;
016400050524                  @date = get_sys_date;
016500050524                   @title = arr(1);
016600050524             endsr;
016700050524
016800050524      /end-free
016900050524     Oqsysprt   e            rep_hedr       1 01
017000050524     O                       @pgm                10
017100050524     O                       coname              55
017200050524     O                       @date               79
017300050524     O
017400050524     O          e            rep_hedr       2
017500050524     O                       @user               10
017600050524     O                       @title              65
017700050524     O                                           74 'PAGE -'
017800050524     O                       page          Z     79
017900050524     ** PRINT HIGHLIGHTED INFORMATION
018000050524     O          e            rep_hedr       2
018100050524     O                                           14 'RUN TYPE'
018200050524     O                                           31 'DESCRIPTION'
018300050524     O          e            detail      1
018400050524     O                       prtyptype           10
018500050524     O                       prtydesc            50
018600050524     O          e            final       2
018700050524     O                                           75 '** END OF REPORT **'
018800050524**
018900050524      PAYROLL RUN TYPES LISTING
