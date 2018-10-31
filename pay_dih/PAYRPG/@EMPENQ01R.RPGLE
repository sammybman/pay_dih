000100040427      // ********************************************************
000200000000      //              B A N K S    D I H    L T D.              *
000300110922      //                    I.T. DEPARTMENT                     *
000400000000      //                                                        *
000500180815      //       PROGRAM NAME......: @EMPENQ01R                   *
000600000000      //                                                        *
000700000000      //       PROGRAMMER NAME....: ALLISON FUNG                *
000800110922      //       DESIGN DATE........: 22.09.2011                  *
000900110929      //       Profound UI Modifications D.Cummings             *
001000111003      //       Last Modified 03.10.2011                         *
001100110929      //                                                        *
001200110929      //     PROGRAM DESCRIPTION: LIST Liability Types/sub-type *
001300000000      //                                                        *
001400110929      //    THIS PROGRAM IS DESIGNED TO VIEW Liability Types    *
001500110922      //    INFORMATION.                                        *
001600000000      //                                                        *
001700000000      //*********************************************************
001800110914     H  dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
001900110914     H/define profoundui
002000111003     F@EMPENQ01Dcf   e             workstn
002100110922     F                                     SFILE(sub1:RRN)
002200110922     F                                     SFILE(sub2:RRN2)
002300111003     F/IF DEFINED(WEB)
002400110914     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002500111003     F/ENDIF
002600000000     F
002700110922
002800110922      /include QCPYSRC,genPR
002900110928      /include QCPYSRC,empPR
003000110922
003100110928     D main_pgm        pr                  extPgm('@EMPENQ01R')
003200110922     D                                1    const
003300110928     D                                3    const
003400110928     D                                2  0
003500110928     D                                3  0 options(*nopass)
003600040430     **
003700040430     D main_pgm        pi
003800110922     D flag                           1    const
003900110928     D origin                         3    const
004000110928     D type                           2  0
004100110928     D stype                          3  0 options(*nopass)
004200110922     D
004300110922     D countr          s              5  0
004400180305     D type#           s              2  0
004500110922     D rrn             s              5  0
004600110922     D rrn2            s              5  0
004700110922     D errmsg          s             75
004800110930     D
004900110930     D* Program status data structure
005000111006     D*ds             SDS                  QUALIFIED
005100111006     D                SDS
005200111006     D  @PGM             *PROC
005300110930     D  PGM_STATUS       *STATUS
005400110930     D  ROUTINE          *ROUTINE
005500110930     D  PARMS            *PARMS
005600110930     D  CURR_USER            358    367
005700180815
005800180815
005900110929          open(e) @empenq01d;
006000110930
006100110930
006200110929       select;
006300110922
006400110928         // empl. liab. types
006500110928         //-------------------
006600110922         when flag = 'T';
006700110922           *in33 = *on;
006800110922           write  subctl1;
006900110922           *in33 = *off;
007000110922           *in55 = *off;
007100110922           rrn = *zeros;
007200110922           sel = *blanks;
007300110922
007400110928           dow not get_ltype_info('RD':-1:ematf_struc:countr);
007500110928            if  origin <> *blanks
007600110928              and %scan(origin:emsysflag) > 0
007700110928               or origin = *blanks;
007800180305              type# = emseltyp;
007900110922
008000110922              rrn += 1;
008100110922              write  sub1;
008200110922            endif;
008300110930
008400110922           enddo;
008500110922
008600110922           *in55 = *on;
008700110922           *in20 = rrn > *zeros;
008800180305
008900180305           if rrn = 1;
009000180305              type = type#;
009100180305             else;
009200110922
009300180305             //  show list
009400180305             exfmt  subctl1;
009500180305             if  *in20;
009600180305               readc  sub1;
009700110922
009800180305                dow not (%eof);
009900180305                 if sel = 'X';
010000180305                  type = emseltyp;
010100180305                 endif;
010200180305                readc  sub1;
010300180305                enddo;
010400110922
010500180305              else;
010600180305               type = *zeros;
010700180305             endif;
010800180305
010900180305           endif;
011000110922
011100110928         // Emp. liab. sub-type
011200110928         //---------------------
011300110928         when flag = 'S';
011400110922
011500110922           *in33 = *on;
011600110922           write  subctl2;
011700110922           *in33 = *off;
011800110922           *in55 = *off;
011900110922           rrn2 = *zeros;
012000110922           sel = *blanks;
012100110922
012200110928           dow not get_lstype_info('RD':type:-1:eastf_struc:countr);
012300110922
012400110922              rrn2 += 1;
012500110922              write  sub2;
012600110922           enddo;
012700110922
012800110922           *in55 = *on;
012900110922           *in20 = rrn2 > *zeros;
013000110922
013100110922
013200110922           exfmt  subctl2;
013300110922           if  *in20;
013400110922              readc  sub2;
013500110922
013600110922              dow not (%eof);
013700110922               if sel = 'X';
013800110928                 if %parms = %parmnum(stype);
013900110928                    stype = easubtyp;
014000110928                 endif;
014100110922               endif;
014200110922              readc  sub2;
014300110922              enddo;
014400110922
014500110922            else;
014600110928              if %parms = %parmnum(stype);
014700110928                stype = *zeros;
014800110928              endif;
014900110922           endif;
015000110922       endsl;
015100110922
015200110922
015300110928          close @empenq01d;
015400110914          *inlr = *on;
015500110914          return;
015600110914
015700110922
015800110922         //*****************************
015900110922         // Session Init
016000110922         //*****************************
016100110922
016200110922          begsr *inzsr;
016300111006            get_session_info('   ':errmsg:@pgm);
016400110930            if errmsg <> *blanks;
016500110930              show_msg(errmsg);
016600110930              *inlr = *on;
016700110930              return;
016800110930              endif;
016900110922
017000110922          endsr;
017100110922
