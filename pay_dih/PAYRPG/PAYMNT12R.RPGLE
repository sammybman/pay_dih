000100030911      //*********************************************************
000200030911      //              D I S E R V   S Y S T E M S.              *
000300151007      //                   PAYROLL SYSTEM                       *
000400030911      //                                                        *
000500030911      //                         BY                             *
000600030911      //                                                        *
000700030911      //             Diserv systems Team                        *
000800030911      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
000900030911      //
001000030911      //                                                        *
001100030911      //                                                        *
001200151104      //       RPG PROGRAM NAME..: PAYMNT12R                    *
001300030911      //                                                        *
001400051229      //       PROGRAMMER NAME: Samuel Britton                  *
001500151007      //       DESIGN DATE....: 2015/10/07                      *
001600030911      //       MODULES........: *NONE                           *
001700030911      //       INDICATORS.....:                                 *
001800030911      //                                                        *
001900030911      //                                                        *
002000060124      //  THIS PROGRAM IS DESIGNED to store Commission rates as *
002100060124      //                           are defined.                 *
002200030911      //                                                        *
002300030911      //*********************************************************
002400151009     H dftactgrp(*no) actgrp(*new) bnddir('DISDIR')
002500120125     h/define PROFOUNDUI
002600151009     Fpaymnt12d CF   E             WORKSTN
002700120125     F/IF DEFINED(WEB)
002800120125     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900120125     F/ENDIF
003000060124
003100120125      /copy QCPYSRC,genpr
003200120125      /COPY QCPYSRC,AUTPR
003300060218
003400151007     Dmain_prog        PR                  extpgm('PAYMNT12R')
003500120125
003600060217     Dmain_prog        PI
003700060217
003800151007     D Global_config   pr                  extpgm('PAYMNT1201')
003900060124
004000151009     D PI_Codes        pr                  extpgm('PAYMNT1202')
004100151009     D PI_Deptconfig   pr                  extpgm('PAYMNT1203')
004200151009     D PI_defaults     pr                  extpgm('PAYMNT1204')
004300151009     D PI_items        pr                  extpgm('PAYMNT1205')
004400151009     D PI_rates        pr                  extpgm('PAYMNT1206')
004401151109     D PI_dupRates     pr                  extpgm('PAYMNT1207')
004500060301
004600060218
004700060124     D ARR             S             40    DIM(1) CTDATA PERRCD(1)
004800060126     D date1           S              8  0
004900060126     D time1           S              6  0
005000030923     D type            s              2
005100030911     D rrn             s              4  0
005200030911     D rrx             s              4  0
005300030911     D rr              s              4  0
005400120125     D idcode          s              3  0
005500120125     D aptr            s               *   inz(*null)
005600060131
005700030911     D                sds
005800030911     D  @pgm             *proc
005900120125     D  @user                358    367
006000030911
006100030911
006200060218     DMsg1             c                   'Enter a valid option'
006300030911
006400060124
006500120125
006600120125          // checking authority
006700120125         monitor;
006800151007          authority_check('PAY':@pgm);
006900120125         on-error;
007000120125          show_msg(get_err_msg('DIH0054'));
007100120125          *inlr = *on;
007200120125          return;
007300120125         endmon;
007400120125
007500030911
007600060301         // Only Controlling area can Create/Modify option
007700120125         if wysctloc <> idcode;
007800060301           *inlr = *on;
007900120126           show_msg(get_err_msg('DIH0141'));
008000120126           return;
008100030911         endif;
008200030911
008300060215       exsr aainit;
008400060124
008500060123       dow 0 = 0;
008600060123
008700060218         exfmt fmt1;
008800060123
008900151009
009000151009         monitor;
009100151009           select;
009200151009               // Setup Global Parameters
009300151009             when option = 1;
009400151012               authority_check('PAY':@pgm:get_level(@PGM:'PIPARAM'));
009500151009               // Define PI Codes
009600151009             when option = 2;
009700151009               authority_check('PAY':@pgm:get_level(@pgm:'PICODES'));
009800151009               // Setup Department Config.
009900151009             when option = 3;
010000151009               authority_check('PAY':@pgm:get_level(@pgm:'PIDEPTCONF'));
010100151009               // Setup PI Defaults
010200151009             when option = 4;
010300151009               authority_check('PAY':@pgm:get_level(@pgm:'PIDEFAULTS'));
010400151009               // Maintain PI Item Codes
010500151009             when option = 5;
010600151009               authority_check('PAY':@pgm:get_level(@pgm:'PIITEMCODE'));
010700151009               // PI  Calcs. Criteria
010800151009             when option = 6;
010900151009               authority_check('PAY':@pgm:get_level(@pgm:'PICALCS'));
010901151109               // PI  Duplicate Rates
010902151109             when option = 7;
010903151109               authority_check('PAY':@pgm:get_level(@pgm:'PIDUPRATE'));
011000151009           endsl;
011100151009
011200151009         on-error;
011300151009          show_msg(get_err_msg('DIH0054'));
011400151009          *inlr = *on;
011500151009          return;
011600151009         endmon;
011700151009
011800151009
011900060123         errmsg = *blanks;
012000060123
012100060123         // end of program
012200060123         if *in03;
012300060216            rolbk;
012400060216            *inlr = *ON;
012500060216            return;
012600060123         endif;
012700060217
012800060218         select;
012900060218          when option = 1;
013000060218            global_config();
013100151009          when option = 2;
013200151009            PI_codes();
013300151009          when option = 3;
013400151009            PI_deptconfig();
013500151009          when option = 4;
013600151009            PI_defaults();
013700151009          when option = 5;
013800151009            PI_items();
013900151009          when option = 6;
014000151009            PI_rates();
014001151109          when option = 7;
014002151109            PI_dupRates();
014100060218         endsl;
014200060123
014300060123       enddo;
014400030911
014500120125
014600120125         //*****************************
014700120125         // Session Init
014800120125         //*****************************
014900120125
015000120125          begsr *inzsr;
015100120125
015200151009              get_session_info('PAY':errmsg:@pgm);
015300120125
015400120125              if errmsg <> *blanks;
015500120125                show_msg(errmsg);
015600120125                *inlr = *on;
015700120125                return;
015800120125              endif;
015900120125
016000120125
016100120125          endsr;
016200120125
016300060124
016400060218       //*******************************************//
016500060218       // Begin subroutine to print header          //
016600060218       //*******************************************//
016700060218       BEGSR AAINIT;
016800060218         coname = get_coname;
016900060218         @date  = get_sys_date(date1:time1);
017000060218         @title = arr(1);
017100060218       ENDSR;
017200060118
017300030911**   Header to be displayed
017400151104Performance Incentive Configuration Menu
