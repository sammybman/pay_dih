000100100210     H   BndDir('QC2LE':'DISDIR') DftActGrp(*No)
000200100210     D old_filename    s             50a
000300100210     D Path            s             50a   inz('/diserviq/payroll/')
000400091119     D new_filename    s             50a   inz(' ')
000500091201     D fileSuffix      s              4a   inz('.CSV')
000600100210     D OfileSuffix     s              4a   inz('.TXT')
000700091116
000800091116     d errorstate      s             10i 0 based( @errorstate )
000900091116     d @errorstate     s               *
001000091116     d message         s             50A
001100091116
001200091116     d GETERROR        PR              *   EXTPROC('__errno')
001300091116     d RENAME          PR            10i 0 EXTPROC('Qp0lRenameUnlink')
001400091116     d   OLD                           *   value options(*string)
001500091116     d   NEW                           *   value options(*string)
001600091201
001700100210     D*-----------------------------------------
001800100210     D* Prototype for procedure: Main_pgm
001900100210     D*-----------------------------------------
002000100923     D  main_pgm       pr                  extpgm('DB0003R')
002100100210     D org                            3    const
002200100210     D  main_pgm       pi
002300100210     D   org                          3    const
002400091116      /copy genpr
002500091116      */ Mainline code:
002600091116      /free
002700091119
002800091119         select;
002900100210         when org = 'PAY';
003000100923            old_filename = 'DEMBPAYROLL.TXT';  //PAYROLL FILE
003100100210         when org = 'SHR';
003200100923           old_filename = 'DEMBSHARES.TXT'; // SAHRES DIVIDENDS
003300100210         when org = 'CRD';
003400100923           old_filename = 'DEMBCUSTOMER.TXT';//CUSTOMER DISCOUNTS
003500091119         ENDSL;
003600091119
003700100210       new_filename = %trimr(old_filename) +filesuffix;
003800091119                    IF        RENAME(%TRIM(PATH)     +
003900091119                                    %TRIMR(OLD_FILENAME)+filesuffix:
004000091119                                    %TRIM(PATH)      +
004100091119                                    %trimr(new_filename))  < *Zero;
004200091119
004300091119                             @errorState = GETERROR();
004400091119                             message='Error State is : ' +
004500091119                                      %Char( errorState );
004600091119               dsply message;
004700091119                  ENDIF;
004800091116
004900091116                   *INLR = *ON;
005000100210                   RETURN;
