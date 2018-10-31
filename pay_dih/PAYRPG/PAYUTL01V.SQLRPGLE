000100000000     **
000200151001     **  Program . . : PAYUTL01V
000300151001     **  Description : Adjust Taxable Gross - VCP
000400151001     **  Author  . . : Samuel  Britton
000500151001     **  Date  . . . : October 1, 2015
000600000000     **
000700000000     **
000800000000     **  Program description:
000900151001     **    This program checks the authorization of the current user profile
001000151001     **     and validate key fields.
001100000000     **
001200000000     **  Program summary
001300000000     **  ---------------
001400000000     **
001500000000     **  Message handling API:
001600000000     **    QMHSNDPM      Send program message  Sends a message to a program stac
001700000000     **                                        entry (current, previous, etc.) o
001800000000     **                                        an external message queue.
001900000000     **
002000000000     **                                        Both messages defined in a messag
002100000000     **                                        file and immediate messages can b
002200000000     **                                        used. For specific message types
002300000000     **                                        only one or the other is allowed.
002400000000     **
002500000000     **
002600000000     **  Compile options:
002700151001     **    CrtRpgMod   Module( PAYUTL01V )
002800000000     **                DbgView( *NONE )
002900000000     **                Aut( *USE )
003000000000     **
003100151001     **    CrtPgm      Pgm( PAYUTL01V )
003200151001     **                Module( PAYUTL01V )
003300000000     **                ActGrp( *NEW )
003400000000     **                UsrPrf( *OWNER )
003500000000     **                Aut( *USE )
003600000000     **
003700151001     **    ChgObjOwn   Obj( PAYUTL01V )
003800000000     **                ObjType( *PGM )
003900151002     **                NewOwn( DISERVOWNR )
004000000000     **
004100151001     **    ChgPgm      Pgm( PAYUTL01V )
004200000000     **                RmvObs( *ALL )
004300000000     **
004400000000     **
004500000000     **-- Control specification:  --------------------------------------------**
004600151002       Ctl-Opt DFTACTGRP(*NO) bnddir('DISDIR') actgrp(*new);
004700151002       Ctl-Opt DATFMT(*ISO) Option(*SrcStmt);
004800000000
004900000000     **-- System information:
005000151011     D*PgmSts         SDs                  Qualified
005100151011     D* Pgmnam           *Proc
005200151011     D* CurJob                       10a   Overlay( PgmSts: 244 )
005300151011     D* UsrPrf                       10a   Overlay( PgmSts: 254 )
005400151011     D* JobNbr                        6a   Overlay( PgmSts: 264 )
005500151011     D* CurUsr                       10a   Overlay( PgmSts: 358 )
005600151011
005700151011
005800151011       Dcl-Ds *n PSDS;
005900151011         @Pgm            *proc;
006000151011         @CurJob         Char(10)        Pos(244);
006100151011         @user           Char(10)        Pos(254);
006200151011         @JobNbr         Char(6)         Pos(264);
006300151011         @CurUser        Char(10)        Pos(358);
006400151011         @parms          *parms;
006500151011       End-Ds;
006600151011
006700151002
006800000000     **-- API error data structure:
006900000000     D ERRC0100        Ds                  Qualified
007000000000     D  BytPrv                       10i 0 Inz( %Size( ERRC0100 ))
007100000000     D  BytAvl                       10i 0
007200000000     D  MsgId                         7a
007300000000     D                                1a
007400000000     D  MsgDta                      512a
007500151002
007600000000     **-- Global constants:
007700000000     D USG_DNY         c                   '1'
007800151002       DCL-C SQLStateOK      '00000';
007900151002
008000151002       DCL-S num        Packed(6);
008100151011       DCL-S wstrdate   Char(7);
008200151011       DCL-S wenddate   Char(7);
008300151011       DCL-S errmsg     Char(75);
008400151002
008500151002      // ...PROTOTYPES
008600151011      /copy QCPYSRC,genPR
008700151011      /copy QCPYSRC,autPR
008800151011      /copy QCPYSRC,chkobj
008900000000
009000000000     **-- Send program message:
009100000000     D SndPgmMsg       Pr                  ExtPgm( 'QMHSNDPM' )
009200000000     D  SpMsgId                       7a   Const
009300000000     D  SpMsgFq                      20a   Const
009400000000     D  SpMsgDta                    128a   Const
009500000000     D  SpMsgDtaLen                  10i 0 Const
009600000000     D  SpMsgTyp                     10a   Const
009700000000     D  SpCalStkE                    10a   Const  Options( *VarSize )
009800000000     D  SpCalStkCtr                  10i 0 Const
009900000000     D  SpMsgKey                      4a
010000000000     D  SpError                    1024a          Options( *VarSize )
010100000000
010200000000     **-- Get function usage:
010300000000     D GetFcnUsg       Pr             1a
010400000000     D  PxFcnId                      30a   Const
010500000000     D  PxUsrPrf                     10a   Const
010600000000
010700000000     **-- Send diagnostic message:
010800000000     D SndDiagMsg      Pr            10i 0
010900000000     D  PxMsgId                       7a   Const
011000000000     D  PxMsgDta                    512a   Const  Varying
011100151002
011200000000     **-- Send escape message:
011300000000     D SndEscMsg       Pr            10i 0
011400000000     D  PxMsgId                       7a   Const
011500000000     D  PxMsgDta                    512a   Const  Varying
011600000000
011700000000     **-- Entry parameters:
011800151001     D PAYUTL01V       Pr
011900151001     D  PxYear                        4P 0
012000151001     D  PxPaytype                     2P 0
012100151002     D  PxStrdate                     7
012200151002     D  PxEnddate                     7
012300000000     **
012400151001     D PAYUTL01V       Pi
012500151001     D  PxYear                        4P 0
012600151001     D  PxPaytype                     2P 0
012700151002     D  PxStrdate                     7
012800151002     D  PxEnddate                     7
012900151002
013000000000
013100151002         // authority check
013200151002         monitor;
013300151002         //authority_check('PAY':@pgm);
013400151002           authority_check('PAY':'PAYSYSCTLR');
013500151002         on-error;
013600151002             SndDiagMsg( 'CPD0006': '0000No authority to this function.' );
013700151002             SndEscMsg( 'CPF0002': '' );
013800151002        // show_msg(get_err_msg('DIH0054'));
013900151002        // *inlr = *on;
014000151002        // return;
014100151002         endmon;
014200151002
014300151011
014400151011         // Create tables
014500151011         BuildTables();
014600151011
014700151011         Wstrdate = *blanks;
014800151011         Wenddate = *blanks;
014900151011
015000151011         exec sql
015100151011             select pu1strdate,pu1enddate into :Wstrdate,:Wenddate
015200151011                from payutl01lg
015300151011                  where pu1year = :Pxyear and pu1paytype = :Pxpaytype;
015400151011
015500000000
015600151002         exec sql
015700151002             select count(*) into :num from paytyp
015800151002                  where ptyptype = :Pxpaytype;
015900151002
016000151002         if num = *zeros;
016100151002       //if sqlstt <> SQLStateOK;
016200151002             SndDiagMsg( 'CPD0006': '0000Invalid Payroll Type.' );
016300151002             SndEscMsg( 'CPF0002': '' );
016400151002         endif;
016500151002
016600151002         if  %date(PxStrdate: *cymd0)  > %date(PxEnddate: *cymd0);
016700151002             SndDiagMsg( 'CPD0006': '0000Start date greater than End date.' );
016800151002             SndEscMsg( 'CPF0002': '' );
016900151002         endif;
017000151002
017100151002         if  PxYear <> %subdt(%date(PxStrdate: *cymd0) : *YEARS)
017200151002           or  PxYear <> %subdt(%date(PxEnddate: *cymd0) : *YEARS);
017300151002             SndDiagMsg( 'CPD0006': '0000Date(s) not within year specified.' );
017400151002             SndEscMsg( 'CPF0002': '' );
017500151002         endif;
017600151011
017700151011         if  wstrdate <> *blanks;
017800151011           if  %date(PxStrdate: *cymd0) <= %date(Wstrdate: *cymd0)
017900151011             and  %date(PxEnddate: *cymd0) >= %date(Wenddate: *cymd0)
018000151011            or  %date(PxEnddate: *cymd0) >= %date(Wstrdate: *cymd0)
018100151011             and  %date(PxEnddate: *cymd0) <= %date(Wenddate: *cymd0)
018200151011            or  %date(PxStrdate: *cymd0) >= %date(Wstrdate: *cymd0)
018300151011             and  %date(PxStrdate: *cymd0) <= %date(Wenddate: *cymd0);
018400151011             SndDiagMsg( 'CPD0006':
018500151011               '0000Command Run before with date range ' + wstrdate +
018600151011               ' - ' + Wenddate );
018700151011             SndEscMsg( 'CPF0002': '' );
018800151011           endif;
018900151011         endif;
019000151002
019100151002         Select;
019200151002        //When  GetFcnUsg( 'DIS_PAYROLL_USAGE': '*CURRENT' ) = USG_DNY;
019300151002        //  SndDiagMsg( 'CPD0006': '0000No authority to this function.' );
019400151002        //  SndEscMsg( 'CPF0002': '' );
019500000000
019600000000        EndSl;
019700000000
019800000000        *InLr = *On;
019900000000        Return;
020000151011
020100151011         //*****************************
020200151011         // Session Init
020300151011         //*****************************
020400151011
020500151011          begsr *inzsr;
020600151011
020700151011            get_session_info('PAY':errmsg:@pgm);
020800151011            if errmsg <> *blanks;
020900151011             SndDiagMsg( 'CPD0006': '0000' + %trim(errmsg) );
021000151011             SndEscMsg( 'CPF0002': '' );
021100151011              *inlr = *on;
021200151011              return;
021300151011            endif;
021400151011
021500151011
021600151011          endsr;
021700151011
021800000000
021900000000
022000000000     **-- Send diagnostic message:  ------------------------------------------**
022100000000     P SndDiagMsg      B
022200000000     D                 Pi            10i 0
022300000000     D  PxMsgId                       7a   Const
022400000000     D  PxMsgDta                    512a   Const  Varying
022500000000     **
022600000000     D MsgKey          s              4a
022700000000
022800000000
022900000000        SndPgmMsg( PxMsgId
023000000000                 : 'QCPFMSG   *LIBL'
023100000000                 : PxMsgDta
023200000000                 : %Len( PxMsgDta )
023300000000                 : '*DIAG'
023400000000                 : '*PGMBDY'
023500000000                 : 1
023600000000                 : MsgKey
023700000000                 : ERRC0100
023800000000                 );
023900000000
024000000000        If  ERRC0100.BytAvl > *Zero;
024100000000          Return  -1;
024200000000
024300000000        Else;
024400000000          Return   0;
024500000000        EndIf;
024600000000
024700000000
024800000000     P SndDiagMsg      E
024900000000     **-- Send escape message:  ----------------------------------------------**
025000000000     P SndEscMsg       B
025100000000     D                 Pi            10i 0
025200000000     D  PxMsgId                       7a   Const
025300000000     D  PxMsgDta                    512a   Const  Varying
025400000000     **
025500000000     D MsgKey          s              4a
025600000000
025700000000
025800000000        SndPgmMsg( PxMsgId
025900000000                 : 'QCPFMSG   *LIBL'
026000000000                 : PxMsgDta
026100000000                 : %Len( PxMsgDta )
026200000000                 : '*ESCAPE'
026300000000                 : '*PGMBDY'
026400000000                 : 1
026500000000                 : MsgKey
026600000000                 : ERRC0100
026700000000                 );
026800000000
026900000000        If  ERRC0100.BytAvl > *Zero;
027000000000          Return  -1;
027100000000
027200000000        Else;
027300000000          Return   *Zero;
027400000000        EndIf;
027500000000
027600000000
027700000000     P SndEscMsg       E
027800151011
027900151011       Dcl-Proc BuildTables;
028000151011         Dcl-Pi BuildTables End-Pi;
028100151011
028200151011         // create payroll utility log table
028300151011              if not chkobj('*LIBL/PAYUTL01LG' : '*FILE');
028400151011                  exec sql
028500151011                      CREATE TABLE PAYUTL01LG (
028600151011                        pu1year     numeric(4, 0) ,
028700151011                        pu1Paytype  numeric(2, 0) ,
028800151011                        pu1strdate  char(7) ,
028900151011                        pu1enddate  char(7) ,
029000151011                        pu1user     char(10) ,
029100151011                        pu1timestamp timestamp not null
029200151011                                            default current_timestamp,
029300151011                        PRIMARY KEY( pu1year, pu1paytype, pu1strdate ) )
029400151011                        RCDFMT PYU01FMT;
029500151011
029600151011                    exec sql
029700151011                        LABEL ON TABLE PAYUTL01LG
029800151011                         IS 'Payroll Utility Log #1 ' ;
029900151011
030000151011                    exec sql
030100151011                          LABEL ON COLUMN PAYUTL01LG
030200151011                          ( pu1year  IS 'Year '  ,
030300151011                           pu1paytype  IS 'Pay Type ' ,
030400151011                           pu1strdate  IS 'Start date ' ,
030500151011                           pu1enddate  IS 'End date ' ,
030600151011                           pu1user  IS 'User ' ,
030700151011                           pu1timestamp  IS 'Timestamp ');
030800151011
030900151011
031000151011                    exec sql
031100151011                        LABEL ON COLUMN PAYUTL01LG
031200151011                          ( pu1year  Text IS 'Year '  ,
031300151011                           pu1paytype  Text IS 'Pay Type ' ,
031400151011                           pu1strdate  Text IS 'Start date ' ,
031500151011                           pu1enddate  Text IS 'End date ' ,
031600151011                           pu1user  Text IS 'User ' ,
031700151011                           pu1timestamp  Text IS 'Timestamp ');
031800151011
031900151011              endif;
032000151011
032100151011       End-Proc BuildTables;
