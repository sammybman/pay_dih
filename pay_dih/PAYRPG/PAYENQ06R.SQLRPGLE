000100151013       //-------------------------------------------------//
000200151013       // Global Defines                                  //
000300151013       //-------------------------------------------------//
000400151013
000500151013       //---------------//
000600151013       // Control Specs //
000700151013       //---------------//
000800151013
000900151013       Ctl-Opt OPTION(*SRCSTMT : *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(*CALLER)
001000151013               MAIN(MainLine) ;
001100151013
001200151013      /define PROFOUNDUI
001300151013
001400151013       //------------//
001500151013       // File Specs //
001600151013       //------------//
001700151013
001800151013       //Dcl-F DisplyFile WORKSTN QUALIFIED ALIAS
001900160409       // EXTFILE(*EXTDESC) EXTDESC('PAYENQ06D')
002000151013       //            usropn
002100151013                  //HANDLER('PROFOUNDUI(HANDLER)');
002200151013      /IF DEFINED(WEB)
002300151013        Dcl-F DisplyFile WORKSTN QUALIFIED ALIAS
002400160409          EXTFILE(*EXTDESC) EXTDESC('PAYENQ06D')
002500160409                      usropn  INFDS(datas)
002600151013                       HANDLER('PROFOUNDUI(HANDLER)');
002700151013      /ELSE
002800151013        Dcl-F DisplyFile WORKSTN QUALIFIED ALIAS
002900160409          EXTFILE(*EXTDESC) EXTDESC('PAYENQ06D')
003000160409                      usropn  INFDS(datas);
003100151013      /ENDIF
003200151013
003300151013
003400151013       //-------------//
003500151013       // Global Data //
003600151013       //-------------//
003700151013
003800151013       //Data Structures
003900151013       Dcl-DS Table_In ExtName('PICODEM') QUALIFIED ALIAS End-DS;
004000151013       Dcl-DS Table_Out ExtName('PICODEM') QUALIFIED ALIAS End-DS;
004100151013       Dcl-DS Prompt_Fields LikeRec(DisplyFile.Prompt:*ALL) ;
004200151013
004300151013
004400151013       Dcl-DS Detail_In LikeRec(DisplyFile.Detail:*ALL) ;
004500151013       Dcl-DS Detail_Out LikeRec(DisplyFile.Detail:*ALL) ;
004600151013
004700151013       Dcl-DS Msg_Fields LikeRec(DisplyFile.Message:*All) ;
004800151013
004900151013       //Standalone Fields
005000151013       Dcl-S Successful ind;
005100151013       Dcl-S NotFound ind;
005200160409
005300160409
005400160409       Dcl-C ENDPROGRAM        x'33';
005500160409       Dcl-C CANCEL            x'3C';
005600160409       Dcl-C ENTER             x'F1';
005700160409
005800160409
005900160409       Dcl-Ds datas;
006000160409         fkey            Char(1)        Pos(369);
006100160409       End-Ds;
006200151013
006300151013       //--------------------------------//
006400151013       // Procedure: MainLine            //
006500151013       //  Main logic for program        //
006600151013       //--------------------------------//
006700151013
006800151013        Dcl-Proc MainLine ;
006900151013       Open DisplyFile;
007000151013       Clear Prompt_Fields;
007100160409      /IF DEFINED(WEB)
007200151013         Dou Prompt_Fields.btnExit=*On;
007300160409      /ELSE
007400160409         Dou fkey = ENDPROGRAM;
007500160409      /ENDIF
007600151013
007700151013         ExFmt  DisplyFile.Prompt Prompt_Fields;
007800151013
007900151013
008000160409      /IF DEFINED(WEB)
008100151013         If Prompt_Fields.btnExit = *Off;
008200160409      /ELSE
008300160409         If fkey <> ENDPROGRAM;
008400160409      /ENDIF
008500151013            Successful = Show_Detail(
008600151013                Prompt_Fields.PI_CODE
008700151013                : 'VIEW' : NotFound) ;
008800151013
008900160409      /IF DEFINED(WEB)
009000151013            If NotFound;
009100151013              Prompt_Fields.SQL100 = *On;
009200151013            Else;
009300151013               Prompt_Fields.SQL100 = *Off;
009400151013            EndIf;
009500160409      /ENDIF
009600151013
009700151013         EndIf;
009800151013         If Not Successful ;
009900151013          Display_Msg();
010000151013          Close DisplyFile;
010100151013          Return ;
010200151013        EndIf;
010300151013
010400151013       EndDo;
010500151013
010600151013
010700151013       Close DisplyFile;
010800151013       Return;
010900151013
011000151013       End-Proc;
011100151013
011200151013
011300151013
011400151013
011500151013       //-------------------------------------//
011600151013       // Procedure: Show_Detail              //
011700151013       //  Procedure to display detail screen //
011800151013       //    Parms:                           //
011900151013       //      Mode - 'EDIT' or 'VIEW'        //
012000151013       //    Return:                          //
012100151013       //      None                           //
012200151013       //-------------------------------------//
012300151013
012400151013       Dcl-Proc Show_Detail ;
012500151013
012600151013         Dcl-Pi *n ind;
012700151013            KeyPICCODE like(Table_Out.PI_CODE) Const;
012800151013           Mode CHAR(4) Const ;
012900151013           NotFound ind ;
013000151013         End-Pi;
013100151013
013200151013         Dcl-S Validated Ind ;
013300151013         NotFound = *off;
013400151013         Clear Detail_In ;
013500151013         Clear Detail_Out ;
013600151013
013700151013
013800151013         EXEC SQL
013900151013           SELECT * INTO :Table_IN FROM PICODEM
014000151013                  WHERE
014100151013                   PICCODE = :KeyPICCODE
014200151013                  ;
014300151013
014400151013         If SQLCODE = 100;
014500151013           NotFound = *on;
014600151013           Return *On;
014700151013         EndIf;
014800151013
014900151013         If SQLCODE < 0;
015000151013           Return *Off;
015100151013         EndIf;
015200151013
015300151013         Eval-Corr Detail_Out = Table_In ;
015400151013         Detail_In = Detail_Out ;
015500151013
015600151013         Dow Not Validated ;
015700151013
015800151013           Exfmt DisplyFile.Detail Detail_Out ;
015900160409      /IF DEFINED(WEB)
016000151013           If Detail_Out.BtnBack = *On ;
016100160409      /ELSE
016200160409           If fkey = ENTER;
016300160409      /ENDIF
016400151013             Return *on ;
016500151013           EndIf;
016600151013
016700151013           If Mode = 'EDIT' ;
016800151013             Validated = Validate_Detail() ;
016900151013           Else ;
017000151013             Validated = *On ;
017100151013           EndIf;
017200151013
017300151013         EndDo ;
017400151013
017500151013         If Detail_In <> Detail_Out ;
017600151013           Clear Table_Out ;
017700151013           Eval-Corr Table_Out = Detail_Out ;
017800151013
017900151013           EXEC SQL
018000151013            UPDATE PICODEM  SET
018100151013                 PICSTS = :Table_OUT.RECORD_STATUS
018200151013               , PICCODE = :Table_OUT.PI_CODE
018300151013               , PICDESC = :Table_OUT.PI_DESCRIPTION
018400151013               , PICWHCDLST = :Table_OUT.WHOUSE_CODE_LST
018500151013               , PICREVFLG = :Table_OUT.CALC_REV_FLAG
018600151013            WHERE
018700151013               PICCODE = :KeyPICCODE
018800151013                   ;
018900151013           If SQLCODE < 0;
019000151013              Return *Off;
019100151013           EndIf;
019200151013         EndIf;
019300151013         return *on;
019400151013       End-Proc;
019500151013
019600151013
019700151013
019800151013       //-------------------------------------------------//
019900151013       // Procedure: Validate_Detail                      //
020000151013       //  Put all data validation in this procedure      //
020100151013       //    Parms:                                       //
020200151013       //      None                                       //
020300151013       //    Return:                                      //
020400151013       //      Indicator (*On if passed validation)       //
020500151013       //-------------------------------------------------//
020600151013
020700151013       Dcl-Proc Validate_Detail ;
020800151013         Dcl-Pi *n Ind End-Pi;
020900151013        // Add your own validation code here.
021000151013        // Add your own validation code here.
021100151013        // Add your own validation code here.
021200151013
021300151013        // Return *off if validation fails.
021400151013         Return *On ;
021500151013
021600151013       End-Proc;
021700151013
021800151013       //-------------------------------------------------//
021900151013       // Procedure:Display_Msg                           //
022000151013       //  Dislay an SQL error message                    //
022100151013       //    Parms:                                       //
022200151013       //      None                                       //
022300151013       //    Return:                                      //
022400151013       //      none                                       //
022500151013       //-------------------------------------------------//
022600151013
022700151013       Dcl-Proc Display_Msg ;
022800151013         Dcl-Pi Display_Msg End-Pi;
022900151013
023000151013         Dcl-S MessageInfo CHAR(3000) ;
023100151013
023200151013
023300151013
023400151013       dcl-pr RtvMsgText EXTPGM('QMHRTVM');
023500151013         oMsgInfo      char(1);
023600151013         iMsgInfoLen   int(10)  const;
023700151013         iReturnFmt    char(8)  const;
023800151013         iMsgId        char(7)  const;
023900151013         iMsgFile      char(20) const;
024000151013         iMsgData      char(1)  const;
024100151013         iMsgDataLen   int(10)  const;
024200151013         iReplSubst    char(10) const;
024300151013         iRtnCtrl      char(10) const;
024400151013         ioApiErrorDS  char(1);
024500151013         iRtvOption    char(10)       options(*nopass);
024600151013         iCCSID        int(10)  const options(*nopass);
024700151013         iReplCCSID    int(10)  const options(*nopass);
024800151013       end-pr;
024900151013
025000151013
025100151013
025200151013
025300151013       dcl-ds Msg QUALIFIED;
025400151013         SQL       char(3)   INZ('SQL');
025500151013         Code      zoned(4);
025600151013         Identifer char(10) pos(1);
025700151013       end-ds;
025800151013
025900151013
026000151013
026100151013
026200151013       dcl-ds RTVM0100 Qualified;
026300151013         BytesRtn          int(10);
026400151013         BytesAvail        int(10);
026500151013         LengthRtn         int(10);
026600151013         LengthAvail       int(10);
026700151013         LengthHlpRtn      int(10);
026800151013         LengthHlpAvail    int(10);
026900151013         RTVM0100Msg       char(32767);
027000151013       end-ds;
027100151013
027200151013
027300151013
027400151013
027500151013        dcl-ds ERRC0100 Qualified;
027600151013         BytesPrv    int(10);
027700151013         BytesAvail  int(10);
027800151013         ExceptId    char(7);
027900151013         Reserved    char(1);
028000151013         ExceptData  char(32767);
028100151013        end-ds;
028200151013
028300151013
028400151013       Msg.Code = %Abs(SQLCODE);
028500151013
028600151013       RtvMsgText(MessageInfo :
028700151013                  %size(MessageInfo) :
028800151013                  'RTVM0100' :
028900151013                  Msg.Identifer :
029000151013                  'QSQLMSG   QSYS      ':
029100151013                  SQLERM :
029200151013                  SQLERL :
029300151013                  '*YES' :
029400151013                  '*NO' :
029500151013                  ERRC0100 );
029600151013       RTVM0100 = MessageInfo;
029700151013
029800151013       If RTVM0100.LengthRtn > 0;
029900151013         Msg_Fields.MsgText = %SubSt(RTVM0100.RTVM0100Msg : 1 :
030000151013                               RTVM0100.LengthRtn);
030100151013
030200151013       EndIf;
030300151013       If  RTVM0100.LengthHlpRtn > 0;
030400151013         Msg_Fields.MsgText = %Trim(Msg_Fields.MsgText) +  ' ' +
030500151013              %SubSt(RTVM0100.RTVM0100Msg : RTVM0100.LengthRtn + 1 :
030600151013                     RTVM0100.LengthHlpRtn);
030700151013       EndIf;
030800151013
030900151013
031000151013       ExFmt DisplyFile.Message Msg_Fields;
031100151013       Return;
031200151013
031300151013
031400151013       End-Proc;
031500151013
