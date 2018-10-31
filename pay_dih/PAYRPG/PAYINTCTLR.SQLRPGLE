000100111002     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000200111002     H/define profoundui
000300141009     Fpayintctldcf   e             workstn indds(IndicatorDs)
000301140922     F                                     sfile(sflrec:rrn)
000400111002     F/IF DEFINED(WEB)
000500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600111002     F/ENDIF
001100141008
001200111002      /copy QCPYSRC,genpr
001300111002      /copy QCPYSRC,PAYPR
001301141008      /copy QCPYSRC,emppr
001500141008
001600971012     D                sds
001700971012     D  @pgm             *proc
001800111002     D  @user                358    367
001801141008
001802141008         // Escape Program   Prototype
001803141008     D  escape_pgm     pr                  extpgm('ESCPMSG')
001804141008     D                               10    const
001805141008     D                                7    const
001806141008
001807141008
001808141008     D Line_command    PR                  extpgm('QCMDEXC')
001809141008     D cmd                          100    options(*varsize) const
001810141008     D cmdlen                        15p 5 const
001811141008
001900141008
002401141008       DCL-C MSG1            'Invalid Normal time code specified';
002402141008       DCL-C MSG2            'Invalid Overtime code specified';
002403141008       DCL-C MSG3            'Invalid Normal time multiplier specified';
002404141008       DCL-C MSG4            'Invalid Overtime multiplier specified';
002405141008       DCL-C MSG5            'Kronos Company code not Found';
002500141008
002501141008       DCL-S arr             CHAR(40)    DIM(1) CTDATA PERRCD(1);
002502141008       DCL-S rrn             PACKED(4:0) INZ(*zeros);
002503141008       DCL-S library         CHAR(10);
002507141008       DCL-S recFound        PACKED(4);
002508141008       DCL-S cmd             CHAR(200);
002509141008       //Purpose: found control record
002510141008       DCL-S found_con IND inz(*off);
002511141008
002512141009       DCL-DS payint1DS EXTNAME('PAYINTF1') INZ;
002513141008       END-DS ;
002514141008
002515141009       DCL-DS payint2DS EXTNAME('PAYINTF2') INZ;
002516140922       END-DS ;
002600010411
002601141009
002602141009       DCL-DS IndicatorDS;
002603141009         F3_Pressed        IND             Pos(3);
002604141009         F10_Pressed       IND             Pos(10);
002605141009         F15_Pressed       IND             Pos(15);
002607141009         SflClear          IND             Pos(33);
002608141009         SflEnd            IND             Pos(55);
002609141009         Errors            Char(6)         Pos(70);
002610141009         KronosCode_error  IND             Pos(70);
002611141009         Description_error IND             Pos(71);
002612141009         Paytype_error     IND             Pos(72);
002613141009         Multiplier_error  IND             Pos(73);
002614141009         Duplication_error IND             Pos(74);
002615141009       END-DS ;
002616141009
002617141009
002618141009
002700140922         // Iniz routine
002800111002      /copy QCPYSRC,aainit
002802140922
002864141008
002866141008
002900140922       *in = *off;
002901141008
002902141008         // Get Kronos data Library from config. table
002903141008         exec sql
002904141008            select hckrondta into :library from hmrcon;
002905141008
002906141008         if library = *blanks;
002907141008            *inlr = *on;
002908141008      /if not defined(web)
002909141008            escape_pgm(@pgm:'PAY0049');
002910141008      /else
002911141008            show_msg(get_err_msg('PAY0049'));
002912141008            return;
002913141008      /endif
002914141008         endif;
002915141008
002916141008         // Add Kronos data Library from library list
002917141008         cmd = 'ADDLIBLE LIB(' + library + ')' +
002918141008               ' POSITION(*LAST)';
002919141008         callp(e) line_command(cmd:%size(cmd));
002920141008
003100141008       // Get Default Configuration record
003101141008
003102141008       exec sql
003200141008          SELECT * INTO :PAYINT2DS FROM PAYINTF2;
003201141008
003202141008       if sqlstt = '00000';
003203141008           found_con = *on;
003204141008       else;
003205141008           reset found_con;
003206141008       endif;
003207141008
003400141008
003500140922         //************************
003600140922         //* Start of Program   ***
003700140922         //************************
003800140922       dow 0 = 0;
003900141008
003901141008
003902141008      /IF DEFINED(WEB)
003903141008           if errmsg <> *blanks;
003904141008               show_msg(errmsg);
003905141008           endif;
003906141008      /ENDIF
004000140922         exfmt fmt1;
004001141008
004100140922         clear errmsg;
004101141008         recFound = *zeros;
004200141008
004300141009         if F3_pressed;
004301141008           // Remove Kronos data Library from library list
004302141008           cmd = 'RMVLIBLE LIB(' + library +')';
004303141008           callp(e) line_command(cmd:%size(cmd));
004400140922           *inlr = *on;
004500140922           return;
004600140922         endif;
004700141008
004701141008         // Validate Normal time pay type
004703141008         if get_payty_info('RD':pin2ntpay:payty_struc);
004704141008             errmsg = msg1;
004705141008             iter;
004706141008         endif;
004803141008
004804141008         // Validate Overtime pay type
004805141008         if get_payty_info('RD':pin2otpay:payty_struc);
004806141008             errmsg = msg2;
004807141008             iter;
004808141008         endif;
004809141008
004810141008         // Validate normal time multiplier
004811141008         if pin2ntmult = *zeros;
004812141008           //errmsg = msg3;
004813141008             pin2ntmult = 1.00000;
004814141008             iter;
004815141008         endif;
004816141008
004817141008         // Validate overtime multiplier
004818141008         if pin2otmult = *zeros;
004819141008             errmsg = msg4;
004820141008             iter;
004821141008         endif;
004822141008
004823141008         // Validate Kronos Company entry
004824141008         exec sql
004825141008             select 1 into :recFound
004826141008                from ckcompan where cocono = :pin2dccode;
004827141008         if recFound = *zeros;
004828141008             errmsg = msg5;
004829141008             iter;
004830141008         endif;
004831141008
007100141008         //  Interface Details  processing...
007300141008           handleInterfaceDetails();
007500140922           leave;
007700141008
007800140922       enddo;
007801141008
011201141008
011202141008       // -------------------------------------------------------
011203141008       // Procedure name: confirm_rtn
011204141008       // Purpose:  Update Payroll/Kronos Interface Control File
011205141008       // Returns:
011206141008       // -------------------------------------------------------
011207141008
011208141008       DCL-PROC confirm_rtn;
011300141008
011301141008           DCL-S rrx PACKED(4);
011302141008           DCL-S rr  PACKED(4);
011303141008
011304141008
011305141008          // update interface default file
011306141008       //   exec sql
011307141008       //       select * into :payint2DS from payintf2;
011308141008
011309141008          // record is found
011310141008          if found_con = *on;
011311141008
011312141008
011313141008             exec sql
011314141008                update payintf2 set pin2ntpay = :pin2ntpay,
011315141008                      pin2otpay = :pin2otpay,
011316141008                      pin2ntmult = :pin2ntmult,
011317141008                      pin2otmult = :pin2otmult,
011318141008                      pin2dccode = :pin2dccode
011319141008                        where exists(select * from payintf2);
011320141008           else;
011321141008             exec sql
011322141008                insert into payintf2
011326141008                               values(:payint2DS);
011331141008           endif;
011401141008
011402141008
011403141008          // update interface details file
011404141008          rrx = rrn;
011405141008          for rr = 1 to rrx;
011406141008
011407141008            CHAIN RR Sflrec;
011408141008
011409141008            // perform insert,update & delete operations
011411141008
011412141008              exec sql
011413141008                 merge into payintf1 as int1
011414141008                     using  (values ('A', :pin1kpayc, :pin1paytp,
011415141008                                     :pin1desc, :pin1mult, curdate(),
011416141008                                     curtime(), :@user))
011417141008                              NEWROW (pin1sts, pin1kpayc, pin1paytp,
011418141008                                      pin1desc, pin1mult, pin1ldate,
011419141008                                      pin1ltime, pin1luser)
011420141008
011421141008                            on int1.pin1kpayc = NEWROW.pin1kpayc
011422141008
011423141008                       when MATCHED  and :sel# <> 'X' then
011424141008                         update  set pin1desc =  NEWROW.pin1desc,
011425141008                                  pin1paytp = NEWROW.pin1paytp,
011426141008                                  pin1mult = NEWROW.pin1mult,
011427141008                                  pin1ldate = NEWROW.pin1ldate,
011428141008                                  pin1ltime = NEWROW.pin1ltime,
011429141008                                  pin1luser = NEWROW.pin1luser
011430141008
011431141008                       when MATCHED and :sel# = 'X' THEN
011432141008                         delete
011434141008
011435141008                       when not MATCHED and :pin1kpayc <> ' '
011436141008                         THEN
011437141008                         insert values(NEWROW.pin1sts, NEWROW.pin1kpayc,
011438141008                                  NEWROW.pin1paytp, NEWROW.pin1mult,
011439141008                                  NEWROW.pin1desc, NEWROW.pin1ldate,
011440141008                                  NEWROW.pin1ltime, NEWROW.pin1luser);
011442141008
011444141008
011445141008          endfor;
011446141008
011447141008          commit;
013600141008
013701141008       END-PROC ;
017600141008
017700140922
017701140922       // --------------------------------------------------
017702141008       // Procedure name: handleInterfaceDetails
017703140922       // Purpose:
017704140922       // Returns:
017705140922       // --------------------------------------------------
017706140922
017707141008       DCL-PROC handleInterfaceDetails;
017708141008
017709140922       DCL-S rrn# LIKE(rrn) INZ(0);
017717140922       DCL-S start LIKE(rrn) inz(0) ;
017718141008
017719141008             // reset Subfile
017720140922              ResetSubfile();
017721140922
017722141008             // open Subfile
017723140922              openFilePrc();
017724141008
017725141008             // read Subfile
017726140922              dow not(readFilePrc());
017727140922                sel# = ' ';
017730141008                rrn+= 1;
017731141008                write sflrec;
017732140922              enddo;
017733141008
017734140922              closeFilePrc();
017735141009
017736141009              // Add a new line if file is empty
017737141009              if rrn = *zeros;
017738141009                new_line();
017739141009              endif;
017740141009
017741141009              SflEnd = *on;
017746141008
017747141008           // subfile processing
017748141008           dow 0 = 0 ;
017749141008
017750141008              if errmsg <> *blanks;
017751141008                  show_msg(errmsg);
017752141008              endif;
017753140922              exfmt sflctl;
017754141008
017755141009              if F3_Pressed;
017756141008                // Remove Kronos data Library from library list
017757141008                cmd = 'RMVLIBLE LIB(' + library +')';
017758141008                callp(e) line_command(cmd:%size(cmd));
017759140922                *inlr = *on;
017760140922                return;
017761141008              endif;
017762141008
017763141008
017764141008              // validate subfile
017765141008              if  validate_sr();
017766141008                iter;
017767141008              endif;
017768141008
017769141008              // Add a new line
017770141009              if F10_Pressed;
017771141008                new_line();
017772141008              endif;
017792141008
017793140922              // Confirm updates
017794141009              if  F15_Pressed;
017795141008
017796141008                confirm_rtn();
017807141008                leave;
017808141008              endif;
017810140922
017811140922         enddo;
017812141008
017813140922       END-PROC ;
017814141008
017818141008
017819141008       // --------------------------------------------------
017820141008       // Procedure name: new_line
017821141008       // Purpose:   Add a new line to subfile
017822141008       // Returns:
017823141008       // --------------------------------------------------
017824141008       DCL-PROC new_line ;
017825141008
017830141008
017831141008         pin1kpayc = *blanks;
017832141008         pin1desc = *blanks;
017833141008         pin1paytp = *zeros;
017834141008         pin1mult = *zeros;
017835141008
017836141008         rrn += 1;
017838141008         write sflrec;
017839141008
017840141008       END-PROC ;
017841141008
017848141008
017849141008       // --------------------------------------------------
017850141008       // Procedure name: Validate_sr
017851141008       // Purpose:   Validate subfile entries
017852141008       // Returns:
017853141008       // --------------------------------------------------
017854141008       DCL-PROC Validate_sr;
017855141008         DCL-PI *N IND END-PI;
017857141008
017858141008           DCL-S rrx PACKED(4);
017859141008           DCL-S rr  PACKED(4);
017860141008           DCL-S recFound  PACKED(4);
017861141008           DCL-S errorFound IND;
017862141008           DCL-S KronosCodeARR  CHAR(5) DIM(99);
017863141008
017864141009
017865141009         // Setoff all indicators
017866141009         Errors = *all'0';
017880141008
017883141008         rrx = rrn;
017884141008         for rr = 1 to rrx;
017885141008
017886141008           chain RR sflrec;
017887141008
017888141008           // validate record not set for deletion
017889141008           if sel# <> 'X';
017890141008
017891141008             // Load Kronos Pay code into array
017892141008             // and Validate duplicate kronos pay code entry
017893141008                if %lookup(pin1kpayc:KronosCodeARR) <> *zeros;
017895141009                   Duplication_error = *on;
017896141008                  else;
017897141008                   KronosCodeARR(rr) = pin1kpayc;
017898141008                endif;
017900141008
017901141008             // validate Kronos Pay code
017902141008             if pin1kpayc <> *blanks;
017903141008
017904141008                recFound = *zeros;
017905141008
017906141008                 exec sql
017907141008                     select 1 into :recFound
017908141008                        from ckworkcd where wccode = :pin1kpayc;
017909141008
017910141008                if recFound = *zeros;
017911141009                  KronosCode_error = *on;
017912141008                endif;
017913141008
017914141008                // Validate description
017915141008                // Check for blanks
017916141008                if pin1desc = *blanks;
017917141009                  Description_error = *on;
017918141008                endif;
017919141008
017920141008
017921141008                // Validate pay type
017922141008                // Check for valid code
017923141008                if get_payty_info('RD':pin1paytp:payty_struc);
017924141009                  Paytype_error = *on;
017925141008                endif;
017926141008
017927141008                // Validate multiplier
017928141008                if pin1mult = *zeros;
017929141009                  Multiplier_error = *on;
017930141008                endif;
017931141008
017932141008
017933141008             endif;
017934141008
017985141008
017986141009             If %scan('1':Errors) <> *zeros;
017988141008               errorFound = *on;
017989141008             endif;
017990141008
017991141008             update sflrec;
017992141008             If errorFound;
017994141008               leave;
017995141008             endif;
017996141008
018003141008
018004141008           endif;
018005141008
018006141008         endfor;
018007141008
018008141008         // Reset relative record number for application
018009141008         rrn = rrx;
018010141008         return  errorFound;
018011141008
018012141008       END-PROC ;
018013141008
018014141008
018015140922
018016140922       // --------------------------------------------------
018017140922       // Procedure name: ResetSubfile
018018140922       // Purpose:
018019140922       // Returns:
018020140922       // --------------------------------------------------
018021140922       DCL-PROC ResetSubfile ;
018022141008
018023141009         SflClear = *on;
018024140922         write sflctl;
018025141009         SflClear = *off;
018027141008
018028141008         // Clear and reset entry fields
018029140922         clear sel#;
018031140922         reset rrn;
018032141008         *in55 = *off;
018033141008
018034140922       END-PROC ;
018035140922
018036141008
018037141008       // ---------------------------------------------------------
018038140922       // Procedure name: openFilePrc
018039141008       // Purpose:        set cursor for file to be opened-payintf1
018040140922       // Returns:
018041141008       // ---------------------------------------------------------
018042140922       DCL-PROC openFilePrc ;
018043141008
018044141008
018045140922           exec sql
018046141008            declare payint1file cursor for select *
018047141008              from payintf1 where
018048141008                pin1sts = 'A';
018049140922
018050140922            exec sql
018051141008               open payint1file;
018052140922       END-PROC ;
018053140922
018054140922       // --------------------------------------------------
018055140922       // Procedure name: closeFilePrc
018056141008       // Purpose:        close file cursor for payintf1
018057140922       // Returns:
018058140922       // --------------------------------------------------
018059140922       DCL-PROC closeFilePrc ;
018060140922            exec sql
018061141008               close payint1file;
018062140922       END-PROC ;
018063140922
018064140922       // --------------------------------------------------
018065140922       // Procedure name: readFilePrc
018066140922       // Purpose:        Read data from file into data structure
018067140922       // Returns:
018068140922       // --------------------------------------------------
018069140922       DCL-PROC readFilePrc ;
018070140922         DCL-PI *N IND;
018071141008         END-PI;
018072141008
018073140922           DCL-S retField IND;
018074140922
018075141008              exec sql
018076141008                 fetch next from payint1file into :payint1DS;
018077141008
018078140922                   if sqlstt <> '00000';
018079140922                      return *on;
018080140922                     else;
018081140922                      return *off;
018082140922                   endif;
018083140922
018084140922           return retField;
018085141008
018086140922       END-PROC ;
018087140922
018119140922
018173141008
018174140922**  Program Title
018175141009Payroll Kronos Interface Configuration
