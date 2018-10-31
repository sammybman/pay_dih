000100180312     H bnddir('DISDIR':'WHOUSEDIR') dftactgrp(*NO) actgrp(*CALLER)
000200140312      //* LISTING PAYROLL YEAR TO DATES
000300140312      //* ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS
000400140312      //*                     C.WILLIAMS   & S.BRITTON
000500140312      // Payroll PROTOTYPES
000600140307      /copy qcpysrc,genPR
000700140307      /copy qcpysrc,empPR
000800140307      /copy qcpysrc,PAYPR
000900180131      /copy qcpysrc,salpr
001000140307      /copy qcpysrc,taxpr
001100140312      //-------------------------------------
001200140312      //
001300950930     D                SDS
001400950930     D  @PGM             *PROC
001500140307     D  @USER                358    367
001600140312      //
001700180312
001800180312
001900180312
002000180312
002100180312       //
002200180312       DCL-DS graDataDS EXTNAME('GRAEMPFT') qualified INZ;
002300180312       END-DS ;
002400180312
002500180312       DCL-DS CompanyDataDS INZ LIKEDS(companyInfoDS);
002600180312       // --------------------------------------------------
002700180312       // Prototype for procedure: ReadFilePrc
002800180312       // --------------------------------------------------
002900180312       DCL-PR ReadFilePrc IND;
003000180312       END-PR ;
003100180316     DGET_TAXCLC       PR                  extpgm('PAYTAXCALX')
003200180316     D   txemp##                      6  0       options(*nopass)
003300180316     D   gross##                     11  2       options(*nopass)
003400180316     D   tottax##                    11  2       options(*nopass)
003500180316     D   employed##                   8  0       options(*nopass)
003600180319     D   STATUS##                     1          options(*nopass)
003700180312
003800180312
003900090220
004000180312             graDataDS.graconame = get_coname(*omit:CompanyDATADS);
004100021030
004200180312         //month_year_code = 'WK';
004300021030
004400180312          openFilePrc();
004500180312          dow not(ReadFilePrc());
004600180312          if not writeFilePrc();
004700180312            dsply  graDataDS.graemp;
004800180312          ENDIF;
004900180312
005000021030          enddo;
005100180312          closeFilePrc();
005200180312          eval *inlr = *on;
005300021030          return;
005400021030
005500021031
005600160301        //*************************************************
005700160301        // First initilisation routine for all programs  **
005800160301       //*************************************************
005900140312       Begsr aainit;
006000180312        // @date = get_sys_date;
006100180312        // @title = arr(1);
006200140312         //
006300140312       Endsr;
006400140312       //***********************
006500140312       // Start-up subroutine **
006600140312       //***********************
006700140312       begsr *inzsr;
006800140312      /END-FREE
006900180312     C
007000021030     C
007100140312       endsr;
007200180312
007300180312
007400090225
007500180213
007600180213
007700180312
007800180312       // --------------------------------------------------
007900180312       // Procedure name: openFilePrc
008000180312       // Purpose:
008100180312       // Returns:
008200180312       // --------------------------------------------------
008300180312       DCL-PROC openFilePrc ;
008400180312
008500180312                 exec sql
008600180312                   declare empcursor cursor for select * from
008700180312              graempft  where graemp <> 0 order by graemp for fetch only;
008800180312
008900180312              exec sql
009000180312             open empcursor;
009100180312
009200180312
009300180312       END-PROC ;
009400180312       // --------------------------------------------------
009500180312       // Procedure name: ReadFilePrc
009600180312       // Purpose:
009700180312       // Returns:
009800180312       // --------------------------------------------------
009900180312       DCL-PROC ReadFilePrc ;
010000180312         DCL-PI *N IND;
010100180312         END-PI ;
010200180312         // Your local fields go here
010300180312
010400180312         DCL-S retField IND;
010500180312
010600180312            exec sql
010700180312            fetch next from empcursor into :GRAdataDS;
010800180312          if SQLSTT = '02000';
010900180312           retField = *on;
011000180312        else;
011100180312
011200180312          retField =  *off;
011300180312        endif;
011400180312         return retField ;
011500180312       END-PROC ;
011600180312
011700180312       // --------------------------------------------------
011800180312       // Procedure name: closeFilePrc
011900180312       // Purpose:
012000180312       // Returns:
012100180312       // --------------------------------------------------
012200180312       DCL-PROC closeFilePrc ;
012300180312              exec sql close empcursor;
012400180312
012500180312           return ;
012600180312       END-PROC ;
012700180312
012800180312       // --------------------------------------------------
012900180312       // Procedure name: WriteFilePrc
013000180312       // Purpose:
013100180312       // Returns:        Success/Failure Flag
013200180312       // --------------------------------------------------
013300180312       DCL-PROC WriteFilePrc ;
013400180312         DCL-PI *N IND;
013500180312         END-PI ;
013600180312         // Your local fields go here
013700180312
013800180312         DCL-S retField IND;
013900180312         //
014000180312       DCL-S cophone LIKE(phone#) INZ(*BLANK);
014100180312       DCL-S ephone# LIKE(phone#) INZ(*BLANK);
014200180312       DCL-S edescrp# LIKE(descrp) INZ(*BLANK);
014300180312       DCL-S eempemail# LIKE(empemail) INZ(*BLANK);
014400180312         //
014500180312       DCL-S file_name# CHAR(30) INZ(*BLANK);
014600180312       DCL-DS GRA_EMP_STRUCDS EXTNAME('EMPMAST') QUALIFIED;
014700180312
014800180312       END-DS;
014900180312         //
015000180312       DCL-S year_of_assessment# packed(2) INZ(18);
015100180312         //
015200180312       DCL-S empID# CHAR(11) INZ(*BLANK);
015300180312         //
015400180312       DCL-S period_emp# PACKED(5:2) INZ(0);
015500180312       //
015600180312       DCL-S DNAME# CHAR(30) INZ(*BLANK);
015700180316     D   tottax        S             11  2 INZ(0)
015800180316     D   DUMMYGROSS    S             11  2 INZ(0)
015900180316     D   employed      S              8  0 INZ(0)
016000180312          graDataDS.gracotin = companyinfoDS.taxreg#;
016100180312
016200180312          get_emp_info2('R': graDataDS.graemp:GRA_EMP_STRUCDS);
016300180312          empid# = GRA_EMP_STRUCDS.regno;
016400180316           EMPLOYED = GRA_EMP_STRUCDS.EMPLY;
016500180312                period_emp# =  graDataDS.graweeks# /52 *12;
016600180312                ephone# = graDataDS.phone#;
016700180312                eempemail# = graDataDS.empemail;
016800180312                edescrp# = graDataDS.descrp;
016900180316         file_name# = *BLANKS;
017000180312            graDataDS.gratin = %trim(graDataDS.gratin);
017100180312            EVAL dname# =  graDataDS.dname;
017200180319               tottax = *zero;
017300180316               GET_TAXCLC(graDataDS.graemp:DUMMYGROSS
017400180319               :TOTTAX:employed:GRA_EMP_STRUCDS.STATUS);
017401180320               IF totax > graDataDS.grapaye
017402180320                 and  graDataDS.graemp <> 426
017403180320                 and graDataDS.graemp <> 700;
017404180320                 tottax = graDataDS.grapaye;
017405180320               ENDIF;
017500180312            exec sql
017600180312              insert into graempf
017700180312                values('Y',:graDataDS.GRATIN,:graDataDS.graemp,
017800180312              :graDataDS.grafname,:graDataDS.gralname,:graDataDS.graonames,
017900180312                :graDataDS.graaddress,:period_emp#,:graDataDS.gragross,
018000180312              :graDataDS.otherinc,:graDataDS.grastsded,
018100180319                :graDataDS.granis,:graDataDS.totdeduct,:TOTTAX,
018200180312              :graDataDS.grapaye,:dname#,:file_name#,
018300180312              :year_of_assessment#,:graDataDS.gradob,:empID#,:edescrp#,:ephone#,
018400180312              :eempemail#,:graDataDS.gracotin,:graDataDS.graconame,:cophone);
018500180312
018600180312                retField = sqlstt = '00000';
018700180312
018800180312         return retField ;
018900180312       END-PROC ;
019000180312
