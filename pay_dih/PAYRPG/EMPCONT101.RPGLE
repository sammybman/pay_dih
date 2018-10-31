000100111013     H DECEDIT('.') bnddir('DISDIR') dftactgrp(*no) actgrp(*caller)
000200111013     H/define profoundui
000300021010     Fsavconf2  uf a e           k disk
000400021010     Fempcont1d1cf   e             workstn
000500021010     F                                     sfile(sub1:RRN1)
000600111013     F/IF DEFINED(WEB)
000700111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000800111013     F/ENDIF
000900111013
001000111013      /copy QCPYSRC,genpr
001100020115      **
001200021010     D ARR             S             40    dim(1) CTDATA PERRCD(1)
001300020115     D date            S              8  0
001400021010     D upper           S                   like(sac2upsrv)
001500021010     D heading1        c                   'Interest'
001600021010     D heading2        c                   'Company Contribution'
001700021010
001800000306     D                sds
001900000306     D  @pgm             *proc
002000111013     D  @user                358    367
002100000403     C
002200000605     C
002300981005     C
002400000327     C     *ENTRY        PLIST
002500021010     C                   PARM                    sav_type          1
002600020621     **
002700020621     C     trn_key       klist
002800021010     C                   kfld                    sav_type
002900020621     C
003000021010     C                   select
003100021010     C                   when      sav_type = 'I'
003200021010     C                   eval      sav_desc = heading1
003300021010     C                   when      sav_type = 'E'
003400021010     C                   eval      sav_desc = heading2
003500021010     C                   endsl
003600000606     C
003700000605     C
003800021010     C     trn_key       chain     sac2fmt
003900021010     C                   if        not(%found)
004000000403     **
004100000403     C     entry_top     tag
004200000331     C                   exsr      range_entry
004300000331     C                   else
004400000403     **
004500000403     C     update_top    tag
004600000331     C                   exsr      range_update
004700000403     C
004800000331     C                   endif
004900000306     C
005000000331     *****************************************
005100000331     ** subroutine to enter Discount Ranges **
005200000331     *****************************************
005300000324     C
005400000331     C     range_entry   Begsr
005500000214     C
005600021010     C                   move      '1'           *IN33
005700000306     C                   WRITE     subctl1
005800021010     C                   move      '0'           *IN33
005900000406     C                   Z-ADD     0             RRN1              4 0
006000000306     C
006100000406     ** write line to subfile
006200021010     c                   eval      sac2lwsrv@ = *zeros
006300021010     c                   eval      sac2upsrv@ = *zeros
006400021010     c                   eval      sac2per@ = *zeros
006500000406     C
006600000406     C                   add       1             RRN1
006700000406     C                   write     sub1
006800000406     C
006900000323     C                   exsr      aainit
007000000331     C                   movel(p)  'ENTRY'       mode
007100000313     C
007200000313     C     err_tag       tag
007300000405     C                   write     fmt1
007400000306     C                   exfmt     subctl1
007500000607     C                   setoff                                       76
007600000406     C                   setoff                                       707172
007700000406     C                   setoff                                       737475
007800000406     C                   move      *blanks       msgline
007900000214     C
008000020115     C                   if        *in03
008100000306     C                   seton                                        lr
008200000306     C                   return
008300000331     C                   endif
008400000403     C
008500000403     ** validate subfile
008600000403     C                   exsr      validate_sr
008700000405     C     *in70         cabeq     *on           err_tag
008800000331     C
008900000607     C     *in76         cabeq     *on           err_tag
009000000331     ** Add a new line
009100000331     C                   if        *in04
009200000331     C                   exsr      new_line
009300000331     C                   endif
009400000331     C
009500000331     ** Confirm Record Entry
009600000331     C
009700000331     C
009800000331     C     *in15         ifeq      *on
009900000403     C                   exsr      confirm
010000000331     C                   endif
010100000331     C
010200000331     C                   goto      err_tag
010300000330     C
010400000331     C                   endsr
010500000331     C
010600000331     C
010700000331     ******************************************
010800000331     ** subroutine to update Discount Ranges **
010900000331     ******************************************
011000000331     C     Range_update  Begsr
011100000406     c     up_top        tag
011200000406     C                   MOVE      '1'           *IN33
011300000406     C                   WRITE     subctl1
011400000406     C                   MOVE      '0'           *IN33
011500000406     C                   Z-ADD     0             RRN1              4 0
011600000406     C                   z-add     0             count
011700000331     C
011800021010     C     trn_key       chain     sac2fmt                            30
011900000331     C
012000000331     C
012100000331     C                   movel(p)  'UPDATE'      mode
012200000331     C
012300000331     C                   exsr      aainit
012400000331     C                   write     fmt1
012500000331     C
012600000331     C     *IN30         DOWEQ     '0'
012700021010     C                   eval      sac2lwsrv@ = sac2lwsrv
012800021010     C                   eval      sac2upsrv@ = sac2upsrv
012900021010     C                   eval      sac2per@ = sac2per
013000000331     C
013100000406     C                   ADD       1             RRN1
013200000331     C                   WRITE     sub1
013300021010     C     trn_key       READE     sac2fmt                                30
013400000331     C                   ENDDO
013500000331     C
013600000331     C     sub_top       tag
013700000331     C                   exfmt     subctl1
013800000605     C*                  seton                                        80
013900000406     C                   if        *in03
014000000406     C                   seton                                        lr
014100000406     C                   return
014200000406     C                   endif
014300000406
014400000406     C                   setoff                                       707172
014500000406     C                   setoff                                       737475
014600000607     C                   setoff                                       76
014700000406
014800000406     C                   if        count = 1
014900000406     C                   goto      up_top
015000000406     C                   endif
015100000406
015200000331     C
015300000331     C
015400000331     C                   read      fmt1
015500000331     C
015600000331     ** end of program
015700000331     C
015800000403     C
015900000403     ** Validate subfile
016000000403     C                   exsr      validate_sr
016100000406     C     count         cabeq     1             sub_top
016200000403     C
016300000403     C
016400000403     ** Add a line to Sub-file
016500000403     C
016600000403     C                   if        *in04
016700000403     C                   exsr      new_line
016800000403     C                   endif
016900000403     C
017000000331     C
017100000403     ** Confirm Record Update
017200000331     C
017300000331     C                   if        *in15
017400000403     c                   exsr      delete_sr
017500000403     C                   exsr      confirm
017600000331     C                   endif
017700000331     C
017800000331     C
017900000331     C                   goto      sub_top
018000000331     C
018100000331     C                   endsr
018200000331     C
018300000330     C
018400000331     ********************************************
018500000331     ** Subroutine to add new line to subfile  **
018600000331     ********************************************
018700000330     C
018800000331     C     new_line      Begsr
018900000330     C
019000000407     C                   update    sub1
019100000524     **
019200021010     c                   eval      sac2lwsrv@ = *zeros
019300021010     c                   eval      sac2upsrv@ = *zeros
019400021010     c                   eval      sac2per@ = *zeros
019500021010     C                   add       1             rrn1
019600000330     C                   write     sub1
019700000331     C                   endsr
019800000330     C
019900000331     C
020000000331     ******************************************
020100000331     ** Subroutine to confirm changes made   **
020200000331     ******************************************
020300000324     C
020400000403     C     Confirm       begsr
020500000324     C
020600000324     C                   z-add     1             x                 2 0
020700000327     C     1             add       rrn1          y                 2 0
020800000324     C     x             downe     y
020900000330     c
021000000407     C     x             chain     sub1
021100000525     ** write record not set for deletion
021200000524     C                   if        delopt <> 'X'
021300000330     C
021400021010     c                   eval      sac2lwsrv = sac2lwsrv@
021500021010     c                   eval      sac2upsrv = sac2upsrv@
021600021010     c                   eval      sac2per = sac2per@
021700000330     C
021800021010     C                   eval      sac2styp = sav_type
021900000324     C
022000021010     C                   if        sac2lwsrv@ <> *zeros and
022100021010     C                             sac2upsrv@ <> *zeros
022200021010     C                   write     sac2fmt
022300000406     C                   endif
022400000524     C                   endif
022500000406     C
022600000404     C                   add       1             x
022700000324     C                   enddo
022800000324     C
022900000324     C
023000000330     C                   seton                                        lr
023100000330     C                   return
023200000324     c
023300000324     C                   endsr
023400000403     C
023500000403     C
023600000403     C
023700000403     *****************************************************************
023800000403     ** Begin subroutine to Delete records before updating
023900000403     *****************************************************************
024000000403     C     Delete_sr     BEGSR
024100000403     C
024200021010     C     trn_key       setll     sac2fmt
024300021010     C     trn_key       reade     sac2fmt
024400000403     C
024500021010     C                   dow       not(%eof)
024600021010     C                   delete    sac2fmt
024700021010     C     trn_key       reade     sac2fmt
024800000403     C                   enddo
024900000403     C
025000000403     C
025100000403     C                   endsr
025200000403     C
025300000403     C
025400000403     C
025500000403     **********************
025600000403     ** Validate Subfile **
025700000403     **********************
025800000403     C     Validate_sr   BEGSR
025900000403     C
026000000406     C                   setoff                                       707172
026100000406     C                   setoff                                       737475
026200000524     C                   setoff                                       76
026300000406     c                   z-add     0             count
026400000524     **
026500021010     C                   eval      upper = *zeros
026600000524     **
026700000605     C
026800000403     C                   Z-ADD     rrn1          RRX               4 0
026900000403     C     1             DO        RRX           RR                4 0
027000000403     C     RR            CHAIN     Sub1                               H6
027100000524     ** validate record not set for deletion
027200000524     C                   if        delopt <> 'X'
027300000403     C
027400000403     ** validate Lower and Upper Range
027500021010     c                   if        sac2upsrv@ = 0
027600000403     C                   seton                                        70
027700000403     C                   endif
027800000403     **
027900000406     C
028000021010     ** Validate percentage entry
028100000406     ** Check for blanks
028200021010
028300021010     C                   if        sac2per@ = *zeros
028400000406     C                   seton                                        74
028500000406     C                   endif
028600000406     C
028700000406     C
028800000524     C
028900000524     ** If lower range value is higher than upper range value
029000021010     C                   eval      *in76 = sac2lwsrv@ >= sac2upsrv@
029100000524     C
029200000524     ** If lower range value is lower than upper range value
029300000524     ** of a previous record
029400021010     C                   if        upper <> *zeros
029500000530     c
029600021010     C                   if        sac2lwsrv@ <= upper
029700000607     C
029800000530     C                   endif
029900000530     C
030000000524     C                   endif
030100000406
030200000406     C                   If        *in70 or *in71 or *in72
030300000406     C                             or *in73 or *in74 or *in75
030400000607     c                   seton                                        70
030500000607     C                   endif
030600000607     C                   If        *in70 or *in76
030700000406     c                   z-add     1             count             1 0
030800000406     C                   update    sub1
030900000406     C                   leave
031000000406     C                   endif
031100000524     ** save previous upper range record
031200021010     C                   eval      upper = sac2upsrv@
031300000524     C                   endif
031400000403     C
031500000403     C                   enddo
031600000404     ** Reset relative record number for apllication
031700000405     C                   Z-ADD     rrx           rrn1
031800000404     **
031900980728     C
032000000406     C                   endsr
032100000403     C
032200000403     C
032300000403     C
032400000306     *****************************************************************
032500000306     ** Begin subroutine to print header
032600000306     *****************************************************************
032700000306     C     AAINIT        BEGSR
032800010815     C                   eval      coname = get_coname
032900020115     C                   eval      @date  = get_sys_date(date)
033000020115     C                   eval      @title = arr(1)
033100000306     C                   TIME                    @TIME             6 0
033200000306     C
033300000306     C                   ENDSR
033400000306     **
033500000306**   Header to be displayed
033600021010         Savings Configuration
