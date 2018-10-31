000100950711     H DECEDIT('.') DATEDIT(*DMY/)
000200980128     F*EMPLOYEES ACCOUNTS---BY EMPLOYEE NUMBER
000300980122     F*PROGRAMMER:-S. britton
000400020206     F* LIABILITY TRANSACTION FILE
000500980119     Fliabtl06  if   e           k disk
000600020206     F* LIABILITY MASTER FILE
000700050127     Fliabm     if   e           k disk
000800020206     F* EMPLOYEE MASTER FILE
000900980121     Fempmast   if   e           k disk
001000970403     **
001100980120     D                sds
001200980120     D  @pgm             *proc
001300980120     D  @user                254    263
001400980119     **
001500980119     D la              s             11  2 dim(12)
001600020206     D prd             s              6  0
001700020206     D prd#            s              6  0
001800020412     D smth            s              2  0
001900050127     D Bigtotal        s             11  2
002000980121     **
002100980121     D                 ds
002200980121     D s_prd                   1      6  0
002300980121     D year                    1      4  0
002400050323     D month                   5      6  0 inz(01)
002500980202     **
002600980119     Ilifmt
002700980119     I              liamt01                     la(01)
002800980119     I              liamt02                     la(02)
002900980119     I              liamt03                     la(03)
003000980119     I              liamt04                     la(04)
003100980119     I              liamt05                     la(05)
003200980119     I              liamt06                     la(06)
003300980119     I              liamt07                     la(07)
003400980119     I              liamt08                     la(08)
003500980119     I              liamt09                     la(09)
003600980119     I              liamt10                     la(10)
003700980119     I              liamt11                     la(11)
003800980119     I              liamt12                     la(12)
003900980122     **
004000980122     C     trn_key       klist
004100980122     C                   kfld                    typ
004200980122     C                   kfld                    liemp
004300980217     C                   kfld                    styp              3 0
004400980122     C                   kfld                    s_prd
004500980119     **
004600980218
004700980122     C     typ           chain     lifmt                              lr
004800980122     **
004900980122     C                   dow       not(*inlr)
005000020412     C                   eval      totdeb = *zeros
005100980122     **
005200020412     C     trn_key       setll     ltfmt
005300020412     C     trn_key       reade     ltfmt                                  99
005400980404     **
005500980122     ** Select transactions for report period
005600980122     C                   dow       not(*in99)
005700980120     **
005800020412     C                   add       ltamt         totdeb           11 2
005900980120     **
006000020412     C     trn_key       reade     ltfmt                                  99
006100980119     C                   enddo
006200980122     **
006300980122     ** Print employee total lines
006400980122     **
006500050127     C                   eval      Bigtotal = la(smth) + totdeb
006600050127     C**                 eval      la(emth) = la(smth) + totdeb
006700050127      /free
006800050127            if la(emth) <> Bigtotal;
006900050127               dsply ltemp;
007000050127            endif;
007100050127      /end-free
007200020412     C                   eval      totdeb = *zeros
007300980122
007400980122     C     typ           reade     lifmt                                  lr
007500980122     C                   enddo
007600980218
007700980218
007800980120     ******************************************************
007900980120     ** Initial routine **
008000980120     ******************************************************
008100020206     C     *inzsr        begsr
008200020206     **
008300050323     C                   z-add     200501        prd               6 0
008400050323     C                   z-add     1             typ               2 0
008500020412     **
008600020206     C                   move      prd           emth              2 0
008700020206     C                   movel     prd           year              4 0
008800980122     **
008900020412     C                   eval      smth = emth - 1
009000980122     C                   if        smth = 0
009100980122     C                   z-add     12            smth
009200980122     C                   endif
009300980121     **
009400980120     C                   endsr
009500980120     **
