000100950711     H DECEDIT('.') DATEDIT(*DMY/)
000200980128     F*EMPLOYEES ACCOUNTS---BY EMPLOYEE NUMBER
000300980122     F*PROGRAMMER:-S. britton
000400020206     F* LIABILITY TRANSACTION FILE
000500980119     Fliabtl06  if   e           k disk
000600020206     F* LIABILITY MASTER FILE
000700051102     Fliabm     uf   e           k disk    commit
000800970403     **
000900980120     D                sds
001000980120     D  @pgm             *proc
001100980120     D  @user                254    263
001200980119     **
001300980119     D la              s             11  2 dim(12)
001400020206     D prd             s              6  0
001500020206     D prd#            s              6  0
001600020412     D smth            s              2  0
001700980121     **
001800980121     D                 ds
001900980121     D s_prd                   1      6  0
002000070802     D year                    1      4  0 inz(2007)
002100070802     D month                   5      6  0 inz(08)
002200980202     **
002300980119     Ilifmt
002400980119     I              liamt01                     la(01)
002500980119     I              liamt02                     la(02)
002600980119     I              liamt03                     la(03)
002700980119     I              liamt04                     la(04)
002800980119     I              liamt05                     la(05)
002900980119     I              liamt06                     la(06)
003000980119     I              liamt07                     la(07)
003100980119     I              liamt08                     la(08)
003200980119     I              liamt09                     la(09)
003300980119     I              liamt10                     la(10)
003400980119     I              liamt11                     la(11)
003500980119     I              liamt12                     la(12)
003600980122     **
003700980122     C     trn_key       klist
003800980122     C                   kfld                    typ
003900980122     C                   kfld                    liemp
004000980217     C                   kfld                    styp              3 0
004100980122     C                   kfld                    s_prd
004200020611     **
004300020611     C     trn_key2      klist
004400020611     C                   kfld                    typ
004500020611     C                   kfld                    styp
004600020611     **
004700020611     C     trn_key3      klist
004800020611     C                   kfld                    typ
004900020611     C                   kfld                    liemp
005000020611     C                   kfld                    styp
005100980218
005200020611     C     trn_key2      chain     lifmt                              lr
005300980122     **
005400980122     C                   dow       not(*inlr)
005500020611     C*                  eval      totdeb = *zeros
005600980122     **
005700051102     C     trn_key       setll     ltfmt
005800051102     C     trn_key       reade     ltfmt
005900980404     **
006000980122     ** Select transactions for report period
006100051102     C                   dow       not(%eof)
006200980120     **
006300051102     C                   add       ltamt         totdeb           11 2
006400980120     **
006500051102     C     trn_key       reade     ltfmt
006600051102     C                   enddo
006700980122     **
006800980122     ** Print employee total lines
006900980122     **
007000051129     C***************    eval      la(11) = 0
007100070802     C                   eval      la(emth) = totdeb + la(07)
007200051102     C                   eval      totdeb = *zeros
007300020412     C                   update    lifmt
007400980122
007500020611     C     trn_key2      reade     lifmt                                  lr
007600980122     C                   enddo
007700980218
007800980218
007900980120     ******************************************************
008000980120     ** Initial routine **
008100980120     ******************************************************
008200020206     C     *inzsr        begsr
008300020206     **
008400070802     C                   z-add     200708        prd               6 0
008500070802     C                   z-add     22            typ               2 0
008600051102     C                   z-add     0             styp              3 0
008700020412     **
008800020206     C                   move      prd           emth              2 0
008900980122     **
009000020412     C                   eval      smth = emth - 1
009100980122     C                   if        smth = 0
009200980122     C                   z-add     12            smth
009300980122     C                   endif
009400980121     **
009500980120     C                   endsr
009600980120     **
