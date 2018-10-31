000100950811     H DECEDIT('.') DATEDIT(*DMY/)
000200940929     F******* GRADES 6'S TO 9'S ALLOWANCES ---- S.BRITTON ******
000300961217     Fapthl01   IF   E           K DISK
000400990115
000500990114     **
000600950929     D                SDS
000700950929     D  @PGM             *PROC
000800950929     D  @USER                254    263
000900961218     **
001000961218     Dtimegrd                          D   datfmt(*eur)
001100961218     Dtimecur                          D   datfmt(*eur)
001200951101     **
001300990115     C     *entry        plist
001400990115     C                   parm                    emp               6 0
001500990115     C                   parm                    class             3
001600990115     C                   parm                    per_alloc         3 2
001700990115     C                   parm                    mnts              5 0
001800990115     **
001900961218     C     *like         define    class         aclass
002000961217     **
002100961217     C     aptkey        klist
002200961217     C                   kfld                    emp
002300961218     C                   kfld                    aclass
002400950811     **
002500950811     C     'P'           SCAN      CLASS                                  06
002600950811     **
002700961218     **
002800961218      *** ROUTINE HANDLING ALLOWANCE PERCENTAGE ALLOCATION FOR GRADES 8 & 9
002900961218      *** EMPLOYEES
003000961218     **
003100990213     C                   if        class = '  8' or class =' P8'
003200961218     C                             or class = '  9' or class = ' P9'
003300961218     C                   move      *date         timecur
003400961218     C                   move      class         aclass
003500961218     ** Search for first occurance of grade change
003600961218     C  n06              movel     ' P'          aclass
003700961218     C     aptkey        chain     aptfmt                             98
003800961218     **
003900961218     C                   if        *in98 and not(*in06)
004000961218     C                   movel     '  '          aclass
004100961218     C     aptkey        chain     aptfmt                             98
004200961218     C                   endif
004300961218     **
004400961218     C     *in98         ifeq      *off
004500991109     C     appcls        andne     '  9'
004600991109     C     appcls        andne     ' P9'
004700991109     C     appcls        andne     ' 9D'
004800961218     C                   move      apdate        timegrd
004900961218     C     timecur       subdur    timegrd       nummnt:*m         5 0
005000961218
005100961218     ** 75% for grades 8 & 9 for first year of promotion
005200970929     C                   if        nummnt < 12
005300961218     **
005400990115     C                   z-add     .75           per_alloc
005500990115     C                   z-add     nummnt        mnts
005600961218     **
005700961218     C                   else
005800961218     ** 85% for grades 8 & 9 during second year of promotion
005900970929     C                   if        nummnt >= 12 and nummnt < 24
006000961218     **
006100990115     C                   z-add     .85           per_alloc
006200990115     C                   z-add     nummnt        mnts
006300961218     **
006400990213     C                   else
006500990213     ** 100% for grades 8 & 9 during second year of promotion
006600990213     C                   if        nummnt >= 24
006700990213     **
006800990213     C                   z-add     1.00          per_alloc
006900990213     **
007000990213     C                   endif
007100961218     C                   endif
007200961218     C                   endif
007300961218
007400990213     C                   else
007500990213     C                   z-add     1.00          per_alloc
007600961218     C                   endif
007700990213     **
007800990115     C                   else
007900990115     C                   z-add     1.00          per_alloc
008000961218     C                   endif
008100990115     **
008200990115     C                   seton                                        lr
008300990115     C                   return
008400990115     **
