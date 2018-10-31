000100941121     H DATEDIT(*DMY)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300021005     F* System Control File
000400051016     Fempcon    up   e             disk    commit
000500021005     **
000600021005     d prno            s              6  0
000700021005     **
000800021005     C                   eval      esysupdprd = prno
000900021005     C                   if        esysdedcyh <> esysdedcyc
001000021005     C                   eval      esysdedcyh = esysdedcyc
001100021005     C                   endif
001200021005     ** update control record
001300021005     C                   update    esyfmt
001400021005     **
001500021005     **********
001600021005     ** iniz **
001700021005     **********
001800020206     C     *inzsr        begsr
001900020206     C     *entry        plist
002000021005     C                   parm                    prno
002100021005     C                   endsr
002200980115     **
