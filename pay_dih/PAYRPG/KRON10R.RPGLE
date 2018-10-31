000101051230     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
000200950429     H******  PROGRAMMER - SAMUEL BRITTON
000300060511     Fkempm     uf   e           k disk
000400960307     **
000500960307     **
000600050126      /free
000601060511        setll *loval kerec;
000602060511        read kerec;
000603060511        dow not %eof();
000700060511         //******************************//
000800060511         //**  DELETE EMPLOYEE RECORD  **//
000900060511         //******************************//
001000050126          if kreqc = 4
001100050126            or kreqc = 3;
001200050126              delete(e) kerec;
001300050126          endif;
001301060511         read kerec;
001400060511        enddo;
001500060511
001600060511        *inlr = *on;
001700060511        return;
