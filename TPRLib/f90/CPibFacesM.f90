      MODULE CPibFaces
!
!     C interface for routines contained in CPibAccess.c
!
      INTERFACE
!
        SUBROUTINE OpenPibImportFile(err, fnum, filename, pibFileType,   &
     &                               version,description)
        CHARACTER(LEN=*) :: filename,pibFileType,version,description
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C,ALIAS:'_openpibimportfile'::openpibimportfile
        !DEC$ATTRIBUTES REFERENCE :: filename, err, fnum, pibFileType
        !DEC$ATTRIBUTES REFERENCE :: version,description
        END SUBROUTINE OpenPibImportFile
!
        SUBROUTINE OpenPibExportFile(err, fnum, filename, pibFileType,   &
     &                               version,description)
        CHARACTER(LEN=*) :: filename,pibFileType,version,description
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C,ALIAS:'_openpibexportfile'::openpibexportfile
        !DEC$ATTRIBUTES REFERENCE :: filename, err, fnum, pibFileType
        !DEC$ATTRIBUTES REFERENCE :: version,description
        END SUBROUTINE OpenPibExportFile
!
        SUBROUTINE OpenPibAppendFile(err, fnum, filename, pibFileType,   &
     &                               version,description,pibPtr)
        CHARACTER(LEN=*) :: filename,pibFileType,version,description
        INTEGER :: err,fnum,pibPtr
        !DEC$ATTRIBUTES C,ALIAS:'_openpibappendfile'::openpibappendfile
        !DEC$ATTRIBUTES REFERENCE :: filename, err, fnum, pibFileType
        !DEC$ATTRIBUTES REFERENCE :: version,description,pibPtr
        END SUBROUTINE OpenPibAppendFile
!
        SUBROUTINE ClosePibFile(fnum)
        INTEGER :: fnum
        !DEC$ATTRIBUTES C, ALIAS:'_closepibfile' :: closepibfile
        !DEC$ATTRIBUTES REFERENCE :: fnum
        END SUBROUTINE ClosePibFile
!
        SUBROUTINE RewindPibFile(fnum)
        INTEGER :: fnum
        !DEC$ATTRIBUTES C, ALIAS:'_rewindpibfile' :: rewindpibfile
        !DEC$ATTRIBUTES REFERENCE :: fnum
        END SUBROUTINE RewindPibFile
!
        SUBROUTINE openPibBlock(err,fnum,blocktype, revision)
        CHARACTER(LEN=*) :: blocktype
        INTEGER :: err,fnum,revision
        !DEC$ATTRIBUTES C, ALIAS:'_openpibblock' :: openpibblock
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,blocktype, revision
        END SUBROUTINE openPibBlock
!
        SUBROUTINE closePibBlock(err,fnum)
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_closepibblock' :: closepibblock
        !DEC$ATTRIBUTES REFERENCE :: err,fnum
        END SUBROUTINE closePibBlock
!
        SUBROUTINE getNextPibBlock(err,fnum,blocktype, blockparm)
        CHARACTER(LEN=*) :: blocktype
        INTEGER, DIMENSION(3) :: blockparm
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_getnextpibblock' :: getnextpibblock
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,blocktype, blockparm
        END SUBROUTINE getNextPibBlock
!
        SUBROUTINE getNextPibBlockLong(err,fnum,blocktype, blocksize,        &
     &                                 blockCompression, version)
        CHARACTER(LEN=*) :: blocktype
        INTEGER :: blocksize,blockCompression,version,err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_getnextpibblocklong' :: getnextpibblocklong
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,blocktype,blocksize,blockCompression,version
        END SUBROUTINE getNextPibBlockLong
!
        SUBROUTINE skipPibBlock(err,fnum,blocksize)
        INTEGER err,fnum,blocksize
        !DEC$ATTRIBUTES C, ALIAS:'_skippibblock' :: skippibblock
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,blocksize
        END SUBROUTINE skipPibBlock
!
        SUBROUTINE flushPibFile(fnum)
        INTEGER fnum
        !DEC$ATTRIBUTES C, ALIAS:'_flushpibfile' :: flushpibfile
        !DEC$ATTRIBUTES REFERENCE :: fnum
        END SUBROUTINE flushPibFile
!
        SUBROUTINE SetPibPos(err,fnum,val)
        INTEGER :: err,fnum
        INTEGER(SELECTED_INT_KIND(16)) :: val
        !DEC$ATTRIBUTES C, ALIAS:'_setpibpos' :: setpibpos
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE SetPibPos
!
        SUBROUTINE GetPibPos(fnum,val)
        INTEGER :: fnum
        INTEGER(SELECTED_INT_KIND(16)) :: val
        !DEC$ATTRIBUTES C, ALIAS:'_getpibpos' :: getpibpos
        !DEC$ATTRIBUTES REFERENCE :: fnum
        !DEC$ATTRIBUTES REFERENCE :: val
        END SUBROUTINE getPibPos
!
        SUBROUTINE CReadXdrInt64(err,fnum,val)
        INTEGER :: err,fnum
        INTEGER(SELECTED_INT_KIND(16)) :: val
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrint64' :: creadxdrint64
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrInt64
!
        SUBROUTINE CReadXdrInt(err,fnum,val)
        INTEGER :: err,fnum,val
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrint' :: creadxdrint
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrInt
!
        SUBROUTINE CReadXdrFloat(err,fnum,val)
        REAL :: val
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrfloat' :: creadxdrfloat
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrFloat
!
        SUBROUTINE CReadXdrDouble(err,fnum,val)
        INTEGER :: err,fnum
        DOUBLE PRECISION :: val
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrdouble' :: creadxdrdouble
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrDouble
!
        SUBROUTINE CReadXdrString(err,fnum,val,maxlen)
        CHARACTER(LEN=*) :: val
        INTEGER :: err,fnum,maxlen
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrstring' :: creadxdrstring
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val,maxlen
        END SUBROUTINE CReadXdrString
!
        SUBROUTINE CReadXdrBytes(err,fnum,val,maxlen)
        CHARACTER(LEN=*) :: val
        INTEGER :: err,fnum,maxlen
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrbytes' :: creadxdrbytes
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val,maxlen
        END SUBROUTINE CReadXdrBytes
!
        SUBROUTINE CWriteXdrInt(err,fnum,val)
        INTEGER :: err,fnum,val
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrint' :: cwritexdrint
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CWriteXdrInt
!
        SUBROUTINE CWriteXdrInt64(err,fnum,val)
        INTEGER :: err,fnum
        INTEGER(SELECTED_INT_KIND(16)) :: val
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrint64' :: cwritexdrint64
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CWriteXdrInt64
!
        SUBROUTINE CWriteXdrFloat(err,fnum,val)
        REAL :: val
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrfloat' :: cwritexdrfloat
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CWriteXdrFloat
!
        SUBROUTINE CWriteXdrDouble(err,fnum,val)
        DOUBLE PRECISION :: val
        INTEGER :: err,fnum
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrdouble' :: cwritexdrdouble
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CWriteXdrDouble
!
        SUBROUTINE CWriteXdrString(err,fnum,val,maxlen)
        CHARACTER(LEN=*) :: val
        INTEGER :: err,fnum,maxlen
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrstring' :: cwritexdrstring
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val,maxlen
        END SUBROUTINE CWriteXdrString
!
        SUBROUTINE CWriteXdrBytes(err,fnum,val,maxlen)
        CHARACTER(LEN=*) :: val
        INTEGER :: err,fnum,maxlen
        !DEC$ATTRIBUTES C, ALIAS:'_cwritexdrbytes' :: cwritexdrbytes
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val,maxlen
        END SUBROUTINE CWriteXdrBytes
!
        SUBROUTINE CReadXdrArrSize(err,fnum,val)
        INTEGER :: err,fnum,val
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrarrsize' :: creadxdrarrsize
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrArrSize
!
        SUBROUTINE CReadXdrIntArr(err,fnum,len,val)
        INTEGER :: err,fnum,len
        INTEGER,POINTER :: val(:)
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrintarr' :: creadxdrintarr
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrIntArr
!
        SUBROUTINE CReadXdrFloatArr(err,fnum,len,val)
        INTEGER :: err,fnum,len
        REAL,POINTER :: val(:)
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrfloatarr' :: creadxdrfloatarr
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrFloatArr
!
        SUBROUTINE CReadXdrDoubleArr(err,fnum,len,val)
        INTEGER :: err,fnum,len
        DOUBLE PRECISION,POINTER :: val(:)
        !DEC$ATTRIBUTES C, ALIAS:'_creadxdrdoublearr'::creadxdrdoublearr
        !DEC$ATTRIBUTES REFERENCE :: err,fnum,val
        END SUBROUTINE CReadXdrDoubleArr
!
      END INTERFACE
!
      END MODULE CPibFaces
