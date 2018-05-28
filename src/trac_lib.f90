SUBROUTINE trace_init
!
!     (c) 2002 The Pennsylvania University Applied Research Laboratory
!     This Software was developed for Purdue Subcontract Agreement 640-01812-2
!     under NRC Prime Contract no. NRC-04-97-046 (Task #2)
!
      USE IntrType, ONLY: sdk, sik
!
      IMPLICIT NONE
!
     INTEGER(sik) :: i
     REAL(sdk) :: RealVar
!     REAL(sdk) :: dtMean, dtStDev , dt1D, dt3D, dtNet
!
!
      RealVar = i
!
END SUBROUTINE trace_init
!
!  Call the calculational engine
!         CALL trans()
!
!  The calculation is over.  Write the last timestep's information to
!  the output file
SUBROUTINE trace_finalize
!
      USE TPRGlobalSD
      USE IntrType

      INTEGER(sik) :: i

      i = 0
      nstep = i


END SUBROUTINE trace_finalize
