MODULE IntrType
!
!     (c) 2002 The Pennsylvania University Applied Research Laboratory
!     This Software was developed for Purdue Subcontract Agreement 640-01812-2
!     under NRC Prime Contract no. NRC-04-97-046 (Task #2)
!
      IMPLICIT NONE
!
!       These same definitions are repeated in all FUNCTION
!       declarations, for the NagWare F90 compiler
!
      INTEGER, PARAMETER :: sdk = SELECTED_REAL_KIND(13, 307)
      INTEGER, PARAMETER :: sik = KIND(10000000)
!
      TYPE RealPtrT
         REAL(sdk), POINTER :: val => NULL()
      END TYPE RealPtrT
!
      TYPE IntPtrT
         INTEGER(sik), POINTER :: val => NULL()
      END TYPE IntPtrT
!
      TYPE hsDatT
         INTEGER(sik) :: nHSS
         INTEGER(sik), POINTER, DIMENSION(:) :: idxHS => NULL()
         INTEGER(sik), POINTER, DIMENSION(:) :: celHS => NULL()
         INTEGER(sik), POINTER, DIMENSION(:) :: node => NULL()
         REAL(sdk) :: totASHS
         REAL(sdk), POINTER, DIMENSION(:) :: aSHS => NULL()
      END TYPE hsDatT
!
!     Geometry data structure for 2nd order FE
      TYPE feGeomT
         REAL(sdk), DIMENSION(3, 3) :: shapeF = 0.0_sdk  ! Shape function matrix for 2nd order FE element.
         REAL(sdk), DIMENSION(3, 3) :: dShapeF = 0.0_sdk ! Derivative of shape function matrix for 2nd order
                                                         ! FE element.
         REAL(sdk), DIMENSION(3) :: volE = 0.0_sdk       ! Volume per unit length of left boundary, middle,
                                                         ! and right boundary of 2nd order finite element.
         REAL(sdk), DIMENSION(3) :: areaE  = 0.0_sdk     ! Radial surface area per unit lenth at first
                                                         ! Gauss-Qaudrature point (i.e., eta = -SQRT(3/5)),
                                                         ! at second Gauss-Qaudrature point (i.e, eta = 0),
                                                         ! and at third Gauss-Qaudrature point (i.e., eta = SQRT(3/5)),
                                                         ! where eta = 0 at mid-point of finite element
                                                         ! eta = -1 at left boundary, and eta = 1 at right boundary.
         REAL(sdk), DIMENSION(3) :: areaZ  = 0.0_sdk     ! Axail heat transfer area for each of the three segments of
                                                         ! the finite element.
      END TYPE feGeomT
!
END MODULE IntrType
