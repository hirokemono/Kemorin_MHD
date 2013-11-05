!
!      module m_solver_djds_linear
!
!      Written by Hiroaki Matsui on Nov., 2003
!      Modified by H. Matsui on Apr., 2008
!
      module m_solver_djds_linear
!
      use m_precision
!
      implicit none
!
!
!C-- MATRIX arrays
!
      integer (kind=kint) :: itotal1_l
      integer (kind=kint) :: itotal1_u
! 
      integer(kind=kint) :: NHYP1
      integer(kind=kint), target, allocatable :: IVECT1(:)
!
      integer(kind=kint), target, allocatable :: OLDtoNEW1(:)
      integer(kind=kint), target, allocatable :: NEWtoOLD1(:)
!
      integer(kind=kint), target, allocatable :: NEWtoOLD_DJDS1_L(:)
      integer(kind=kint), target, allocatable :: NEWtoOLD_DJDS1_U(:)
      integer(kind=kint), target, allocatable :: OLDtoNEW_DJDS1_L(:)
      integer(kind=kint), target, allocatable :: OLDtoNEW_DJDS1_U(:)
      integer(kind=kint), target, allocatable :: LtoU1(:)
!
      integer(kind=kint), target, allocatable :: indexDJDS1_L(:)
      integer(kind=kint), target, allocatable :: indexDJDS1_U(:)
      integer(kind=kint), target, allocatable ::  itemDJDS1_L(:)
      integer(kind=kint), target, allocatable ::  itemDJDS1_U(:)
      integer(kind=kint), target, allocatable :: NLmaxHYP1(:)
      integer(kind=kint), target, allocatable :: NUmaxHYP1(:)
!
      integer(kind=kint), target,  allocatable :: STACKmcG1(:)
      integer(kind=kint), target,  allocatable :: STACKmc1(:)
      integer(kind=kint), target,  allocatable :: COLORon1(:)
      integer(kind=kint), target,  allocatable :: PEon1(:)
!
      integer(kind=kint), target, allocatable :: NOD_EXPORT_NEW1(:)
!
!  ---  constants
!
      integer(kind=kint) :: NLmax1, NUmax1
      integer(kind=kint) :: npLX1_1, npUX1_1
!
! ------------------------------------------
!
      contains
!
       subroutine allocate_4_RCM_l
!
       use m_geometry_parameter
!
       allocate (OLDtoNEW1(numnod) )
       allocate (NEWtoOLD1(numnod) )

       OLDtoNEW1= 0
       NEWtoOLD1= 0
!
       end subroutine allocate_4_RCM_l
!
! ------------------------------------------
!
       subroutine allocate_number_4_djds_l
!
       allocate (IVECT1(0:NHYP1))
       allocate (NLmaxHYP1(NHYP1))
       allocate (NUmaxHYP1(NHYP1))
!
       IVECT1   = 0
       NLmaxHYP1= 0
       NUmaxHYP1= 0
!
       end subroutine allocate_number_4_djds_l
!
! ------------------------------------------
!
       subroutine allocate_lists_4_DJDS_l
!
       use m_geometry_parameter
       use m_machine_parameter
!
       allocate (NEWtoOLD_DJDS1_L(numnod), OLDtoNEW_DJDS1_L(numnod))
       allocate (NEWtoOLD_DJDS1_U(numnod), OLDtoNEW_DJDS1_U(numnod))
       allocate (LtoU1(numnod))

       allocate (indexDJDS1_L(0:np_smp*NLmax1*NHYP1))
       allocate (indexDJDS1_U(0:np_smp*NUmax1*NHYP1))

       allocate (STACKmcG1 (0:np_smp) )
       allocate (STACKmc1 (0:np_smp*NHYP1) )
!
       allocate (PEon1(numnod))
       allocate (COLORon1(numnod))
!
       indexDJDS1_L= 0
       indexDJDS1_U= 0
!
       NEWtoOLD_DJDS1_L= 0
       NEWtoOLD_DJDS1_U= 0
       OLDtoNEW_DJDS1_L= 0
       OLDtoNEW_DJDS1_U= 0
       LtoU1 = 0
!
       STACKmcG1 = 0
       STACKmc1 = 0
!
       PEon1 = 0
       COLORon1 = 0

       end subroutine allocate_lists_4_DJDS_l
!
! ------------------------------------------
!
       subroutine allocate_address_4_DJDS_l
!
       allocate (itemDJDS1_L(itotal1_l))
       allocate (itemDJDS1_U(itotal1_u))
!
       itemDJDS1_L = 0
       itemDJDS1_U = 0
!
       end subroutine allocate_address_4_DJDS_l
!
!
      end module m_solver_djds_linear
