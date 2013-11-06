!
!     module m_solver_djds_fluid
!
!      Written by Hiroaki Matsui on Nov., 2003
!
      module m_solver_djds_fluid
!
      use m_precision
!
      implicit none
!
!
!C-- MATRIX arrays
!
      integer (kind=kint) :: itotal_fl_l
      integer (kind=kint) :: itotal_fl_u
! 
      integer(kind=kint) :: NHYP
      integer(kind=kint), target, allocatable :: IVECT(:)
!
      integer(kind=kint), target, allocatable :: OLDtoNEW(:)
      integer(kind=kint), target, allocatable :: NEWtoOLD(:)
!
      integer(kind=kint), target, allocatable:: NEWtoOLD_DJDS_L(:)
      integer(kind=kint), target, allocatable:: NEWtoOLD_DJDS_U(:)
      integer(kind=kint), target, allocatable:: OLDtoNEW_DJDS_L(:)
      integer(kind=kint), target, allocatable:: OLDtoNEW_DJDS_U(:)
      integer(kind=kint), target, allocatable:: LtoU(:)
!
      integer(kind=kint), target, allocatable :: indexDJDS_L(:)
      integer(kind=kint), target, allocatable :: indexDJDS_U(:)
      integer(kind=kint), target, allocatable ::  itemDJDS_L(:)
      integer(kind=kint), target, allocatable ::  itemDJDS_U(:)
      integer(kind=kint), target, allocatable :: NLmaxHYP(:)
      integer(kind=kint), target, allocatable :: NUmaxHYP(:)
!
      integer(kind=kint), target,  allocatable :: STACKmcG(:)
      integer(kind=kint), target,  allocatable :: STACKmc(:)
      integer(kind=kint), target,  allocatable :: COLORon(:)
      integer(kind=kint), target,  allocatable :: PEon(:)
!
      integer(kind = kint), target, allocatable                         &
     &              :: NOD_EXPORT_NEW_fl(:)
!
!  ---  constants
!
      integer(kind=kint) :: NLmax, NUmax
      integer(kind=kint) :: npLX1, npUX1
!
! ------------------------------------------
!
      contains
!
! ------------------------------------------
!
       subroutine allocate_4_RCM_fl
!
       use m_geometry_parameter
!
       allocate (OLDtoNEW(numnod) )
       allocate (NEWtoOLD(numnod) )

       OLDtoNEW= 0
       NEWtoOLD= 0
!
       end subroutine allocate_4_RCM_fl
!
! ------------------------------------------
!
       subroutine allocate_number_4_djds_fl
!
       allocate (IVECT(0:NHYP))
       allocate (NLmaxHYP(NHYP))
       allocate (NUmaxHYP(NHYP))
!
       IVECT   = 0
       NLmaxHYP= 0
       NUmaxHYP= 0
!
       end subroutine allocate_number_4_djds_fl
!
! ------------------------------------------
!
       subroutine allocate_lists_4_DJDS_fl
!
       use m_geometry_parameter
       use m_machine_parameter
!
       allocate (NEWtoOLD_DJDS_L(numnod), OLDtoNEW_DJDS_L(numnod))
       allocate (NEWtoOLD_DJDS_U(numnod), OLDtoNEW_DJDS_U(numnod))
       allocate (LtoU(numnod))

       allocate (indexDJDS_L(0:np_smp*NLmax*NHYP))
       allocate (indexDJDS_U(0:np_smp*NUmax*NHYP))

       allocate (STACKmcG (0:np_smp) )
       allocate (STACKmc (0:np_smp*NHYP) )
!
       allocate (PEon(numnod))
       allocate (COLORon(numnod))
!
       indexDJDS_L= 0
       indexDJDS_U= 0
!
       NEWtoOLD_DJDS_L= 0     
       NEWtoOLD_DJDS_U= 0     
       OLDtoNEW_DJDS_L= 0     
       OLDtoNEW_DJDS_U= 0     
!
!
       end subroutine allocate_lists_4_DJDS_fl
!
! ------------------------------------------
!
       subroutine allocate_address_4_DJDS_fl
!
       allocate (itemDJDS_L(itotal_fl_l))
       allocate (itemDJDS_U(itotal_fl_u))
!
       itemDJDS_L = 0
       itemDJDS_U = 0
!
       end subroutine allocate_address_4_DJDS_fl
!
! ------------------------------------------
!
      end module m_solver_djds_fluid
