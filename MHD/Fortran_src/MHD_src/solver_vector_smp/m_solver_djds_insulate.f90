!
!      module m_solver_djds_insulate
!
!      Written by Hiroaki Matsui on ???, 2001
!      Modified by H. Matsui on Apr., 2008
!
      module m_solver_djds_insulate
!
      use m_precision
!
      implicit none
!
!
!C-- MATRIX arrays
!
      integer (kind=kint) :: itotal_ins_l
      integer (kind=kint) :: itotal_ins_u
!
      integer(kind=kint) :: NHYP
      integer(kind=kint), target, allocatable :: IVECT(:)
!
      integer(kind=kint), target,  allocatable :: OLDtoNEW(:)
      integer(kind=kint), target,  allocatable :: NEWtoOLD(:)
!
      integer(kind=kint), target, allocatable :: NEWtoOLD_DJDS_L(:)
      integer(kind=kint), target, allocatable :: NEWtoOLD_DJDS_U(:)
      integer(kind=kint), target, allocatable :: OLDtoNEW_DJDS_L(:)
      integer(kind=kint), target, allocatable :: OLDtoNEW_DJDS_U(:)
      integer(kind=kint), target, allocatable :: LtoU(:)
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
     &              :: NOD_EXPORT_NEW_ins(:)
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
       subroutine allocate_4_RCM_ins
!
       use m_geometry_parameter
!
       allocate (OLDtoNEW(numnod) )
       allocate (NEWtoOLD(numnod) )

       OLDtoNEW= 0
       NEWtoOLD= 0
!
       end subroutine allocate_4_RCM_ins
!
! ------------------------------------------
!
       subroutine allocate_number_4_djds_ins
!
       allocate (IVECT(0:NHYP))
       allocate (NLmaxHYP(NHYP))
       allocate (NUmaxHYP(NHYP))
!
       IVECT   = 0
       NLmaxHYP= 0
       NUmaxHYP= 0
!
       end subroutine allocate_number_4_djds_ins
!
! ------------------------------------------
!
       subroutine allocate_lists_4_DJDS_ins
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
       end subroutine allocate_lists_4_DJDS_ins
!
! ------------------------------------------
!
       subroutine allocate_address_4_DJDS_ins
!
       allocate (itemDJDS_L(itotal_ins_l))
       allocate (itemDJDS_U(itotal_ins_u))
!
       itemDJDS_L = 0
       itemDJDS_U = 0
!
       end subroutine allocate_address_4_DJDS_ins
!
! ------------------------------------------
!
       subroutine allocate_new_comm_table_ins
!
       use m_nod_comm_table
!
       allocate  ( NOD_EXPORT_NEW_ins(ntot_export) )
       NOD_EXPORT_NEW_ins = 0
!
       end subroutine allocate_new_comm_table_ins
!
! ------------------------------------------
!
       subroutine deallocate_4_djds_table_ins
!
       deallocate (NEWtoOLD_DJDS_L)
!
       end subroutine deallocate_4_djds_table_ins
!
! ------------------------------------------
!
       subroutine check_DJDS_ordering_info(my_rank)
!
       use m_geometry_parameter
!
       integer (kind = kint) :: my_rank
       integer(kind = kint) :: i
!
      write(50+my_rank,*) 'inod, NEWtoOLD, OLDtoNEW_DJDS_L, ',         &
     &        ' OLDtoNEW_DJDS_U, LtoU 4 insulator'
      do i = 1, numnod
      write(50+my_rank,'(10i8)') i, NEWtoOLD(i),                       &
     &            OLDtoNEW_DJDS_L(i), OLDtoNEW_DJDS_U(i), LtoU(i)
      end do
!
      write(50+my_rank,*) 'indexDJDS_L 4 insulator'
       write(50+my_rank,'(10i8)') indexDJDS_L
      write(50+my_rank,*) 'itemDJDS_l 4 insulator'
      write(50+my_rank,'(10i8)') itemDJDS_l
      write(50+my_rank,*) 'indexDJDS_U 4 insulator'
       write(50+my_rank,'(10i8)') indexDJDS_U
      write(50+my_rank,*) 'itemDJDS_u 4 insulator'
       write(50+my_rank,'(10i8)') itemDJDS_u
!
!
       end subroutine check_DJDS_ordering_info
!
!
! ------------------------------------------
!
      end module m_solver_djds_insulate
