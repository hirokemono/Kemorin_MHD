!>@file   m_solver_djds.f90
!!@brief  module m_solver_djds
!!
!!@author K. Nakajima and H. Matsui
!!@date Programmed by Kengo Nakajima in 2001
!!@n      Modified by Hiroaki Matsui in May, 2007
!!@n      Modified by Hiroaki Matsui in Apr., 2008
!
!>      DJDS ordering table
!!
!!@verbatim
!!       subroutine allocate_4_RCM(numnod)
!!       subroutine allocate_number_4_djds
!!       subroutine allocate_lists_4_DJDS(np_smp, numnod)
!!       subroutine allocate_address_4_DJDS
!!       subroutine allocate_new_comm_table(ntot_export)
!!
!!       subroutine deallocate_4_djds_table
!!       subroutine deallocate_4_RCM
!!       subroutine deallocate_number_4_djds
!!       subroutine deallocate_lists_4_DJDS
!!       subroutine deallocate_address_4_DJDS
!!       subroutine deallocate_new_comm_table
!!
!!       subroutine check_DJDS_ordering_info(my_rank, numnod)
!!@endverbatim
!
      module m_solver_djds
!
      use m_precision
!
      implicit none
!
!
!C-- Ordering arrays
!
      integer (kind=kint) :: itotal_l
      integer (kind=kint) :: itotal_u
!
      integer(kind=kint) :: NHYP
      integer(kind=kint), allocatable, target :: IVECT(:)
!
      integer(kind=kint), allocatable, target :: OLDtoNEW(:)
      integer(kind=kint), allocatable, target :: NEWtoOLD(:)
!
      integer(kind=kint), allocatable, target :: NEWtoOLD_DJDS_L(:)
      integer(kind=kint), allocatable, target :: NEWtoOLD_DJDS_U(:)
      integer(kind=kint), allocatable, target :: OLDtoNEW_DJDS_L(:)
      integer(kind=kint), allocatable, target :: OLDtoNEW_DJDS_U(:)
      integer(kind=kint), allocatable, target :: LtoU(:)
!
      integer(kind=kint), allocatable, target :: indexDJDS_L(:)
      integer(kind=kint), allocatable, target :: indexDJDS_U(:)
      integer(kind=kint), allocatable, target ::  itemDJDS_L(:)
      integer(kind=kint), allocatable, target ::  itemDJDS_U(:)
      integer(kind=kint), allocatable, target :: NLmaxHYP(:)
      integer(kind=kint), allocatable, target :: NUmaxHYP(:)
!
      integer(kind=kint), allocatable, target :: STACKmcG(:)
      integer(kind=kint), allocatable, target :: STACKmc(:)
      integer(kind=kint), allocatable, target :: COLORon(:)
      integer(kind=kint), allocatable, target :: PEon(:)
!
      integer(kind = kint), allocatable, target :: NOD_EXPORT_NEW(:)
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
       subroutine allocate_4_RCM(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate (OLDtoNEW(numnod) )
       allocate (NEWtoOLD(numnod) )

       OLDtoNEW= 0
       NEWtoOLD= 0
!
       end subroutine allocate_4_RCM
!
! ------------------------------------------
!
       subroutine allocate_number_4_djds
!
       allocate (IVECT(0:NHYP))
       allocate (NLmaxHYP(NHYP))
       allocate (NUmaxHYP(NHYP))
!
       IVECT   = 0
       NLmaxHYP= 0
       NUmaxHYP= 0
!
       end subroutine allocate_number_4_djds
!
! ------------------------------------------
!
       subroutine allocate_lists_4_DJDS(np_smp, numnod)
!
       integer(kind = kint), intent(in) :: np_smp, numnod
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
       end subroutine allocate_lists_4_DJDS
!
! ------------------------------------------
!
       subroutine allocate_address_4_DJDS
!
       allocate (itemDJDS_L(itotal_l))
       allocate (itemDJDS_U(itotal_u))
!
       itemDJDS_L = 0
       itemDJDS_U = 0
!
       end subroutine allocate_address_4_DJDS
!
! ------------------------------------------
!
       subroutine allocate_new_comm_table(ntot_export)
!
       integer(kind = kint), intent(in) :: ntot_export
!
       allocate  ( NOD_EXPORT_NEW(ntot_export) )
       NOD_EXPORT_NEW = 0
!
       end subroutine allocate_new_comm_table
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine deallocate_4_djds_table
!
       deallocate (NEWtoOLD_DJDS_L)
!
       end subroutine deallocate_4_djds_table
!
! ------------------------------------------
!
       subroutine deallocate_4_RCM
!
       deallocate (OLDtoNEW )
       deallocate (NEWtoOLD )
!
       end subroutine deallocate_4_RCM
!
! ------------------------------------------
!
       subroutine deallocate_number_4_djds
!
       deallocate (IVECT)
       deallocate (NLmaxHYP)
       deallocate (NUmaxHYP)
!
       end subroutine deallocate_number_4_djds
!
! ------------------------------------------
!
       subroutine deallocate_lists_4_DJDS
!
       deallocate (OLDtoNEW_DJDS_L)
       deallocate (NEWtoOLD_DJDS_U, OLDtoNEW_DJDS_U)
       deallocate (LtoU)

       deallocate (indexDJDS_L)
       deallocate (indexDJDS_U)

       deallocate (STACKmc, STACKmcG)
!
       deallocate (PEon)
       deallocate (COLORon)
!
       end subroutine deallocate_lists_4_DJDS
!
! ------------------------------------------
!
       subroutine deallocate_address_4_DJDS
!
       deallocate (itemDJDS_L)
       deallocate (itemDJDS_U)
!
       end subroutine deallocate_address_4_DJDS
!
! ------------------------------------------
!
       subroutine deallocate_new_comm_table
!
       deallocate  ( NOD_EXPORT_NEW )
!
       end subroutine deallocate_new_comm_table
!
! ------------------------------------------
! ------------------------------------------
!
       subroutine check_DJDS_ordering_info(my_rank, numnod)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       integer(kind = kint) :: i
!
      write(50+my_rank,*) 'inod, NEWtoOLD, OLDtoNEW_DJDS_L, ',          &
     &        ' OLDtoNEW_DJDS_U, LtoU'
      do i = 1, numnod
      write(50+my_rank,'(10i16)') i, NEWtoOLD(i),                       &
     &            OLDtoNEW_DJDS_L(i), OLDtoNEW_DJDS_U(i), LtoU(i)
      end do
!
      write(50+my_rank,*) 'indexDJDS_L'
       write(50+my_rank,'(10i16)') indexDJDS_L
      write(50+my_rank,*) 'itemDJDS_l'
      write(50+my_rank,'(10i16)') itemDJDS_l
      write(50+my_rank,*) 'indexDJDS_U'
       write(50+my_rank,'(10i16)') indexDJDS_U
      write(50+my_rank,*) 'itemDJDS_u'
       write(50+my_rank,'(10i16)') itemDJDS_u
!
!
       end subroutine check_DJDS_ordering_info
!
! ------------------------------------------
!
      end module m_solver_djds
