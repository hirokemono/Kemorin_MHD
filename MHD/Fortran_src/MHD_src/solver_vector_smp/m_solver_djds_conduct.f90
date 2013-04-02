!
!      module m_solver_djds_conduct
!
!      Written by H. Matsui
!      Modified by H. Matsui on Apr., 2008
!
      module m_solver_djds_conduct
!
      use m_precision
!
      implicit none
!
!
!C-- MATRIX arrays
!
      integer (kind=kint) :: itotal_cd_l
      integer (kind=kint) :: itotal_cd_u
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
      integer(kind=kint),  allocatable, target :: STACKmcG(:)
      integer(kind=kint),  allocatable, target :: STACKmc(:)
      integer(kind=kint),  allocatable, target :: COLORon(:)
      integer(kind=kint),  allocatable, target :: PEon(:)
!
      integer(kind = kint), allocatable, target :: NOD_EXPORT_NEW_cd(:)
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
       subroutine allocate_4_RCM_cd
!
       use m_geometry_parameter
!
       allocate (OLDtoNEW(numnod) )
       allocate (NEWtoOLD(numnod) )

       OLDtoNEW= 0
       NEWtoOLD= 0
!
       end subroutine allocate_4_RCM_cd
!
! ------------------------------------------
!
       subroutine allocate_number_4_djds_cd
!
       allocate (IVECT(0:NHYP))
       allocate (NLmaxHYP(NHYP))
       allocate (NUmaxHYP(NHYP))
!
       IVECT   = 0
       NLmaxHYP= 0
       NUmaxHYP= 0
!
       end subroutine allocate_number_4_djds_cd
!
! ------------------------------------------
!
       subroutine allocate_lists_4_DJDS_cd
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
       end subroutine allocate_lists_4_DJDS_cd
!
! ------------------------------------------
!
       subroutine allocate_address_4_DJDS_cd
!
       allocate (itemDJDS_L(itotal_cd_l))
       allocate (itemDJDS_U(itotal_cd_u))
!
       itemDJDS_L = 0
       itemDJDS_U = 0
!
       end subroutine allocate_address_4_DJDS_cd
!
! ------------------------------------------
!
       subroutine allocate_new_comm_table_cd
!
       use m_nod_comm_table
!
       allocate  ( NOD_EXPORT_NEW_cd(ntot_export) )
       NOD_EXPORT_NEW_cd = 0
!
       end subroutine allocate_new_comm_table_cd
!
! ------------------------------------------
!
       subroutine deallocate_4_djds_table_cd
!
       deallocate (NEWtoOLD_DJDS_L)
!
       end subroutine deallocate_4_djds_table_cd
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
     &        ' OLDtoNEW_DJDS_U, LtoU 4 conductor'
      do i = 1, numnod
      write(50+my_rank,'(10i8)') i, NEWtoOLD(i),                       &
     &            OLDtoNEW_DJDS_L(i), OLDtoNEW_DJDS_U(i), LtoU(i)
      end do
!
      write(50+my_rank,*) 'indexDJDS_L 4 conductor'
       write(50+my_rank,'(10i8)') indexDJDS_L
      write(50+my_rank,*) 'itemDJDS_l 4 conductor'
      write(50+my_rank,'(10i8)') itemDJDS_l
      write(50+my_rank,*) 'indexDJDS_U 4 conductor'
       write(50+my_rank,'(10i8)') indexDJDS_U
      write(50+my_rank,*) 'itemDJDS_u 4 conductor'
       write(50+my_rank,'(10i8)') itemDJDS_u
!
!
       end subroutine check_DJDS_ordering_info
!
!
! ------------------------------------------
!
      end module m_solver_djds_conduct
