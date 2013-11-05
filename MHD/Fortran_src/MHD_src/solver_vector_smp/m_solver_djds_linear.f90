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
! ------------------------------------------
!
       subroutine allocate_new_comm_table_l
!
       use m_nod_comm_table
!
       allocate  ( NOD_EXPORT_NEW1(ntot_export) )
       NOD_EXPORT_NEW1 = 0
!
       end subroutine allocate_new_comm_table_l
!
! ------------------------------------------
!
       subroutine deallocate_4_djds_table_l
!
       deallocate (NEWtoOLD_DJDS1_L)
!
       end subroutine deallocate_4_djds_table_l
!
! ------------------------------------------
!
       subroutine set_djds_4_linear
!
       use m_geometry_parameter
       use m_solver_djds_MHD
!
!
       itotal1_u = DJDS_entire%itotal_u
       itotal1_l = DJDS_entire%itotal_l
!
       call allocate_4_RCM_l
!
       OLDtoNEW1= DJDS_entire%OLDtoNEW
       NEWtoOLD1= DJDS_entire%NEWtoOLD
!
       NHYP1 = DJDS_entire%NHYP
!
       call allocate_number_4_djds_l
!
       IVECT1   = DJDS_entire%IVECT
       NLmaxHYP1= DJDS_entire%NLmaxHYP
       NUmaxHYP1= DJDS_entire%NUmaxHYP
!
       NLmax1 = DJDS_entire%NLmax
       NUmax1 = DJDS_entire%NUmax
       npLX1_1 = DJDS_entire%npLX1
       npUX1_1 = DJDS_entire%npUX1
!
       call allocate_lists_4_DJDS_l
!
       indexDJDS1_L= DJDS_entire%indexDJDS_L
       indexDJDS1_U= DJDS_entire%indexDJDS_U
!
       NEWtoOLD_DJDS1_L= DJDS_entire%NEWtoOLD_DJDS_L
       NEWtoOLD_DJDS1_U= DJDS_entire%NEWtoOLD_DJDS_U
       OLDtoNEW_DJDS1_L= DJDS_entire%OLDtoNEW_DJDS_L
       OLDtoNEW_DJDS1_U= DJDS_entire%OLDtoNEW_DJDS_U
       LtoU1 = DJDS_entire%LtoU
!
       STACKmcG1 = DJDS_entire%STACKmcG
       STACKmc1 =  DJDS_entire%STACKmc
!
       PEon1 = DJDS_entire%PEon
       COLORon1 = DJDS_entire%COLORon
!
       call allocate_address_4_DJDS_l
!
       itemDJDS1_L = DJDS_entire%itemDJDS_L
       itemDJDS1_U = DJDS_entire%itemDJDS_U
!
       call allocate_new_comm_table_l
       NOD_EXPORT_NEW1 = DJDS_entire%NOD_EXPORT_NEW
!
       end subroutine set_djds_4_linear
!
! ------------------------------------------
!
       subroutine check_DJDS_ordering_info_l(my_rank)
!
       use m_geometry_parameter
!
       integer (kind = kint) :: my_rank
       integer(kind = kint) :: i
!
      write(50+my_rank,*) 'inod, NEWtoOLD1, OLDtoNEW_DJDS _L, ',       &
     &        ' OLDtoNEW_DJDS1_U, LtoU1'
      do i = 1, numnod
      write(50+my_rank,'(10i8)') i, NEWtoOLD1(i),                      &
     &            OLDtoNEW_DJDS1_L(i), OLDtoNEW_DJDS1_U(i), LtoU1(i)
      end do
!
      write(50+my_rank,*) 'indexDJDS1_L'
       write(50+my_rank,'(10i8)') indexDJDS1_L
      write(50+my_rank,*) 'itemDJDS1_l'
      write(50+my_rank,'(10i8)') itemDJDS1_l
      write(50+my_rank,*) 'indexDJDS1_U'
       write(50+my_rank,'(10i8)') indexDJDS1_U
      write(50+my_rank,*) 'itemDJDS1_u'
       write(50+my_rank,'(10i8)') itemDJDS1_u
!
!
       end subroutine check_DJDS_ordering_info_l
!
!
! ------------------------------------------
!
      end module m_solver_djds_linear
