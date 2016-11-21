!> @file  t_work_DJDS_ordering.f90
!!      module t_work_DJDS_ordering
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2006
!! @n    Modified in Nov., 2016
!
!> @brief Work area for construct DJDS ordering
!!
!!@verbatim
!!       subroutine  alloc_IVECT_rcm(NP, WK_DJDS)
!!       subroutine  alloc_iW_ordering(NP, WK_DJDS)
!!       subroutine  alloc_work_4_rcm(NP, NHYP, WK_DJDS)
!!       subroutine  alloc_work_num_4_djds(NP, WK_DJDS)
!!       subroutine  alloc_work_4_djds(NP, NHYP, npLX1, npUX1, WK_DJDS)
!!       subroutine  alloc_conn_work_4_djds(N, NLmax, NUmax, WK_DJDS)
!!
!!       subroutine reset_4_new_rcm(WK_DJDS)
!!
!!       subroutine dealloc_IVECT_rcm(WK_DJDS)
!!       subroutine dealloc_iW_ordering(WK_DJDS)
!!       subroutine dealloc_work_4_RCM(WK_DJDS)
!!       subroutine dealloc_work_4_djds(WK_DJDS)
!!       subroutine dealloc_4_IVECmc(WK_DJDS)
!!
!!       subroutine check_istack_and_items_mc(my_rank, N, WK_DJDS)
!!@endverbatim
!
      module t_work_DJDS_ordering
!
      use m_precision
!
      implicit none
!
      type work_DJDS_ordering
!C-- GENERAL SCALARs
        integer(kind=kint) :: NCOLORtot
!
!C-- MATRIX arrays
        integer(kind=kint), allocatable :: INLmc(:)
        integer(kind=kint), allocatable :: INUmc(:)
        integer(kind=kint), allocatable :: IALmc(:,:)
        integer(kind=kint), allocatable :: IAUmc(:,:)

        integer(kind=kint), allocatable :: IWKX(:,:)

        integer(kind=kint), allocatable :: IW(:)

        integer(kind=kint), allocatable :: OLDtoNEWmc(:)
        integer(kind=kint), allocatable :: NEWtoOLDmc(:)

        integer(kind=kint), allocatable :: ICHK(:)
        integer(kind=kint), allocatable :: IVECmc(:), IVnew(:)
!
        integer(kind=kint), allocatable :: inumDJDS_L(:)
        integer(kind=kint), allocatable :: inumDJDS_U(:)
!
        integer(kind=kint), allocatable :: IVECT_rcm(:)
      end type work_DJDS_ordering
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  alloc_IVECT_rcm(NP, WK_DJDS)
!
      integer (kind = kint), intent(in) :: NP
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
      allocate(WK_DJDS%IVECT_rcm(0:NP) )
      WK_DJDS%IVECT_rcm(0:NP) = 0
!
      end subroutine  alloc_IVECT_rcm
!
!-----------------------------------------------------------------------
!
      subroutine  alloc_iW_ordering(NP, WK_DJDS)
!
      integer (kind = kint), intent(in) :: NP
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
      allocate(WK_DJDS%IW(NP) )
      if(NP .gt. 0) WK_DJDS%IW(1:NP) = 0
!
      end subroutine  alloc_iW_ordering
!
!-----------------------------------------------------------------------
!
      subroutine  alloc_work_4_rcm(NP, NHYP, WK_DJDS)
!
      integer (kind = kint), intent(in)  :: NP, NHYP
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
      allocate(WK_DJDS%ICHK(NHYP))
      allocate(WK_DJDS%OLDtoNEWmc(NP))
      allocate(WK_DJDS%NEWtoOLDmc(NP))
      allocate(WK_DJDS%IVECmc(0:WK_DJDS%NCOLORtot))
      allocate(WK_DJDS%IVnew(NP))

      if(NHYP .gt. 0) WK_DJDS%ICHK(1:NHYP) = 0
      if(NP .gt. 0) then
        WK_DJDS%IVnew(1:NP) = 0
        WK_DJDS%NEWtoOLDmc(1:NP) = 0
        WK_DJDS%OLDtoNEWmc(1:NP) = 0
      end if
      WK_DJDS%IVECmc(0:WK_DJDS%NCOLORtot) = 0

       end subroutine  alloc_work_4_rcm
!
!-----------------------------------------------------------------------
!
      subroutine  alloc_work_num_4_djds(NP, WK_DJDS)
!
      integer (kind = kint), intent(in)  :: NP
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
      allocate (WK_DJDS%INLmc(NP))
      allocate (WK_DJDS%INUmc(NP))
!
       if(NP .gt. 0) then
         WK_DJDS%INLmc(1:NP) = 0
         WK_DJDS%INUmc(1:NP) = 0
       end if
!
      end subroutine  alloc_work_num_4_djds
!
!-----------------------------------------------------------------------
!
      subroutine  alloc_work_4_djds(NP, NHYP, npLX1, npUX1, WK_DJDS)
!
      integer (kind = kint), intent(in)  :: NP
      integer (kind=kint), intent(in) :: NHYP, npLX1, npUX1
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
!
       allocate(WK_DJDS%IWKX(0:NP,3))
       allocate(WK_DJDS%IW(0:NP))
!
       allocate(WK_DJDS%inumDJDS_L(0:npLX1*NHYP))
       allocate(WK_DJDS%inumDJDS_U(0:npUX1*NHYP))
!
       WK_DJDS%IWKX(0:NP,1:3) = 0
       WK_DJDS%IW(0:NP) =   0
!
       WK_DJDS%inumDJDS_L(0:npLX1*NHYP) = 0
       WK_DJDS%inumDJDS_U(0:npUX1*NHYP) = 0
!
       end subroutine  alloc_work_4_djds
!
!-----------------------------------------------------------------------
!
       subroutine alloc_conn_work_4_djds(N, NLmax, NUmax, WK_DJDS)
!
       integer (kind = kint), intent(in) :: N
       integer (kind = kint), intent(in) :: NLmax, NUmax
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
       allocate(WK_DJDS%IALmc(N,NLmax) )
       allocate(WK_DJDS%IAUmc(N,NUmax) )
!
       if(N .gt. 0) then
         if(NLmax .gt. 0) WK_DJDS%IALmc = 0
         if(NUmax .gt. 0) WK_DJDS%IAUmc = 0
       end if
!
      end subroutine alloc_conn_work_4_djds
!
!-----------------------------------------------------------------------
!
      subroutine reset_4_new_rcm(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
       deallocate(WK_DJDS%OLDtoNEWmc, WK_DJDS%NEWtoOLDmc)
       deallocate(WK_DJDS%ICHK)
       deallocate(WK_DJDS%IVECmc, WK_DJDS%IVnew)
!
       end subroutine reset_4_new_rcm
!
!-----------------------------------------------------------------------
!
      subroutine  dealloc_IVECT_rcm(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
      deallocate(WK_DJDS%IVECT_rcm)
!
      end subroutine  dealloc_IVECT_rcm
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iW_ordering(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
       deallocate(WK_DJDS%IW)
!
      end subroutine dealloc_iW_ordering
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_work_4_RCM(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
       deallocate (WK_DJDS%ICHK, WK_DJDS%IVnew)
!
      end subroutine dealloc_work_4_RCM
!
!-----------------------------------------------------------------------
!
       subroutine dealloc_work_4_djds(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
!
       deallocate(WK_DJDS%OLDtoNEWmc, WK_DJDS%NEWtoOLDmc)
       deallocate(WK_DJDS%INLmc, WK_DJDS%INUmc)
       deallocate(WK_DJDS%IALmc, WK_DJDS%IAUmc)
       deallocate(WK_DJDS%IWKX)
       deallocate(WK_DJDS%inumDJDS_L, WK_DJDS%inumDJDS_U)
!
       end subroutine dealloc_work_4_djds
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_4_IVECmc(WK_DJDS)
!
      type(work_DJDS_ordering), intent(inout) ::WK_DJDS
!
       deallocate (WK_DJDS%IVECmc)
!
      end subroutine dealloc_4_IVECmc
!
!-----------------------------------------------------------------------
!
      subroutine check_istack_and_items_mc(my_rank, N, WK_DJDS)
!
      integer (kind = kint), intent(in) :: my_rank
      integer (kind = kint), intent(in) :: N
      type(work_DJDS_ordering), intent(in) ::WK_DJDS
!
       integer (kind = kint) :: i, j
!
        do i = 1, N
          write(50+my_rank,*) 'IALmc', i, WK_DJDS%INLmc(i)
          write(50+my_rank,'(10i16)')                                   &
     &                      (WK_DJDS%IALmc(i,j),j=1,WK_DJDS%INLmc(i))
        end do
        do i = 1, N
          write(50+my_rank,*) 'IAUmc', i, WK_DJDS%INUmc(i)
          write(50+my_rank,'(10i16)')                                   &
     &                      (WK_DJDS%IAUmc(i,j),j=1,WK_DJDS%INUmc(i))
        end do
!
       end subroutine check_istack_and_items_mc
!
!-----------------------------------------------------------------------
!
      end module t_work_DJDS_ordering
