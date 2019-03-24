!
!      module t_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine alloc_each_filter_coef(numnod, numele, fil_coef)
!!      subroutine dealloc_each_filter_coef(fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!
      module t_filter_coefs
!
      use m_precision
!
      implicit none
!
!
      type each_filter_coef
        integer(kind = kint) :: ilevel_exp_1nod_w
!
        integer(kind = kint) :: nnod
        integer(kind = kint) :: nnod_4_1nod_w
        integer(kind = kint) :: nnod_4_1nod_f
        integer(kind = kint), allocatable :: inod_4_1nod_w(:)
        integer(kind = kint), allocatable :: idist_from_1nod(:)
        integer(kind = kint), allocatable :: iweight_for_1nod(:)
!
        integer(kind = kint) :: nele
        integer(kind = kint) :: nele_4_1nod_w
        integer(kind = kint) :: nele_4_1nod_f
        integer(kind = kint), allocatable :: iele_4_1nod_w(:)
!
        real(kind = kreal), allocatable :: weight_1nod(:)
        real(kind = kreal), allocatable :: filter_1nod(:)
      end type each_filter_coef
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_each_filter_coef(numnod, numele, fil_coef)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      fil_coef%nnod = numnod
      allocate(fil_coef%inod_4_1nod_w(fil_coef%nnod))
      allocate(fil_coef%idist_from_1nod(fil_coef%nnod))
      allocate(fil_coef%iweight_for_1nod(fil_coef%nnod))
!
      allocate(fil_coef%weight_1nod(fil_coef%nnod))
      allocate(fil_coef%filter_1nod(fil_coef%nnod))
!
      fil_coef%nele = numele
      allocate(fil_coef%iele_4_1nod_w(fil_coef%nele))
!
      if(fil_coef%nnod .gt. 0) then
!$omp parallel workshare
        fil_coef%inod_4_1nod_w =   0
        fil_coef%idist_from_1nod =   -1
        fil_coef%iweight_for_1nod =  -1
        fil_coef%weight_1nod =  0.0d0
        fil_coef%filter_1nod =  0.0d0
!$omp end parallel workshare
      end if
!
      if(fil_coef%nele .gt. 0) fil_coef%iele_4_1nod_w = 0
!
      end subroutine alloc_each_filter_coef
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_each_filter_coef(fil_coef)
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      deallocate(fil_coef%filter_1nod, fil_coef%weight_1nod)
      deallocate(fil_coef%iele_4_1nod_w)
      deallocate(fil_coef%inod_4_1nod_w)
      deallocate(fil_coef%idist_from_1nod, fil_coef%iweight_for_1nod)
!
      end subroutine dealloc_each_filter_coef
!
! ----------------------------------------------------------------------
!
      end module t_filter_coefs
