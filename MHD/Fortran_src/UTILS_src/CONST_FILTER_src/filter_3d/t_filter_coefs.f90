!
!      module t_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine alloc_each_filter_coef(numnod, numele, fil_coef)
!!      subroutine alloc_each_ele_filter_coef(numele, fil_coef)
!!      subroutine dealloc_each_filter_coef(fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!
!!      subroutine alloc_correct_filter_flag(node, fil_flag)
!!        type(node_data), intent(in) :: node
!!      subroutine alloc_filter_num_sort(inter_nod, fil_flag)
!!      subroutine dealloc_correct_filter_flag(fil_flag)
!!      subroutine dealloc_filter_num_sort(fil_flag)
!!      subroutine reset_failed_filter_flag(fil_flag)
!!        type(filter_area_flag), intent(inout) :: fil_flag
!
      module t_filter_coefs
!
      use m_precision
      use t_filter_coefficients
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
        integer(kind = kint), allocatable :: nnod_near_nod_w(:)
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
!
      type filter_area_flag
        integer(kind = kint) :: nnod
        integer(kind = kint), allocatable :: iflag_make_filter(:)
        integer(kind = kint) :: inter_nod
        integer(kind = kint), allocatable :: i_exp_level(:)
        integer(kind = kint), allocatable :: itbl_near_nod(:)
        integer(kind = kint) :: num_failed
      end type filter_area_flag
!
!
      type const_filter_coefs
        integer(kind = kint) :: nmax_nod_near_all_w
        integer(kind = kint) :: nmin_nod_near_all_w
!
        type(filter_coefficients_type) :: fil_sorted
!
        type(each_filter_coef) :: fil_coef
        type(each_filter_coef) :: tmp_coef
!
        type(filter_area_flag) :: whole_area
        type(filter_area_flag) :: fluid_area
      end type const_filter_coefs
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_each_filter_coef(numnod, fil_coef)
!
      integer(kind = kint), intent(in) :: numnod
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      fil_coef%nnod = numnod
      allocate(fil_coef%inod_4_1nod_w(fil_coef%nnod))
      allocate(fil_coef%idist_from_1nod(fil_coef%nnod))
      allocate(fil_coef%iweight_for_1nod(fil_coef%nnod))
      allocate(fil_coef%nnod_near_nod_w(fil_coef%nnod))
!
      allocate(fil_coef%weight_1nod(fil_coef%nnod))
      allocate(fil_coef%filter_1nod(fil_coef%nnod))
!
      if(fil_coef%nnod .gt. 0) then
!$omp parallel workshare
        fil_coef%inod_4_1nod_w =   0
        fil_coef%nnod_near_nod_w =    0
        fil_coef%idist_from_1nod =   -1
        fil_coef%iweight_for_1nod =  -1
        fil_coef%weight_1nod =  0.0d0
        fil_coef%filter_1nod =  0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_each_filter_coef
!
! ----------------------------------------------------------------------
!
      subroutine alloc_each_ele_filter_coef(numele, fil_coef)
!
      integer(kind = kint), intent(in) :: numele
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      fil_coef%nele = numele
      allocate(fil_coef%iele_4_1nod_w(fil_coef%nele))
!
      if(fil_coef%nele .gt. 0) fil_coef%iele_4_1nod_w = 0
!
      end subroutine alloc_each_ele_filter_coef
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_each_filter_coef(fil_coef)
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      deallocate(fil_coef%filter_1nod, fil_coef%weight_1nod)
      deallocate(fil_coef%inod_4_1nod_w)
      deallocate(fil_coef%idist_from_1nod, fil_coef%iweight_for_1nod)
      deallocate(fil_coef%nnod_near_nod_w)
!
      end subroutine dealloc_each_filter_coef
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_each_ele_filter_coef(fil_coef)
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      deallocate(fil_coef%iele_4_1nod_w)
!
      end subroutine dealloc_each_ele_filter_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_correct_filter_flag(node, fil_flag)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(filter_area_flag), intent(inout) :: fil_flag
!
!
      fil_flag%nnod = node%numnod
      allocate(fil_flag%iflag_make_filter(fil_flag%nnod))
!
      if(fil_flag%nnod .gt. 0) then
!$omp parallel workshare
        fil_flag%iflag_make_filter = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_correct_filter_flag
!
! -----------------------------------------------------------------------
!
      subroutine alloc_filter_num_sort(inter_nod, fil_flag)
!
      integer(kind = kint), intent(in) :: inter_nod
      type(filter_area_flag), intent(inout) :: fil_flag
!
      fil_flag%inter_nod = inter_nod
      allocate( fil_flag%i_exp_level(fil_flag%inter_nod) )
      allocate( fil_flag%itbl_near_nod(fil_flag%inter_nod) )
!
      if(fil_flag%inter_nod .gt. 0) then
!$omp parallel workshare
        fil_flag%i_exp_level = 0
        fil_flag%itbl_near_nod = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_filter_num_sort
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_correct_filter_flag(fil_flag)
!
      type(filter_area_flag), intent(inout) :: fil_flag
!
!
      deallocate(fil_flag%iflag_make_filter)
!
      end subroutine dealloc_correct_filter_flag
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_filter_num_sort(fil_flag)
!
      type(filter_area_flag), intent(inout) :: fil_flag
!
!
      deallocate(fil_flag%i_exp_level)
      deallocate(fil_flag%itbl_near_nod)
!
      end subroutine dealloc_filter_num_sort
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine reset_failed_filter_flag(fil_flag)
!
      type(filter_area_flag), intent(inout) :: fil_flag
!
      fil_flag%num_failed = 0
!
      end subroutine reset_failed_filter_flag
!
! -----------------------------------------------------------------------
!
      end module t_filter_coefs
