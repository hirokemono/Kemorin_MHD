!t_filter_func_4_sorting.f90
!      module t_filter_func_4_sorting
!
!     Written by H. Matsui on Apr., 2008
!
!!      subroutine alloc_filter_num_4_sort(intnod, f_sorting)
!!      subroutine alloc_filter_func_4_sort(f_sorting)
!!      subroutine dealloc_filter_func_4_sort(f_sorting)
!!      subroutine dealloc_filter_num_4_sort(f_sorting)
!!        type(filter_func_4_sorting), intent(inout) :: f_sorting
!!
!!      subroutine copy_filter_num_4_sort                               &
!!     &         (ist, ied, org_fsort, new_fsort)
!!      subroutine copy_filter_func_4_sort                              &
!!     &         (ist, ied, org_fsort, new_fsort)
!!        type(filter_func_4_sorting), intent(in) :: org_fsort
!!        type(filter_func_4_sorting), intent(inout) :: new_fsort
!!      subroutine dup_filter_func_4_sort(org_fsort, new_fsort)
!!        type(filter_func_4_sorting), intent(inout) :: org_fsort
!!        type(filter_func_4_sorting), intent(inout) :: new_fsort
!
      module t_filter_func_4_sorting
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type filter_func_4_sorting
        real(kind = kreal), allocatable :: filter_func(:)
        real(kind = kreal), allocatable :: filter_weight(:)
!
        integer(kind = kint) :: inter_nod
        integer(kind = kint) :: ntot_nod_near_filter
        integer(kind = kint), allocatable :: nnod_near_nod_filter(:)
        integer(kind = kint), allocatable :: istack_near_nod_filter(:)
        integer(kind = kint), allocatable :: inod_near_nod_filter(:)
!
        integer(kind = kint), allocatable :: i_exp_level_filter(:)
      end type filter_func_4_sorting
!
      type filters_4_sorting
        type(filter_func_4_sorting) :: whole_fil_sort
        type(filter_func_4_sorting) :: fluid_fil_sort
!
        type(filter_func_4_sorting) :: whole_fil_sort2
        type(filter_func_4_sorting) :: fluid_fil_sort2
!
        integer(kind = kint) :: intnod_w_fliter2
        integer(kind = kint), allocatable :: inod_filter_new_2(:)
      end type filters_4_sorting
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_whole_filter_stack2(inter_nod, fils_sort)
!
      integer(kind = kint), intent(in) :: inter_nod
      type(filters_4_sorting), intent(inout) :: fils_sort
!
!
      call alloc_filter_num_4_sort(inter_nod, fils_sort%whole_fil_sort2)
      call alloc_filter_num_4_sort(inter_nod, fils_sort%fluid_fil_sort2)
!
      fils_sort%whole_fil_sort2%ntot_nod_near_filter = 0
      fils_sort%fluid_fil_sort2%ntot_nod_near_filter = 0
      call alloc_filter_func_4_sort(fils_sort%whole_fil_sort2)
      call alloc_filter_func_4_sort(fils_sort%fluid_fil_sort2)
!
      fils_sort%intnod_w_fliter2 = inter_nod
      allocate(fils_sort%inod_filter_new_2(fils_sort%intnod_w_fliter2))
      if(inter_nod .gt. 0) fils_sort%inod_filter_new_2 = 0
!
      end subroutine alloc_whole_filter_stack2
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_whole_filter_stack2(fils_sort)
!
      type(filters_4_sorting), intent(inout) :: fils_sort
!
!
      deallocate(fils_sort%inod_filter_new_2)
!
      call dealloc_filter_func_4_sort(fils_sort%whole_fil_sort2)
      call dealloc_filter_num_4_sort(fils_sort%whole_fil_sort2)
      call dealloc_filter_func_4_sort(fils_sort%fluid_fil_sort2)
      call dealloc_filter_num_4_sort(fils_sort%fluid_fil_sort2)
!
      end subroutine dealloc_whole_filter_stack2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_filter_num_4_sort(intnod, f_sorting)
!
      integer(kind = kint), intent(in) :: intnod
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
!
      f_sorting%inter_nod = intnod
      allocate( f_sorting%nnod_near_nod_filter(intnod) )
      allocate( f_sorting%istack_near_nod_filter(0:intnod) )
      allocate( f_sorting%i_exp_level_filter(intnod) )
!
      if(f_sorting%inter_nod .gt. 0) then
        f_sorting%nnod_near_nod_filter =   0
        f_sorting%i_exp_level_filter =     0
      end if
      f_sorting%istack_near_nod_filter = 0
!
      end subroutine alloc_filter_num_4_sort
!
! ----------------------------------------------------------------------
!
      subroutine alloc_filter_func_4_sort(f_sorting)
!
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
      integer(kind = kint) :: num
!
      num = f_sorting%ntot_nod_near_filter
      allocate( f_sorting%inod_near_nod_filter(num) )
      allocate( f_sorting%filter_func(num) )
      allocate( f_sorting%filter_weight(num) )
!
      if(f_sorting%ntot_nod_near_filter .gt. 0) then
        f_sorting%inod_near_nod_filter = 0
        f_sorting%filter_func =  0.0d0
        f_sorting%filter_weight = 0.0d0
      end if
!
      end subroutine alloc_filter_func_4_sort
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_filter_func_4_sort(f_sorting)
!
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
!
      deallocate( f_sorting%nnod_near_nod_filter )
      deallocate( f_sorting%istack_near_nod_filter )
      deallocate( f_sorting%i_exp_level_filter )
!
      end subroutine dealloc_filter_func_4_sort
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_filter_num_4_sort(f_sorting)
!
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
      deallocate( f_sorting%inod_near_nod_filter )
      deallocate( f_sorting%filter_func )
      deallocate( f_sorting%filter_weight )
!
      end subroutine dealloc_filter_num_4_sort
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_filter_num_4_sort                                 &
     &         (ist, ied, org_fsort, new_fsort)
!
      integer(kind = kint), intent(in) :: ist, ied
      type(filter_func_4_sorting), intent(in) :: org_fsort
      type(filter_func_4_sorting), intent(inout) :: new_fsort
!
!
!$omp parallel workshare
      new_fsort%i_exp_level_filter(ist:ied)                             &
     &     = org_fsort%i_exp_level_filter(ist:ied)
      new_fsort%nnod_near_nod_filter(ist:ied)                           &
     &     = org_fsort%nnod_near_nod_filter(ist:ied)
      new_fsort%istack_near_nod_filter(ist:ied)                         &
     &     = org_fsort%istack_near_nod_filter(ist:ied)
!$omp end parallel workshare
!
      end subroutine copy_filter_num_4_sort
!
! ----------------------------------------------------------------------
!
      subroutine copy_filter_func_4_sort                                &
     &         (ist, ied, org_fsort, new_fsort)
!
      integer(kind = kint), intent(in) :: ist, ied
      type(filter_func_4_sorting), intent(in) :: org_fsort
      type(filter_func_4_sorting), intent(inout) :: new_fsort
!
!
!$omp parallel workshare
      new_fsort%inod_near_nod_filter(ist:ied)                           &
     &     = org_fsort%inod_near_nod_filter(ist:ied)
      new_fsort%filter_func(ist:ied)                                    &
     &     = org_fsort%filter_func(ist:ied)
      new_fsort%filter_weight(ist:ied)                                  &
     &     = org_fsort%filter_weight(ist:ied)
!$omp end parallel workshare
!
      end subroutine copy_filter_func_4_sort
!
! ----------------------------------------------------------------------
!
      subroutine dup_filter_func_4_sort(org_fsort, new_fsort)
!
      type(filter_func_4_sorting), intent(inout) :: org_fsort
      type(filter_func_4_sorting), intent(inout) :: new_fsort
!
!
      call alloc_filter_num_4_sort(org_fsort%inter_nod, new_fsort)
!
      new_fsort%istack_near_nod_filter(0)                               &
     &     = org_fsort%istack_near_nod_filter(0)
      call copy_filter_num_4_sort                                       &
     &   (ione, new_fsort%inter_nod, org_fsort, new_fsort)
!
      new_fsort%ntot_nod_near_filter                                    &
     &     = org_fsort%ntot_nod_near_filter
      call alloc_filter_func_4_sort(new_fsort)
      call copy_filter_func_4_sort                                      &
     &   (ione, new_fsort%ntot_nod_near_filter,                         &
     &    org_fsort, new_fsort)
!
      call dealloc_filter_func_4_sort(org_fsort)
!
      end subroutine dup_filter_func_4_sort
!
! ----------------------------------------------------------------------
!
      end module t_filter_func_4_sorting
