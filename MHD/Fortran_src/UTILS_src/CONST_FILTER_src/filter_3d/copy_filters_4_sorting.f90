!copy_filters_4_sorting.f90
!      module copy_filters_4_sorting
!
!
!     Written by H. Matsui on May., 2008
!
!!      subroutine s_copy_filters_4_sorting                             &
!!     &         (whole_fil_sort, fluid_fil_sort,                       &
!!     &          whole_fil_sort2, fluid_fil_sort2)
!!        type(filter_func_4_sorting), intent(in) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
!!      subroutine reorder_filter_new_domain(fils_sort)
!!        type(filters_4_sorting), intent(inout) :: fils_sort
!
      module copy_filters_4_sorting
!
      use m_precision
!
      use m_nod_filter_comm_table
      use t_filter_func_4_sorting
!
      implicit none
!
      private :: order_filter_num_2_new_domain
      private :: order_filter_item_2_new_domain
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_filters_4_sorting                               &
     &         (whole_fil_sort, fluid_fil_sort,                         &
     &          whole_fil_sort2, fluid_fil_sort2)
!
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort2
!
!
      call dup_filter_func_4_sort(whole_fil_sort, whole_fil_sort2)
      call dup_filter_func_4_sort(fluid_fil_sort, fluid_fil_sort2)
!
      end subroutine s_copy_filters_4_sorting
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reorder_filter_new_domain(fils_sort)
!
      type(filters_4_sorting), intent(inout) :: fils_sort
!
!
      call alloc_filter_num_4_sort                                      &
     &   (fils_sort%intnod_w_fliter2, fils_sort%whole_fil_sort)
      call order_filter_num_2_new_domain                                &
     &   (fils_sort%intnod_w_fliter2, fils_sort%inod_filter_new_2,      &
     &    fils_sort%whole_fil_sort2, fils_sort%whole_fil_sort)
!
      call count_whole_fil_4_new_domain(fils_sort%intnod_w_fliter2,     &
     &    fils_sort%whole_fil_sort2, fils_sort%whole_fil_sort)
      call alloc_filter_func_4_sort(fils_sort%whole_fil_sort)
!
      call order_filter_item_2_new_domain                               &
     &   (fils_sort%intnod_w_fliter2, fils_sort%inod_filter_new_2,      &
     &    fils_sort%whole_fil_sort2, fils_sort%whole_fil_sort)
!
!
      call alloc_filter_num_4_sort                                      &
     &   (fils_sort%intnod_w_fliter2, fils_sort%fluid_fil_sort)
      call order_filter_num_2_new_domain                                &
     &   (fils_sort%intnod_w_fliter2, fils_sort%inod_filter_new_2,      &
     &    fils_sort%fluid_fil_sort2, fils_sort%fluid_fil_sort)
!
      call count_fluid_fil_4_new_domain(fils_sort%intnod_w_fliter2,     &
     &    fils_sort%fluid_fil_sort2, fils_sort%fluid_fil_sort)
      call alloc_filter_func_4_sort(fils_sort%fluid_fil_sort)
!
      call order_filter_item_2_new_domain                               &
     &   (fils_sort%intnod_w_fliter2, fils_sort%inod_filter_new_2,      &
     &    fils_sort%fluid_fil_sort2, fils_sort%fluid_fil_sort)
!
      end subroutine reorder_filter_new_domain
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_whole_fil_4_new_domain                           &
     &         (inter_nod, whole_fil_sort2, whole_fil_sort)
!
      integer(kind = kint), intent(in) :: inter_nod
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod
        whole_fil_sort%istack_near_nod_filter(inod)                     &
     &        = whole_fil_sort%istack_near_nod_filter(inod-1)           &
     &         + whole_fil_sort%nnod_near_nod_filter(inod)
      end do
      whole_fil_sort%ntot_nod_near_filter                               &
     &       = whole_fil_sort2%ntot_nod_near_filter
!
      end subroutine count_whole_fil_4_new_domain
!
! ----------------------------------------------------------------------
!
      subroutine count_fluid_fil_4_new_domain                           &
     &         (inter_nod, fluid_fil_sort2, fluid_fil_sort)
!
      integer(kind = kint), intent(in) :: inter_nod
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort2
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod
        if( fluid_fil_sort%nnod_near_nod_filter(inod) .le. 0) then
          fluid_fil_sort%istack_near_nod_filter(inod)                   &
     &        = fluid_fil_sort%istack_near_nod_filter(inod-1)
        else
          fluid_fil_sort%istack_near_nod_filter(inod)                   &
     &        = fluid_fil_sort%istack_near_nod_filter(inod-1)           &
     &         + fluid_fil_sort%nnod_near_nod_filter(inod)
        end if
      end do
      fluid_fil_sort%ntot_nod_near_filter                               &
     &       = fluid_fil_sort2%ntot_nod_near_filter
!
      end subroutine count_fluid_fil_4_new_domain
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine order_filter_num_2_new_domain                          &
     &         (inter_nod, inod_filter_new_2, org_fsort, new_fsort)
!
      integer(kind = kint), intent(in) :: inter_nod
      integer(kind = kint), intent(in) :: inod_filter_new_2(inter_nod)
      type(filter_func_4_sorting), intent(in) :: org_fsort
!
      type(filter_func_4_sorting), intent(inout) :: new_fsort
!
      integer(kind = kint) :: i, inod
!
!
      new_fsort%istack_near_nod_filter(0)                               &
     &       = org_fsort%istack_near_nod_filter(0)
      do i = 1, inter_nod
        inod = inod_filter_new_2(i)
        new_fsort%i_exp_level_filter(inod)                              &
     &       = org_fsort%i_exp_level_filter(i)
        new_fsort%nnod_near_nod_filter(inod)                            &
     &       =   org_fsort%nnod_near_nod_filter(i)
      end do
!
      end subroutine order_filter_num_2_new_domain
!
!   --------------------------------------------------------------------
!
      subroutine order_filter_item_2_new_domain                         &
     &         (inter_nod, inod_filter_new_2, org_fsort, new_fsort)
!
      integer(kind = kint), intent(in) :: inter_nod
      integer(kind = kint), intent(in) :: inod_filter_new_2(inter_nod)
      type(filter_func_4_sorting), intent(in) :: org_fsort
!
      type(filter_func_4_sorting), intent(inout) :: new_fsort
!
      integer(kind = kint) :: i, inod, inum
      integer(kind = kint) :: jnum_org, jnum_new
      integer(kind = kint) :: ist_org, ist_new
!
      do i = 1, inter_nod
        inod = inod_filter_new_2(i)
        ist_org = org_fsort%istack_near_nod_filter(i-1)
        ist_new = new_fsort%istack_near_nod_filter(inod-1)
        do inum = 1, new_fsort%nnod_near_nod_filter(inod)
          jnum_org = inum + ist_org
          jnum_new = inum + ist_new
          new_fsort%inod_near_nod_filter(jnum_new)                      &
     &             = org_fsort%inod_near_nod_filter(jnum_org)
          new_fsort%filter_func(jnum_new)                               &
     &             = org_fsort%filter_func(jnum_org)
          new_fsort%filter_weight(jnum_new)                             &
     &             = org_fsort%filter_weight(jnum_org)
        end do
      end do
!
      end subroutine order_filter_item_2_new_domain
!
!   --------------------------------------------------------------------
!
      end module copy_filters_4_sorting
