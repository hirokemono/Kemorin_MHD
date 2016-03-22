!
!      module t_filter_coefficients
!
!     Written by H. Matsui on Nov., 2006
!
!
!      subroutine alloc_num_filtering_comb(np_smp, filters)
!      subroutine alloc_inod_filter_comb(filters)
!      subroutine alloc_3d_filter_comb(filters)
!      subroutine alloc_3d_filter_func(filters)
!
!      subroutine alloc_stack_vec_filter(np_smp, filters)
!      subroutine alloc_istart_vec_filter(filters)
!
!      subroutine dealloc_num_filtering_comb(filters)
!      subroutine dealloc_inod_filter_weights(filters)
!      subroutine dealloc_3d_filter_function(filters)
!      subroutine dealloc_3d_filter_weight(filters)
!
!      subroutine dealloc_stack_vec_filter(filters)
!      subroutine dealloc_istart_vec_filter(filters)
!
      module t_filter_coefficients
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type filter_coefficients_type
        integer(kind = kint) :: ngrp_node
        character(len=kchara), pointer :: group_name(:)
        integer(kind = kint), pointer :: num_node(:)
        integer(kind = kint), pointer :: istack_node(:)
!
        integer(kind = kint) :: ntot_nod
        integer(kind = kint), pointer :: inod_filter(:)
        integer(kind = kint), pointer :: nnod_near(:)
        integer(kind = kint), pointer :: istack_near_nod(:)
!
        integer(kind = kint) :: ntot_near_nod
        integer(kind = kint), pointer :: inod_near(:)
        real(kind = kreal), pointer :: func(:)
        real(kind = kreal), pointer :: weight(:)
!
!
        integer(kind = kint), pointer :: min_nsum(:)
        integer(kind = kint), pointer :: max_nsum(:)
        integer(kind = kint), pointer :: istack_nsum(:)
!
        integer(kind = kint) :: ntot_nsum
        integer(kind = kint), pointer :: ist_nsum(:)
        integer(kind = kint), pointer :: ied_nsum(:)
      end type filter_coefficients_type
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_filtering_comb(np_smp, filters)
!
      integer(kind = kint), intent(in) :: np_smp
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%group_name(filters%ngrp_node))
      allocate(filters%num_node(filters%ngrp_node*np_smp))
      allocate(filters%istack_node(0:filters%ngrp_node*np_smp))
!
      filters%num_node = 0
      filters%istack_node = -1
!
      end subroutine alloc_num_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_filter_comb(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%inod_filter(filters%ntot_nod))
      allocate(filters%nnod_near(filters%ntot_nod))
      allocate(filters%istack_near_nod(0:filters%ntot_nod))
!
      filters%inod_filter = 0
      filters%nnod_near = 0
      filters%istack_near_nod = 0
!
      end subroutine alloc_inod_filter_comb
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_3d_filter_comb(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%inod_near(filters%ntot_near_nod))
      allocate(filters%weight(filters%ntot_near_nod))
!
      filters%inod_near = 0
      filters%weight = 0.0d0
!
      end subroutine alloc_3d_filter_comb
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_3d_filter_func(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%func(filters%ntot_near_nod))
      filters%func =   0.0d0
!
      end subroutine alloc_3d_filter_func
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_stack_vec_filter(np_smp, filters)
!
      integer(kind = kint), intent(in) :: np_smp
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%min_nsum(filters%ngrp_node*np_smp))
      allocate(filters%max_nsum(filters%ngrp_node*np_smp))
      allocate(filters%istack_nsum(0:filters%ngrp_node*np_smp))
!
      filters%min_nsum = 0
      filters%max_nsum = 0
      filters%istack_nsum = -1
!
      end subroutine alloc_stack_vec_filter
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istart_vec_filter(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      allocate(filters%ist_nsum(filters%ntot_nsum))
      allocate(filters%ied_nsum(filters%ntot_nsum))
      filters%ist_nsum = 0
      filters%ied_nsum = 0
!
      end subroutine alloc_istart_vec_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_filtering_comb(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%group_name)
      deallocate(filters%num_node, filters%istack_node)
!
      end subroutine dealloc_num_filtering_comb
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_filter_weights(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%inod_filter)
      deallocate(filters%nnod_near, filters%istack_near_nod)
!
      end subroutine dealloc_inod_filter_weights
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_filter_function(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%func)
!
      end subroutine dealloc_3d_filter_function
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_filter_weight(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%inod_near, filters%weight)
!
      end subroutine dealloc_3d_filter_weight
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_stack_vec_filter(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%min_nsum, filters%max_nsum)
      deallocate(filters%istack_nsum)
!
      end subroutine dealloc_stack_vec_filter
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istart_vec_filter(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%ist_nsum, filters%ied_nsum)
!
      end subroutine dealloc_istart_vec_filter
!
!  ---------------------------------------------------------------------
!
      end module t_filter_coefficients
