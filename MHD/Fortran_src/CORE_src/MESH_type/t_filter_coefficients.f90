!>@file   t_filter_coefficients.f90
!!@brief  module t_filter_coefficients
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!
!>@brief Structure for filtering informations
!!
!!@verbatim
!!      subroutine alloc_num_filtering_comb(np_smp, filters)
!!      subroutine alloc_inod_filter_comb(filters)
!!      subroutine alloc_3d_filter_comb(filters)
!!      subroutine alloc_3d_filter_func(filters)
!!
!!      subroutine alloc_stack_vec_filter(np_smp, filters)
!!      subroutine alloc_istart_vec_filter(filters)
!!
!!      subroutine dealloc_num_filtering_comb(filters)
!!      subroutine dealloc_inod_filter_weights(filters)
!!      subroutine dealloc_3d_filter_func(filters)
!!      subroutine dealloc_3d_filter_weight(filters)
!!
!!      subroutine dealloc_stack_vec_filter(filters)
!!      subroutine dealloc_istart_vec_filter(filters)
!!
!!      subroutine copy_3d_filter_stacks(org_filter, new_filter)
!!      subroutine copy_3d_filter_weights(org_filter, new_filter)
!!      subroutine copy_3d_filter_weight_func(org_filter, new_filter)
!!        type(filter_coefficients_type), intent(inout) :: org_filter
!!        type(filter_coefficients_type), intent(inout) :: new_filter
!!
!!      subroutine check_num_near_all_f(id_rank)
!!      subroutine check_near_nod_all_filter(id_rank)
!!      subroutine check_filter_functions(id_rank, id_base)
!!        type(filter_coefficients_type), intent(in) :: filters
!!@endverbatim
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
        character(len=kchara), allocatable :: group_name(:)
        integer(kind = kint), allocatable :: num_node(:)
        integer(kind = kint), allocatable :: istack_node(:)
!
        integer(kind = kint) :: ntot_nod
        integer(kind = kint), allocatable :: inod_filter(:)
        integer(kind = kint), allocatable :: nnod_near(:)
        integer(kind = kint), allocatable :: istack_near_nod(:)
!
        integer(kind = kint) :: ntot_near_nod
        integer(kind = kint), allocatable :: inod_near(:)
        real(kind = kreal), allocatable :: func(:)
        real(kind = kreal), allocatable :: weight(:)
!
!
        integer(kind = kint), allocatable :: min_nsum(:)
        integer(kind = kint), allocatable :: max_nsum(:)
        integer(kind = kint), allocatable :: istack_nsum(:)
!
        integer(kind = kint) :: ntot_nsum
        integer(kind = kint), allocatable :: ist_nsum(:)
        integer(kind = kint), allocatable :: ied_nsum(:)
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
      subroutine dealloc_3d_filter_func(filters)
!
      type(filter_coefficients_type), intent(inout) :: filters
!
!
      deallocate(filters%func)
!
      end subroutine dealloc_3d_filter_func
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
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_stacks(org_filter, new_filter)
!
      type(filter_coefficients_type), intent(inout) :: org_filter
      type(filter_coefficients_type), intent(inout) :: new_filter
!
!
      new_filter%ngrp_node = org_filter%ngrp_node
      call alloc_num_filtering_comb(ione, new_filter)
!
      new_filter%group_name(1:new_filter%ngrp_node)                     &
     &         = org_filter%group_name(1:new_filter%ngrp_node)
      new_filter%num_node(1:new_filter%ngrp_node)                       &
     &         = org_filter%num_node(1:new_filter%ngrp_node)
      new_filter%istack_node(0:new_filter%ngrp_node)                    &
     &         = org_filter%istack_node(0:new_filter%ngrp_node)
!
      new_filter%ntot_nod = org_filter%ntot_nod
      call alloc_inod_filter_comb(new_filter)
!
      new_filter%inod_filter(1:new_filter%ntot_nod)                     &
     &      = org_filter%inod_filter(1:new_filter%ntot_nod)
      new_filter%nnod_near(1:new_filter%ntot_nod)                       &
     &      = org_filter%nnod_near(1:new_filter%ntot_nod)
      new_filter%istack_near_nod(0:new_filter%ntot_nod)                 &
     &      = org_filter%istack_near_nod(0:new_filter%ntot_nod)
      new_filter%ntot_near_nod = org_filter%ntot_near_nod
!
      call dealloc_inod_filter_weights(org_filter)
      call dealloc_num_filtering_comb(org_filter)
!
      end subroutine copy_3d_filter_stacks
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weights(org_filter, new_filter)
!
      type(filter_coefficients_type), intent(inout) :: org_filter
      type(filter_coefficients_type), intent(inout) :: new_filter
!
!
      new_filter%ntot_near_nod = org_filter%ntot_near_nod
      call alloc_3d_filter_comb(new_filter)
!
!$omp parallel workshare
      new_filter%inod_near(1:new_filter%ntot_near_nod)                  &
     &      = org_filter%inod_near(1:new_filter%ntot_near_nod)
      new_filter%weight(1:new_filter%ntot_near_nod)                     &
     &      = org_filter%weight(1:new_filter%ntot_near_nod)
!$omp end parallel workshare
!
      call dealloc_3d_filter_weight(org_filter)
!
      end subroutine copy_3d_filter_weights
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weight_func(org_filter, new_filter)
!
      type(filter_coefficients_type), intent(inout) :: org_filter
      type(filter_coefficients_type), intent(inout) :: new_filter
!
!
      call copy_3d_filter_weights(org_filter, new_filter)
!
      call alloc_3d_filter_func(new_filter)
!
      new_filter%func(1:org_filter%ntot_near_nod)                       &
     &      = org_filter%func(1:org_filter%ntot_near_nod)
!
      call dealloc_3d_filter_func(org_filter)
!
      end subroutine copy_3d_filter_weight_func
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_num_near_all_f(id_rank, filters)
!
      integer, intent(in) :: id_rank
      type(filter_coefficients_type), intent(in) :: filters
!
      integer(kind = kint) :: inum
!
      write(50+id_rank,*) 'near node ID for filter and entire'
      do inum = 1, filters%ntot_nod
        write(50+id_rank,*) inum, filters%nnod_near(inum)
      end do
!
      end subroutine check_num_near_all_f
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_all_filter(id_rank, filters)
!
      integer, intent(in) :: id_rank
      type(filter_coefficients_type), intent(in) :: filters
!
      integer(kind = kint) :: inum, ist, ied
!
      do inum = 1, filters%ntot_nod
        ist = filters%istack_near_nod(inum-1) + 1
        ied = filters%istack_near_nod(inum)
        write(50+id_rank,*) 'near node ID filters%inod_near',           &
     &     inum, filters%inod_filter(inum), ist, ied,                   &
     &     filters%nnod_near(inum)
        write(50+id_rank,'(8i16)') filters%inod_near(ist:ied)
      end do
!
      end subroutine check_near_nod_all_filter
!
! -----------------------------------------------------------------------
!
      subroutine check_filter_functions(id_rank, id_base, filters)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: id_base
      type(filter_coefficients_type), intent(in) :: filters
!
      integer(kind = kint) :: inum, ist, ied, i
!
      do inum = 1, filters%ntot_nod
        ist = filters%istack_near_nod(inum-1)
        ied = filters%istack_near_nod(inum)
        write(id_base+id_rank,*) 'filter',                              &
     &     inum, filters%inod_filter(inum), filters%nnod_near(inum)
        do i = 1, (ied-ist)
          write(id_base+id_rank,*) i, filters%inod_near(ist+i),         &
     &         filters%func(ist+i), filters%weight(ist+i)
        end do
      end do
!
      end subroutine check_filter_functions
!
! ----------------------------------------------------------------------
!
      end module t_filter_coefficients
