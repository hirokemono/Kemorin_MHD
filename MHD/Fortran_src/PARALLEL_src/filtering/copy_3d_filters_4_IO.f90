!
!      module copy_3d_filters_4_IO
!
!     Written by H. Matsui on Nov., 2008
!
!!      subroutine copy_3d_filter_stacks_from_IO(filter)
!!      subroutine copy_3d_filter_weights_from_IO(filter)
!!
!!      subroutine copy_3d_filter_stacks_to_IO(filter)
!!      subroutine copy_3d_filter_weights_to_IO(filter)
!!        type(filter_coefficients_type), intent(inout) :: filter
!
      module copy_3d_filters_4_IO
!
      use m_precision
!
      use t_filter_coefficients
      use m_combained_filter_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_stacks_from_IO(filter)
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      filter%ngrp_node = ngrp_nod_filter_IO
      call alloc_num_filtering_comb(ione, filter)
!
      filter%group_name(1:filter%ngrp_node)                             &
     &         = grp_name_filter_IO(1:filter%ngrp_node)
      filter%num_node(1:filter%ngrp_node)                               &
     &         = num_nod_filter_IO(1:filter%ngrp_node)
      filter%istack_node(0:filter%ngrp_node)                            &
     &         = istack_nod_filter_IO(0:filter%ngrp_node)
!
      filter%ntot_nod = ntot_nod_filter_IO
      call alloc_inod_filter_comb(filter)
!
      filter%inod_filter(1:filter%ntot_nod)                             &
     &      = inod_filter_IO(1:filter%ntot_nod)
      filter%nnod_near(1:filter%ntot_nod)                               &
     &      = num_near_nod_filter_IO(1:filter%ntot_nod)
      filter%istack_near_nod(0:filter%ntot_nod)                         &
     &      = istack_near_nod_filter_IO(0:filter%ntot_nod)
      filter%ntot_near_nod = ntot_near_nod_filter_IO
!
      call deallocate_inod_filter_comb_IO
      call deallocate_num_filtering_IO
!
      end subroutine copy_3d_filter_stacks_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weights_from_IO(filter)
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      filter%ntot_near_nod = ntot_near_nod_filter_IO
      call alloc_3d_filter_comb(filter)
!
      filter%inod_near(1:filter%ntot_near_nod)                          &
     &      = inod_near_nod_IO(1:filter%ntot_near_nod)
      filter%weight(1:filter%ntot_near_nod)                             &
     &      = filter_weight_IO(1:filter%ntot_near_nod)
!
      call deallocate_3d_filter_data_IO
!
      end subroutine copy_3d_filter_weights_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_stacks_to_IO(filter)
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      ngrp_nod_filter_IO = filter%ngrp_node
      call allocate_num_filtering_IO
!
      grp_name_filter_IO(1:filter%ngrp_node)                            &
     &         = filter%group_name(1:filter%ngrp_node)
      num_nod_filter_IO(1:filter%ngrp_node)                             &
     &         = filter%num_node(1:filter%ngrp_node)
      istack_nod_filter_IO(0:filter%ngrp_node)                          &
     &         = filter%istack_node(0:filter%ngrp_node)
!
      ntot_nod_filter_IO = filter%ntot_nod
      call allocate_inod_filter_comb_IO
!
      inod_filter_IO(1:filter%ntot_nod)                                 &
     &      = filter%inod_filter(1:filter%ntot_nod)
      num_near_nod_filter_IO(1:filter%ntot_nod)                         &
     &      = filter%nnod_near(1:filter%ntot_nod)
      istack_near_nod_filter_IO(0:filter%ntot_nod)                      &
     &      = filter%istack_near_nod(0:filter%ntot_nod)
      ntot_near_nod_filter_IO = filter%ntot_near_nod
!
      call dealloc_inod_filter_weights(filter)
      call dealloc_num_filtering_comb(filter)
!
      end subroutine copy_3d_filter_stacks_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weights_to_IO(filter)
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      ntot_near_nod_filter_IO = filter%ntot_near_nod
      call allocate_3d_filter_data_IO
!
      inod_near_nod_IO(1:filter%ntot_near_nod)                          &
     &      = filter%inod_near(1:filter%ntot_near_nod)
      filter_weight_IO(1:filter%ntot_near_nod)                          &
     &      = filter%weight(1:filter%ntot_near_nod)
      filter_func_IO(1:filter%ntot_near_nod)                            &
     &      = filter%func(1:filter%ntot_near_nod)
!
      call dealloc_3d_filter_weight(filter)
      call dealloc_3d_filter_function(filter)
!
      end subroutine copy_3d_filter_weights_to_IO
!
!  ---------------------------------------------------------------------
!
      end module copy_3d_filters_4_IO
