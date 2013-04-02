!
!      module copy_3d_filters_4_IO
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine copy_3d_filter_stacks_from_IO
!      subroutine copy_3d_filter_weights_from_IO
!
!      subroutine copy_3d_filter_stacks_to_IO
!      subroutine copy_3d_filter_weights_to_IO
!
      module copy_3d_filters_4_IO
!
      use m_precision
!
      use m_filter_coef_combained
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
      subroutine copy_3d_filter_stacks_from_IO
!
!
      ngrp_nod_3d_filter = ngrp_nod_filter_IO
      call allocate_num_filtering_comb
!
      grp_name_3d_filter(1:ngrp_nod_3d_filter)                          &
     &         = grp_name_filter_IO(1:ngrp_nod_3d_filter)
      num_nod_3d_filter(1:ngrp_nod_3d_filter)                           &
     &         = num_nod_filter_IO(1:ngrp_nod_3d_filter)
      istack_nod_3d_filter(0:ngrp_nod_3d_filter)                        &
     &         = istack_nod_filter_IO(0:ngrp_nod_3d_filter)
!
      ntot_nod_3d_filter = ntot_nod_filter_IO
      call allocate_inod_filter_comb
!
      inod_3d_filter(1:ntot_nod_3d_filter)                              &
     &      = inod_filter_IO(1:ntot_nod_3d_filter)
      num_near_nod_3d_filter(1:ntot_nod_3d_filter)                      &
     &      = num_near_nod_filter_IO(1:ntot_nod_3d_filter)
      istack_near_nod_3d_filter(0:ntot_nod_3d_filter)                   &
     &      = istack_near_nod_filter_IO(0:ntot_nod_3d_filter)
      ntot_near_nod_3d_filter = ntot_near_nod_filter_IO
!
      call deallocate_inod_filter_comb_IO
      call deallocate_num_filtering_IO
!
      end subroutine copy_3d_filter_stacks_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weights_from_IO
!
!
      ntot_near_nod_3d_filter = ntot_near_nod_filter_IO
      call allocate_3d_filter_comb
!
      inod_near_nod_3d(1:ntot_near_nod_3d_filter)                       &
     &      = inod_near_nod_IO(1:ntot_near_nod_3d_filter)
      filter_weight_3d(1:ntot_near_nod_3d_filter)                       &
     &      = filter_weight_IO(1:ntot_near_nod_3d_filter)
!
      call deallocate_3d_filter_data_IO
!
      end subroutine copy_3d_filter_weights_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_stacks_to_IO
!
!
      ngrp_nod_filter_IO = ngrp_nod_3d_filter
      call allocate_num_filtering_IO
!
      grp_name_filter_IO(1:ngrp_nod_3d_filter)                          &
     &         = grp_name_3d_filter(1:ngrp_nod_3d_filter)
      num_nod_filter_IO(1:ngrp_nod_3d_filter)                           &
     &         = num_nod_3d_filter(1:ngrp_nod_3d_filter)
      istack_nod_filter_IO(0:ngrp_nod_3d_filter)                        &
     &         = istack_nod_3d_filter(0:ngrp_nod_3d_filter)
!
      ntot_nod_filter_IO = ntot_nod_3d_filter
      call allocate_inod_filter_comb_IO
!
      inod_filter_IO(1:ntot_nod_3d_filter)                              &
     &      = inod_3d_filter(1:ntot_nod_3d_filter)
      num_near_nod_filter_IO(1:ntot_nod_3d_filter)                      &
     &      = num_near_nod_3d_filter(1:ntot_nod_3d_filter)
      istack_near_nod_filter_IO(0:ntot_nod_3d_filter)                   &
     &      = istack_near_nod_3d_filter(0:ntot_nod_3d_filter)
      ntot_near_nod_filter_IO = ntot_near_nod_3d_filter
!
      call deallocate_inod_filter_comb
      call deallocate_num_filtering_comb
!
      end subroutine copy_3d_filter_stacks_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_3d_filter_weights_to_IO
!
!
      ntot_near_nod_filter_IO = ntot_near_nod_3d_filter
      call allocate_3d_filter_data_IO
!
      inod_near_nod_IO(1:ntot_near_nod_3d_filter)                       &
     &      = inod_near_nod_3d(1:ntot_near_nod_3d_filter)
      filter_weight_IO(1:ntot_near_nod_3d_filter)                       &
     &      = filter_weight_3d(1:ntot_near_nod_3d_filter)
      filter_func_IO(1:ntot_near_nod_3d_filter)                         &
     &      = filter_func_3d(1:ntot_near_nod_3d_filter)
!
      call deallocate_3d_filter_comb
      call deallocate_3d_filter_func_comb
!
      end subroutine copy_3d_filter_weights_to_IO
!
!  ---------------------------------------------------------------------
!
      end module copy_3d_filters_4_IO
