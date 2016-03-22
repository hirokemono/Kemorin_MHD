!t_filtering_data.f90
!      module t_filtering_data
!
!     Written by H. Matsui on Nov., 2006
!
      module t_filtering_data
!
      use m_precision
      use m_constants
      use t_filter_coefficients
      use t_comm_table
!
      implicit none
!
      type filtering_data_type
        type(filter_coefficients_type) :: filter
!
        type(filter_coefficients_type) :: filter_smp
!
!> data structure for filter communication table
        type(communication_table) :: comm
!
!
        integer(kind = kint) :: nnod_fil
!
        real(kind = kreal), pointer :: x_fil(:)
      end type filtering_data_type
!
      private :: s_const_tbl_3d_filtering_smp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_filter(filtering_data)
!
      type(filtering_data_type), intent(inout) :: filtering_data
!
!
      allocate( filtering_data%x_fil(6*filtering_data%nnod_fil) )
      filtering_data%x_fil = 0.0d0
!
      end subroutine alloc_nod_data_4_filter
!
!------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_filter(filtering_data)
!
      type(filtering_data_type), intent(inout) :: filtering_data
!
!
      deallocate( filtering_data%x_fil )
!
      end subroutine dealloc_nod_data_4_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_tbl_3d_filtering_smp(filtering_data)
!
      type(filtering_data_type), intent(inout) :: filtering_data
!
!
      call s_const_tbl_3d_filtering_smp                                 &
     &   (filtering_data%filter, filtering_data%filter_smp)
!
      end subroutine const_tbl_3d_filtering_smp
!
! ----------------------------------------------------------------------
!
      subroutine s_set_istart_3d_filtering(filter)
!
      use t_filter_coefficients
      use set_istart_3d_filtering
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: filter
!
!
      call alloc_stack_vec_filter(ione, filter)
!
      call count_num_3d_filtering_sum(filter%ngrp_node,                 &
     &    filter%istack_node, filter%ntot_nod,                          &
     &    filter%nnod_near, filter%min_nsum, filter%max_nsum)
      call s_cal_total_and_stacks(filter%ngrp_node,                     &
          filter%max_nsum, izero, filter%istack_nsum, filter%ntot_nsum)
!
      call alloc_istart_vec_filter(filter)
!
      call set_start_id_4_3d_filtering(filter%ngrp_node,                &
     &    filter%istack_node, filter%ntot_nod,                          &
     &    filter%nnod_near, filter%istack_nsum,                         &
     &    filter%ntot_nsum, filter%ist_nsum, filter%ied_nsum)
!
      end subroutine s_set_istart_3d_filtering
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_const_tbl_3d_filtering_smp(filter, filter_smp)
!
      use m_machine_parameter
      use t_filter_coefficients
!
      use set_3d_filtering_tbl_smp
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: filter
      type(filter_coefficients_type), intent(inout) :: filter_smp
!
      integer(kind = kint) :: max_tmp, min_tmp
!
!
      filter_smp%ngrp_node = filter%ngrp_node
      call alloc_num_filtering_comb(np_smp, filter_smp)
!
      call count_nnod_3d_filter_smp(np_smp, filter%ngrp_node,           &
     &    filter%group_name, filter%istack_node, filter_smp%ngrp_node,  &
     &    filter_smp%group_name, filter_smp%num_node)
!
      call s_cal_dbl_minmax_and_stacks(filter_smp%ngrp_node, np_smp,    &
     &    filter_smp%num_node, izero, filter_smp%istack_node,           &
     &    filter_smp%ntot_nod, max_tmp, min_tmp)
!
!   convert node list for filtering
!
      call alloc_inod_filter_comb(filter_smp)
!
      call set_inod_3d_filter_smp(np_smp, filter%ngrp_node,             &
     &    filter%istack_node, filter%ntot_nod, filter%inod_filter,      &
     &    filter%nnod_near, filter_smp%ngrp_node,                       &
     &    filter_smp%num_node, filter_smp%istack_node,                  &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%nnod_near)
!
      call s_cal_total_and_stacks(filter_smp%ntot_nod,                  &
     &    filter_smp%nnod_near, izero, filter_smp%istack_near_nod,      &
     &    filter_smp%ntot_near_nod)
!
!   convert filter coefs
!
      call alloc_3d_filter_comb(filter_smp)
!
      call set_neib_nod_3d_filter_smp(np_smp, filter%ngrp_node,         &
     &    filter%istack_node, filter%ntot_nod,                          &
     &    filter%nnod_near, filter%istack_near_nod,                     &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    filter_smp%ngrp_node, filter_smp%num_node,                    &
     &    filter_smp%istack_node, filter_smp%ntot_nod,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight)
!
      call dealloc_3d_filter_weight(filter)
      call dealloc_inod_filter_weights(filter)
!
!   set start and end address for summation
!
      call alloc_stack_vec_filter(np_smp, filter_smp)
      call count_num_3d_filtering_sum_smp(np_smp,                       &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%nnod_near,                    &
     &    filter_smp%min_nsum, filter_smp%max_nsum)
!
      call s_cal_dbl_minmax_and_stacks(filter_smp%ngrp_node, np_smp,    &
     &    filter_smp%max_nsum, izero, filter_smp%istack_nsum,           &
     &    filter_smp%ntot_nsum, max_tmp, min_tmp)
!
      call alloc_istart_vec_filter(filter_smp)
      call set_start_id_3d_filtering_smp(np_smp,                        &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%nnod_near,                    &
     &    filter_smp%istack_nsum, filter_smp%ntot_nsum,                 &
     &    filter_smp%ist_nsum, filter_smp%ied_nsum)
!
      end subroutine s_const_tbl_3d_filtering_smp
!
!  ---------------------------------------------------------------------
!
      end module t_filtering_data
