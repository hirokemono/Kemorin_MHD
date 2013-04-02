!const_tbl_3d_filtering_smp.f90
!      module const_tbl_3d_filtering_smp
!
!     Written by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine s_const_tbl_3d_filtering_smp
!
      module const_tbl_3d_filtering_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_tbl_3d_filtering_smp
!
      use m_machine_parameter
      use m_filter_coef_combained
      use m_3d_filter_coef_smp
!
      use set_3d_filtering_tbl_smp
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: max_tmp, min_tmp
!
      ngrp_nod_3d_filter_smp = ngrp_nod_3d_filter
      call allocate_nnod_3d_filter_smp(np_smp)
!
      call count_nnod_3d_filter_smp(np_smp, ngrp_nod_3d_filter,         &
     &    grp_name_3d_filter, istack_nod_3d_filter,                     &
     &    ngrp_nod_3d_filter_smp, grp_name_3d_filter_smp,               &
     &    num_nod_3d_filter_smp)
!
      call s_cal_dbl_minmax_and_stacks(ngrp_nod_3d_filter_smp, np_smp,  &
     &    num_nod_3d_filter_smp, izero, istack_nod_3d_fil_smp,          &
     &    ntot_nod_3d_filter_smp, max_tmp, min_tmp)
!
!   convert node list for filtering
!
      call allocate_inod_3d_filter_smp
!
      call set_inod_3d_filter_smp(np_smp, ngrp_nod_3d_filter,           &
     &    istack_nod_3d_filter, ntot_nod_3d_filter, inod_3d_filter,     &
     &    num_near_nod_3d_filter, ngrp_nod_3d_filter_smp,               &
     &    num_nod_3d_filter_smp, istack_nod_3d_fil_smp,                 &
     &    ntot_nod_3d_filter_smp, inod_3d_filter_smp,                   &
     &    num_near_nod_3d_fil_smp)
!
      call s_cal_total_and_stacks(ntot_nod_3d_filter_smp,               &
     &    num_near_nod_3d_fil_smp, izero, istack_near_nod_3d_f_smp,     &
     &    ntot_near_nod_3d_filter_smp)
!
!   convert filter coefs
!
      call allocate_3d_filter_coef_smp
!
      call set_neib_nod_3d_filter_smp(np_smp, ngrp_nod_3d_filter,       &
     &    istack_nod_3d_filter, ntot_nod_3d_filter,                     &
     &    num_near_nod_3d_filter, istack_near_nod_3d_filter,            &
     &    ntot_near_nod_3d_filter, inod_near_nod_3d, filter_weight_3d,  &
     &    ngrp_nod_3d_filter_smp, num_nod_3d_filter_smp,                &
     &    istack_nod_3d_fil_smp, ntot_nod_3d_filter_smp,                &
     &    istack_near_nod_3d_f_smp, ntot_near_nod_3d_filter_smp,        &
     &    inod_near_nod_3d_smp, filter_weight_3d_smp)
!
      call deallocate_3d_filter_comb
      call deallocate_inod_filter_comb
!
!   set start and end address for summation
!
      call allocate_stack_vec_fil_smp(np_smp)
      call count_num_3d_filtering_sum_smp(np_smp,                       &
     &    ngrp_nod_3d_filter_smp, istack_nod_3d_fil_smp,                &
     &    ntot_nod_3d_filter_smp, num_near_nod_3d_fil_smp,              &
     &    min_nsum_3d_fil_smp, max_nsum_3d_fil_smp)
!
      call s_cal_dbl_minmax_and_stacks(ngrp_nod_3d_filter_smp, np_smp,  &
     &    max_nsum_3d_fil_smp, izero, istack_nsum_3d_fil_smp,           &
     &    ntot_nsum_3d_fil_smp, max_tmp, min_tmp)
!
      call allocate_istart_vec_fil_smp
      call set_start_id_3d_filtering_smp(np_smp,                        &
     &    ngrp_nod_3d_filter_smp, istack_nod_3d_fil_smp,                &
     &    ntot_nod_3d_filter_smp, num_near_nod_3d_fil_smp,              &
     &    istack_nsum_3d_fil_smp, ntot_nsum_3d_fil_smp,                 &
     &    ist_nsum_3d_fil_smp, ied_nsum_3d_fil_smp)
!
      end subroutine s_const_tbl_3d_filtering_smp
!
!  ---------------------------------------------------------------------
!
      end module const_tbl_3d_filtering_smp
