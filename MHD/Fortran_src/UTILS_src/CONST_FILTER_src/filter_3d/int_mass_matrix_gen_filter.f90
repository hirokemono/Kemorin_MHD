!
!      module int_mass_matrix_gen_filter
!
!      Written by H. Matsui on an., 2006
!
!      subroutine int_mass_matrix_4_filter
!
      module int_mass_matrix_gen_filter
!
      use m_ctl_params_4_gen_filter
!
      use m_precision
      use m_geometry_constants
      use m_geometry_data
!
      implicit none
!
      private :: int_grped_mass_matrix_filter
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_4_filter
!
      use m_machine_parameter
      use m_finite_element_matrix
      use int_vol_mass_matrix
!
!
!      if (id_filter_area_grp(1) .eq. -1) then
         call int_lumped_mass_matrix(num_int_points)
!      else
!        call int_grped_mass_matrix_filter
!      end if
!
!      call check_mass_martix(my_rank, node1%numnod, m1_lump)
!
      end subroutine int_mass_matrix_4_filter
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grped_mass_matrix_filter
!
      use m_finite_element_matrix
      use m_element_list_4_filter
      use int_grouped_mass_matrix
      use cal_ff_smp_to_ffs
!
!
      if     (ele1%nnod_4_ele.eq.num_t_quad                             &
     &   .or. ele1%nnod_4_ele.eq.num_t_lag) then
        call int_grp_mass_matrix_HRZ_full(iele_filter_smp_stack,        &
     &    nele_4_filter, iele_4_filter, num_int_points)
      else
        call int_grp_mass_matrix_diag(iele_filter_smp_stack,            &
     &     nele_4_filter, iele_4_filter, num_int_points)
      end if
!
      call cal_ff_smp_2_ml                                              &
     &   (node1, rhs_tbl1, m1_lump%ml, m1_lump%ml_o, f1_l%ff_smp)
!
      end subroutine int_grped_mass_matrix_filter
!
!-----------------------------------------------------------------------
!
      end module int_mass_matrix_gen_filter
