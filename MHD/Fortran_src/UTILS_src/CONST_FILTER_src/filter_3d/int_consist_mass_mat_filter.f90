!int_consist_mass_mat_filter.f90
!     module int_consist_mass_mat_filter
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine int_vol_consist_mass_matrix
!
      module int_consist_mass_mat_filter
!
      use m_precision
!
      use m_phys_constants
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_jacobians
      use t_crs_matrix
!
      implicit none
!
      private :: int_whole_consist_mass_matrix
      private :: int_group_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_consist_mass_matrix(mass)
!
      use m_machine_parameter
      use m_geometry_data
      use m_sorted_node
      use matrix_initialization
!
      type(CRS_matrix), intent(inout) :: mass
!
!
      call iccg_matrix_init(node1, ele1, rhs_tbl1, mat_tbl_q1,          &
     &    mass%ntot_A, mass%A_crs)
!
!      if (id_filter_area_grp(1) .eq. -1) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'int_whole_consist_mass_matrix'
        call int_whole_consist_mass_matrix(mass)
!      else
!        if (iflag_debug.eq.1)                                          &
!     &    write(*,*) 'int_group_consist_mass_matrix'
!        call int_group_consist_mass_matrix(mass)
!      end if
!
      end subroutine int_vol_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_whole_consist_mass_matrix(mass)
!
      use m_sorted_node
      use m_finite_element_matrix
      use int_vol_mass_matrix
!
      type(CRS_matrix), intent(inout) :: mass
!
!
      call int_consist_mass_matrix                                      &
     &   (ele1, jac1_3d_q, rhs_tbl1, mat_tbl_q1,                        &
     &    ele1%istack_ele_smp, num_int_points, fem1_wk,                 &
     &    mass%ntot_A, mass%A_crs)
!
      end subroutine int_whole_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_group_consist_mass_matrix(mass)
!
      use m_finite_element_matrix
      use m_element_list_4_filter
      use int_grouped_mass_matrix
!
      type(CRS_matrix), intent(inout) :: mass
!
!
      call int_grp_consist_mass_matrix(iele_filter_smp_stack,           &
     &    nele_4_filter, iele_4_filter, num_int_points,                 &
     &    mass%ntot_A, mass%A_crs)
!
      end subroutine int_group_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      end module int_consist_mass_mat_filter
