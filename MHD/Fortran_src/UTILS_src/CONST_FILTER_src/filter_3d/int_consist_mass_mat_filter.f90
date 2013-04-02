!int_consist_mass_mat_filter.f90
!     module int_consist_mass_mat_filter
!
      module int_consist_mass_mat_filter
!
!     Written by H. Matsui on Oct., 2006
!
      use m_precision
!
      use m_phys_constants
      use m_ctl_params_4_gen_filter
      use m_geometry_parameter
      use m_crs_consist_mass_mat
!
      implicit none
!
      private :: int_whole_consist_mass_matrix
      private :: int_group_consist_mass_matrix
!
!      subroutine int_vol_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_consist_mass_matrix
!
      use m_machine_parameter
      use matrix_initialization
!
!
      call iccg_matrix_init(num_mass_mat, aiccg_mass)
!
!      if (id_filter_area_grp(1) .eq. -1) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'int_whole_consist_mass_matrix'
        call int_whole_consist_mass_matrix
!      else
!        if (iflag_debug.eq.1)                                          &
!     &    write(*,*) 'int_group_consist_mass_matrix'
!        call int_group_consist_mass_matrix
!      end if
!
      end subroutine int_vol_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_whole_consist_mass_matrix
!
      use m_finite_element_matrix
      use int_vol_mass_matrix
!
!
      call int_consist_mass_matrix(iele_smp_stack, num_int_points,      &
     &    num_mass_mat, aiccg_mass)
!
      end subroutine int_whole_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_group_consist_mass_matrix
!
      use m_finite_element_matrix
      use m_element_list_4_filter
      use int_grouped_mass_matrix
!
!
      call int_grp_consist_mass_matrix(iele_filter_smp_stack,           &
     &    nele_4_filter, iele_4_filter, num_int_points,                 &
     &    num_mass_mat, aiccg_mass)
!
      end subroutine int_group_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      end module int_consist_mass_mat_filter
