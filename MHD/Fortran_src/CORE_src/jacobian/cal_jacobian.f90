!cal_jacobian.f90
!      module cal_jacobian
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!
!      subroutine set_max_int_point_by_etype
!
!> @brief Construct shape function, difference of shape function, 
!>        and Jacobians for input hexahedral mesh
!
!      subroutine cal_jacobian_element
!      subroutine cal_jacobian_surface
!
!
      module cal_jacobian
!
      use m_constants
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!> Set maximum number for integration points of FEM
!
      subroutine set_max_int_point_by_etype
!
      use m_fem_gauss_int_coefs
!
      if    (first_ele_type .eq. 332 .or. first_ele_type .eq. 333) then
        call maximum_integration_points(ithree)
      else
        call maximum_integration_points(itwo)
      end if
!
      end subroutine set_max_int_point_by_etype
!
! ----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for hexadedral element
!
      subroutine cal_jacobian_element
!
      use m_fem_gauss_int_coefs
      use m_surf_data_infinity
      use m_jacobians
!
      use set_gauss_int_parameters
      use set_integration_indices
      use cal_jacobians_infinity
!
      use cal_jacobians_linear
      use cal_jacobians_quad
      use cal_jacobians_lag
!
!  data allocation
!
      call allocate_integrate_parameters
!
      call allocate_gauss_coef_4_fem
!
      ntot_int_3d = maxtot_int_3d
      call allocate_jacobians
      call allocate_jacobians_quad
      call allocate_dxi_dx_linear
      call allocate_dxi_dx_quad
!
!  set constant for gauss integration with roots
!
      call init_gauss_int_parameters
!
!  set indices for gauss integration
!
      call set_integrate_indices_1d
      call set_integrate_indices_2d
      call set_integrate_indices_3d
!
!  set weighting for integration
!
      call set_gauss_coefs_4_1d
      call set_gauss_coefs_4_2d
      call set_gauss_coefs_4_3d
!
!  set jacobians
!
      call cal_jacobian_trilinear
!
      if (first_ele_type .eq. 332) then
        call cal_jacobian_quad
      else if (first_ele_type .eq. 333) then
        call cal_jacobian_lag
      end if
!
      if (infty_list%ngrp_sf .ne. 0) then
        call cal_jacobian_infinity
!
        if (first_ele_type .eq. 332) then
          call cal_jacobian_infty_quad
        else if (first_ele_type .eq. 333) then
          call cal_jacobian_infty_lag
        end if
!
      end if
!
      if (first_ele_type .eq. 331) then
        call copy_jacobians_quad
        call copy_dxi_dx_2_quad
        if (infty_list%ngrp_sf .ne. 0) then
          call copy_jacobians_inf_quad
        end if
      end if
!
      call deallocate_inv_jacobians_quad
      call deallocate_inv_jacobians
!
      end subroutine cal_jacobian_element
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface element
!
      subroutine cal_jacobian_surface
!
      use m_machine_parameter
      use m_jacobians_4_surface
!
      use cal_jacobians_linear
      use cal_jacobians_quad
      use cal_jacobians_lag
!
!
      ntot_int_2d = maxtot_int_2d
      call allocate_jacobians_surf_linear
      call allocate_jacobians_surf_quad
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_linear'
      call cal_jacobian_surface_linear
!
      if (first_ele_type .eq. 332) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_quad'
        call cal_jacobian_surface_quad
      else if (first_ele_type .eq. 333) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_lag'
        call cal_jacobian_surface_lag
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_surface_quad'
        call copy_jacobians_surface_quad
      end if
!
      end subroutine cal_jacobian_surface
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian
