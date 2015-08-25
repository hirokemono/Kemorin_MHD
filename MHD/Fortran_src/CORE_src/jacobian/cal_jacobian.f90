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
!
!
      module cal_jacobian
!
      use m_constants
      use m_precision
!
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
      if      (ele1%first_ele_type .eq. 332                             &
     &    .or. ele1%first_ele_type .eq. 333) then
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
      use m_group_data
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
!
!  data allocation
!
      call allocate_integrate_parameters
!
      call allocate_gauss_coef_4_fem
!
      jac1_3d_l%ntot_int = maxtot_int_3d
      call allocate_jacobians(ele1%numele)
      call allocate_jacobians_quad(ele1%numele, ele1%nnod_4_ele)
      call allocate_dxi_dx_linear(ele1%numele)
      call allocate_dxi_dx_quad(ele1%numele)
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
      if (ele1%first_ele_type .eq. 332) then
        call cal_jacobian_quad
      else if (ele1%first_ele_type .eq. 333) then
        call cal_jacobian_lag
      end if
!
      if (infty_list%ngrp_sf .ne. 0) then
        call cal_jacobian_infinity(sf_grp1)
!
        if (ele1%first_ele_type .eq. 332) then
          call cal_jacobian_infty_quad(sf_grp1)
        else if (ele1%first_ele_type .eq. 333) then
          call cal_jacobian_infty_lag(sf_grp1)
        end if
!
      end if
!
      if (ele1%first_ele_type .eq. 331) then
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
!
      end module cal_jacobian
