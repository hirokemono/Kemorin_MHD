!
!     module m_jacobians
!
!> @brief Shape function and jacobians for 3D elements
!      Written by H. Matsui on Dec., 2003
!
!>  
!>@n @param   ntot_int_3d
!>     Total number of integration point for 3D element
!
!>@n @param   an(Shape_function_ID,integration_point)
!>      Shape function at integration point for linear element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dnx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of linear shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    an_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for linear infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac(element_ID,integration_point)
!>      Jacobian at integration point for linear element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!
!>
!>@n @param   aw(Shape_function_ID,integration_point)
!>      Shape function at integration point for element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dwx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    aw_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac_q(element_ID,integration_point)
!>      Jacobian at integration point for element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac_q(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!
!>
!>@n @param   am(Shape_function_ID,integration_point)
!>      Shape function at integration point for element
!>@n      \f[ N_\alpha(\xi,\chi,\eta) \f]
!
!>@n @param   dmx(element_ID,Shape_function_ID,integration_point,direction)
!>      Spatial differnce of shape function at integration
!>      point for element
!>@n      \f[ \frac{ dN_\alpha(\xi,\chi,\eta) }{ dx}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dy}, 
!>            \frac{ dN_\alpha(\xi,\chi,\eta) }{ dz} \f]
!
!>@n @param    am_infty(Shape_function_ID,surface_ID,integration_point)
!>      Shape function at integration point for infinity element
!>@n      \f[ N_{\infty\alpha}(\xi,\chi,\eta) \f]
!
!>@n @param   xjac_lq(element_ID,integration_point)
!>      Jacobian at integration point for element
!>@n      \f[ Ja = \det \left(\frac{ d{\bf x} }{ d {\bf \xi}} \right)\f]
!>@n @param   axjac_lq(element_ID,integration_point)
!>        \f[ Ja^{-1}\f]
!  define of matrix
!     jac1_3d_l%dxidx_3d(iele,ix,1,1) :: dxi / dx
!     jac1_3d_l%dxidx_3d(iele,ix,2,1) :: dei / dx
!     jac1_3d_l%dxidx_3d(iele,ix,3,1) :: dzi / dx
!
!     jac1_3d_l%dxidx_3d(iele,ix,1,2) :: dxi / dy
!     jac1_3d_l%dxidx_3d(iele,ix,2,2) :: dei / dy
!     jac1_3d_l%dxidx_3d(iele,ix,3,2) :: dzi / dy
!
!     jac1_3d_l%dxidx_3d(iele,ix,1,3) :: dxi / dz
!     jac1_3d_l%dxidx_3d(iele,ix,2,3) :: dei / dz
!     jac1_3d_l%dxidx_3d(iele,ix,3,3) :: dzi / dz
!
!     iele: element ID
!     ix:   integration point ID
!
!      subroutine cal_jacobian_element
!      subroutine allocate_jacobians_linear_quad(numele)
!
!       subroutine deallocate_jacobians
!       subroutine deallocate_jacobians_quad
!       subroutine deallocate_jacobians_lq
!       subroutine deallocate_inv_jacobians_lq
!
      module m_jacobians
!
      use m_constants
      use m_precision
      use t_jacobian_3d
!
      implicit  none
!
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac1_3d_l
!  jac1_3d_l%dxidx_3d
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_q
!  jac1_3d_q%dxidx_3d
!
!>     Stracture for linear Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_lq
!  jac1_3d_lq%dxidx_3d
!
! 
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!> Set maximum number for integration points of FEM
!
      subroutine set_max_int_point_by_etype
!
      use m_geometry_data
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
      use m_geometry_data
      use m_group_data
!
      use m_fem_gauss_int_coefs
      use m_surf_data_infinity
!
      use set_gauss_int_parameters
      use set_integration_indices
      use const_jacobians_infty_type
!
      use const_jacobians_3d
!
!  data allocation
!
      call allocate_integrate_parameters
      call allocate_gauss_coef_4_fem
!
      call alloc_jacobians_type(ele1%numele, num_t_linear,              &
     &                          maxtot_int_3d, jac1_3d_l)
      call alloc_jacobians_type(ele1%numele, ele1%nnod_4_ele,           &
     &                          jac1_3d_l%ntot_int, jac1_3d_q)
      call alloc_dxi_dx_type(ele1%numele, jac1_3d_l)
      call alloc_dxi_dx_type(ele1%numele, jac1_3d_q)
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
      call cal_jacobian_trilinear(node1, ele1, jac1_3d_l)
!
      if (ele1%first_ele_type .eq. 332) then
        call cal_jacobian_quad(node1, ele1, jac1_3d_q)
      else if (ele1%first_ele_type .eq. 333) then
        call cal_jacobian_lag(node1, ele1, jac1_3d_q)
      end if
!
      if (infty_list%ngrp_sf .ne. 0) then
        call cal_jacobian_infty_linear                                  &
     &     (node1, ele1, sf_grp1, infty_list, jac1_3d_l)
!
        if (ele1%first_ele_type .eq. 332) then
          call cal_jacobian_infty_quad                                  &
     &       (node1, ele1, sf_grp1, infty_list, jac1_3d_q)
        else if (ele1%first_ele_type .eq. 333) then
          call cal_jacobian_infty_lag                                   &
     &       (node1, ele1, sf_grp1, infty_list, jac1_3d_q)
        end if
!
      end if
!
      if (ele1%first_ele_type .eq. 331) then
        call copy_jacobians_3d(jac1_3d_l, jac1_3d_q)
        call copy_dxidx_3d(jac1_3d_l, jac1_3d_q)
        if (infty_list%ngrp_sf .ne. 0) then
          call copy_shape_func_infty(jac1_3d_l, jac1_3d_q)
        end if
      end if
!
      call dealloc_inv_jac_type(jac1_3d_q)
      call dealloc_inv_jac_type(jac1_3d_l)
!
      end subroutine cal_jacobian_element
!
!-----------------------------------------------------------------------
!
       subroutine allocate_jacobians_linear_quad(numele, ntot_int_3d)
!
       use m_geometry_constants
       use m_fem_gauss_int_coefs
!
       integer(kind = kint), intent(in) :: numele, ntot_int_3d
!
!
      call alloc_jacobians_type                                         &
     &   (numele, num_t_quad, ntot_int_3d, jac1_3d_lq)
!
       end subroutine allocate_jacobians_linear_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
       subroutine deallocate_jacobians
!
      call dealloc_jacobians_type(jac1_3d_l)
!
       end subroutine deallocate_jacobians
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_jacobians_quad
!
      call dealloc_jacobians_type(jac1_3d_q)
!
       end subroutine deallocate_jacobians_quad
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_jacobians_lq
!
      call dealloc_jacobians_type(jac1_3d_lq)
!
       end subroutine deallocate_jacobians_lq
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_inv_jacobians_lq
!
      call dealloc_inv_jac_type(jac1_3d_lq)
!
       end subroutine deallocate_inv_jacobians_lq
!
!  ------------------------------------------------------------------
!
      end module m_jacobians
