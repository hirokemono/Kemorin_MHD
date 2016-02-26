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
      use t_jacobian_2d
!
      implicit  none
!
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac1_3d_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_q
!
!
!>     Stracture for linear Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_lq
!
!
!>     Stracture of linear Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_l
!>     Stracture of quadrature Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_q
!>     Stracture of quadrature Jacobians for linear surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_ql
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
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
