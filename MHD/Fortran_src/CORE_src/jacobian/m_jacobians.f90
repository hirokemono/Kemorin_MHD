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
!     dxidx_1(iele,ix,1,1) :: dxi / dx
!     dxidx_1(iele,ix,2,1) :: dei / dx
!     dxidx_1(iele,ix,3,1) :: dzi / dx
!
!     dxidx_1(iele,ix,1,2) :: dxi / dy
!     dxidx_1(iele,ix,2,2) :: dei / dy
!     dxidx_1(iele,ix,3,2) :: dzi / dy
!
!     dxidx_1(iele,ix,1,3) :: dxi / dz
!     dxidx_1(iele,ix,2,3) :: dei / dz
!     dxidx_1(iele,ix,3,3) :: dzi / dz
!
!     iele: element ID
!     ix:   integration point ID
!       subroutine allocate_jacobians(numele)
!       subroutine allocate_jacobians_quad(numele, nnod_4_ele)
!       subroutine allocate_jacobians_linear_quad(numele)
!
!      subroutine copy_jacobians_quad
!      subroutine copy_jacobians_inf_quad
!
!       subroutine deallocate_inv_jacobians
!       subroutine deallocate_inv_jacobians_quad
!
!      subroutine allocate_dxi_dx_linear(numele)
!      subroutine allocate_dxi_dx_quad(numele)
!      subroutine allocate_dxi_dx_l_quad(numele)
!      subroutine deallocate_dxi_dx_linear
!      subroutine deallocate_dxi_dx_quad
!      subroutine copy_dxi_dx_2_quad
!
      module m_jacobians
!
      use m_precision
      use t_jacobian_3d
!
      implicit  none
!
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac1_3d_l
!  jac1_3d_l%an
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_q
!  jac1_3d_q%an
!
!>     Stracture for linear Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_lq
!  jac1_3d_lq%an
!
!
!      integer(kind = kint) :: ntot_int_3d
!      real (kind=kreal), allocatable :: an(:,:)
      real (kind=kreal), allocatable :: dnx(:,:,:,:)
!
      real (kind=kreal), allocatable :: an_infty(:,:,:)
! 
! 
      real (kind=kreal), allocatable :: dxidx_1(:,:,:,:)
!      real (kind=kreal), allocatable :: xjac(:,:)
!      real (kind=kreal), allocatable :: axjac(:,:)
! 
!
!      integer(kind = kint) :: ntot_int_3q
      real (kind=kreal), allocatable :: aw(:,:)
      real (kind=kreal), allocatable :: dwx(:,:,:,:)
! 
      real (kind=kreal), allocatable :: aw_infty(:,:,:)
! 
      real (kind=kreal), allocatable :: dxidx_20(:,:,:,:)
!      real (kind=kreal), allocatable :: xjac_q(:,:)
!      real (kind=kreal), allocatable :: axjac_q(:,:)
!
!
!      integer(kind = kint) :: ntot_int_3l
!      real (kind=kreal), allocatable :: am(:,:)
      real (kind=kreal), allocatable :: dmx(:,:,:,:)
! 
      real (kind=kreal), allocatable :: am_infty(:,:,:)
! 
      real (kind=kreal), allocatable :: dxidx_lq(:,:,:,:)
!      real (kind=kreal), allocatable :: xjac_lq(:,:)
!      real (kind=kreal), allocatable :: axjac_lq(:,:)
! 
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_jacobians(numele)
!
       use m_geometry_constants
       use m_fem_gauss_int_coefs
!
       integer(kind = kint), intent(in) :: numele
!
!
!
      call alloc_jacobians_type(numele, num_t_linear, jac1_3d_l)
!
!       allocate(an(num_t_linear,jac1_3d_l%ntot_int))
       allocate(an_infty(num_t_linear,nsurf_4_ele,jac1_3d_l%ntot_int))
!
       allocate(dnx(numele,num_t_linear,jac1_3d_l%ntot_int,3))
!
!       allocate(xjac(numele,jac1_3d_l%ntot_int))
!       allocate(axjac(numele,jac1_3d_l%ntot_int))
!
       dnx = 0.0d0
!
       an_infty = 0.0d0
!
       end subroutine allocate_jacobians
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_jacobians_quad(numele, nnod_4_ele)
!
       use m_geometry_constants
       use m_fem_gauss_int_coefs
!
       integer(kind = kint), intent(in) :: numele, nnod_4_ele
!
!
       jac1_3d_q%ntot_int = jac1_3d_l%ntot_int
!
      call alloc_jacobians_type(numele, nnod_4_ele, jac1_3d_q)
!
       allocate(aw(nnod_4_ele,jac1_3d_q%ntot_int))
       allocate(aw_infty(nnod_4_ele,nsurf_4_ele,jac1_3d_q%ntot_int))
!
       allocate(dwx(numele,nnod_4_ele,jac1_3d_q%ntot_int,3))
!
!       allocate(xjac_q(numele,jac1_3d_q%ntot_int))
!       allocate(axjac_q(numele,jac1_3d_q%ntot_int)) 
!
       aw = 0.0d0
       dwx = 0.0d0
!
       aw_infty = 0.0d0
!
       end subroutine allocate_jacobians_quad
!
!  ------------------------------------------------------------------
!
       subroutine allocate_jacobians_linear_quad(numele)
!
       use m_geometry_constants
       use m_fem_gauss_int_coefs
!
       integer(kind = kint), intent(in) :: numele
!
!
       jac1_3d_lq%ntot_int = jac1_3d_l%ntot_int
!
      call alloc_jacobians_type(numele, num_t_quad, jac1_3d_lq)
!
!       allocate(am(num_t_quad,jac1_3d_lq%ntot_int))
       allocate(am_infty(num_t_quad,nsurf_4_ele,jac1_3d_lq%ntot_int))
!
       allocate(dmx(numele,num_t_quad,jac1_3d_lq%ntot_int,3))
!
!       allocate(xjac_lq(numele,jac1_3d_lq%ntot_int))
!       allocate(axjac_lq(numele,jac1_3d_lq%ntot_int)) 
!
       dmx = 0.0d0
!
       am_infty = 0.0d0
!
       end subroutine allocate_jacobians_linear_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine copy_jacobians_quad
!
       aw      = jac1_3d_l%an
       dwx     = dnx
!
       jac1_3d_q%xjac  = jac1_3d_l%xjac
       jac1_3d_q%axjac = jac1_3d_l%axjac
!
       end subroutine copy_jacobians_quad
!
!  ------------------------------------------------------------------
!
       subroutine copy_jacobians_inf_quad
!
       aw_infty = an_infty
!
       end subroutine copy_jacobians_inf_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
       subroutine deallocate_jacobians
!
       deallocate(an_infty)
       deallocate(dnx)
!
       end subroutine deallocate_jacobians
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_inv_jacobians
!
      call dealloc_inv_jac_type(jac1_3d_l)
!
       end subroutine deallocate_inv_jacobians
!
!  ------------------------------------------------------------------
!
       subroutine deallocate_jacobians_quad
!
       deallocate(aw, aw_infty)
       deallocate(dwx)
!
       end subroutine deallocate_jacobians_quad
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_inv_jacobians_quad
!
      call dealloc_inv_jac_type(jac1_3d_q)
!
       end subroutine deallocate_inv_jacobians_quad
!
!  ------------------------------------------------------------------
!
       subroutine deallocate_jacobians_lq
!
       deallocate(am_infty)
       deallocate(dmx)
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
!-----------------------------------------------------------------------
!
      subroutine allocate_dxi_dx_linear(numele)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate( dxidx_1(numele,jac1_3d_l%ntot_int,3,3) )
      dxidx_1 = 0.0d0
!
      end subroutine allocate_dxi_dx_linear
!
!-----------------------------------------------------------------------
!
      subroutine allocate_dxi_dx_quad(numele)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate( dxidx_20(numele,jac1_3d_q%ntot_int,3,3) )
      dxidx_20 = 0.0d0
!
      end subroutine allocate_dxi_dx_quad
!
!-----------------------------------------------------------------------
!
      subroutine allocate_dxi_dx_l_quad(numele)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate( dxidx_lq(numele,jac1_3d_lq%ntot_int,3,3) )
      dxidx_lq = 0.0d0
!
      end subroutine allocate_dxi_dx_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_dxi_dx_linear
!
      deallocate( dxidx_1 )
!
      end subroutine deallocate_dxi_dx_linear
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_dxi_dx_quad
!
      deallocate( dxidx_20 )
!
      end subroutine deallocate_dxi_dx_quad
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_dxi_dx_l_quad
!
      deallocate( dxidx_lq )
!
      end subroutine deallocate_dxi_dx_l_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_dxi_dx_2_quad
!
!
      dxidx_20 = dxidx_1
!
      end subroutine copy_dxi_dx_2_quad
!
!-----------------------------------------------------------------------
!
      end module m_jacobians
