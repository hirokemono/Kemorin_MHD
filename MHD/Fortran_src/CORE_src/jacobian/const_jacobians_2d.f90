!>@file  const_jacobians_2d.f90
!!       module const_jacobians_2d
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Construct Jacobians on surfaces
!!
!!@verbatim
!!      subroutine cal_jacobian_surface                                 &
!!     &         (node, ele, surf, jac_2d_l, jac_2d_q)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in)  :: surf
!!        type(jacobians_2d), intent(inout) :: jac_2d_l
!!        type(jacobians_2d), intent(inout) :: jac_2d_q
!!
!!      subroutine cal_jacobian_surface_linear(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_quad(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_lag(node, surf, jac_2d)
!!      subroutine cal_jacobian_surface_quad_on_l(node, surf, jac_2d)
!!        type(jacobians_2d), intent(inout) :: jac_2d
!!@endverbatim
!
      module const_jacobians_2d
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_shape_functions
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface element
!
      subroutine cal_jacobian_surface                                   &
     &         (node, ele, surf, jac_2d_l, jac_2d_q)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in)  :: surf
!
      type(jacobians_2d), intent(inout) :: jac_2d_l
      type(jacobians_2d), intent(inout) :: jac_2d_q
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_linear'
      call alloc_2d_jac_type                                            &
     &   (surf%numsurf, num_linear_sf, maxtot_int_2d, jac_2d_l)
      call cal_jacobian_surface_linear(node, surf, jac_2d_l)
!
      if (ele%first_ele_type .eq. 332) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_quad'
        call alloc_2d_jac_type                                          &
     &     (surf%numsurf, surf%nnod_4_surf, maxtot_int_2d, jac_2d_q)
        call cal_jacobian_surface_quad(node, surf, jac_2d_q)
      else if (ele%first_ele_type .eq. 333) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_lag'
        call alloc_2d_jac_type                                          &
     &     (surf%numsurf, surf%nnod_4_surf, maxtot_int_2d, jac_2d_q)
        call cal_jacobian_surface_lag(node, surf, jac_2d_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_2d'
        call copy_jacobians_2d                                          &
     &     (surf%numsurf, surf%nnod_4_surf, jac_2d_l, jac_2d_q)
      end if
!
      end subroutine cal_jacobian_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_linear(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_linear(jac_2d%ntot_int,              &
     &    jac_2d%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_2d_4                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf1, dnei_sf1)

      end subroutine cal_jacobian_surface_linear
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_quad(jac_2d%ntot_int,                &
     &    jac_2d%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_8                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf20, dnei_sf20)
!
      end subroutine cal_jacobian_surface_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_lag(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_lag(jac_2d%ntot_int,                 &
     &    jac_2d%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_2d_9                                            &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf27, dnei_sf27)
!
      end subroutine cal_jacobian_surface_lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_surface_quad_on_l(node, surf, jac_2d)
!
      use cal_1surf_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_2d
!
!
      call s_cal_shape_function_2d_quad(jac_2d%ntot_int,                &
     &    jac_2d%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature elaments
      call cal_jacobian_2d_4_8                                          &
     &    (node%numnod, surf%numsurf, surf%nnod_4_surf,                 &
     &     surf%ie_surf, node%xx, np_smp, surf%istack_surf_smp,         &
     &     jac_2d%ntot_int, jac_2d%xj_sf, jac_2d%axj_sf, jac_2d%xsf_sf, &
     &     dnxi_sf20, dnei_sf20)
!
      end subroutine cal_jacobian_surface_quad_on_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_shape_func_from_array(ntot_int_2d, nnod_4_surf,   &
     &          an_org, an_tgt)
!
      integer(kind = kint), intent(in) :: ntot_int_2d, nnod_4_surf
      real(kind=kreal), intent(in) :: an_org(nnod_4_surf,ntot_int_2d)
      real(kind=kreal), intent(inout)                                   &
     &         :: an_tgt(nnod_4_surf,ntot_int_2d)
      integer(kind = kint) :: ix, k1
!
!
      do ix = 1, ntot_int_2d
        do k1 = 1, nnod_4_surf
          an_tgt(k1,ix) = an_org(k1,ix)
        end do
      end do
!
      end subroutine copy_shape_func_from_array
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_2d
