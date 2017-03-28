!>@file  const_jacobians_sf_grp.f90
!!       module const_jacobians_sf_grp
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Construct Jacobians on surfaces
!!
!!@verbatim
!!      subroutine const_jacobian_sf_grp                                &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp_l, jac_sf_grp_q)
!!
!!      subroutine sel_jacobian_surf_grp_type(node, ele, surf,          &
!!     &          surf_grp, jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_linear(node, ele, surf_grp,    &
!!     &          jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_quad(node, ele, surf_grp,      &
!!     &          jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_lag(node, ele, surf_grp,       &
!!     &          jac_sf_grp)
!!      subroutine const_jacobian_sf_grp_l_quad(node, ele, surf_grp,    &
!!     &          jac_sf_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(jacobians_2d), intent(inout) :: jac_sf_grp
!!@endverbatim
!
      module const_jacobians_sf_grp
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
!> for surface group
!
      subroutine const_jacobian_sf_grp                                  &
     &         (node, ele, surf, sf_grp, jac_sf_grp_l, jac_sf_grp_q)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
!
      type(jacobians_2d), intent(inout) :: jac_sf_grp_l
      type(jacobians_2d), intent(inout) :: jac_sf_grp_q
!
!
      if (sf_grp%num_grp .le. 0) return
      call alloc_2d_jac_type(sf_grp%num_item, num_linear_sf,            &
     &                       maxtot_int_2d, jac_sf_grp_l)
      if (iflag_debug.eq.1) write(*,*) 'const_jacobian_sf_grp_linear'
      call const_jacobian_sf_grp_linear                                 &
     &   (node, ele, sf_grp, jac_sf_grp_l)
!
      if(surf%nnod_4_surf .eq. num_quad_sf) then
        if (iflag_debug.eq.1)  write(*,*) 'const_jacobian_sf_grp_quad'
        call alloc_2d_jac_type(sf_grp%num_item, surf%nnod_4_surf,       &
     &      maxtot_int_2d, jac_sf_grp_q)
        call const_jacobian_sf_grp_quad                                 &
     &    (node, ele, sf_grp, jac_sf_grp_q)
      else if (surf%nnod_4_surf .eq. num_lag_sf) then
        if (iflag_debug.eq.1) write(*,*) 'const_jacobian_sf_grp_lag'
        call alloc_2d_jac_type(sf_grp%num_item, surf%nnod_4_surf,       &
     &      maxtot_int_2d, jac_sf_grp_q)
        call const_jacobian_sf_grp_lag                                  &
     &     (node, ele, sf_grp, jac_sf_grp_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_2d_quad'
        call copy_jacobians_2d(sf_grp%num_item, surf%nnod_4_surf,       &
     &                         jac_sf_grp_l, jac_sf_grp_q)
      end if
!
      end subroutine const_jacobian_sf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_jacobian_surf_grp_type(node, ele, surf,            &
     &          surf_grp, jac_sf_grp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_data), intent(in)  :: surf
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      if (surf_grp%num_grp .gt. 0) then
        if      (surf%nnod_4_surf .eq. num_linear_sf) then
          call const_jacobian_sf_grp_linear(node, ele,                  &
     &        surf_grp, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_quad_sf)   then
          call const_jacobian_sf_grp_quad(node, ele,                    &
     &        surf_grp, jac_sf_grp)
        else if (surf%nnod_4_surf .eq. num_lag_sf)   then
          call const_jacobian_sf_grp_lag(node, ele,                     &
     &        surf_grp, jac_sf_grp)
        end if
      end if
!
      end subroutine sel_jacobian_surf_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_linear(node, ele, surf_grp,      &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_linear(jac_sf_grp%ntot_int,          &
     &    jac_sf_grp%an_sf, dnxi_sf1, dnei_sf1, xi2, ei2)
!
!   jacobian for tri-linear elaments
      call cal_jacobian_sf_grp_4                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf1, dnei_sf1)
!
      end subroutine const_jacobian_sf_grp_linear
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_quad(node, ele, surf_grp,        &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_8                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf20, dnei_sf20 )
!
      end subroutine const_jacobian_sf_grp_quad
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_lag(node, ele, surf_grp,         &
     &          jac_sf_grp)
!
      use m_geometry_constants
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_lag(jac_sf_grp%ntot_int,             &
     &    jac_sf_grp%an_sf, dnxi_sf27, dnei_sf27, xi2, ei2)
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_9                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf27, dnei_sf27)
!
!
      end subroutine const_jacobian_sf_grp_lag
!
!-----------------------------------------------------------------------
!
      subroutine const_jacobian_sf_grp_l_quad(node, ele, surf_grp,      &
     &          jac_sf_grp)
!
      use cal_1surf_grp_jacobians
      use cal_shape_function_2d
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(jacobians_2d), intent(inout) :: jac_sf_grp
!
!
      call s_cal_shape_function_2d_quad(jac_sf_grp%ntot_int,            &
     &    jac_sf_grp%an_sf, dnxi_sf20, dnei_sf20, xi2, ei2)
!
!
!   jacobian for quadrature  elaments
      call cal_jacobian_sf_grp_4_8                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie, node%xx,     &
     &    surf_grp%num_grp, surf_grp%num_item, surf_grp%item_sf_grp,    &
     &    np_smp, surf_grp%num_grp_smp, surf_grp%istack_grp_smp,        &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%axj_sf,     &
     &    jac_sf_grp%xsf_sf, dnxi_sf20, dnei_sf20)
!
      end subroutine const_jacobian_sf_grp_l_quad
!
!-----------------------------------------------------------------------
!
      end module const_jacobians_sf_grp
