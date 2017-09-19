!>@file  field_data_4_pvr.f90
!!       module field_data_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Set field data for volume rendering
!!
!!@verbatim
!!      subroutine cal_field_4_each_pvr(node, ele, g_FEM, jac_3d,       &
!!     &          nod_fld, fld_params, field_pvr)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(pvr_field_parameter), intent(in) :: fld_params
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_projected_field), intent(inout) :: field_pvr
!!      subroutine set_pixel_on_pvr_screen(view, pixel_xy, pvr_rgb)
!!      subroutine flush_pixel_on_pvr_screen(pixel_xy, pvr_rgb)
!!        type(pvr_view_parameter), intent(in) :: view
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module field_data_4_pvr
!
      use m_precision
      use m_constants
!
      use t_control_params_4_pvr
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_each_pvr(node, ele, g_FEM, jac_3d,         &
     &          nod_fld, fld_params, field_pvr)
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_geometries_in_pvr_screen
      use cal_gradient_on_element
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(pvr_field_parameter), intent(in) :: fld_params
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      i_field = fld_params%id_pvr_output
      ist_fld = nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    fld_params%icomp_pvr_output, nod_fld%d_fld(1,ist_fld+1),      &
     &    field_pvr%d_pvr)
!
      call fem_gradient_on_element(ele%istack_ele_smp, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, ione, jac_3d%dnx, jac_3d%xjac,  &
     &    field_pvr%grad_ele, field_pvr%d_pvr)
!
      end subroutine cal_field_4_each_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_pixel_on_pvr_screen(view, pixel_xy, pvr_rgb)
!
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use set_projection_matrix
!
      type(pvr_view_parameter), intent(in) :: view
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call allocate_pixel_position_pvr(view%n_pvr_pixel, pixel_xy)
      call alloc_pvr_image_array_type(view%n_pvr_pixel,pvr_rgb)
!
      call set_pixel_points_on_project                                  &
     &   (view%n_pvr_pixel(1), view%n_pvr_pixel(2),                     &
          pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine set_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine flush_pixel_on_pvr_screen(pixel_xy, pvr_rgb)
!
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call deallocate_pixel_position_pvr(pixel_xy)
      call dealloc_pvr_image_array_type(pvr_rgb)
!
      end subroutine flush_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!
      end module field_data_4_pvr
