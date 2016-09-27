!>@file  field_data_4_pvr.f90
!!       module field_data_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Set field data for volume rendering
!!
!!@verbatim
!!      subroutine cal_field_4_each_pvr(node, ele, jac_3d,              &
!!     &          n_point, num_nod_phys, num_tot_nod_phys,              &
!!     &          istack_nod_component, d_nod, fld_params, field_pvr)
!!      subroutine set_pixel_on_pvr_screen(n_pvr_pixel, pixel_xy)
!!@endverbatim
!
      module field_data_4_pvr
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_each_pvr(node, ele, jac_3d,                &
     &          n_point, num_nod_phys, num_tot_nod_phys,                &
     &          istack_nod_component, d_nod, fld_params, field_pvr)
!
      use t_geometry_data
      use t_jacobian_3d
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use cal_gradient_on_element
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(pvr_field_parameter), intent(in) :: fld_params
!
      integer(kind = kint), intent(in) :: n_point, num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in) :: d_nod(n_point,num_tot_nod_phys)
!
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      i_field = fld_params%id_pvr_output
      ist_fld = istack_nod_component(i_field-1)
      num_comp = istack_nod_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    fld_params%icomp_pvr_output, d_nod(1,ist_fld+1),              &
     &    field_pvr%d_pvr)
!
      call fem_gradient_on_element(ele%istack_ele_smp, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    jac_3d%ntot_int, ione, jac_3d%dnx, jac_3d%xjac,               &
     &    field_pvr%grad_ele, field_pvr%d_pvr)
!
      end subroutine cal_field_4_each_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_pixel_on_pvr_screen(n_pvr_pixel, pixel_xy)
!
      use t_geometries_in_pvr_screen
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      pixel_xy%num_pixel_x = n_pvr_pixel(1)
      pixel_xy%num_pixel_y = n_pvr_pixel(2)
      call allocate_pixel_position_pvr(pixel_xy)
!
      call set_pixel_points_on_project(n_pvr_pixel(1), n_pvr_pixel(2),  &
          pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine set_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!
      end module field_data_4_pvr
