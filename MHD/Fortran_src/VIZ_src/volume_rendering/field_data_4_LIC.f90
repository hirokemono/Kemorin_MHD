!>@file  field_data_4_LIC.f90
!!       module field_data_4_LIC
!!
!!@author H. Matsui
!!@date   Programmed in Feb., 2018
!
!> @brief Set field data for volume rendering
!!
!!@verbatim
!!      subroutine cal_field_4_each_lic(node, ele, g_FEM, jac_3d,       &
!!     &          nod_fld, lic_p, field_pvr)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_projected_field), intent(inout) :: field_pvr
!!@endverbatim
!
      module field_data_4_LIC
!
      use m_precision
      use m_constants
!
      use t_control_params_4_pvr
      use t_control_param_LIC
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_each_lic(node, ele, g_FEM, jac_3d,         &
     &          nod_fld, lic_p, field_pvr)
!
      use m_error_IDs
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_geometries_in_pvr_screen
      use cal_gradient_on_element
      use convert_components_4_viz
      use set_components_flags
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(lic_parameters), intent(in) :: lic_p
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_projected_field), intent(inout) :: field_pvr
!
      integer(kind = kint) :: i_field, ist_fld, num_comp, i
!
!
      i_field =  lic_p%lic_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    icomp_VECTOR, nod_fld%d_fld(1,ist_fld+1),                     &
     &    field_pvr%v_lic)
!
      i_field =  lic_p%lic_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    icomp_NORM, nod_fld%d_fld(1,ist_fld+1), field_pvr%d_pvr)
!
      call fem_gradient_on_element(ele%istack_ele_smp, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, ione, jac_3d%dnx, jac_3d%xjac,  &
     &    field_pvr%grad_ele, field_pvr%d_pvr)
!
      i_field =  lic_p%color_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    lic_p%color_field%id_component, nod_fld%d_fld(1,ist_fld+1),   &
     &    field_pvr%d_pvr)
!
      do i = 1, lic_p%num_masking
        i_field =  lic_p%masking(i)%field_info%id_field
        ist_fld =  nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        call convert_comps_4_viz                                        &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,         &
     &      node%a_r, node%ss, node%a_s, ione, num_comp,                &
     &      lic_p%masking(i)%field_info%id_component,                   &
     &      nod_fld%d_fld(1,ist_fld+1), field_pvr%s_lic(1,i))
      end do
!
      end subroutine cal_field_4_each_lic
!
!  ---------------------------------------------------------------------
!
      end module field_data_4_LIC
