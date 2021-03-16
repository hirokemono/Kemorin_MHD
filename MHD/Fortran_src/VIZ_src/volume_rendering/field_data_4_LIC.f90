!>@file  field_data_4_LIC.f90
!!       module field_data_4_LIC
!!
!!@author H. Matsui
!!@date   Programmed in Feb., 2018
!
!> @brief Set field data for volume rendering
!!
!!@verbatim
!!      subroutine cal_field_4_each_lic                                 &
!!     &         (node, ele, nod_fld, lic_p, field_lic)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(phys_data), intent(in) :: nod_fld
!!        type(lic_field_data), intent(inout) :: field_lic
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
      subroutine cal_field_4_each_lic                                   &
     &         (node, ele, nod_fld, lic_p, field_lic)
!
      use m_error_IDs
      use t_geometry_data
      use t_phys_data
      use t_lic_field_data
      use cal_gradient_on_element
      use convert_components_4_viz
      use set_components_flags
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(lic_parameters), intent(in) :: lic_p
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_field_data), intent(inout) :: field_lic
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
     &    field_lic%v_lic)
!
      i_field =  lic_p%lic_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    icomp_NORM, nod_fld%d_fld(1,ist_fld+1), field_lic%d_lic)
!
      if(lic_p%iflag_color_mode .eq. iflag_from_control) then
        i_field =  lic_p%color_field%id_field
        ist_fld =  nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        call convert_comps_4_viz                                        &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,         &
     &      node%a_r, node%ss, node%a_s, ione, num_comp,                &
     &      lic_p%color_field%id_component, nod_fld%d_fld(1,ist_fld+1), &
     &      field_lic%d_lic)
      end if
!
      do i = 1, lic_p%num_masking
        if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
          i_field =  lic_p%masking(i)%field_info%id_field
          ist_fld =  nod_fld%istack_component(i_field-1)
          num_comp = nod_fld%istack_component(i_field) - ist_fld
          call convert_comps_4_viz                                      &
       &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,       &
       &      node%a_r, node%ss, node%a_s, ione, num_comp,              &
       &      lic_p%masking(i)%field_info%id_component,                 &
       &      nod_fld%d_fld(1,ist_fld+1), field_lic%s_lic(1,i))
        end if
      end do
!
      end subroutine cal_field_4_each_lic
!
!  ---------------------------------------------------------------------
!
      end module field_data_4_LIC
