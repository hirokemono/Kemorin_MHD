!>@file   init_reference_field_data.f90
!!        module init_reference_field_data
!!
!! @author H. Matsui
!! @date     Programmed in June, 2023
!!
!> @brief Initialize reference field structure for FEM_MHD
!!
!!@verbatim
!!      subroutine s_init_reference_field_data(node, iphys,             &
!!     &          iref_base, iref_grad, ref_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(base_field_address), intent(inout) :: iref_base
!!        type(gradient_field_address), intent(inout) :: iref_grad
!!        type(phys_data), intent(inout) :: ref_fld
!!@endverbatim
!
      module init_reference_field_data
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
!
      private :: count_reference_field_address
      private :: set_reference_field_address
!
! --------------------------------------------------------------------
!
      contains
!
! --------------------------------------------------------------------
!
      subroutine s_init_reference_field_data(node, iphys,               &
     &          iref_base, iref_grad, ref_fld)
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_data), intent(inout) :: ref_fld
!
!
      call count_reference_field_address(iphys%base, ref_fld)
      call alloc_phys_name(ref_fld)
!
      call set_reference_field_address(iphys%base,                      &
     &                                 iref_base, iref_grad, ref_fld)
      call alloc_phys_data(node%numnod, ref_fld)
!
      end subroutine s_init_reference_field_data
!
! --------------------------------------------------------------------
!
      subroutine count_reference_field_address(iphys_base, ref_fld)
!
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data), intent(inout) :: ref_fld
!
!
      ref_fld%num_phys = 0
      if(iphys_base%i_per_temp .gt. 0)                                  &
     &                        ref_fld%num_phys = ref_fld%num_phys + 2
      if(iphys_base%i_per_temp .gt. 0)                                  &
     &                        ref_fld%num_phys = ref_fld%num_phys + 2
!
      end subroutine count_reference_field_address
!
! --------------------------------------------------------------------
!
      subroutine set_reference_field_address(iphys_base,                &
     &          iref_base, iref_grad, ref_fld)
!
      use m_base_field_labels
      use m_grad_field_labels
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_data), intent(inout) :: ref_fld
!
      integer(kind = kint) :: ifld
!
!
      ifld = 0
      if(iphys_base%i_per_temp .gt. 0) then
        ifld = ifld + 1
        iref_base%i_temp = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = temperature%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
!
        ifld = ifld + 1
        iref_grad%i_grad_temp = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = grad_temp%name
        ref_fld%num_component(ifld) = n_vector
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_vector
      end if
      if (iphys_base%i_per_light .gt. 0) then
        ifld = ifld + 1
        iref_base%i_light = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = composition%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
!
        ifld = ifld + 1
        iref_grad%i_grad_composit = ref_fld%istack_component(ifld-1)+1
        ref_fld%phys_name(ifld) = grad_composition%name
        ref_fld%num_component(ifld) = n_vector
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_vector
      end if
      ref_fld%ntot_phys = ref_fld%istack_component(ifld)
!
      end subroutine set_reference_field_address
!
! --------------------------------------------------------------------
!
      end module init_reference_field_data
