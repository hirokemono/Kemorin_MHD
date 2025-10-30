!>@file   t_reference_field_data.f90
!!        module t_reference_field_data
!!
!! @author H. Matsui
!! @date     Programmed in June, 2023
!!
!> @brief Initialize reference field structure for FEM_MHD
!!
!!@verbatim
!!      subroutine init_reference_field_data(node, iphys, reference)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(reference_field_data), intent(inout) :: reference
!!@endverbatim
!
      module t_reference_field_data
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_diffusion_term_labels
!
      implicit none
!
      type reference_field_data
!>        Base field address for reference field
        type(base_field_address) :: iref_base
!>        Gradient field address for reference field
        type(gradient_field_address) :: iref_grad
!>        Diffusivity field address for reference field
        type(diffusivity_adress) :: iref_diffusivity
!>        Reference field data
        type(phys_data) :: ref_fld
      end type reference_field_data
!
      private :: count_reference_fields
      private :: set_reference_field_address
!
! --------------------------------------------------------------------
!
      contains
!
! --------------------------------------------------------------------
!
      subroutine init_reference_field_data(node, iphys, reference)
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(reference_field_data), intent(inout) :: reference
!
!
      reference%ref_fld%num_phys                                        &
     &           = count_reference_fields(iphys%base, iphys%diffusion)
      call alloc_phys_name(reference%ref_fld)
!
      call set_reference_field_address(iphys%base, iphys%diffusion,     &
     &    reference%iref_base, reference%iref_grad,                     &
     &    reference%iref_diffusivity, reference%ref_fld)
      call alloc_phys_data(node%numnod, reference%ref_fld)
!
      end subroutine init_reference_field_data
!
! --------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            count_reference_fields(iphys_base, iphys_diffusion)
!
      type(base_field_address), intent(in) :: iphys_base
      type(diffusion_address), intent(in) ::  iphys_diffusion
!
      integer(kind = kint) :: num
!
      num = 0
      if(iphys_base%i_per_temp .gt. 0) num = num + 2
      if(iphys_base%i_per_temp .gt. 0) num = num + 2
!
      if((iphys_diffusion%i_v_diffuse                                   &
     &  + iphys_diffusion%i_w_diffuse) .gt. 0) num = num + 1
      if((iphys_diffusion%i_b_diffuse                                   &
     &  + iphys_diffusion%i_vp_diffuse) .gt. 0) num = num + 1
      if(iphys_diffusion%i_t_diffuse .gt. 0)    num = num + 1
      if(iphys_diffusion%i_c_diffuse .gt. 0)    num = num + 1
      count_reference_fields = num
!
      end function count_reference_fields
!
! --------------------------------------------------------------------
!
      subroutine set_reference_field_address                            &
     &         (iphys_base, iphys_diffusion, iref_base, iref_grad,      &
     &          iref_diffusivity, ref_fld)
!
      use m_base_field_labels
      use m_grad_field_labels
      use m_diffusion_term_labels
!
      type(base_field_address), intent(in) :: iphys_base
      type(diffusion_address), intent(in) ::  iphys_diffusion
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(diffusivity_adress), intent(inout) :: iref_diffusivity
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
!
!
      if((iphys_diffusion%i_v_diffuse                                   &
     &  + iphys_diffusion%i_w_diffuse) .gt. 0)  then
        ifld = ifld + 1
        iref_diffusivity%i_K_viscosity                                  &
     &       = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = kinetic_viscosity%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
      end if
      if((iphys_diffusion%i_b_diffuse                                   &
     &  + iphys_diffusion%i_vp_diffuse) .gt. 0) then
        ifld = ifld + 1
        iref_diffusivity%i_B_diffusivity                                &
     &       = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = magnetic_diffusivity%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
      end if
      if(iphys_diffusion%i_t_diffuse .gt. 0)    then
        ifld = ifld + 1
        iref_diffusivity%i_T_diffusivity                                &
     &       = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = thermal_diffusivity%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
      end if
      if(iphys_diffusion%i_c_diffuse .gt. 0)    then
        ifld = ifld + 1
        iref_diffusivity%i_C_diffusivity                                &
     &       = ref_fld%istack_component(ifld-1) + 1
        ref_fld%phys_name(ifld) = chemical_diffusivity%name
        ref_fld%num_component(ifld) = n_scalar
        ref_fld%istack_component(ifld)                                  &
     &        = ref_fld%istack_component(ifld-1) + n_scalar
      end if
      ref_fld%ntot_phys = ref_fld%istack_component(ifld)
!
      end subroutine set_reference_field_address
!
! --------------------------------------------------------------------
!
      end module t_reference_field_data
