!>@file   t_control_data_LIC.f90
!!@brief  module t_control_data_LIC
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine dealloc_lic_control_flags(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!
!!      subroutine add_fields_4_lic_to_fld_ctl(lic_ctl, field_ctl)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!!        type(lic_parameter_ctl), intent(in) :: org_lic_c
!!        type(lic_parameter_ctl), intent(inout) :: new_lic_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags
!!
!!    vr_sample_mode:         'fixed_size' or 'element_count'
!!    normalization_type:     'set_by_control' or 'set_by_range'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin LIC_ctl
!!    LIC_field    magnetic_field
!!
!!    color_field         magnetic_field
!!    color_component     amplitude
!!
!!!    opacity_field       magnetic_field
!!!    opacity_component   amplitude
!!
!!    begin viz_repartition_ctl
!!     ...
!!    end viz_repartition_ctl
!!
!!    array masking_control    1
!!      begin masking_control
!!        masking_field        magnetic_field
!!        masking_component    magnetic_field
!!        array masking_range      1
!!          masking_range       0.5    0.8
!!          ...
!!        end array masking_range
!!      end masking_control
!!      ...
!!    end array masking_control
!!
!!    begin cube_noise_ctl
!!      ...
!!    end cube_noise_ctl
!!
!!    begin kernel_ctl
!!      ...
!!    end kernel_ctl
!!
!!    vr_sample_mode         'fixed_size'
!!    step_size              0.005
!!
!!    normalization_type     'set_by_control'
!!    normalization_value     20.0
!!  end LIC_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_maskings
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
      use t_ctl_data_volume_repart
      use skip_comment_f
!
      implicit  none
!
!
      type lic_parameter_ctl
!>        Block name
        character(len=kchara) :: block_name = 'LIC_ctl'
!
        type(read_character_item) :: LIC_field_ctl
        type(read_character_item) :: subdomain_elapsed_dump_ctl
!
        type(read_character_item) :: color_field_ctl
        type(read_character_item) :: color_component_ctl
        type(read_character_item) :: opacity_field_ctl
        type(read_character_item) :: opacity_component_ctl
!
        type(multi_masking_ctl) :: mul_mask_c
!
!>         File name for noise control block
        character(len = kchara) :: fname_LIC_noise_ctl = 'NO_FILE'
!>        structure of noise control
        type(cube_noise_ctl) :: noise_ctl
!
!>        File name for kernel control block
        character(len = kchara) :: fname_LIC_kernel_ctl = 'NO_FILE'
!>        structure of kernel control
        type(lic_kernel_ctl) :: kernel_ctl
!
        type(read_character_item) :: vr_sample_mode_ctl
        type(read_real_item) :: step_size_ctl
!
        type(read_character_item) :: normalization_type_ctl
        type(read_real_item) ::      normalization_value_ctl
!
!>         File name for repartition control block
        character(len = kchara) :: fname_vol_repart_ctl = 'NO_FILE'
!>         structure for repartition
        type(viz_repartition_ctl) :: repart_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_lic_control = 0
      end type lic_parameter_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_control_flags(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      lic_ctl%LIC_field_ctl%iflag =              0
      lic_ctl%subdomain_elapsed_dump_ctl%iflag = 0
!
      lic_ctl%color_field_ctl%iflag =       0
      lic_ctl%color_component_ctl%iflag =   0
      lic_ctl%opacity_field_ctl%iflag =     0
      lic_ctl%opacity_component_ctl%iflag = 0
!
      lic_ctl%vr_sample_mode_ctl%iflag = 0
      lic_ctl%step_size_ctl%iflag = 0
!
      lic_ctl%normalization_type_ctl%iflag =   0
      lic_ctl%normalization_value_ctl%iflag =  0
!
      call reset_cube_noise_control_data(lic_ctl%noise_ctl)
      call reset_kernel_control_data(lic_ctl%kernel_ctl)
      call dealloc_control_vol_repart(lic_ctl%repart_ctl)
!
      call dealloc_mul_masking_ctl(lic_ctl%mul_mask_c)
!
      lic_ctl%i_lic_control = 0
!
      end subroutine dealloc_lic_control_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_lic_to_fld_ctl(lic_ctl, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(lic_parameter_ctl), intent(in) :: lic_ctl
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_fld
!
!
      if(lic_ctl%LIC_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (lic_ctl%LIC_field_ctl%charavalue, field_ctl)
      end if
!
      if(lic_ctl%color_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (lic_ctl%color_field_ctl%charavalue, field_ctl)
      end if
!
      if(lic_ctl%opacity_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (lic_ctl%opacity_field_ctl%charavalue, field_ctl)
      end if
!
      do i_fld = 1, lic_ctl%mul_mask_c%num_masking_ctl
        call add_mask_field_to_fld_ctl                                  &
     &     (lic_ctl%mul_mask_c%mask_ctl(i_fld), field_ctl)
      end do
!
      end subroutine add_fields_4_lic_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
!
      type(lic_parameter_ctl), intent(in) :: org_lic_c
      type(lic_parameter_ctl), intent(inout) :: new_lic_c
!
      integer(kind = kint) :: i
!
      new_lic_c%block_name =    org_lic_c%block_name
      new_lic_c%i_lic_control = org_lic_c%i_lic_control
      new_lic_c%fname_LIC_kernel_ctl = org_lic_c%fname_LIC_kernel_ctl
      new_lic_c%fname_LIC_noise_ctl =  org_lic_c%fname_LIC_noise_ctl
      new_lic_c%fname_vol_repart_ctl = org_lic_c%fname_vol_repart_ctl
!
      call copy_chara_ctl(org_lic_c%LIC_field_ctl,                      &
     &                    new_lic_c%LIC_field_ctl)
      call copy_chara_ctl(org_lic_c%subdomain_elapsed_dump_ctl,         &
     &                    new_lic_c%subdomain_elapsed_dump_ctl)
!
      call copy_chara_ctl(org_lic_c%color_field_ctl,                    &
     &                    new_lic_c%color_field_ctl)
      call copy_chara_ctl(org_lic_c%color_component_ctl,                &
     &                    new_lic_c%color_component_ctl)
      call copy_chara_ctl(org_lic_c%opacity_field_ctl,                  &
     &                    new_lic_c%opacity_field_ctl)
      call copy_chara_ctl(org_lic_c%opacity_component_ctl,              &
     &                    new_lic_c%opacity_component_ctl)
!
      call copy_chara_ctl(org_lic_c%vr_sample_mode_ctl,                 &
     &                    new_lic_c%vr_sample_mode_ctl)
      call copy_real_ctl(org_lic_c%step_size_ctl,                       &
     &                   new_lic_c%step_size_ctl)
!
      call copy_chara_ctl(org_lic_c%normalization_type_ctl,             &
     &                    new_lic_c%normalization_type_ctl)
      call copy_real_ctl(org_lic_c%normalization_value_ctl,             &
     &                    new_lic_c%normalization_value_ctl)
!
      call copy_cube_noise_control_data(org_lic_c%noise_ctl,            &
     &                                  new_lic_c%noise_ctl)
      call copy_kernel_control_data(org_lic_c%kernel_ctl,               &
     &                              new_lic_c%kernel_ctl)
      call dup_control_vol_repart(org_lic_c%repart_ctl,                 &
     &                            new_lic_c%repart_ctl)
!
      call dup_mul_masking_ctl(org_lic_c%mul_mask_c,                    &
     &                         new_lic_c%mul_mask_c)
!
      end subroutine dup_lic_control_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC
