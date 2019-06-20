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
!!      subroutine read_lic_control_data                                &
!!     &         (id_control, hd_lic_ctl, lic_ctl, c_buf)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_lic_control_flags(lic_ctl)
!!      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!!        type(lic_parameter_ctl), intent(in) :: org_lic_c
!!        type(lic_parameter_ctl), intent(inout) :: new_lic_c
!!      subroutine bcast_lic_control_data(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags
!!
!!    vr_sample_mode:         'fixed_size' or 'element_count'
!!    noise_type:             'external_file' or 'randum'
!!    kernel_function_type:   'external_file' or 'linear'
!!    LIC_trace_length_mode:  'length'  or  'element_count'
!!    normalization_type:     'set_by_control' or 'set_by_range'
!!    reflection_reference:   'noise_file''color_field'
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
!!   end array masking_control
!!
!!    noise_type             'external_file'
!!    noise_file_prefix      'noise/noise_64'
!!    noise_resolution         2048
!!
!!    kernel_function_type   'external_file'
!!    kernal_image_prefix       'kernel'
!!
!!    vr_sample_mode         'fixed_size'
!!    step_size              0.005
!!
!!    LIC_trace_length_mode   'length'
!!    LIC_trace_length        0.5
!!    LIC_trace_count         8
!!
!!    normalization_type     'set_by_control'
!!    normalization_value     20.0
!!
!!    reflection_reference   'noise_file'
!!
!!    referection_parameter    2.0
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
      use t_control_elements
      use t_control_data_LIC_masking
      use skip_comment_f
!
      implicit  none
!
!
      type lic_parameter_ctl
        type(read_character_item) :: LIC_field_ctl
!
        type(read_character_item) :: color_field_ctl
        type(read_character_item) :: color_component_ctl
        type(read_character_item) :: opacity_field_ctl
        type(read_character_item) :: opacity_component_ctl
!
        integer(kind = kint) :: num_masking_ctl = 0
        type(lic_masking_ctl), allocatable :: mask_ctl(:)
!
        type(read_character_item) :: noise_type_ctl
        type(read_character_item) :: noise_file_prefix_ctl
        type(read_integer_item) ::   noise_resolution_ctl
!
        type(read_character_item) :: kernel_function_type_ctl
        type(read_character_item) :: kernal_file_prefix_ctl
!
        type(read_character_item) :: vr_sample_mode_ctl
        type(read_real_item) :: step_size_ctl
!
        type(read_character_item) :: LIC_trace_length_def_ctl
        type(read_real_item) ::      LIC_trace_length_ctl
        type(read_integer_item) ::   LIC_trace_count_ctl
!
        type(read_character_item) :: normalization_type_ctl
        type(read_real_item) ::      normalization_value_ctl
!
        type(read_character_item) :: reflection_ref_type_ctl
        type(read_real_item) ::      reflection_parameter_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_lic_control = 0
      end type lic_parameter_ctl
!
!
!     3rd level for LIC_ctl
!
      character(len=kchara) :: hd_LIC_field =   'LIC_field'
!
      character(len=kchara) :: hd_color_field =     'color_field'
      character(len=kchara) :: hd_color_component = 'color_component'
      character(len=kchara) :: hd_opacity_field =   'opacity_field'
      character(len=kchara) :: hd_opacity_component                     &
     &                        = 'opacity_component'
!
      character(len=kchara) :: hd_masking_ctl = 'masking_control'
!
      character(len=kchara) :: hd_noise_type =      'noise_type'
      character(len=kchara) :: hd_noise_file_head = 'noise_file_prefix'
      character(len=kchara) :: hd_noise_grid_size = 'noise_resolution'
!
      character(len=kchara) :: hd_kernel_function_type                  &
     &                        = 'kernel_function_type'
      character(len=kchara) :: hd_kernal_file_name                      &
     &                        = 'kernel_image_prefix'
!
      character(len=kchara) :: hd_vr_sample_mode                        &
     &                        = 'vr_sample_mode'
      character(len=kchara) :: hd_step_size = 'step_size'
!
      character(len=kchara) :: hd_LIC_trace_type                        &
     &                        = 'LIC_trace_length_mode'
      character(len=kchara) :: hd_LIC_trace_length = 'LIC_trace_length'
      character(len=kchara) :: hd_LIC_trace_count = 'LIC_trace_count'
!
      character(len=kchara) :: hd_normalization_type                    &
     &                        = 'normalization_type'
      character(len=kchara) :: hd_normalization_value                   &
     &                        = 'normalization_value'
!
      character(len=kchara) :: hd_reflection_ref_type                   &
     &                        = 'reflection_reference'
      character(len=kchara) :: hd_referection_parameter                 &
     &                        = 'referection_parameter'
!
      private :: hd_LIC_field, hd_color_field, hd_color_component
      private :: hd_opacity_field, hd_opacity_component
      private :: hd_masking_ctl
      private :: hd_noise_type, hd_noise_file_head, hd_noise_grid_size
      private :: hd_kernel_function_type, hd_kernal_file_name
      private :: hd_vr_sample_mode
      private :: hd_step_size
      private :: hd_LIC_trace_type
      private :: hd_LIC_trace_length, hd_LIC_trace_count
      private :: hd_normalization_type, hd_normalization_value
      private :: hd_reflection_ref_type, hd_referection_parameter
!
      private :: read_lic_masking_ctl_array
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_control_data                                  &
     &         (id_control, hd_lic_ctl, lic_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_lic_ctl) .eqv. .FALSE.) return
      if(lic_ctl%i_lic_control .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_lic_ctl)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_LIC_field, lic_ctl%LIC_field_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_color_field, lic_ctl%color_field_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_color_component, lic_ctl%color_component_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_opacity_field, lic_ctl%opacity_field_ctl)
        call read_chara_ctl_type(c_buf, hd_opacity_component,           &
     &      lic_ctl%opacity_component_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_noise_type, lic_ctl%noise_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_noise_file_head, lic_ctl%noise_file_prefix_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_noise_grid_size, lic_ctl%noise_resolution_ctl)
!
        call read_chara_ctl_type(c_buf, hd_kernel_function_type,        &
     &      lic_ctl%kernel_function_type_ctl)
        call read_chara_ctl_type(c_buf, hd_kernal_file_name,            &
     &      lic_ctl%kernal_file_prefix_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_vr_sample_mode, lic_ctl%vr_sample_mode_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_step_size, lic_ctl%step_size_ctl)
!
        call read_chara_ctl_type(c_buf, hd_LIC_trace_type,              &
     &      lic_ctl%LIC_trace_length_def_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_LIC_trace_length, lic_ctl%LIC_trace_length_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_LIC_trace_count, lic_ctl%LIC_trace_count_ctl)
!
        call read_chara_ctl_type(c_buf, hd_normalization_type,          &
     &      lic_ctl%normalization_type_ctl)
        call read_real_ctl_type(c_buf, hd_normalization_value,          &
     &      lic_ctl%normalization_value_ctl)
!
        call read_chara_ctl_type(c_buf, hd_reflection_ref_type,         &
     &      lic_ctl%reflection_ref_type_ctl)
        call read_real_ctl_type(c_buf, hd_referection_parameter,        &
     &      lic_ctl%reflection_parameter_ctl)
!
        if(check_array_flag(c_buf, hd_masking_ctl)) then
          call read_lic_masking_ctl_array                               &
     &       (id_control, hd_masking_ctl, lic_ctl, c_buf)
        end if
      end do
      lic_ctl%i_lic_control = 1
!
      end subroutine read_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_masking_ctl_array                             &
     &         (id_control, hd_block, lic_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(lic_ctl%mask_ctl)) return
      lic_ctl%num_masking_ctl = 0
      allocate(lic_ctl%mask_ctl(lic_ctl%num_masking_ctl))
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if (check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_lic_masking_ctl(lic_ctl)
          call read_lic_masking_ctl_data(id_control, hd_block,          &
     &        lic_ctl%mask_ctl(lic_ctl%num_masking_ctl), c_buf)
        end if
      end do
!
      end subroutine read_lic_masking_ctl_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_control_flags(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      lic_ctl%LIC_field_ctl%iflag = 0
!
      lic_ctl%color_field_ctl%iflag =       0
      lic_ctl%color_component_ctl%iflag =   0
      lic_ctl%opacity_field_ctl%iflag =     0
      lic_ctl%opacity_component_ctl%iflag = 0
!
      lic_ctl%noise_type_ctl%iflag =      0
      lic_ctl%noise_file_prefix_ctl%iflag = 0
      lic_ctl%noise_resolution_ctl%iflag = 0
!
      lic_ctl%kernel_function_type_ctl%iflag = 0
      lic_ctl%kernal_file_prefix_ctl%iflag =   0
!
      lic_ctl%vr_sample_mode_ctl%iflag = 0
      lic_ctl%step_size_ctl%iflag = 0
!
      lic_ctl%LIC_trace_length_def_ctl%iflag = 0
      lic_ctl%LIC_trace_length_ctl%iflag =     0
      lic_ctl%LIC_trace_count_ctl%iflag =      0
!
      lic_ctl%normalization_type_ctl%iflag =   0
      lic_ctl%normalization_value_ctl%iflag =  0
!
      lic_ctl%reflection_ref_type_ctl%iflag =  0
      lic_ctl%reflection_parameter_ctl%iflag = 0
!
!
      if(lic_ctl%num_masking_ctl .gt. 0) then
        call dealloc_lic_masking_ctls                                   &
     &     (lic_ctl%num_masking_ctl, lic_ctl%mask_ctl)
        deallocate(lic_ctl%mask_ctl)
      end if
      lic_ctl%num_masking_ctl =  0
!
      lic_ctl%i_lic_control = 0
!
      end subroutine dealloc_lic_control_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_control_data(lic_ctl)
!
      use bcast_control_arrays
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
      integer(kind = kint) :: i
!
!
      call MPI_BCAST(lic_ctl%i_lic_control,  1,                         &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(lic_ctl%LIC_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%color_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%color_component_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_field_ctl)
      call bcast_ctl_type_c1(lic_ctl%opacity_component_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%noise_type_ctl)
      call bcast_ctl_type_c1(lic_ctl%noise_file_prefix_ctl)
      call bcast_ctl_type_i1(lic_ctl%noise_resolution_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%kernel_function_type_ctl)
      call bcast_ctl_type_c1(lic_ctl%kernal_file_prefix_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%vr_sample_mode_ctl)
      call bcast_ctl_type_r1(lic_ctl%step_size_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%LIC_trace_length_def_ctl)
      call bcast_ctl_type_r1(lic_ctl%LIC_trace_length_ctl)
      call bcast_ctl_type_i1(lic_ctl%LIC_trace_count_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%normalization_type_ctl)
      call bcast_ctl_type_r1(lic_ctl%normalization_value_ctl)
!
      call bcast_ctl_type_c1(lic_ctl%reflection_ref_type_ctl)
      call bcast_ctl_type_r1(lic_ctl%reflection_parameter_ctl)
!
      call MPI_BCAST(lic_ctl%num_masking_ctl,  1,                       &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(my_rank .ne. 0) then
        allocate(lic_ctl%mask_ctl(lic_ctl%num_masking_ctl))
      end if
      do i = 1, lic_ctl%num_masking_ctl
        call bcast_lic_masking_ctl_data(lic_ctl%mask_ctl(i))
      end do
!
      end subroutine bcast_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine dup_lic_control_data(org_lic_c, new_lic_c)
!
      use copy_control_elements
!
      type(lic_parameter_ctl), intent(in) :: org_lic_c
      type(lic_parameter_ctl), intent(inout) :: new_lic_c
!
!
      new_lic_c%i_lic_control = org_lic_c%i_lic_control
!
      call copy_chara_ctl(org_lic_c%LIC_field_ctl,                      &
     &                    new_lic_c%LIC_field_ctl)
      call copy_chara_ctl(org_lic_c%color_field_ctl,                    &
     &                    new_lic_c%color_field_ctl)
      call copy_chara_ctl(org_lic_c%color_component_ctl,                &
     &                    new_lic_c%color_component_ctl)
      call copy_chara_ctl(org_lic_c%opacity_field_ctl,                  &
     &                    new_lic_c%opacity_field_ctl)
      call copy_chara_ctl(org_lic_c%opacity_component_ctl,              &
     &                    new_lic_c%opacity_component_ctl)
!
      call copy_chara_ctl(org_lic_c%noise_type_ctl,                     &
     &                    new_lic_c%noise_type_ctl)
      call copy_chara_ctl(org_lic_c%noise_file_prefix_ctl,              &
     &                    new_lic_c%noise_file_prefix_ctl)
      call copy_integer_ctl(org_lic_c%noise_resolution_ctl,             &
     &                      new_lic_c%noise_resolution_ctl)
!
      call copy_chara_ctl(org_lic_c%kernel_function_type_ctl,           &
     &                    new_lic_c%kernel_function_type_ctl)
      call copy_chara_ctl(org_lic_c%kernal_file_prefix_ctl,             &
     &                    new_lic_c%kernal_file_prefix_ctl)
!
      call copy_chara_ctl(org_lic_c%vr_sample_mode_ctl,                 &
     &                    new_lic_c%vr_sample_mode_ctl)
      call copy_real_ctl(org_lic_c%step_size_ctl,                       &
     &                   new_lic_c%step_size_ctl)
!
      call copy_chara_ctl(org_lic_c%LIC_trace_length_def_ctl,           &
     &                    new_lic_c%LIC_trace_length_def_ctl)
      call copy_real_ctl(org_lic_c%LIC_trace_length_ctl,                &
     &                   new_lic_c%LIC_trace_length_ctl)
      call copy_integer_ctl(org_lic_c%LIC_trace_count_ctl,              &
     &                      new_lic_c%LIC_trace_count_ctl)
!
      call copy_chara_ctl(org_lic_c%normalization_type_ctl,             &
     &                    new_lic_c%normalization_type_ctl)
      call copy_real_ctl(org_lic_c%normalization_value_ctl,             &
     &                    new_lic_c%normalization_value_ctl)
!
      call copy_chara_ctl(org_lic_c%reflection_ref_type_ctl,            &
     &                    new_lic_c%reflection_ref_type_ctl)
      call copy_real_ctl(org_lic_c%reflection_parameter_ctl,            &
     &                    new_lic_c%reflection_parameter_ctl)
!
      new_lic_c%num_masking_ctl = org_lic_c%num_masking_ctl
      if(new_lic_c%num_masking_ctl .gt. 0) then
        allocate(new_lic_c%mask_ctl(new_lic_c%num_masking_ctl))
        call dup_lic_masking_ctls(org_lic_c%num_masking_ctl,            &
     &      org_lic_c%mask_ctl, new_lic_c%mask_ctl)
      end if
!
      end subroutine dup_lic_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_new_lic_masking_ctl(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
      integer(kind=kint) :: ntmp_masking
      type(lic_masking_ctl), allocatable :: tmp_mask_c(:)
!
!
      ntmp_masking = lic_ctl%num_masking_ctl
      allocate(tmp_mask_c(ntmp_masking))
      call dup_lic_masking_ctls                                         &
     &   (ntmp_masking, lic_ctl%mask_ctl, tmp_mask_c)
!
      call dealloc_lic_masking_ctls                                     &
     &   (lic_ctl%num_masking_ctl, lic_ctl%mask_ctl)
      deallocate(lic_ctl%mask_ctl)
!
      lic_ctl%num_masking_ctl = ntmp_masking + 1
      allocate(lic_ctl%mask_ctl(lic_ctl%num_masking_ctl))
      call dup_lic_masking_ctls                                         &
     &   (ntmp_masking, tmp_mask_c, lic_ctl%mask_ctl(1))
!
      call dealloc_lic_masking_ctls(ntmp_masking, tmp_mask_c)
      deallocate(tmp_mask_c)
!
      end subroutine append_new_lic_masking_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC
