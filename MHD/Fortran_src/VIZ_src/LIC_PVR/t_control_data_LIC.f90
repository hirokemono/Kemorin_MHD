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
!!      subroutine read_lic_control_data(hd_lic_ctl, lic_ctl)
!!      subroutine dealloc_lic_control_flags(lic_ctl)
!!      subroutine bcast_lic_control_data(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags
!!
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
!!    opacity_field       magnetic_field
!!    opacity_component   amplitude
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
      use m_read_control_elements
      use t_control_elements
      use t_read_control_arrays
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
        integer(kind=kint) :: i_masking_ctl = 0
        type(lic_masking_ctl), allocatable :: mask_ctl(:)
!
        type(read_character_item) :: noise_type_ctl
        type(read_character_item) :: noise_file_prefix_ctl
        type(read_integer_item) ::   noise_resolution_ctl
!
        type(read_character_item) :: kernel_function_type_ctl
        type(read_character_item) :: kernal_file_prefix_ctl
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
      subroutine read_lic_control_data(hd_lic_ctl, lic_ctl)
!
      character(len = kchara), intent(in) :: hd_lic_ctl
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      if(right_begin_flag(hd_lic_ctl) .eq. 0) return
      if (lic_ctl%i_lic_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        lic_ctl%i_lic_control = find_control_end_flag(hd_lic_ctl)
        if(lic_ctl%i_lic_control .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_LIC_field, lic_ctl%LIC_field_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_color_field, lic_ctl%color_field_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_color_component, lic_ctl%color_component_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_opacity_field, lic_ctl%opacity_field_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_opacity_component, lic_ctl%opacity_component_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_noise_type, lic_ctl%noise_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_noise_file_head, lic_ctl%noise_file_prefix_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_noise_grid_size, lic_ctl%noise_resolution_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_kernel_function_type, lic_ctl%kernel_function_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_kernal_file_name, lic_ctl%kernal_file_prefix_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_LIC_trace_type, lic_ctl%LIC_trace_length_def_ctl)
        call read_real_ctl_type                                         &
     &     (hd_LIC_trace_length, lic_ctl%LIC_trace_length_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_LIC_trace_count, lic_ctl%LIC_trace_count_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_normalization_type, lic_ctl%normalization_type_ctl)
        call read_real_ctl_type                                         &
     &     (hd_normalization_value, lic_ctl%normalization_value_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_reflection_ref_type, lic_ctl%reflection_ref_type_ctl)
        call read_real_ctl_type                                         &
     &     (hd_referection_parameter, lic_ctl%reflection_parameter_ctl)
!
        call find_control_array_flag                                    &
     &     (hd_masking_ctl, lic_ctl%num_masking_ctl)
        if(lic_ctl%num_masking_ctl .gt. 0                               &
     &      .and. lic_ctl%i_masking_ctl .eq. 0) then
          call read_lic_masking_ctl_array(lic_ctl)
        end if
      end do
!
      end subroutine read_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_masking_ctl_array(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      if (lic_ctl%i_masking_ctl .gt. 0) return
      allocate(lic_ctl%mask_ctl(lic_ctl%num_masking_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(hd_masking_ctl,                &
     &      lic_ctl%num_masking_ctl, lic_ctl%i_masking_ctl)
        if(lic_ctl%i_masking_ctl .ge. lic_ctl%num_masking_ctl) exit
!
        if(right_begin_flag(hd_masking_ctl) .gt. 0) then
          lic_ctl%i_masking_ctl = lic_ctl%i_masking_ctl + 1
          call read_lic_masking_ctl_data                                &
     &      (hd_masking_ctl, lic_ctl%mask_ctl(lic_ctl%i_masking_ctl))
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
      integer(kind = kint) :: i
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
        do i = 1, lic_ctl%num_masking_ctl
          call dealloc_lic_masking_ctl_flags(lic_ctl%mask_ctl(i))
        end do
        deallocate(lic_ctl%mask_ctl)
      end if
      lic_ctl%num_masking_ctl =  0
      lic_ctl%i_masking_ctl =    0
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
      call MPI_BCAST(lic_ctl%i_lic_control,  ione,                      &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(lic_ctl%num_masking_ctl,  ione,                    &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
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
      end module t_control_data_LIC
