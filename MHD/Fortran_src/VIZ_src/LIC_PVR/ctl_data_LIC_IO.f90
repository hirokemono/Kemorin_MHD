!>@file   ctl_data_LIC_IO.f90
!!@brief  module ctl_data_LIC_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine s_read_lic_control_data                              &
!!     &         (id_control, hd_lic_ctl, lic_ctl, c_buf)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      integer(kind = kint) function num_ctl_label_LIC()
!!      subroutine set_ctl_label_LIC(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags
!!
!!    vr_sample_mode:         'fixed_size' or 'element_count'
!!    normalization_type:     'set_by_control' or 'set_by_range'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin LIC_ctl
!!    elapsed_time_monitor    On
!!
!!    LIC_field           magnetic_field
!!
!!    color_field         magnetic_field
!!    color_component     amplitude
!!
!!!    opacity_field       magnetic_field
!!!    opacity_component   amplitude
!!
!!    begin LIC_repartition_ctl
!!     ...
!!    end LIC_repartition_ctl
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
      module ctl_data_LIC_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_control_data_LIC
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
      use t_ctl_data_volume_repart
!
      implicit  none
!
!     3rd level for LIC_ctl
!
      character(len=kchara), parameter, private                         &
     &              :: hd_LIC_field =       'LIC_field'
      character(len=kchara), parameter, private                         &
     &              :: hd_sub_elapse_dump = 'elapsed_time_monitor'
!
      character(len=kchara), parameter, private                         &
     &              :: hd_color_field =     'color_field'
      character(len=kchara), parameter, private                         &
     &              :: hd_color_component = 'color_component'
      character(len=kchara), parameter, private                         &
     &              :: hd_opacity_field =   'opacity_field'
      character(len=kchara), parameter, private                         &
     &              :: hd_opacity_component = 'opacity_component'
!
      character(len=kchara), parameter, private                         &
     &              :: hd_masking_ctl = 'masking_control'
      character(len=kchara), parameter, private                         &
     &              :: hd_cube_noise =  'cube_noise_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_kernel =      'kernel_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_vr_sample_mode = 'vr_sample_mode'
      character(len=kchara), parameter, private                         &
     &             :: hd_step_size = 'step_size'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_normalization_type = 'normalization_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_normalization_value = 'normalization_value'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_partition = 'LIC_repartition_ctl'
!
      integer(kind = kint), parameter, private :: n_label_LIC = 14
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_lic_control_data                                &
     &         (id_control, hd_lic_ctl, lic_ctl, c_buf)
!
      use ctl_file_LIC_kernel_IO
      use ctl_file_LIC_noise_IO
      use skip_comment_f
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
        call read_chara_ctl_type(c_buf, hd_LIC_field,                   &
     &                           lic_ctl%LIC_field_ctl)
        call read_chara_ctl_type(c_buf, hd_sub_elapse_dump,             &
     &                           lic_ctl%subdomain_elapsed_dump_ctl)
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
     &     (c_buf, hd_vr_sample_mode, lic_ctl%vr_sample_mode_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_step_size, lic_ctl%step_size_ctl)
!
        call read_chara_ctl_type(c_buf, hd_normalization_type,          &
     &      lic_ctl%normalization_type_ctl)
        call read_real_ctl_type(c_buf, hd_normalization_value,          &
     &      lic_ctl%normalization_value_ctl)
!
        call read_control_vol_repart(id_control, hd_lic_partition,      &
     &                               lic_ctl%repart_ctl, c_buf)
!
        call sel_read_ctl_file_vol_repart(id_control, hd_lic_partition, &
     &                                    lic_ctl%repart_ctl, c_buf)
!
        call sel_read_cube_noise_ctl_file                               &
     &     (id_control, hd_cube_noise, lic_ctl%noise_ctl, c_buf)
        call sel_read_LIC_kernel_ctl_file                               &
     &     (id_control, hd_kernel, lic_ctl%kernel_ctl, c_buf)
!
        call read_lic_masking_ctl_array                                 &
     &     (id_control, hd_masking_ctl, lic_ctl, c_buf)
      end do
      lic_ctl%i_lic_control = 1
!
      end subroutine s_read_lic_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_lic_control_data                                 &
     &         (id_control, hd_lic_ctl, lic_ctl, c_buf)
!
      use ctl_file_LIC_kernel_IO
      use ctl_file_LIC_noise_IO
      use skip_comment_f
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
        call read_chara_ctl_type(c_buf, hd_LIC_field,                   &
     &                           lic_ctl%LIC_field_ctl)
        call read_chara_ctl_type(c_buf, hd_sub_elapse_dump,             &
     &                           lic_ctl%subdomain_elapsed_dump_ctl)
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
     &     (c_buf, hd_vr_sample_mode, lic_ctl%vr_sample_mode_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_step_size, lic_ctl%step_size_ctl)
!
        call read_chara_ctl_type(c_buf, hd_normalization_type,          &
     &      lic_ctl%normalization_type_ctl)
        call read_real_ctl_type(c_buf, hd_normalization_value,          &
     &      lic_ctl%normalization_value_ctl)
!
        call read_control_vol_repart(id_control, hd_lic_partition,      &
     &                               lic_ctl%repart_ctl, c_buf)
!
        call sel_read_ctl_file_vol_repart(id_control, hd_lic_partition, &
     &                                    lic_ctl%repart_ctl, c_buf)
!
        call sel_read_cube_noise_ctl_file                               &
     &     (id_control, hd_cube_noise, lic_ctl%noise_ctl, c_buf)
        call sel_read_LIC_kernel_ctl_file                               &
     &     (id_control, hd_kernel, lic_ctl%kernel_ctl, c_buf)
!
        call read_lic_masking_ctl_array                                 &
     &     (id_control, hd_masking_ctl, lic_ctl, c_buf)
      end do
      lic_ctl%i_lic_control = 1
!
      end subroutine write_lic_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_label_LIC()
      num_ctl_label_LIC = n_label_LIC
      return
      end function num_ctl_label_LIC
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC(names)
!
      character(len = kchara), intent(inout) :: names(n_label_LIC)
!
!
      call set_control_labels(hd_LIC_field,       names( 1))
!
      call set_control_labels(hd_color_field,     names( 2))
      call set_control_labels(hd_color_component, names( 3))
!
      call set_control_labels(hd_opacity_field,     names( 4))
      call set_control_labels(hd_opacity_component, names( 5))
!
      call set_control_labels(hd_masking_ctl, names( 6))
      call set_control_labels(hd_cube_noise,  names( 7))
      call set_control_labels(hd_kernel,      names( 8))
!
      call set_control_labels(hd_vr_sample_mode,      names( 9))
      call set_control_labels(hd_step_size,           names(10))
      call set_control_labels(hd_normalization_type,  names(11))
      call set_control_labels(hd_normalization_value, names(12))
!
      call set_control_labels(hd_lic_partition,   names(13))
      call set_control_labels(hd_sub_elapse_dump, names(14))
!
      end subroutine set_ctl_label_LIC
!
! ----------------------------------------------------------------------
!
      end module ctl_data_LIC_IO
