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
!!      subroutine read_lic_masking_ctl_array                           &
!!     &         (id_control, hd_block, lic_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_lic_masking_ctl_array                          &
!!     &         (id_control, hd_block, lic_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine add_fields_4_lic_to_fld_ctl(lic_ctl, field_ctl)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!      subroutine alloc_lic_masking_ctl(lic_ctl)
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
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
      use t_control_data_masking
      use t_control_data_LIC_noise
      use t_control_data_LIC_kernel
      use t_ctl_data_volume_repart
      use skip_comment_f
!
      implicit  none
!
!
      type lic_parameter_ctl
        type(read_character_item) :: LIC_field_ctl
        type(read_character_item) :: subdomain_elapsed_dump_ctl
!
        type(read_character_item) :: color_field_ctl
        type(read_character_item) :: color_component_ctl
        type(read_character_item) :: opacity_field_ctl
        type(read_character_item) :: opacity_component_ctl
!
        integer(kind = kint) :: num_masking_ctl = 0
        type(masking_by_field_ctl), allocatable :: mask_ctl(:)
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
      private :: dealloc_lic_masking_ctl
      private :: append_new_lic_masking_ctl
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
      if(lic_ctl%num_masking_ctl .gt. 0) then
        call dealloc_masking_ctls                                       &
     &     (lic_ctl%num_masking_ctl, lic_ctl%mask_ctl)
      end if
      call dealloc_lic_masking_ctl(lic_ctl)
!
      lic_ctl%i_lic_control = 0
!
      end subroutine dealloc_lic_control_flags
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
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(lic_ctl%mask_ctl)) return
      lic_ctl%num_masking_ctl = 0
      call alloc_lic_masking_ctl(lic_ctl)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if (check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_lic_masking_ctl(lic_ctl)
          call read_masking_ctl_data(id_control, hd_block,              &
     &        lic_ctl%mask_ctl(lic_ctl%num_masking_ctl), c_buf)
        end if
      end do
!
      end subroutine read_lic_masking_ctl_array
!
!  ---------------------------------------------------------------------
!
      subroutine write_lic_masking_ctl_array                            &
     &         (id_control, hd_block, lic_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(lic_parameter_ctl), intent(in) :: lic_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, lic_ctl%num_masking_ctl
        call write_masking_ctl_data(id_control, hd_block,               &
     &      lic_ctl%mask_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_lic_masking_ctl_array
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
      do i_fld = 1, lic_ctl%num_masking_ctl
        call add_mask_field_to_fld_ctl                                  &
     &     (lic_ctl%mask_ctl(i_fld), field_ctl)
      end do
!
      end subroutine add_fields_4_lic_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_new_lic_masking_ctl(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
      integer(kind=kint) :: ntmp_masking
      type(masking_by_field_ctl), allocatable :: tmp_mask_c(:)
!
!
      ntmp_masking = lic_ctl%num_masking_ctl
      allocate(tmp_mask_c(ntmp_masking))
      call dup_masking_ctls(ntmp_masking, lic_ctl%mask_ctl, tmp_mask_c)
!
      call dealloc_masking_ctls                                         &
     &   (lic_ctl%num_masking_ctl, lic_ctl%mask_ctl)
      call dealloc_lic_masking_ctl(lic_ctl)
!
      lic_ctl%num_masking_ctl = ntmp_masking + 1
      call alloc_lic_masking_ctl(lic_ctl)
      call dup_masking_ctls                                             &
     &   (ntmp_masking, tmp_mask_c, lic_ctl%mask_ctl(1))
!
      call dealloc_masking_ctls(ntmp_masking, tmp_mask_c)
      deallocate(tmp_mask_c)
!
      end subroutine append_new_lic_masking_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_lic_masking_ctl(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      allocate(lic_ctl%mask_ctl(lic_ctl%num_masking_ctl))
!
      end subroutine alloc_lic_masking_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_ctl(lic_ctl)
!
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      if(allocated(lic_ctl%mask_ctl)) then
        deallocate(lic_ctl%mask_ctl)
      end if
      lic_ctl%num_masking_ctl = 0
!
      end subroutine dealloc_lic_masking_ctl
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
      use bcast_ctl_data_LIC_noise
!
      type(lic_parameter_ctl), intent(in) :: org_lic_c
      type(lic_parameter_ctl), intent(inout) :: new_lic_c
!
!
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
      new_lic_c%num_masking_ctl = org_lic_c%num_masking_ctl
      if(new_lic_c%num_masking_ctl .gt. 0) then
        allocate(new_lic_c%mask_ctl(new_lic_c%num_masking_ctl))
        call dup_masking_ctls(org_lic_c%num_masking_ctl,                &
     &      org_lic_c%mask_ctl, new_lic_c%mask_ctl)
      end if
!
      end subroutine dup_lic_control_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC
