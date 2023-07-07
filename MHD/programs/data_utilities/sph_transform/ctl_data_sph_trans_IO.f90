!>@file   ctl_data_sph_trans_IO.f90
!!@brief  module ctl_data_sph_trans_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine read_control_data_sph_trans(file_name,               &
!!     &                                       spt_ctl, c_buf)
!!      subroutine write_control_data_sph_trans(file_name, spt_ctl)
!!      subroutine dealloc_sph_trans_control_data(spt_ctl)
!!        character(len = kchara), intent(in) :: file_name
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!@endverbatim
!
      module ctl_data_sph_trans_IO
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_sph_trans
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_gen_sph_shell
      use t_control_data_vizs
      use t_control_array_integer
      use t_control_array_character
      use skip_comment_f
!
      implicit  none
!
      integer(kind = kint), parameter, private                          &
     &                     :: control_file_code = 13
!
!   Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_ctl = 'spherical_transform'
!
!   1st level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'

      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: read_sph_trans_control_data
      private :: write_sph_trans_control_data
!
      private :: read_sph_trans_model_ctl, write_sph_trans_model_ctl
      private :: reset_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_trans(file_name,                 &
     &                                       spt_ctl, c_buf)
!
      use viz_step_ctls_to_time_ctl
!
      character(len = kchara), intent(in) :: file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open (control_file_code, file = file_name)
      do
        call load_one_line_from_control                                 &
     &     (control_file_code, hd_sph_trans_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_sph_trans_control_data                                &
     &     (control_file_code, hd_sph_trans_ctl, spt_ctl, c_buf)
        if(spt_ctl%i_sph_trans_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      call s_viz_step_ctls_to_time_ctl                                  &
     &   (spt_ctl%viz_ctls, spt_ctl%t_ctl)
      call add_fields_4_vizs_to_fld_ctl(spt_ctl%viz_ctls,               &
     &    spt_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_data_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine write_control_data_sph_trans(file_name, spt_ctl)
!
      use delete_data_files
!
      character(len = kchara), intent(in) :: file_name
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write control file: ', trim(file_name)
      level1 = 0
      open (control_file_code, file = file_name)
      call write_sph_trans_control_data                                 &
     &   (control_file_code, hd_sph_trans_ctl, spt_ctl, level1)
      close(control_file_code)
!
      end subroutine write_control_data_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data                            &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_data_visualiser_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(spt_ctl%i_sph_trans_ctl .gt. 0) return
      call init_platforms_labels(hd_platform, spt_ctl%plt)
      call init_platforms_labels(hd_org_data, spt_ctl%org_plt)
      call init_FEM_mesh_ctl_label(hd_FEM_mesh, spt_ctl%Fmesh_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, spt_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, spt_ctl%org_plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, spt_ctl%Fmesh_ctl, c_buf)
!
        call sel_read_ctl_gen_shell_grids(id_control, hd_sph_shell,     &
     &      spt_ctl%fname_psph, spt_ctl%psph_ctl, c_buf)
!
        call read_sph_trans_model_ctl                                   &
     &     (id_control, hd_sph_trans_model, spt_ctl, c_buf)
        call read_sph_trans_params_ctl                                  &
     &     (id_control, hd_sph_trans_params, spt_ctl, c_buf)
!
        call s_read_viz_controls(id_control, hd_viz_control,            &
     &                           spt_ctl%viz_ctls, c_buf)
      end do
      spt_ctl%i_sph_trans_ctl = 1
!
      end subroutine read_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_trans_control_data                           &
     &         (id_control, hd_block, spt_ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_data_visualiser_IO
      use ctl_file_gen_sph_shell_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(spt_ctl%i_sph_trans_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, spt_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, spt_ctl%org_plt, level)
      call write_FEM_mesh_control                                       &
     &   (id_control, spt_ctl%Fmesh_ctl, level)
!
      call sel_write_ctl_gen_shell_grids                                &
     &   (id_control, spt_ctl%fname_psph, spt_ctl%psph_ctl, level)
!
      call write_sph_trans_model_ctl                                    &
     &   (id_control, hd_sph_trans_model, spt_ctl, level)
      call write_sph_trans_params_ctl                                   &
     &   (id_control, hd_sph_trans_params, spt_ctl, level)
!
      call write_viz_controls(id_control, spt_ctl%viz_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_control_data(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call reset_sph_trans_model_ctl(spt_ctl)
      call reset_sph_trans_params_ctl(spt_ctl)
!
      call reset_control_platforms(spt_ctl%plt)
      call reset_control_platforms(spt_ctl%org_plt)
      call reset_FEM_mesh_control(spt_ctl%Fmesh_ctl)
!
      spt_ctl%i_sph_trans_ctl = 0
!
      end subroutine dealloc_sph_trans_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl                               &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      use ctl_data_4_time_steps_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spt_ctl%i_sph_trans_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, spt_ctl%fld_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, spt_ctl%t_ctl, c_buf)
      end do
      spt_ctl%i_sph_trans_model = 1
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_trans_model_ctl                              &
     &         (id_control, hd_block, spt_ctl, level)
!
      use ctl_data_4_time_steps_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(spt_ctl%i_sph_trans_model .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_phys_data_control                                      &
     &   (id_control, hd_phys_values, spt_ctl%fld_ctl, level)
      call write_control_time_step_data                                 &
     &   (id_control, spt_ctl%t_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_sph_trans_model_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      spt_ctl%i_sph_trans_model = 0
!
      end subroutine reset_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      end module ctl_data_sph_trans_IO
