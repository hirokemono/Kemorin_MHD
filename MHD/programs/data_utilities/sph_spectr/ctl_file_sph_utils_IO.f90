!
!      module ctl_file_sph_utils_IO
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine read_control_data_sph_utils(file_name, spu_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!!      subroutine write_control_data_sph_utils(file_name, spu_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
!
      module ctl_file_sph_utils_IO
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_sph_utils
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_sph_monitor
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
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
!
!   2nd level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_pick_sph = 'sph_monitor_ctl'
!
      private :: control_file_code
      private :: read_sph_utils_control_data
      private :: write_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_utils(file_name, spu_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open (control_file_code, file = file_name)
!
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_sph_utils_control_data                                &
     &     (control_file_code, hd_sph_trans_ctl, spu_ctl, c_buf1)
        if(spu_ctl%i_sph_trans_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      end subroutine read_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine write_control_data_sph_utils(file_name, spu_ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
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
      call write_sph_utils_control_data                                &
     &   (control_file_code, hd_sph_trans_ctl, spu_ctl, level1)
      close(control_file_code)
!
      end subroutine write_control_data_sph_utils
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_sph_utils_control_data                            &
     &         (id_control, hd_block, spu_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spu_ctl%i_sph_trans_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, spu_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, spu_ctl%org_plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, spu_ctl%Fmesh_ctl, c_buf)
!
        call read_sph_trans_model_ctl                                   &
     &     (id_control, hd_sph_trans_model, spu_ctl, c_buf)
        call read_sph_trans_params_ctl                                  &
     &     (id_control, hd_sph_trans_params, spu_ctl, c_buf)
!
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, spu_ctl%smonitor_ctl, c_buf)
      end do
      spu_ctl%i_sph_trans_ctl = 1
!
      end subroutine read_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_utils_control_data                           &
     &         (id_control, hd_block, spu_ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(spu_ctl%i_sph_trans_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, spu_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, spu_ctl%org_plt, level)
      call write_FEM_mesh_control                                       &
     &   (id_control, hd_FEM_mesh, spu_ctl%Fmesh_ctl, level)
!
      call write_sph_trans_model_ctl                                    &
     &   (id_control, hd_sph_trans_model, spu_ctl, level)
      call write_sph_trans_params_ctl                                   &
     &   (id_control, hd_sph_trans_params, spu_ctl, level)
!
      call write_sph_monitoring_ctl                                     &
     &   (id_control, hd_pick_sph, spu_ctl%smonitor_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_utils_control_data
!
! -----------------------------------------------------------------------
!
      end module ctl_file_sph_utils_IO
