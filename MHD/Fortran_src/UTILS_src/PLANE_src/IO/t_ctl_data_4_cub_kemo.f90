!!
!!      module t_ctl_data_4_cub_kemo
!!
!!        programmed by H.Matsui on Aug., 2007
!!
!!      subroutine read_control_data_plane_mesh
!!      subroutine reset_plane_mesh_ctl_data(cubmesh_c)
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin const_plane_mesh
!!  
!!    begin  data_files_def
!!      mesh_file_prefix       'mesh/in'
!!    end data_files_def
!!  
!!    begin filter_files_def
!!      filter_file_head_ctl   'mesh/filter_node_l'
!!    end filter_files_def
!!  
!!    begin plane_mesh_ctl
!!      nnod_plane_ctl        12     12     12      end
!!      ndomain_plane_ctl      2      2      2      end
!!      unit_len_plane_ctl    one    one    one     end
!!      plane_size_ctl      1.0e0  1.0e0  1.0e0   end
!!      horizontal_grid_ctl   Chebyshev
!!      num_of_sleeve_ctl    2
!!    end plane_mesh_ctl
!!  
!!    begin line_filter_ctl
!!      num_z_filter_ctl       2
!!      z_filter_head_ctl      'filter_info'
!!      vert_filter_type_ctl   norm
!!      omitting_value_ctl     1.0e-30
!!    end line_filter_ctl
!!  
!!  end const_plane_mesh
!!
!!    -------------------------------------------------------------------
!
      module t_ctl_data_4_cub_kemo
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use t_ctl_data_4_plane_model
      use t_control_elements
!
      implicit  none
!
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name='ctl_plane_mesh'
!
!
      type ctl_data_4_cub_kemo
        type(platform_data_control) :: cubmesh_plt
!
!>        Structure for filtering files
        type(filter_file_control) :: ffile_cub_ctl
!>        Structure for cube domain
        type(ctl_data_4_plane_model) :: cube_c
!
        type(read_integer_item) :: num_z_filter_ctl
        type(read_real_item) ::   omitting_value_ctl
!
        type(read_character_item) :: z_filter_head_ctl
        type(read_character_item) :: vert_filter_type_ctl
!
        integer (kind=kint) :: i_plane_mesh = 0
!
        integer (kind=kint) :: i_l_filter_ctl =  0
      end type ctl_data_4_cub_kemo
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_plane_mesh = 'const_plane_mesh'
!
!   1st level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_l_filter_ctl = 'line_filter_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_plane_def = 'plane_mesh_ctl'
!
!   2nd level for filter data
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_z_filter =     'num_z_filter_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_z_filter_header =  'z_filter_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_vert_filter_type = 'vert_filter_type_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_omitting_value =   'omitting_value_ctl'
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
!
      private :: control_file_name, control_file_code
!
      private :: hd_plane_mesh, hd_l_filter_ctl, hd_platform
      private :: hd_num_z_filter,     hd_z_filter_header
      private :: hd_vert_filter_type, hd_omitting_value
      private :: hd_filter_fnames, hd_plane_def
!
      private :: read_plane_mesh_ctl_data, read_z_filter_mesh_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine read_control_data_plane_mesh(cubmesh_c)
!
      use skip_comment_f
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
!
      type(buffer_for_control) :: c_buf1
!
!
      open (control_file_code, file = control_file_name)
!
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_plane_mesh_ctl_data                                   &
     &     (control_file_code, hd_plane_mesh, cubmesh_c, c_buf1)
        if(cubmesh_c%i_plane_mesh .gt. 0) exit
      end do
      close(control_file_code)
!
      end subroutine read_control_data_plane_mesh
!
! -----------------------------------------------------------------------
!
      subroutine read_plane_mesh_ctl_data                               &
     &         (id_control, hd_block, cubmesh_c, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cubmesh_c%i_plane_mesh .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, cubmesh_c%cubmesh_plt, c_buf)
        call read_filter_fnames_control(id_control, hd_filter_fnames,   &
     &      cubmesh_c%ffile_cub_ctl, c_buf)
!
        call read_plane_model_param_ctl(id_control, hd_plane_def,       &
     &      cubmesh_c%cube_c, c_buf)
        call read_z_filter_mesh_ctl(id_control, hd_l_filter_ctl,        &
     &      cubmesh_c, c_buf)
      end do
      cubmesh_c%i_plane_mesh = 1
!
      end subroutine read_plane_mesh_ctl_data
!
! -----------------------------------------------------------------------
!
      subroutine reset_plane_mesh_ctl_data(cubmesh_c)
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
!
!
      call reset_filter_fnames_control(cubmesh_c%ffile_cub_ctl)
      call reset_z_filter_mesh_ctl(cubmesh_c)
!
      cubmesh_c%i_plane_mesh = 0
!
      end subroutine reset_plane_mesh_ctl_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_z_filter_mesh_ctl                                 &
     &         (id_control, hd_block, cubmesh_c, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cubmesh_c%i_l_filter_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_z_filter_header,             &
     &      cubmesh_c%z_filter_head_ctl)
        call read_chara_ctl_type(c_buf, hd_vert_filter_type,            &
     &      cubmesh_c%vert_filter_type_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_omitting_value, cubmesh_c%omitting_value_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_z_filter, cubmesh_c%num_z_filter_ctl)
      end do
      cubmesh_c%i_l_filter_ctl = 1
!
      end subroutine read_z_filter_mesh_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_z_filter_mesh_ctl(cubmesh_c)
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
!
!
      cubmesh_c%z_filter_head_ctl%iflag =    0
      cubmesh_c%vert_filter_type_ctl%iflag = 0
      cubmesh_c%omitting_value_ctl%iflag =   0
      cubmesh_c%num_z_filter_ctl%iflag =     0
!
      cubmesh_c%i_l_filter_ctl = 0
!
      end subroutine reset_z_filter_mesh_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_cub_kemo
