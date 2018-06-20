!
!      module m_ctl_data_4_cub_kemo
!
!        programmed by H.Matsui on Aug., 2007
!
!      subroutine read_control_data_plane_mesh
!      subroutine read_plane_mesh_ctl_data
!
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
      module m_ctl_data_4_cub_kemo
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
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
      type(platform_data_control), save :: cubmesh_plt
!
!>      Structure for filtering files
      type(filter_file_control), save :: ffile_cub_ctl
!
      type(read_integer_item), save :: num_z_filter_ctl
      type(read_real_item), save ::   omitting_value_ctl
!
      type(read_character_item), save :: z_filter_head_ctl
      type(read_character_item), save :: vert_filter_type_ctl
!
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_plane_mesh = 'const_plane_mesh'
      integer (kind=kint) :: i_plane_mesh = 0
!
!   1st level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_l_filter_ctl = 'line_filter_ctl'
!
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_l_filter_ctl =  0
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
      private :: control_file_name, control_file_code
!
      private :: hd_plane_mesh, i_plane_mesh
      private :: hd_l_filter_ctl, i_l_filter_ctl
      private :: hd_platform, i_platform
      private :: hd_num_z_filter,     hd_z_filter_header
      private :: hd_vert_filter_type, hd_omitting_value
!
      private :: read_plane_mesh_ctl_data, read_z_filter_mesh_ctl
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
!
      private :: hd_filter_fnames, i_filter_fnames
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine read_control_data_plane_mesh
!
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_plane_mesh_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_plane_mesh
!
! -----------------------------------------------------------------------
!
      subroutine read_plane_mesh_ctl_data
!
      use m_read_control_elements
      use skip_comment_f
      use m_ctl_data_4_plane_model
!
!
!   1 begin phys_values_ctl
!
      if(right_begin_flag(hd_plane_mesh) .eq. 0) return
      if (i_plane_mesh .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_plane_mesh = find_control_end_flag(hd_plane_mesh)
        if(i_plane_mesh .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, cubmesh_plt)
        call read_filter_fnames_control                                 &
     &     (hd_filter_fnames, i_filter_fnames, ffile_cub_ctl)
!
        call read_plane_model_param_ctl
        call read_z_filter_mesh_ctl
      end do
!
      end subroutine read_plane_mesh_ctl_data
!
! -----------------------------------------------------------------------
!
      subroutine read_z_filter_mesh_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_l_filter_ctl) .eq. 0) return
      if (i_l_filter_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_l_filter_ctl = find_control_end_flag(hd_l_filter_ctl)
        if(i_l_filter_ctl .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_z_filter_header,                    &
     &      z_filter_head_ctl)
        call read_chara_ctl_type(hd_vert_filter_type,                   &
     &      vert_filter_type_ctl)
!
        call read_real_ctl_type(hd_omitting_value, omitting_value_ctl)
!
        call read_integer_ctl_type(hd_num_z_filter, num_z_filter_ctl)
      end do
!
      end subroutine read_z_filter_mesh_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_cub_kemo
