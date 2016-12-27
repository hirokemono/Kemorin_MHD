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
!
      implicit  none
!
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name='ctl_plane_mesh'
!
!
      integer(kind = kint) :: num_z_filter_ctl
      real(kind = kreal) ::   omitting_value_ctl
!
      character(len = kchara) :: z_filter_head_ctl
      character(len = kchara) :: vert_filter_type_ctl
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
     &                    :: hd_l_filter_ctl = 'line_filter_ctl'
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
      integer(kind = kint) :: i_num_z_filter =     0
      integer(kind = kint) :: i_z_filter_header =  0
      integer(kind = kint) :: i_vert_filter_type = 0
      integer(kind = kint) :: i_omitting_value =   0
!
      private :: control_file_name, control_file_code
!
!
      private :: hd_plane_mesh, i_plane_mesh
      private :: hd_l_filter_ctl
      private :: i_l_filter_ctl
      private :: hd_num_z_filter,     hd_z_filter_header
      private :: hd_vert_filter_type, hd_omitting_value
!
      private :: read_plane_mesh_ctl_data, read_z_filter_mesh_ctl
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
      use m_ctl_data_4_platforms
      use m_ctl_data_4_plane_model
      use m_ctl_data_filter_files
!
!
!   1 begin phys_values_ctl
!
      if(right_begin_flag(hd_plane_mesh) .eq. 0) return
      if (i_plane_mesh .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plane_mesh, i_plane_mesh)
        if(i_plane_mesh .gt. 0) exit
!
!
        call read_ctl_data_4_platform(plt1)
        call read_filter_fnames_ctl
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
        call find_control_end_flag(hd_l_filter_ctl, i_l_filter_ctl)
        if(i_l_filter_ctl .gt. 0) exit
!
!
        call read_character_ctl_item(hd_z_filter_header,                &
     &        i_z_filter_header, z_filter_head_ctl)
        call read_character_ctl_item(hd_vert_filter_type,               &
     &        i_vert_filter_type, vert_filter_type_ctl)
!
        call read_real_ctl_item(hd_omitting_value,                      &
     &        i_omitting_value, omitting_value_ctl)
!
        call read_integer_ctl_item(hd_num_z_filter,                     &
     &        i_num_z_filter, num_z_filter_ctl)
      end do
!
      end subroutine read_z_filter_mesh_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_cub_kemo
