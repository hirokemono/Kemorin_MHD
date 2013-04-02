!
!      module m_ctl_data_4_plane_model
!
!        programmed by H.Matsui on June, 2007
!
!      subroutine read_plane_model_param_ctl
!
      module m_ctl_data_4_plane_model
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: nnod_plane_ctl(3)
      integer(kind = kint) :: ndomain_plane_ctl(3)
      integer(kind = kint) :: num_of_sleeve_ctl
      real(kind = kreal) ::   plane_size_ctl(3)
!
      character(len = kchara) :: unit_len_plane_ctl(3)
      character(len = kchara) :: horizontal_grid_ctl
!
!   label for entry
!
      character(len=kchara), parameter :: hd_plane_def = 'plane_mesh_ctl'
      integer (kind=kint) :: i_plane_def = 0
!
!   4th level for grids
!
      character(len=kchara), parameter                                  &
     &       :: hd_nnod_plane_ctl =      'nnod_plane_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_ndomain_plane_ctl =   'ndomain_plane_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_num_of_sleeve_ctl =   'num_of_sleeve_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_size_ctl =      'plane_size_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_unit_len_plane_ctl =  'unit_len_plane_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_horizontal_grid_ctl = 'horizontal_grid_ctl'
!
      integer (kind=kint) :: i_nnod_plane_ctl =      0
      integer (kind=kint) :: i_ndomain_plane_ctl =   0
      integer (kind=kint) :: i_num_of_sleeve_ctl =   0
      integer (kind=kint) :: i_plane_size_ctl =      0
      integer (kind=kint) :: i_unit_len_plane_ctl =  0
      integer (kind=kint) :: i_horizontal_grid_ctl = 0
!
      private :: hd_plane_def, i_plane_def
      private :: hd_nnod_plane_ctl, hd_ndomain_plane_ctl
      private :: hd_num_of_sleeve_ctl, hd_plane_size_ctl
      private :: hd_unit_len_plane_ctl, hd_horizontal_grid_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_plane_model_param_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_plane_def) .eq. 0) return
      if (i_plane_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plane_def, i_plane_def)
        if(i_plane_def .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_num_of_sleeve_ctl,                &
     &        i_num_of_sleeve_ctl, num_of_sleeve_ctl)
!
        call read_character_ctl_item(hd_horizontal_grid_ctl,            &
     &        i_horizontal_grid_ctl, horizontal_grid_ctl)
!
        call read_real3_ctl_item(hd_plane_size_ctl,                     &
     &        i_plane_size_ctl, plane_size_ctl)
!
        call read_integer3_ctl_item(hd_nnod_plane_ctl,                  &
     &        i_nnod_plane_ctl, nnod_plane_ctl)
!
        call read_integer3_ctl_item(hd_ndomain_plane_ctl,               &
     &        i_ndomain_plane_ctl, ndomain_plane_ctl)
!
        call read_character3_ctl_item(hd_unit_len_plane_ctl,            &
     &        i_unit_len_plane_ctl, unit_len_plane_ctl)
      end do
!
      end subroutine read_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_plane_model
