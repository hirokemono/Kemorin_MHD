!
!      module m_ctl_data_2nd_plane
!
!        programmed by H.Matsui on June, 2007
!
!      subroutine read_2nd_plane_model_param_ctl
!
      module m_ctl_data_2nd_plane
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
      type(read_int3_item), save :: nnod_plane2_ctl
      type(read_int3_item), save :: ndomain_plane2_ctl
      type(read_integer_item), save :: num_of_sleeve2_ctl
      type(read_real3_item), save :: plane_size2_ctl
!
      type(read_chara3_item), save :: unit_len_plane2_ctl
      type(read_character_item), save :: horizontal_grid2_ctl
!
!   label for entry
!
      character(len=kchara) :: hd_2nd_plane_def = 'new_plane_mesh_ctl'
      integer (kind=kint) :: i_2nd_plane_def = 0
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
      private :: i_2nd_plane_def
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
      subroutine read_2nd_plane_model_param_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_2nd_plane_def) .eq. 0) return
      if (i_2nd_plane_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_2nd_plane_def, i_2nd_plane_def)
        if(i_2nd_plane_def .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_horizontal_grid_ctl,                &
     &      horizontal_grid2_ctl)
!
        call read_integer_ctl_type(hd_num_of_sleeve_ctl,                &
     &      num_of_sleeve2_ctl)
!
!
        call read_real3_ctl_type(hd_plane_size_ctl, plane_size2_ctl)
!
        call read_integer3_ctl_type(hd_nnod_plane_ctl, nnod_plane2_ctl)
!
        call read_integer3_ctl_type(hd_ndomain_plane_ctl,               &
     &      ndomain_plane2_ctl)
!
        call read_character3_ctl_type(hd_unit_len_plane_ctl,            &
     &      unit_len_plane2_ctl)
      end do
!
      end subroutine read_2nd_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_2nd_plane
