!
!      module t_ctl_data_4_plane_model
!
!        programmed by H.Matsui on June, 2007
!
!!   --------------------------------------------------------------------
!!    Example of control block
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
!!    -------------------------------------------------------------------
!!
!!      subroutine read_plane_model_param_ctl(cube_c)
!!        type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
      module t_ctl_data_4_plane_model
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!>      Structure for cube domain
      type ctl_data_4_plane_model
        type(read_int3_item) :: nnod_plane_ctl
        type(read_int3_item) :: ndomain_plane_ctl
        type(read_integer_item) :: num_of_sleeve_ctl
        type(read_real3_item) :: plane_size_ctl
!
        type(read_chara3_item) :: unit_len_plane_ctl
        type(read_character_item) :: horizontal_grid_ctl
!
        integer(kind=kint) :: i_plane_def = 0
      end type ctl_data_4_plane_model
!
!   label for entry
!
      character(len=kchara), parameter :: hd_plane_def = 'plane_mesh_ctl'
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
      private :: hd_plane_def
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
      subroutine read_plane_model_param_ctl(cube_c)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
!
      if(right_begin_flag(hd_plane_def) .eq. 0) return
      if(cube_c%i_plane_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        cube_c%i_plane_def = find_control_end_flag(hd_plane_def)
        if(cube_c%i_plane_def .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_num_of_sleeve_ctl,                &
     &      cube_c%num_of_sleeve_ctl)
!
        call read_chara_ctl_type(hd_horizontal_grid_ctl,                &
     &      cube_c%horizontal_grid_ctl)
!
        call read_real3_ctl_type(hd_plane_size_ctl,                     &
     &     cube_c%plane_size_ctl)
!
        call read_integer3_ctl_type(hd_nnod_plane_ctl,                  &
     &      cube_c%nnod_plane_ctl)
!
        call read_integer3_ctl_type(hd_ndomain_plane_ctl,               &
     &      cube_c%ndomain_plane_ctl)
!
        call read_character3_ctl_type(hd_unit_len_plane_ctl,            &
     &      cube_c%unit_len_plane_ctl)
      end do
!
      end subroutine read_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_plane_model
