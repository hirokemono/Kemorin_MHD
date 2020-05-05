!
!      module t_ctl_data_4_plane_model
!
!        programmed by H.Matsui on June, 2007
!
!!      subroutine read_plane_model_param_ctl                           &
!!     &         (id_control, hd_block, cube_c, c_buf)
!!      subroutine reset_plane_model_param_ctl(cube_c)
!!        type(ctl_data_4_plane_model), intent(inout) :: cube_c
!!
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
!
      module t_ctl_data_4_plane_model
!
      use m_precision
      use m_machine_parameter
      use t_control_elements
      use t_control_array_real3
      use t_control_array_integer3
      use t_control_array_character3
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
      subroutine read_plane_model_param_ctl                             &
     &         (id_control, hd_block, cube_c, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_4_plane_model), intent(inout) :: cube_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(cube_c%i_plane_def .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type(c_buf, hd_num_of_sleeve_ctl,         &
     &      cube_c%num_of_sleeve_ctl)
!
        call read_chara_ctl_type(c_buf, hd_horizontal_grid_ctl,         &
     &      cube_c%horizontal_grid_ctl)
!
        call read_real3_ctl_type(c_buf, hd_plane_size_ctl,              &
     &     cube_c%plane_size_ctl)
!
        call read_integer3_ctl_type(c_buf, hd_nnod_plane_ctl,           &
     &      cube_c%nnod_plane_ctl)
!
        call read_integer3_ctl_type(c_buf, hd_ndomain_plane_ctl,        &
     &      cube_c%ndomain_plane_ctl)
!
        call read_character3_ctl_type(c_buf, hd_unit_len_plane_ctl,     &
     &      cube_c%unit_len_plane_ctl)
      end do
      cube_c%i_plane_def = 1
!
      end subroutine read_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_plane_model_param_ctl(cube_c)
!
      type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
!
      cube_c%num_of_sleeve_ctl%iflag =   0
      cube_c%horizontal_grid_ctl%iflag = 0
      cube_c%plane_size_ctl%iflag =      0
      cube_c%nnod_plane_ctl%iflag =      0
      cube_c%ndomain_plane_ctl%iflag =   0
      cube_c%unit_len_plane_ctl%iflag =  0
!
      cube_c%i_plane_def = 0
!
      end subroutine reset_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_plane_model
