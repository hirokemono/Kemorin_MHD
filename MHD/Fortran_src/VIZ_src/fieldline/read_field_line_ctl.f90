!>@file   read_field_line_ctl.f90
!!@brief  module read_field_line_ctl
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each field line
!!
!!@verbatim
!!      subroutine s_read_field_line_ctl(id_control, hd_block,          &
!!     &                                 fln, c_buf)
!!        type(fline_ctl), intent(inout) :: fln
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      integer(kind = kint) function num_label_fline_ctl()
!!      subroutine set_label_fline_ctl(names)
!!  ---------------------------------------------------------------------
!!     example of control for Kemo's field line
!!
!!  begin fieldline
!!    fline_file_head    'fline'
!!    fline_output_type   ucd
!!
!!    array chosen_ele_grp_ctl
!!      chosen_ele_grp_ctl   outer_core   end
!!    end array chosen_ele_grp_ctl
!!
!!  starting_type:    position_list, surface_list,  or surface_group
!!    line_direction_ctl        forward
!!    max_line_stepping_ctl     1000
!!    starting_type_ctl     position_list
!!
!!    start_surf_grp_ctl      icb_surf
!!    num_fieldline_ctl       10
!!
!!    selection_type_ctl:    amplitude, area_size
!!    selection_type_ctl     amplitude
!!
!!    array starting_point_ctl  10
!!      starting_point_ctl  0.0  0.0  0.0
!!    end array starting_point_ctl
!!
!!    array starting_gl_surface_id  10
!!      starting_gl_surface_id  12  3
!!    end array
!!
!!     field type:
!!     scalar, vector, sym_tensor, asym_tensor
!!       spherical_vector,   spherical_sym_tensor
!!       cylindrical_vector, cylindrical_sym_tensor
!!       norm, 
!!
!!    field_line_field_ctl      magnetic_field   end
!!    coloring_field_ctl        magnetic_field   end
!!    coloring_comp_ctl        radial   end
!!
!!  end fieldline
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module read_field_line_ctl
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_fline
      use t_read_control_elements
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_integer2
      use t_control_array_real3
      use calypso_mpi
!
      implicit  none
!
      character(len=kchara) :: hd_fline_file_head = 'fline_file_head'
      character(len=kchara) :: hd_fline_output_type                     &
     &                             = 'fline_output_type'
!
      character(len=kchara) :: hd_fline_grp = 'chosen_ele_grp_ctl'
!
      character(len=kchara) :: hd_field_line_field                      &
     &                        =  'field_line_field_ctl'
      character(len=kchara) :: hd_coloring_field = 'coloring_field_ctl'
      character(len=kchara) :: hd_coloring_comp =  'coloring_comp_ctl'
!
      character(len=kchara) :: hd_starting_type =  'starting_type_ctl'
      character(len=kchara) :: hd_start_surf_grp = 'start_surf_grp_ctl'
      character(len=kchara) :: hd_xx_start_point = 'starting_point_ctl'
      character(len=kchara) :: hd_selection_type = 'selection_type_ctl'
      character(len=kchara) :: hd_start_global_surf                     &
     &                        = 'starting_gl_surface_id'
      character(len=kchara) :: hd_num_fieldline = 'num_fieldline_ctl'
      character(len=kchara) :: hd_max_line_stepping                     &
     &                        = 'max_line_stepping_ctl'
      character(len=kchara) :: hd_line_direction = 'line_direction_ctl'
!
      integer(kind = kint), parameter :: n_label_fline_ctl = 14
!
      private :: hd_fline_file_head, hd_fline_output_type
      private :: hd_fline_grp, hd_line_direction
      private :: hd_max_line_stepping, hd_num_fieldline
      private :: hd_starting_type, hd_start_surf_grp
      private :: hd_xx_start_point, hd_selection_type
      private :: hd_start_global_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_field_line_ctl(id_control, hd_block,            &
     &                                 fln, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fline_ctl), intent(inout) :: fln
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
!
      if (fln%i_vr_fline_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_c1(id_control,                          &
     &      hd_fline_grp, fln%fline_area_grp_ctl, c_buf)
!
        call read_control_array_r3(id_control,                          &
     &      hd_xx_start_point, fln%seed_point_ctl, c_buf)
        call read_control_array_i2(id_control,                          &
     &      hd_start_global_surf, fln%seed_surface_ctl, c_buf)
!
!
        call read_chara_ctl_type(c_buf, hd_fline_file_head,             &
     &      fln%fline_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_fline_output_type,           &
     &      fln%fline_output_type_ctl)
!
        call read_chara_ctl_type(c_buf, hd_field_line_field,            &
     &      fln%fline_field_ctl )
        call read_chara_ctl_type(c_buf, hd_coloring_field,              &
     &      fln%fline_color_field_ctl )
        call read_chara_ctl_type(c_buf, hd_coloring_comp,               &
     &      fln%fline_color_comp_ctl )
        call read_chara_ctl_type(c_buf, hd_starting_type,               &
     &      fln%starting_type_ctl )
        call read_chara_ctl_type(c_buf, hd_start_surf_grp,              &
     &      fln%start_surf_grp_ctl )
        call read_chara_ctl_type(c_buf, hd_selection_type,              &
     &      fln%selection_type_ctl )
        call read_chara_ctl_type(c_buf, hd_line_direction,              &
     &      fln%line_direction_ctl )
!
        call read_integer_ctl_type(c_buf, hd_num_fieldline,             &
     &      fln%num_fieldline_ctl )
        call read_integer_ctl_type(c_buf, hd_max_line_stepping,         &
     &      fln%max_line_stepping_ctl)
      end do
      fln%i_vr_fline_ctl = 1 
!
      end subroutine s_read_field_line_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_fline_ctl()
      num_label_fline_ctl = n_label_fline_ctl
      return
      end function num_label_fline_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_label_fline_ctl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_fline_ctl)
!
!
      call set_control_labels(hd_fline_file_head,   names( 1))
      call set_control_labels(hd_fline_output_type, names( 2))
!
      call set_control_labels(hd_fline_grp,        names( 3))
      call set_control_labels(hd_field_line_field, names( 4))
      call set_control_labels(hd_coloring_field,   names( 5))
      call set_control_labels(hd_coloring_comp,    names( 6))
!
      call set_control_labels(hd_num_fieldline,     names( 7))
      call set_control_labels(hd_line_direction,    names( 8))
      call set_control_labels(hd_max_line_stepping, names( 9))
!
      call set_control_labels(hd_starting_type,     names(10))
      call set_control_labels(hd_selection_type,    names(11))
      call set_control_labels(hd_start_surf_grp,    names(12))
      call set_control_labels(hd_xx_start_point,    names(13))
      call set_control_labels(hd_start_global_surf, names(14))
!
      end subroutine set_label_fline_ctl
!
! ----------------------------------------------------------------------
!
      end module read_field_line_ctl
