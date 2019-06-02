!t_control_data_4_fline.f90
!      module t_control_data_4_fline
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine deallocate_cont_dat_fline(fln)
!
!!      subroutine read_field_line_ctl(hd_block, fln)
!!      subroutine bcast_field_line_ctl(fln)
!!      subroutine reset_fline_control_flags(fln)
!
!  ---------------------------------------------------------------------
!     example of control for Kemo's field line
!!
!  begin fieldline
!    fline_file_head    'psf'
!    fline_output_type   ucd
!!
!    array chosen_ele_grp_ctl 1
!      chosen_ele_grp_ctl   outer_core   end
!    end array chosen_ele_grp_ctl
!!
!!  starting_type:    position_list, surface_list,  or surface_group
!    line_direction_ctl        forward
!    max_line_stepping_ctl     1000
!    starting_type_ctl     position_list
!!
!    start_surf_grp_ctl      icb_surf
!    num_fieldline_ctl       10
!!
!!    selection_type_ctl:    amplitude, area_size
!    selection_type_ctl     amplitude
!!
!    array starting_point_ctl  10
!      starting_point_ctl  0.0  0.0  0.0
!    end array starting_point_ctl
!!
!    array starting_gl_surface_id  10
!      starting_gl_surface_id  12  3
!    end array
!!
!!     field type:
!!     scalar, vector, sym_tensor, asym_tensor
!!       spherical_vector,   spherical_sym_tensor
!!       cylindrical_vector, cylindrical_sym_tensor
!!       norm, 
!!
!    field_line_field_ctl      magnetic_field   end
!    coloring_field_ctl        magnetic_field   end
!    coloring_comp_ctl        radial   end
!!
!  end fieldline
!  ---------------------------------------------------------------------
!
      module t_control_data_4_fline
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_array_character
      use t_control_array_integer2
      use t_control_array_real3
      use t_control_elements
      use skip_comment_f
      use calypso_mpi
!
      implicit  none
!
!
      integer(kind = kint), parameter :: fline_ctl_file_code = 11
!
      type fline_ctl
        type(read_character_item) :: fline_file_head_ctl
        type(read_character_item) :: fline_output_type_ctl
!
        type(read_character_item) :: fline_field_ctl
        type(read_character_item) :: fline_color_field_ctl
        type(read_character_item) :: fline_color_comp_ctl
!
!>      Structure for element group to draw field line
!!@n      fline_area_grp_ctl%c_tbl:  element group to draw field line
        type(ctl_array_chara) :: fline_area_grp_ctl
!
!
        type(read_character_item) :: starting_type_ctl
        type(read_character_item) :: selection_type_ctl
        type(read_character_item) :: line_direction_ctl
!
        type(read_character_item) :: start_surf_grp_ctl
!
        type(read_integer_item) :: num_fieldline_ctl
        type(read_integer_item) :: max_line_stepping_ctl
!
!>      Structure for seed points
!!@n      seed_point_ctl%vec1:  X-component of seed points
!!@n      seed_point_ctl%vec2:  Y-component of seed points
!!@n      seed_point_ctl%vec3:  Z-component of seed points
        type(ctl_array_r3) :: seed_point_ctl
!
!>      Structure for seed points on center of the surfaces
!!@n      seed_surface_ctl%int1:  element ID for seed points
!!@n      seed_surface_ctl%int2:  Surface ID for seed points
        type(ctl_array_i2) :: seed_surface_ctl
!
!     Top level
!
        integer (kind=kint) :: i_vr_fline_ctl = 0
      end type fline_ctl
!
!     2nd level for field line
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
      subroutine deallocate_cont_dat_fline(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
      call dealloc_control_array_i2(fln%seed_surface_ctl)
      call dealloc_control_array_r3(fln%seed_point_ctl)
      call dealloc_control_array_chara(fln%fline_area_grp_ctl)
!
      end subroutine deallocate_cont_dat_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_field_line_ctl(hd_block, fln)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(fline_ctl), intent(inout) :: fln
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (fln%i_vr_fline_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        fln%i_vr_fline_ctl = find_control_end_flag(hd_block)
        if(fln%i_vr_fline_ctl .gt. 0) exit
!
!
        call read_control_array_c1(ctl_file_code,                       &
     &      hd_fline_grp, fln%fline_area_grp_ctl, c_buf1)
!
        call read_control_array_r3(ctl_file_code,                       &
     &      hd_xx_start_point, fln%seed_point_ctl, c_buf1)
        call read_control_array_i2(ctl_file_code,                       &
     &      hd_start_global_surf, fln%seed_surface_ctl, c_buf1)
!
!
        call read_chara_ctl_type(hd_fline_file_head,                    &
     &      fln%fline_file_head_ctl)
        call read_chara_ctl_type(hd_fline_output_type,                  &
     &      fln%fline_output_type_ctl)
!
        call read_chara_ctl_type(hd_field_line_field,                   &
     &      fln%fline_field_ctl )
        call read_chara_ctl_type(hd_coloring_field,                     &
     &      fln%fline_color_field_ctl )
        call read_chara_ctl_type(hd_coloring_comp,                      &
     &      fln%fline_color_comp_ctl )
        call read_chara_ctl_type(hd_starting_type,                      &
     &      fln%starting_type_ctl )
        call read_chara_ctl_type(hd_start_surf_grp,                     &
     &      fln%start_surf_grp_ctl )
        call read_chara_ctl_type(hd_selection_type,                     &
     &      fln%selection_type_ctl )
        call read_chara_ctl_type(hd_line_direction,                     &
     &      fln%line_direction_ctl )
!
        call read_integer_ctl_type(hd_num_fieldline,                    &
     &      fln%num_fieldline_ctl )
        call read_integer_ctl_type(hd_max_line_stepping,                &
     &      fln%max_line_stepping_ctl)
      end do
!
      end subroutine read_field_line_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_field_line_ctl(fln)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(fline_ctl), intent(inout) :: fln
!
!
      call MPI_BCAST(fln%i_vr_fline_ctl,  1,                            &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_c1(fln%fline_area_grp_ctl)
!
      call bcast_ctl_array_r3(fln%seed_point_ctl)
      call bcast_ctl_array_i2(fln%seed_surface_ctl)
!
!
      call bcast_ctl_type_c1(fln%fline_file_head_ctl)
      call bcast_ctl_type_c1(fln%fline_output_type_ctl)
!
      call bcast_ctl_type_c1(fln%fline_field_ctl )
      call bcast_ctl_type_c1(fln%fline_color_field_ctl )
      call bcast_ctl_type_c1(fln%fline_color_comp_ctl )
      call bcast_ctl_type_c1(fln%starting_type_ctl )
      call bcast_ctl_type_c1(fln%start_surf_grp_ctl )
      call bcast_ctl_type_c1(fln%selection_type_ctl )
      call bcast_ctl_type_c1(fln%line_direction_ctl )
!
      call bcast_ctl_type_i1(fln%num_fieldline_ctl)
      call bcast_ctl_type_i1(fln%max_line_stepping_ctl)
!
      end subroutine bcast_field_line_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_control_flags(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      fln%fline_area_grp_ctl%num = 0
      fln%seed_point_ctl%num =     0
      fln%seed_surface_ctl%num =   0
!
      fln%fline_file_head_ctl%iflag = 0
      fln%fline_output_type_ctl%iflag = 0
!
      fln%num_fieldline_ctl%iflag  =     0
      fln%max_line_stepping_ctl%iflag  = 0
      fln%starting_type_ctl%iflag =  0
      fln%selection_type_ctl%iflag = 0
      fln%start_surf_grp_ctl%iflag = 0
!
      fln%i_vr_fline_ctl = 0
!
      fln%fline_area_grp_ctl%icou = 0
      fln%seed_point_ctl%icou =     0
      fln%seed_surface_ctl%icou =   0
!
      fln%fline_color_field_ctl%iflag =   0
      fln%fline_color_comp_ctl%iflag =    0
      fln%fline_field_ctl%iflag = 0
      fln%line_direction_ctl%iflag = 0
!
      end subroutine reset_fline_control_flags
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_fline
