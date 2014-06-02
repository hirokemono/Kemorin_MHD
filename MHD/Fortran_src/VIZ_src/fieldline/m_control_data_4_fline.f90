!m_control_data_4_fline.f90
!      module m_control_data_4_fline
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_fline(fln)
!
!      subroutine read_control_data_fline(fln)
!      subroutine read_field_line_ctl(fln)
!      subroutine reset_fline_control_flags
!
!  ---------------------------------------------------------------------
!     example of control for Kemo's field line
!!
!  begin field_line
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
!  end field_line
!  ---------------------------------------------------------------------
!
      module m_control_data_4_fline
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_read_control_arrays
      use skip_comment_f
      use calypso_mpi
!
      implicit  none
!
!
!
      type fline_ctl
        character(len=kchara) :: fline_file_head_ctl
        character(len=kchara) :: fline_output_type_ctl
!
        character(len=kchara) :: fline_field_ctl(1)
        character(len=kchara) :: fline_comp_ctl(1) = 'vector'
        character(len=kchara) :: fline_color_field_ctl(1)
        character(len=kchara) :: fline_color_comp_ctl(1)
!
        integer(kind = kint) :: num_fline_area_grp_ctl = 0
        character(len=kchara), pointer :: fline_area_ele_grp_ctl(:)
!
!
        character(len=kchara) :: starting_type_ctl
        character(len=kchara) :: selection_type_ctl
        character(len=kchara) :: line_direction_ctl
!
        character(len=kchara) :: start_surf_grp_ctl
        integer(kind = kint) :: num_fieldline_ctl = 3
        integer(kind = kint) :: max_line_stepping_ctl = 1000
!
!!      Structure for light positions
!!@n      light_position_ctl%vec1:  X-component of seed points
!!@n      light_position_ctl%vec2:  Y-component of seed points
!!@n      light_position_ctl%vec3:  Z-component of seed points
        type(ctl_array_r3) :: seed_point_ctl
!
        integer (kind=kint) :: num_start_gl_surf_ctl = 0
        integer(kind = kint), pointer :: id_start_gl_surf_ctl(:,:)
!
!     Top level
!
        integer (kind=kint) :: i_vr_fline_ctl = 0
!
!     2nd level for surface_rendering
!
        integer (kind=kint) :: i_fline_grp = 0
!
        integer (kind=kint) :: i_field_line_field = 0
        integer (kind=kint) :: i_coloring_field =   0
        integer (kind=kint) :: i_coloring_comp =    0
!
        integer (kind=kint) :: i_fline_file_head =   0
        integer (kind=kint) :: i_fline_output_type = 0
        integer (kind=kint) :: i_line_direction =    0
!
        integer (kind=kint) :: i_starting_type =  0
        integer (kind=kint) :: i_selection_type = 0
        integer (kind=kint) :: i_start_surf_grp = 0
        integer (kind=kint) :: i_num_fieldline =     0
        integer (kind=kint) :: i_max_line_stepping = 0
!
        integer (kind=kint) :: i_num_start_gl_surf = 0
      end type fline_ctl
!
!
!     Top level
!
      character(len=kchara) :: hd_vr_fline_ctl = 'field_line'
!
!     2nd level for surface_rendering
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
      character(len=kchara) :: hd_num_start_gl_surf                     &
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
      private :: hd_num_start_gl_surf
      private :: hd_vr_fline_ctl
!
      private :: allocate_area_grp_vr_psf
      private :: allocate_start_gl_surf_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_area_grp_vr_psf(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      allocate(fln%fline_area_ele_grp_ctl(fln%num_fline_area_grp_ctl) )
!
      end subroutine allocate_area_grp_vr_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_start_gl_surf_ctl(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      allocate(fln%id_start_gl_surf_ctl(fln%num_start_gl_surf_ctl,2))
!
      if(fln%num_start_gl_surf_ctl .gt. 0) fln%id_start_gl_surf_ctl=0
!
      end subroutine allocate_start_gl_surf_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_fline(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
      if(fln%num_start_gl_surf_ctl .gt. 0) then
        deallocate( fln%id_start_gl_surf_ctl )
      end if
      if(fln%seed_point_ctl%num .gt. 0) then
        call dealloc_control_array_r3(fln%seed_point_ctl)
      end if
!
      deallocate(fln%fline_area_ele_grp_ctl)
!
      end subroutine deallocate_cont_dat_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_fline(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      call load_ctl_label_and_line
      call read_field_line_ctl(fln)
!
      end subroutine read_control_data_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_field_line_ctl(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
!      if(right_begin_flag(hd_vr_fline_ctl) .eq. 0) return
      if (fln%i_vr_fline_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_vr_fline_ctl, fln%i_vr_fline_ctl)
        if(fln%i_vr_fline_ctl .gt. 0) exit
!
!
        call find_control_array_flag(hd_fline_grp,                      &
     &      fln%num_fline_area_grp_ctl)
        if(fln%num_fline_area_grp_ctl.gt.0                              &
     &      .and. fln%i_fline_grp.eq.0) then
          call allocate_area_grp_vr_psf(fln)
          call read_control_array_chara_list(hd_fline_grp,              &
     &        fln%num_fline_area_grp_ctl, fln%i_fline_grp,              &
     &        fln%fline_area_ele_grp_ctl )
        end if
!
        call read_control_array_r3                                      &
     &     (hd_xx_start_point, fln%seed_point_ctl)
!
        call find_control_array_flag(hd_num_start_gl_surf,              &
     &      fln%num_start_gl_surf_ctl)
        if(fln%num_start_gl_surf_ctl.gt.0                               &
     &       .and. fln%i_num_start_gl_surf.eq.0) then
          call allocate_start_gl_surf_ctl(fln)
          call read_control_array_int2_list(hd_num_start_gl_surf,       &
     &        fln%num_start_gl_surf_ctl, fln%i_num_start_gl_surf,       &
     &        fln%id_start_gl_surf_ctl(1:fln%num_start_gl_surf_ctl,1),  &
     &        fln%id_start_gl_surf_ctl(1:fln%num_start_gl_surf_ctl,2) )
        end if
!
!
        call read_character_ctl_item(hd_fline_file_head,                &
     &          fln%i_fline_file_head,  fln%fline_file_head_ctl)
        call read_character_ctl_item(hd_fline_output_type,              &
     &          fln%i_fline_output_type,  fln%fline_output_type_ctl)
!
        call read_character_ctl_item(hd_field_line_field,               &
     &          fln%i_field_line_field,  fln%fline_field_ctl(1) )
        call read_character_ctl_item(hd_coloring_field,                 &
     &          fln%i_coloring_field,  fln%fline_color_field_ctl(1) )
        call read_character_ctl_item(hd_coloring_comp,                  &
     &          fln%i_coloring_comp,  fln%fline_color_comp_ctl(1) )
        call read_character_ctl_item(hd_starting_type,                  &
     &          fln%i_starting_type, fln%starting_type_ctl )
        call read_character_ctl_item(hd_start_surf_grp,                 &
     &          fln%i_start_surf_grp, fln%start_surf_grp_ctl )
        call read_character_ctl_item(hd_selection_type,                 &
     &          fln%i_selection_type, fln%selection_type_ctl )
        call read_character_ctl_item(hd_line_direction,                 &
     &          fln%i_line_direction,  fln%line_direction_ctl )
!
!
        call read_integer_ctl_item(hd_num_fieldline,                    &
     &          fln%i_num_fieldline, fln%num_fieldline_ctl )
        call read_integer_ctl_item(hd_max_line_stepping,                &
     &          fln%i_max_line_stepping, fln%max_line_stepping_ctl )
      end do
!
      end subroutine read_field_line_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_control_flags(fln)
!
      type(fline_ctl), intent(inout) :: fln
!
!
      fln%num_fline_area_grp_ctl = 0
      fln%seed_point_ctl%num =    0
      fln%num_start_gl_surf_ctl =  0
!
      fln%i_fline_file_head = 0
      fln%i_fline_file_head = 0
!
      fln%i_num_fieldline  =     0
      fln%i_max_line_stepping  = 0
      fln%i_starting_type =  0
      fln%i_selection_type = 0
      fln%i_start_surf_grp = 0
!
      fln%i_vr_fline_ctl = 0
      fln%i_fline_grp =    0
!
      fln%seed_point_ctl%icou = 0
      fln%i_num_start_gl_surf = 0
!
      fln%i_coloring_field =   0
      fln%i_coloring_comp =    0
      fln%i_field_line_field = 0
      fln%i_line_direction = 0
!
      end subroutine reset_fline_control_flags
!
!  ---------------------------------------------------------------------
!
      end module m_control_data_4_fline
