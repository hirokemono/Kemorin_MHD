!
!      module m_control_data_4_psf
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_4_psf(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_coefs_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_center_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine deallocate_psf_axis_ctl(psf)
!        type(psf_ctl), intent(inout) :: psf
!
!      subroutine read_control_data_4_psf(psf)
!        type(psf_ctl), intent(inout) :: psf
!      subroutine read_psf_control_data(psf)
!        type(psf_ctl), intent(inout) :: psf
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! example of control for Kemo's surface rendering
!
!  begin surface_rendering
!    psf_file_head    'psf'
!    psf_output_type   ucd
!  
!    begin surface_define
!      section_method    equation
!  
!      array coefs_ctl  10
!        coefs_ctl  x2     1.0
!        coefs_ctl  y2     1.0
!        coefs_ctl  z2     0.0
!        coefs_ctl  xy     0.0
!        coefs_ctl  yz     0.0
!        coefs_ctl  zx     0.0
!        coefs_ctl  x      0.0
!        coefs_ctl  y      0.0
!        coefs_ctl  z      0.0
!        coefs_ctl  const  1.0
!      end array coefs_ctl
!  
!      begin plot_area_ctl
!        array chosen_ele_grp_ctl 1
!          chosen_ele_grp_ctl   outer_core   end
!        end array chosen_ele_grp_ctl
!      end plot_area_ctl
!    end surface_define
!  
!    begin output_field_define
!      array  output_field   2
!        output_field    velocity         vector   end
!        output_field    magnetic_field   radial   end
!      end  array output_field
!    end output_field_define
!  end  surface_rendering
!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  
!      psf_output_type:
!           ucd, OpenDX
!
!    num_result_comp: number of fields
!    output_field: (Original name: color_comp and color_subcomp)
!         field and componenet name for output
!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!           vector, sym_tensor, asym_tensor
!           spherical_vector, cylindrical_vector
!    output_value: (Original name: specified_color)
!
!    section_method: (original: method)
!           sphere, ellipsoid, hyperboloid, paraboloid, equation
!           group
!    center_position: position of center (for sphere)
!        array center_position    3
!          center_position  x   0.0
!          center_position  y   0.0
!          center_position  z   0.0
!        end array center_position
!    radius:  radius of sphere
!    axial_length: length of axis
!          (for ellipsoid, hyperboloid, paraboloid)
!        array axial_length   3
!          axial_length  x   1.0
!          axial_length  y   0.5
!          axial_length  z   0.0
!        end array axial_length
!    coefficients:  coefficients for equation
!        array coefs_ctl  10
!          coefs_ctl  x2     1.0
!          coefs_ctl  y2     0.5
!          coefs_ctl  z2     0.0
!          coefs_ctl  xy     1.0
!          coefs_ctl  yz     0.5
!          coefs_ctl  zx     0.0
!          coefs_ctl  x      1.0
!          coefs_ctl  y      0.5
!          coefs_ctl  z      0.0
!          coefs_ctl  const  1.0
!        end array coefs_ctl
!    group_type:  (Original: defined_style)
!           node_group or surface_group
!    group_name:  name of group to plot
!
!   field type:
!     scalar, vector,     sym_tensor, asym_tensor
!     spherical_vector,   spherical_sym_tensor
!     cylindrical_vector, cylindrical_sym_tensor
!     norm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_4_psf
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      type psf_ctl
        character(len=kchara) :: psf_file_head_ctl
        character(len=kchara) :: psf_output_type_ctl
!
        character(len=kchara) :: section_method_ctl
!
!
        integer(kind = kint) :: num_const_psf_ctl = 0
        character(len=kchara), pointer :: coef_name_psf_ctl(:)
        real(kind = kreal), pointer :: const_psf_ctl(:)
!
        integer(kind = kint) :: num_center_psf_ctl = 0
        character(len=kchara), pointer :: center_name_psf_ctl(:)
        real(kind = kreal), pointer :: center_psf_ctl(:)
!
        integer(kind = kint) :: num_axis_psf_ctl = 0
        character(len=kchara), pointer :: axis_name_psf_ctl(:)
        real(kind = kreal), pointer :: axis_psf_ctl(:)
!
        real(kind = kreal) :: radius_psf_ctl
!
        character(len=kchara) :: psf_group_name_ctl
!
        integer(kind = kint) :: num_psf_output_ctl = 0
        character(len=kchara), pointer :: psf_out_field_ctl(:)
        character(len=kchara), pointer :: psf_out_comp_ctl(:)
!
        integer(kind = kint) :: num_psf_area_grp_ctl = 0
        character(len=kchara), pointer :: psf_area_ele_grp_ctl(:)
!
!     Top level
        integer (kind=kint) :: i_psf_ctl = 0
!     2nd level for surface_rendering
        integer (kind=kint) :: i_psf_file_head =  0
        integer (kind=kint) :: i_psf_out_type =   0
        integer (kind=kint) :: i_surface_define = 0
        integer (kind=kint) :: i_output_field =   0
!     3rd level for surface_define
        integer (kind=kint) :: i_section_method =  0
        integer (kind=kint) :: i_radius =          0
        integer (kind=kint) :: i_plot_area =       0
        integer (kind=kint) :: i_group_name =  0
!     4th level for center_position
        integer (kind=kint) :: i_center_ctl =  0
!     4th level for axial_length
        integer (kind=kint) :: i_axis_ctl =    0
!     4th level for coefficients
        integer (kind=kint) :: i_coefs_ctl =   0
!     4th level for plot_area
        integer (kind=kint) :: i_plot_grp =    0
!     3rd level for output_field_define
        integer (kind=kint) :: i_num_psf_result_comp = 0
!
      end type psf_ctl
!
!
!     Top level
      character(len=kchara) :: hd_psf_ctl = 'surface_rendering'
!
!     2nd level for surface_rendering
      character(len=kchara) :: hd_psf_file_head = 'psf_file_head'
      character(len=kchara) :: hd_psf_out_type =  'psf_output_type'
      character(len=kchara) :: hd_surface_define = 'surface_define'
      character(len=kchara) :: hd_output_field = 'output_field_define'
!
!     3rd level for surface_define
      character(len=kchara) :: hd_section_method =  'section_method'
      character(len=kchara) :: hd_radius =          'radius'
      character(len=kchara) :: hd_plot_area =       'plot_area_ctl'
      character(len=kchara) :: hd_group_name =      'group_name'
!
!     4th level for center_position
      character(len=kchara) :: hd_center_ctl = 'center_position'
!
!     4th level for axial_length
      character(len=kchara) :: hd_axis_ctl = 'axial_length'
!
!     4th level for coefficients
      character(len=kchara) :: hd_coefs_ctl = 'coefs_ctl'
!
!     4th level for plot_area
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
!
!     3rd level for output_field_define
      character(len=kchara) :: hd_n_psf_result_comp = 'output_field'
!
!
      private :: hd_psf_file_head
      private :: hd_psf_out_type, hd_surface_define, hd_output_field
      private :: hd_section_method
      private :: hd_radius, hd_plot_area
      private :: hd_group_name, hd_center_ctl, hd_axis_ctl
      private :: hd_coefs_ctl, hd_plot_grp, hd_n_psf_result_comp
!
      private :: allocate_cont_dat_4_psf, allocate_area_grp_4_psf
      private :: allocate_psf_coefs_ctl
      private :: read_psf_output_ctl
      private :: read_psf_plot_area_ctl, read_section_def_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_cont_dat_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      allocate( psf%psf_out_field_ctl(psf%num_psf_output_ctl) )
      allocate( psf%psf_out_comp_ctl(psf%num_psf_output_ctl) )
!
      end subroutine allocate_cont_dat_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_area_grp_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      allocate( psf%psf_area_ele_grp_ctl(psf%num_psf_area_grp_ctl) )
!
      end subroutine allocate_area_grp_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_psf_coefs_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      allocate( psf%coef_name_psf_ctl(psf%num_const_psf_ctl))
      allocate( psf%const_psf_ctl(psf%num_const_psf_ctl))
      if(psf%num_const_psf_ctl .gt. 0) psf%const_psf_ctl = 0.0d0
!
      end subroutine allocate_psf_coefs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_psf_center_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      allocate( psf%center_name_psf_ctl(psf%num_center_psf_ctl))
      allocate( psf%center_psf_ctl(psf%num_center_psf_ctl))
      if(psf%num_center_psf_ctl .gt. 0) psf%center_psf_ctl = 0.0d0
!
      end subroutine allocate_psf_center_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_psf_axis_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      allocate( psf%axis_name_psf_ctl(psf%num_axis_psf_ctl))
      allocate( psf%axis_psf_ctl(psf%num_axis_psf_ctl))
      if(psf%num_axis_psf_ctl .gt. 0) psf%axis_psf_ctl = 0.0d0
!
      end subroutine allocate_psf_axis_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if (psf%num_psf_output_ctl .gt. 0) then
        deallocate( psf%psf_out_field_ctl )
        deallocate( psf%psf_out_comp_ctl )
      end if
!
      if (psf%num_psf_area_grp_ctl .gt. 0) then
        deallocate( psf%psf_area_ele_grp_ctl )
      end if
!
      end subroutine deallocate_cont_dat_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_coefs_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      deallocate( psf%coef_name_psf_ctl)
      deallocate( psf%const_psf_ctl)
!
      end subroutine deallocate_psf_coefs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_center_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      deallocate( psf%center_name_psf_ctl)
      deallocate( psf%center_psf_ctl)
!
      end subroutine deallocate_psf_center_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_psf_axis_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      deallocate( psf%axis_name_psf_ctl)
      deallocate( psf%axis_psf_ctl)
!
      end subroutine deallocate_psf_axis_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_4_psf(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      call load_ctl_label_and_line
      call read_psf_control_data(psf)
!
      end subroutine read_control_data_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine read_psf_control_data(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_psf_ctl) .eq. 0) return
      if (psf%i_psf_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_psf_ctl, psf%i_psf_ctl)
        if(psf%i_psf_ctl .gt. 0) exit
!
        call  read_section_def_control(psf)
        call  read_psf_output_ctl(psf)
!
!
        call read_character_ctl_item(hd_psf_file_head,                  &
     &        psf%i_psf_file_head, psf%psf_file_head_ctl)
        call read_character_ctl_item(hd_psf_out_type,                   &
     &        psf%i_psf_out_type, psf%psf_output_type_ctl)
      end do
!
      end subroutine read_psf_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_section_def_control(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_surface_define) .eq. 0) return
      if (psf%i_surface_define.gt.0) return
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_surface_define,                   &
     &         psf%i_surface_define)
        if(psf%i_surface_define .gt. 0) exit
!
!
        call find_control_array_flag(hd_coefs_ctl,                      &
     &        psf%num_const_psf_ctl)
        if(psf%num_const_psf_ctl.gt.0 .and. psf%i_coefs_ctl.eq.0) then
          call allocate_psf_coefs_ctl(psf)
          call read_control_array_char_r_list(hd_coefs_ctl,             &
     &        psf%num_const_psf_ctl, psf%i_coefs_ctl,                   &
     &        psf%coef_name_psf_ctl, psf%const_psf_ctl )
        end if
!
        call find_control_array_flag(hd_center_ctl,                     &
     &        psf%num_center_psf_ctl)
        if(psf%num_center_psf_ctl.gt.0                                  &
     &      .and. psf%i_center_ctl.eq.0) then
          call allocate_psf_center_ctl(psf)
          call read_control_array_char_r_list(hd_center_ctl,            &
     &        psf%num_center_psf_ctl, psf%i_center_ctl,                 &
     &        psf%center_name_psf_ctl, psf%center_psf_ctl )
        end if
!
        call find_control_array_flag(hd_axis_ctl, psf%num_axis_psf_ctl)
        if(psf%num_axis_psf_ctl.gt.0 .and. psf%i_axis_ctl.eq.0) then
          call allocate_psf_axis_ctl(psf)
          call read_control_array_char_r_list(hd_axis_ctl,              &
     &        psf%num_axis_psf_ctl, psf%i_axis_ctl,                     &
     &        psf%axis_name_psf_ctl, psf%axis_psf_ctl )
        end if
!
        call read_psf_plot_area_ctl(psf)
!
!
        call read_real_ctl_item(hd_radius,                              &
     &        psf%i_radius, psf%radius_psf_ctl)
!
        call read_character_ctl_item(hd_section_method,                 &
     &        psf%i_section_method, psf%section_method_ctl)
        call read_character_ctl_item(hd_group_name,                     &
     &        psf%i_group_name, psf%psf_group_name_ctl)
      end do
!
      end subroutine read_section_def_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_psf_output_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_output_field) .eq. 0) return
      if (psf%i_output_field .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_output_field, psf%i_output_field)
        if(psf%i_output_field .gt. 0) exit
!
        call find_control_array_flag(hd_n_psf_result_comp,              &
     &        psf%num_psf_output_ctl)
        if(psf%num_psf_output_ctl.gt.0                                  &
     &      .and. psf%i_num_psf_result_comp.eq.0) then
          call allocate_cont_dat_4_psf(psf)
          call read_control_array_chara2_list(hd_n_psf_result_comp,     &
     &        psf%num_psf_output_ctl, psf%i_num_psf_result_comp,        &
     &        psf%psf_out_field_ctl, psf%psf_out_comp_ctl)
        end if
      end do
!
      end subroutine read_psf_output_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_psf_plot_area_ctl(psf)
!
      type(psf_ctl), intent(inout) :: psf
!
!
      if(right_begin_flag(hd_plot_area) .eq. 0) return
      if (psf%i_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plot_area, psf%i_plot_area)
        if(psf%i_plot_area .gt. 0) exit
!
        call find_control_array_flag(hd_plot_grp,                       &
     &        psf%num_psf_area_grp_ctl)
        if(psf%num_psf_area_grp_ctl.gt.0                                &
     &      .and. psf%i_plot_grp.eq.0) then
          call allocate_area_grp_4_psf(psf)
          call read_control_array_chara_list(hd_plot_grp,               &
     &        psf%num_psf_area_grp_ctl, psf%i_plot_grp,                 &
     &        psf%psf_area_ele_grp_ctl )
        end if
      end do
!
      end subroutine read_psf_plot_area_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_4_psf
