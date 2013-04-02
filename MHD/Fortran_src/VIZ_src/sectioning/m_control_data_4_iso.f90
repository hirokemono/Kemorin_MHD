!
!      module m_control_data_4_iso
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_4_iso(iso)
!        type(iso_ctl), intent(inout) :: iso
!
!      subroutine read_control_data_4_iso(iso)
!        type(iso_ctl), intent(inout) :: iso
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's surface rendering
!!
!!  begin isosurf_rendering
!!    iso_file_head    'psf'
!!    iso_output_type  ucd
!!
!!    begin isosurf_define
!!      isosurf_field        pressure
!!      isosurf_component      scalar
!!      isosurf_value            4000.0
!!
!!      begin plot_area_ctl
!!        array chosen_ele_grp_ctl   2
!!          chosen_ele_grp_ctl   inner_core   end
!!          chosen_ele_grp_ctl   outer_core   end
!!        end array chosen_ele_grp_ctl
!!      end plot_area_ctl
!!    end isosurf_define
!!
!!    begin isosurf_result_define
!!      result_type      constant
!!      result_value     0.7
!!    array output_field   2
!!      output_field    velocity         vector   end
!!      output_field    magnetic_field   radial   end
!!    end array output_field
!!    end isosurf_result_define
!!
!!  end isosurf_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    iso_output_type:
!!           ucd, OpenDX
!!
!!    result_type:  (Original name: display_method)
!!                   specified_fields
!!                   constant
!!    num_result_comp: number of fields
!!    output_field: (Original name: color_comp and color_subcomp)
!!         field and componenet name for output
!!           x, y, z, radial, elevation, azimuth, cylinder_r
!!           norm, vector, tensor, spherical_vector, cylindrical_vector
!!    result_value: (Original name: specified_color)
!!
!!    
!!
!!    isosurf_data: field for isosurface
!!    isosurf_comp: component for isosurface
!!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!!    isosurf_value:  value for isosurface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module m_control_data_4_iso
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
      type iso_ctl
        character(len=kchara) :: iso_file_head_ctl
        character(len=kchara) :: iso_output_type_ctl
!
        character(len=kchara) :: isosurf_data_ctl(1)
        character(len=kchara) :: isosurf_comp_ctl(1)
        real(kind=kreal) :: isosurf_value_ctl
        real(kind=kreal) :: result_value_iso_ctl
!
        character(len=kchara) :: iso_result_type_ctl
!
        integer(kind = kint) :: num_iso_result_ctl
        character(len=kchara), pointer :: iso_result_field_ctl(:)
        character(len=kchara), pointer :: iso_result_comp_ctl(:)
!
        integer(kind = kint) :: num_iso_area_grp_ctl = 0
        character(len=kchara), pointer :: iso_area_ele_grp_ctl(:)
!
!     Top level
        integer (kind=kint) :: i_iso_ctl = 0
!     2nd level for isosurf_rendering
        integer (kind=kint) :: i_iso_file_head = 0
        integer (kind=kint) :: i_iso_out_type =  0
        integer (kind=kint) :: i_iso_define =    0
        integer (kind=kint) :: i_iso_result =    0
!     3nd level for isosurf_define
        integer (kind=kint) :: i_iso_field =     0
        integer (kind=kint) :: i_iso_comp =      0
        integer (kind=kint) :: i_iso_value =     0
        integer (kind=kint) :: i_iso_plot_area = 0
!     4th level for plot_area_ctl
        integer (kind=kint) :: i_iso_plot_grp = 0
!     3rd level for isosurf_result_define
        integer (kind=kint) :: i_result_type =     0
        integer (kind=kint) :: i_num_iso_result_comp = 0
        integer (kind=kint) :: i_result_value =    0
!
      end type iso_ctl
!
!     Top level
      character(len=kchara) :: hd_iso_ctl = 'isosurf_rendering'
!
!     2nd level for isosurf_rendering
      character(len=kchara) :: hd_iso_file_head = 'iso_file_head'
      character(len=kchara) :: hd_iso_out_type = 'iso_output_type'
      character(len=kchara) :: hd_iso_define = 'isosurf_define'
      character(len=kchara) :: hd_iso_result = 'isosurf_result_define'
!
!     3nd level for isosurf_define
      character(len=kchara) :: hd_iso_field =     'isosurf_field'
      character(len=kchara) :: hd_iso_comp =      'isosurf_component'
      character(len=kchara) :: hd_iso_value =     'isosurf_value'
      character(len=kchara) :: hd_iso_plot_area = 'plot_area_ctl'
!
!     4th level for plot_area_ctl
      character(len=kchara) :: hd_iso_plot_grp  = 'chosen_ele_grp_ctl'
!
!     3rd level for isosurf_result_define
      character(len=kchara) :: hd_result_type =       'result_type'
      character(len=kchara) :: hd_n_iso_result_comp = 'output_field'
      character(len=kchara) :: hd_result_value =      'result_value'
!
      private :: hd_iso_plot_grp, hd_result_type, hd_iso_ctl
      private :: hd_result_value, hd_iso_plot_area, hd_iso_value
      private :: hd_iso_comp, hd_iso_field, hd_iso_result
      private :: hd_iso_define, hd_iso_out_type, hd_iso_file_head
!
      private :: allocate_cont_dat_4_iso, allocate_area_grp_4_iso
      private :: read_iso_define_data, read_iso_control_data
      private :: read_iso_result_control, read_iso_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_cont_dat_4_iso(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      allocate( iso%iso_result_field_ctl(iso%num_iso_result_ctl) )
      allocate( iso%iso_result_comp_ctl(iso%num_iso_result_ctl) )
!
      end subroutine allocate_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_area_grp_4_iso(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      allocate( iso%iso_area_ele_grp_ctl(iso%num_iso_area_grp_ctl) )
!
      end subroutine allocate_area_grp_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_4_iso(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
      if(iso%num_iso_result_ctl .gt. 0                                  &
     &   .and. iso%i_num_iso_result_comp .gt. 0) then
        deallocate( iso%iso_result_field_ctl )
        deallocate( iso%iso_result_comp_ctl )
        iso%num_iso_result_ctl = 0
      end if
!
      deallocate( iso%iso_area_ele_grp_ctl )
!
      end subroutine deallocate_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_4_iso(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      call load_ctl_label_and_line
      call read_iso_control_data(iso)
!
      end subroutine read_control_data_4_iso
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_iso_control_data(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_ctl) .eq. 0) return
      if (iso%i_iso_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_ctl, iso%i_iso_ctl)
        if(iso%i_iso_ctl .gt. 0) exit
!
        call read_iso_result_control(iso)
        call read_iso_define_data(iso)
!
!
        call read_character_ctl_item(hd_iso_file_head,                  &
     &          iso%i_iso_file_head, iso%iso_file_head_ctl)
        call read_character_ctl_item(hd_iso_out_type,                   &
     &        iso%i_iso_out_type, iso%iso_output_type_ctl)
      end do
!
      end subroutine read_iso_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_define_data(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_define) .eq. 0) return
      if (iso%i_iso_define.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_define, iso%i_iso_define)
        if(iso%i_iso_define .gt. 0) exit
!
        call  read_iso_plot_area_ctl(iso)
!
!
        call read_character_ctl_item(hd_iso_field,                      &
     &        iso%i_iso_field, iso%isosurf_data_ctl(1) )
        call read_character_ctl_item(hd_iso_comp,                       &
     &        iso%i_iso_comp, iso%isosurf_comp_ctl(1) )
!
        call read_real_ctl_item(hd_iso_value,                           &
     &        iso%i_iso_value, iso%isosurf_value_ctl)
      end do
!
      end subroutine read_iso_define_data
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_result_control(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_result) .eq. 0) return
      if (iso%i_iso_result.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_result, iso%i_iso_result)
        if(iso%i_iso_result .gt. 0) exit
!
        call find_control_array_flag(hd_n_iso_result_comp,             &
     &        iso%num_iso_result_ctl)
        if(iso%num_iso_result_ctl.gt.0                                  &
     &       .and. iso%i_num_iso_result_comp.eq.0) then
          call allocate_cont_dat_4_iso(iso)
          call read_control_array_chara2_list(hd_n_iso_result_comp,     &
     &        iso%num_iso_result_ctl, iso%i_num_iso_result_comp,        &
     &        iso%iso_result_field_ctl, iso%iso_result_comp_ctl)
        end if
!
        call read_character_ctl_item(hd_result_type,                    &
     &        iso%i_result_type, iso%iso_result_type_ctl)
!
        call read_real_ctl_item(hd_result_value,                        &
     &        iso%i_result_value, iso%result_value_iso_ctl)
      end do
!
      end subroutine read_iso_result_control
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_plot_area_ctl(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_plot_area) .eq. 0) return
      if (iso%i_iso_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_plot_area,                    &
     &      iso%i_iso_plot_area)
        if(iso%i_iso_plot_area .gt. 0) exit
!
        call find_control_array_flag(hd_iso_plot_grp,                   &
     &        iso%num_iso_area_grp_ctl)
        if(iso%num_iso_area_grp_ctl.gt.0                                &
     &       .and. iso%i_iso_plot_grp.eq.0) then
          call allocate_area_grp_4_iso(iso)
          call read_control_array_chara_list(hd_iso_plot_grp,           &
     &            iso%num_iso_area_grp_ctl, iso%i_iso_plot_grp,         &
     &            iso%iso_area_ele_grp_ctl)
        end if
      end do
!
      end subroutine read_iso_plot_area_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_4_iso
