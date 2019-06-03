!t_ctl_data_SGS_filter.f90
!      module t_ctl_data_SGS_filter
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_3d_filtering_ctl(hd_block, iflag, s3df_ctl)
!!      subroutine bcast_3d_filtering_ctl(s3df_ctl)
!!      subroutine dealloc_3d_filtering_ctl(s3df_ctl)
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!      subroutine read_control_4_SGS_filter(hd_block, sphf_ctl)
!!      subroutine bcast_control_4_SGS_filter(sphf_ctl)
!!      subroutine reset_control_4_SGS_filter(sphf_ctl)
!!        type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!!
!!!!!!!!!  filter Model !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    3d_filtering_ctl
!!      whole_area:   filtering groups for whole domain
!!      fluid_area:   filtering groups for fluid region
!!
!!    filter_4_eqs_ctl:   filtering area for each equation
!!                      (whole_filtering of fluid filtering)
!!          momentum_filter_ctl:     momentum equation
!!          heat_filter_ctl:         heat equation
!!          induction_filter_ctl:    inducition equation
!!          composition_filter_ctl:  composition equation
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array sph_filter_ctl    2
!!        begin sph_filter_ctl
!!          sph_filter_type      'gaussian'
!!!          sph_filter_type      'cutoff'
!!          radial_filter_type   'gaussian'
!!          number_of_moments   5
!!          radial_filter_width     1.0
!!          sphere_filter_width     1.0
!!        end   sph_filter_ctl
!!        begin sph_filter_ctl
!!          sph_filter_type      'recursive'
!!          radial_filter_type   'recursive'
!!          number_of_moments   7
!!          radial_filter_width     2.0
!!          sphere_filter_width     2.0
!!          sphere_filter_width     2.0
!!          first_reference_filter_ID    1
!!          second_reference_filter_ID   1
!!        end   sph_filter_ctl
!!      end array sph_filter_ctl
!!
!!      begin 3d_filtering_ctl
!!        array whole_filtering_grp_ctl  2
!!          whole_filtering_grp_ctl  Both   end
!!          whole_filtering_grp_ctl  whole  end
!!        end array 3d_filtering_ctl
!!
!!        array fluid_filtering_grp_ctl  2
!!            fluid_filtering_grp_ctl  Both   end
!!            fluid_filtering_grp_ctl  fluid  end
!!        end array fluid_filtering_grp_ctl
!!
!!        momentum_filter_ctl      fluid_filtering
!!        heat_filter_ctl          fluid_filtering
!!        induction_filter_ctl     whole_filtering
!!        composition_filter_ctl   fluid_filtering
!!      end 3d_filtering_ctl
!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!
      module t_ctl_data_SGS_filter
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_control_elements
      use t_control_array_character
!
      implicit  none
!
!
!>        Structure for spherical shell filter
      type sph_filter_ctl_type
!>        Structure of sphere filter type
        type(read_character_item) :: sph_filter_type_ctl
!>        Structure of radial filter type
        type(read_character_item) :: radial_filter_type_ctl
!>        Structure for number of moments of filter
        type(read_integer_item) :: maximum_moments_ctl
!>        Structure for radial filter width
        type(read_real_item) :: sphere_filter_width_ctl
!>        Structure for horizontal filter width
        type(read_real_item) :: radial_filter_width_ctl
!
!>        Structure of first reference filter
        type(read_integer_item) :: first_reference_ctl
!>        Structure of second reference filter
        type(read_integer_item) :: second_reference_ctl
      end type sph_filter_ctl_type
!
!
      type SGS_3d_filter_control
!>        Structure for group list for filtering for whole area
!!@n        whole_filter_grp_ctl%c_tbl: element group name
        type(ctl_array_chara) :: whole_filter_grp_ctl
!>                Structure for field list for filtering in fluid
!!@n                fluid_filter_grp_ctl%c_tbl: element group name
        type(ctl_array_chara) :: fluid_filter_grp_ctl
!
        type(read_character_item) :: momentum_filter_ctl
        type(read_character_item) :: heat_filter_ctl
        type(read_character_item) :: induction_filter_ctl
        type(read_character_item) :: compostion_filter_ctl
      end type SGS_3d_filter_control
!
!
!    4th level for SGS model
!
      character(len=kchara), parameter                                  &
     &             :: hd_sph_filter_type =    'sph_filter_type'
      character(len=kchara), parameter                                  &
     &             :: hd_radial_filter_type = 'radial_filter_type'
      character(len=kchara), parameter                                  &
     &             :: hd_max_mom = 'number_of_moments'
      character(len=kchara), parameter                                  &
     &             :: hd_radial_filter_w = 'radial_filter_width'
      character(len=kchara), parameter                                  &
     &             :: hd_sphere_filter_w = 'sphere_filter_width'
      character(len=kchara), parameter                                  &
     &             :: hd_1st_reference = 'first_reference_filter_ID'
      character(len=kchara), parameter                                  &
     &             :: hd_2nd_reference = 'second_reference_filter_ID'
!
!    5th level for 3d filtering
!
      character(len=kchara) :: hd_whole_filter_grp                      &
     &                        = 'whole_filtering_grp_ctl'
      character(len=kchara) :: hd_fluid_filter_grp                      &
     &                        = 'fluid_filtering_grp_ctl'
!
      character(len=kchara) :: hd_momentum_filter_ctl                   &
     &                        = 'momentum_filter_ctl'
      character(len=kchara) :: hd_heat_filter_ctl                       &
     &                        =  'heat_filter_ctl'
      character(len=kchara) :: hd_induction_filter_ctl                  &
     &                        =  'induction_filter_ctl'
      character(len=kchara) :: hd_comp_filter_ctl                       &
     &                        =  'composition_filter_ctl'
!
      private :: hd_sph_filter_type, hd_radial_filter_type
      private :: hd_radial_filter_w, hd_sphere_filter_w, hd_max_mom
      private :: hd_whole_filter_grp, hd_fluid_filter_grp
      private :: hd_momentum_filter_ctl, hd_heat_filter_ctl
      private :: hd_induction_filter_ctl, hd_comp_filter_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_3d_filtering_ctl(hd_block, iflag, s3df_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      integer(kind = kint), intent(inout) :: iflag
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
!
        call read_control_array_c1(ctl_file_code,                       &
     &      hd_whole_filter_grp, s3df_ctl%whole_filter_grp_ctl, c_buf1)
        call read_control_array_c1(ctl_file_code,                       &
     &      hd_fluid_filter_grp, s3df_ctl%fluid_filter_grp_ctl, c_buf1)
!
        call read_chara_ctl_type(c_buf1, hd_momentum_filter_ctl,        &
     &      s3df_ctl%momentum_filter_ctl)
        call read_chara_ctl_type(c_buf1, hd_heat_filter_ctl,            &
     &      s3df_ctl%heat_filter_ctl)
        call read_chara_ctl_type(c_buf1, hd_induction_filter_ctl,       &
     &      s3df_ctl%induction_filter_ctl)
        call read_chara_ctl_type(c_buf1, hd_comp_filter_ctl,            &
     &      s3df_ctl%compostion_filter_ctl)
      end do
!
      end subroutine read_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filter(hd_block, sphf_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
        call read_chara_ctl_type(c_buf1, hd_sph_filter_type,            &
     &      sphf_ctl%sph_filter_type_ctl)
        call read_chara_ctl_type(c_buf1, hd_radial_filter_type,         &
     &      sphf_ctl%radial_filter_type_ctl)
!
        call read_integer_ctl_type(c_buf1, hd_max_mom,                  &
     &      sphf_ctl%maximum_moments_ctl)
        call read_integer_ctl_type(c_buf1, hd_1st_reference,            &
     &      sphf_ctl%first_reference_ctl)
        call read_integer_ctl_type(c_buf1, hd_2nd_reference,            &
     &      sphf_ctl%second_reference_ctl)
!
        call read_real_ctl_type(c_buf1, hd_radial_filter_w,             &
     &      sphf_ctl%radial_filter_width_ctl)
        call read_real_ctl_type(c_buf1, hd_sphere_filter_w,             &
     &      sphf_ctl%sphere_filter_width_ctl)
      end do
!
      end subroutine read_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_3d_filtering_ctl(s3df_ctl)
!
      use bcast_control_arrays
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!
!
      call bcast_ctl_array_c1(s3df_ctl%whole_filter_grp_ctl)
      call bcast_ctl_array_c1(s3df_ctl%fluid_filter_grp_ctl)
!
      call bcast_ctl_type_c1(s3df_ctl%momentum_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%heat_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%induction_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%compostion_filter_ctl)
!
      end subroutine bcast_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_4_SGS_filter(sphf_ctl)
!
      use bcast_control_arrays
!
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!
!
      call bcast_ctl_type_c1(sphf_ctl%sph_filter_type_ctl)
      call bcast_ctl_type_i1(sphf_ctl%maximum_moments_ctl)
!
      call bcast_ctl_type_r1(sphf_ctl%radial_filter_width_ctl)
      call bcast_ctl_type_r1(sphf_ctl%sphere_filter_width_ctl)
!
      end subroutine bcast_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_3d_filtering_ctl(s3df_ctl)
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!
!
      call dealloc_control_array_chara(s3df_ctl%whole_filter_grp_ctl)
      call dealloc_control_array_chara(s3df_ctl%fluid_filter_grp_ctl)
!
      s3df_ctl%momentum_filter_ctl%iflag =   0
      s3df_ctl%heat_filter_ctl%iflag =       0
      s3df_ctl%induction_filter_ctl%iflag =  0
      s3df_ctl%compostion_filter_ctl%iflag = 0
!
      end subroutine dealloc_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_control_4_SGS_filter(sphf_ctl)
!
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!
!
      sphf_ctl%sph_filter_type_ctl%iflag = 0
      sphf_ctl%maximum_moments_ctl%iflag = 0
!
      sphf_ctl%radial_filter_width_ctl%iflag = 0
      sphf_ctl%sphere_filter_width_ctl%iflag = 0
!
      end subroutine reset_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_filter
