!>@file   t_ctl_data_SGS_filter.f90
!!@brief  module t_ctl_data_SGS_filter
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for filtering controls
!!
!!@verbatim
!!      subroutine copy_control_4_SGS_filter(org_sphf_c, new_sphf_c)
!!        type(sph_filter_ctl_type), intent(in) :: org_sphf_c
!!        type(sph_filter_ctl_type), intent(inout) :: new_sphf_c
!!      subroutine read_3d_filtering_ctl                                &
!!     &         (id_control, hd_block, iflag, s3df_ctl, c_buf)
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine read_control_4_SGS_filter                            &
!!     &         (id_control, hd_block, iflag, sphf_ctl, c_buf)
!!        type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_3d_filtering_ctl(s3df_ctl)
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
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
!!@endverbatim
!!
!
      module t_ctl_data_SGS_filter
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
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
      subroutine copy_control_4_SGS_filter(org_sphf_c, new_sphf_c)
!
      use copy_control_elements
!
      type(sph_filter_ctl_type), intent(in) :: org_sphf_c
      type(sph_filter_ctl_type), intent(inout) :: new_sphf_c
!
!
      call copy_chara_ctl(org_sphf_c%sph_filter_type_ctl,               &
     &                    new_sphf_c%sph_filter_type_ctl)
      call copy_chara_ctl(org_sphf_c%radial_filter_type_ctl,            &
     &                     new_sphf_c%radial_filter_type_ctl)
!
      call copy_integer_ctl(org_sphf_c%maximum_moments_ctl,             &
     &                     new_sphf_c%maximum_moments_ctl)
!
      call copy_real_ctl(org_sphf_c%sphere_filter_width_ctl,            &
     &                     new_sphf_c%sphere_filter_width_ctl)
      call copy_real_ctl(org_sphf_c%radial_filter_width_ctl,            &
     &                     new_sphf_c%radial_filter_width_ctl)
!
      end subroutine copy_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_3d_filtering_ctl                                  &
     &         (id_control, hd_block, iflag, s3df_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_whole_filter_grp, s3df_ctl%whole_filter_grp_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_fluid_filter_grp, s3df_ctl%fluid_filter_grp_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_momentum_filter_ctl,         &
     &      s3df_ctl%momentum_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_heat_filter_ctl,             &
     &      s3df_ctl%heat_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_induction_filter_ctl,        &
     &      s3df_ctl%induction_filter_ctl)
        call read_chara_ctl_type(c_buf, hd_comp_filter_ctl,             &
     &      s3df_ctl%compostion_filter_ctl)
      end do
      iflag = 1
!
      end subroutine read_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filter                              &
     &         (id_control, hd_block, iflag, sphf_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iflag .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_sph_filter_type,             &
     &      sphf_ctl%sph_filter_type_ctl)
        call read_chara_ctl_type(c_buf, hd_radial_filter_type,          &
     &      sphf_ctl%radial_filter_type_ctl)
!
        call read_integer_ctl_type(c_buf, hd_max_mom,                   &
     &      sphf_ctl%maximum_moments_ctl)
        call read_integer_ctl_type(c_buf, hd_1st_reference,             &
     &      sphf_ctl%first_reference_ctl)
        call read_integer_ctl_type(c_buf, hd_2nd_reference,             &
     &      sphf_ctl%second_reference_ctl)
!
        call read_real_ctl_type(c_buf, hd_radial_filter_w,              &
     &      sphf_ctl%radial_filter_width_ctl)
        call read_real_ctl_type(c_buf, hd_sphere_filter_w,              &
     &      sphf_ctl%sphere_filter_width_ctl)
      end do
      iflag = 1
!
      end subroutine read_control_4_SGS_filter
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
