!>@file   t_ctl_data_SGS_filter.f90
!!@brief  module t_ctl_data_SGS_filter
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for filtering controls
!!
!!@verbatim
!!      subroutine read_control_4_SGS_filter                            &
!!     &         (id_control, hd_block, sphf_ctl, c_buf)
!!        type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_4_SGS_filter                           &
!!     &         (id_control, hd_block, sphf_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_filter_ctl_type), intent(in) :: sphf_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine reset_control_4_SGS_filter(sphf_ctl)
!!        type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!!
!!      subroutine copy_control_4_SGS_filter(org_sphf_c, new_sphf_c)
!!        type(sph_filter_ctl_type), intent(in) :: org_sphf_c
!!        type(sph_filter_ctl_type), intent(inout) :: new_sphf_c
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
!!        begin sph_filter_ctl
!!          sph_filter_type      'gaussian'
!!!          sph_filter_type      'cutoff'
!!          radial_filter_type   'gaussian'
!!          number_of_moments
!!          radial_filter_width     1.0
!!          sphere_filter_width     1.0
!!        end   sph_filter_ctl
!!        begin sph_filter_ctl
!!          sph_filter_type      'recursive'
!!          radial_filter_type   'recursive'
!!          number_of_moments
!!          radial_filter_width     2.0
!!          sphere_filter_width     2.0
!!          sphere_filter_width     2.0
!!          first_reference_filter_ID    1
!!          second_reference_filter_ID   1
!!        end   sph_filter_ctl
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
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
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
!
        integer(kind = kint) :: i_sph_filter_ctl = 0
      end type sph_filter_ctl_type
!
!
!    4th level for SGS model
!
      character(len=kchara), parameter, private                         &
     &             :: hd_sph_filter_type =    'sph_filter_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_radial_filter_type = 'radial_filter_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_max_mom = 'number_of_moments'
      character(len=kchara), parameter, private                         &
     &             :: hd_radial_filter_w = 'radial_filter_width'
      character(len=kchara), parameter, private                         &
     &             :: hd_sphere_filter_w = 'sphere_filter_width'
      character(len=kchara), parameter, private                         &
     &             :: hd_1st_reference = 'first_reference_filter_ID'
      character(len=kchara), parameter, private                         &
     &             :: hd_2nd_reference = 'second_reference_filter_ID'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filter                              &
     &         (id_control, hd_block, sphf_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sphf_ctl%i_sph_filter_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      sphf_ctl%i_sph_filter_ctl = 1
!
      end subroutine read_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!
      subroutine write_control_4_SGS_filter                             &
     &         (id_control, hd_block, sphf_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_filter_ctl_type), intent(in) :: sphf_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(sphf_ctl%i_sph_filter_ctl .le. 0) return
!
      maxlen = len_trim(hd_sph_filter_type)
      maxlen = max(maxlen, len_trim(hd_radial_filter_type))
      maxlen = max(maxlen, len_trim(hd_max_mom))
      maxlen = max(maxlen, len_trim(hd_radial_filter_w))
      maxlen = max(maxlen, len_trim(hd_sphere_filter_w))
      maxlen = max(maxlen, len_trim(hd_1st_reference))
      maxlen = max(maxlen, len_trim(hd_2nd_reference))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    sphf_ctl%sph_filter_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    sphf_ctl%radial_filter_type_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    sphf_ctl%maximum_moments_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_radial_filter_w, sphf_ctl%radial_filter_width_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_sphere_filter_w, sphf_ctl%sphere_filter_width_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    sphf_ctl%first_reference_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    sphf_ctl%second_reference_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_4_SGS_filter
!
!   --------------------------------------------------------------------
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
!   --------------------------------------------------------------------
!
      subroutine copy_control_4_SGS_filter(org_sphf_c, new_sphf_c)
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
!
      end module t_ctl_data_SGS_filter
