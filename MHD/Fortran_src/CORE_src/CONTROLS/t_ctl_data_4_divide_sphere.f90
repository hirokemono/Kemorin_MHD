!>@file   t_ctl_data_4_divide_sphere.f90
!!@brief  module t_ctl_data_4_divide_sphere
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine dealloc_ndomain_rtp_ctl(sdctl)
!!
!!      subroutine read_control_shell_domain(hd_block, iflag, sdctl)
!!        type(sphere_domain_control), intent(inout) :: sdctl
!!      subroutine write_control_shell_domain                           &
!!     &         (id_file, hd_block, sdctl, level)
!!
!!  ---------------------------------------------------------------------
!!    example of control data
!!
!!  begin num_domain_ctl
!!    inner_decomp_direction        radial or horizontal
!!    num_radial_domain_ctl         2
!!    num_horizontal_domain_ctl     2
!!
!!    array  num_domain_sph_grid   2
!!      num_domain_sph_grid    radial       2   end
!!      num_domain_sph_grid   meridional    3   end
!!    end array num_domain_sph_grid
!!
!!    array num_domain_legendre   2
!!      num_domain_legendre   radial        2   end
!!      num_domain_legendre   zonal         3   end
!!    end array num_domain_legendre
!!
!!    array num_domain_spectr     1
!!      num_domain_spectr     modes         6   end
!!    end array num_domain_spectr
!!  end num_domain_ctl
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_divide_sphere
!
      use m_precision
      use t_control_elements
      use t_read_control_arrays
      use t_control_array_charaint
!
      implicit  none
!
!
!
!>      control data structure for spherical shell parallelization
      type sphere_domain_control
!>        Direction of inner decomposition
        type(read_character_item) :: inner_decomp_ctl
!
!>        Number of subdomains in raidal direction for reduced definition
        type(read_integer_item) :: num_radial_domain_ctl
!
!>        Number of subdomains in horizontal directions
!!@n      for reduced definition
        type(read_integer_item) :: num_horiz_domain_ctl
!
!
!>        Structure for domain decompostion for spherical grid
!!@n        ndomain_sph_grid_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_sph_grid_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_sph_grid_ctl
!>        Structure for domain decompostion for Legendre transform
!!@n        ndomain_legendre_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_legendre_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_legendre_ctl
!>        Structure for domain decompostion for spherical harmonics
!!@n        ndomain_spectr_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_spectr_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_spectr_ctl
      end type sphere_domain_control
!
!   labels for subdomain define for spherical shell
!
      character(len=kchara), parameter                                  &
     &      :: hd_inner_decomp =       'inner_decomp_direction'
      character(len=kchara), parameter                                  &
     &      :: hd_num_radial_domain =  'num_radial_domain_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_num_horiz_domain =  'num_horizontal_domain_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_ndomain_rtp =  'num_domain_sph_grid'
      character(len=kchara), parameter                                  &
     &       :: hd_ndomain_rtm = 'num_domain_legendre'
      character(len=kchara), parameter                                  &
     &       :: hd_ndomain_rj  = 'num_domain_spectr'
!
      private :: hd_inner_decomp
      private :: hd_num_radial_domain, hd_num_horiz_domain
      private :: hd_ndomain_rtp, hd_ndomain_rtm, hd_ndomain_rj
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ndomain_rtp_ctl(sdctl)
!
      type(sphere_domain_control), intent(inout) :: sdctl
!
!
      call dealloc_control_array_c_i(sdctl%ndomain_sph_grid_ctl)
      call dealloc_control_array_c_i(sdctl%ndomain_legendre_ctl)
      call dealloc_control_array_c_i(sdctl%ndomain_spectr_ctl)
!
      end subroutine dealloc_ndomain_rtp_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_shell_domain(hd_block, iflag, sdctl)
!
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(sphere_domain_control), intent(inout) :: sdctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag =  find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
        call read_chara_ctl_type                                        &
     &     (hd_inner_decomp, sdctl%inner_decomp_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_num_radial_domain, sdctl%num_radial_domain_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_num_horiz_domain, sdctl%num_horiz_domain_ctl)
!
        call read_control_array_c_i(ctl_file_code,                      &
     &      hd_ndomain_rtp, sdctl%ndomain_sph_grid_ctl, c_buf1)
        call read_control_array_c_i(ctl_file_code,                      &
     &      hd_ndomain_rtm, sdctl%ndomain_legendre_ctl, c_buf1)
        call read_control_array_c_i(ctl_file_code,                      &
     &      hd_ndomain_rj, sdctl%ndomain_spectr_ctl, c_buf1)
      end do
!
      end subroutine read_control_shell_domain
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_shell_domain                             &
     &         (id_file, hd_block, sdctl, level)
!
      use t_control_array_charaint
      use m_read_control_elements
      use write_control_elements
      use write_control_arrays
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(sphere_domain_control), intent(in) :: sdctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      maxlen = max(maxlen, len_trim(hd_inner_decomp))
      maxlen = max(maxlen, len_trim(hd_num_radial_domain))
      maxlen = max(maxlen, len_trim(hd_num_horiz_domain))
!
      write(id_file,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_file, level, hd_block)
!
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_inner_decomp, sdctl%inner_decomp_ctl)
!
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_num_radial_domain, sdctl%num_radial_domain_ctl)
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_num_horiz_domain, sdctl%num_horiz_domain_ctl)
!
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rtp, sdctl%ndomain_sph_grid_ctl)
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rtm, sdctl%ndomain_legendre_ctl)
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rj, sdctl%ndomain_spectr_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_control_shell_domain
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_divide_sphere
