!
!      module m_read_ctl_gen_sph_shell
!
!      Written by H. Matsui on July, 2007
!
!      subroutine read_control_4_gen_shell_grids
!      subroutine read_ctl_data_4_shell_in_MHD
!
      module m_read_ctl_gen_sph_shell
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
!
      implicit none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
      type(sphere_data_control), save :: spctl1
      type(sphere_domain_control), save :: sdctl1
!
      character (len = kchara) :: tmp_character
!
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      integer (kind=kint) :: i_sph_shell = 0
      integer (kind=kint) :: ifile_sph_shell = 0
!
      character(len=kchara), parameter                                  &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter                                  &
     &                     :: hd_domains_sph = 'num_domain_ctl'
      integer(kind = kint) :: i_shell_def =   0
      integer(kind = kint) :: i_domains_sph = 0
!
!  Deprecated
      character(len=kchara), parameter :: hd_shell_def = 'num_grid_sph'
!
      private :: control_file_code, control_file_name
      private :: hd_sph_shell
      private :: hd_sph_def, hd_shell_def, i_shell_def
      private :: hd_domains_sph, i_domains_sph
      private :: read_control_data_4_shell
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_gen_shell_grids
!
      use calypso_mpi
      use m_read_control_elements
      use m_ctl_data_4_platforms
      use skip_comment_f
      use bcast_4_platform_ctl
      use bcast_4_sphere_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open(ctl_file_code, file = control_file_name)
!
        call load_ctl_label_and_line
        call read_control_data_4_shell
!
        close(ctl_file_code)
      end if
!
      call bcast_ctl_data_4_platform(plt1)
      call bcast_ctl_4_shell_define(spctl1)
      call bcast_ctl_ndomain_4_shell(sdctl1)
!
      end subroutine read_control_4_gen_shell_grids
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_shell
!
      use m_read_control_elements
      use m_ctl_data_4_platforms
      use skip_comment_f
!
!
      if(right_begin_flag(hd_sph_shell) .eq. 0) return
      if(i_sph_shell .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_shell, i_sph_shell)
        if(i_sph_shell .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_control_shell_define(hd_sph_def, i_shell_def, spctl1)
        call read_control_shell_define                                  &
     &     (hd_shell_def, i_shell_def, spctl1)
!
        call read_control_shell_domain                                  &
     &     (hd_domains_sph, i_domains_sph, sdctl1)
      end do
!
      end subroutine read_control_data_4_shell
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_4_shell_in_MHD
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if((i_sph_shell+ifile_sph_shell) .gt. 0) return
      if(right_file_flag(hd_sph_shell) .gt. 0) then
        call read_file_name_from_ctl_line                               &
     &     (ifile_sph_shell, control_file_name)
      end if
!
      if(right_begin_flag(hd_sph_shell) .eq. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_shell, i_sph_shell)
        if(i_sph_shell .gt. 0) exit
!
        call read_control_shell_define(hd_sph_def, i_shell_def, spctl1)
        call read_control_shell_define                                  &
     &     (hd_shell_def, i_shell_def, spctl1)
!
        call read_control_shell_domain                                  &
     &     (hd_domains_sph, i_domains_sph, sdctl1)
      end do
!
      end subroutine read_ctl_data_4_shell_in_MHD
!
!   --------------------------------------------------------------------
!
      end module m_read_ctl_gen_sph_shell
