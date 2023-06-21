!
      module read_mhd_control_4_c
!
      use iso_c_binding
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_ctl_data_sph_MHD_w_psf
!
      implicit none
!
      type(mhd_simulation_control), save :: MHD_ctl_C
      type(add_sgs_sph_mhd_ctl), save, private :: add_SSMHD_ctl_C
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl_C
      integer(kind = kint), parameter :: id_ctl = 11
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine c_read_control_sph_SGS_MHD()                           &
     &          bind(C, NAME = 'c_read_control_sph_SGS_MHD')
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      call read_control_4_sph_SGS_MHD(MHD_ctl_name,                     &
     &    MHD_ctl_C, add_SSMHD_ctl_C, c_buf1)
      if(c_buf1%iend .gt. 0) stop 'Error in control file'
!
      len = len_trim(MHD_ctl_C%hd_mhd_ctl) + 1
      write(MHD_ctl_C%hd_mhd_ctl(len:len),'(a1)') char(0)
      call c_f_pointer(names_c, name_f, [len+1])
!
      end subroutine c_read_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_write_control_sph_SGS_MHD()                          &
     &          bind(C, NAME = 'c_write_control_sph_SGS_MHD')
!
      use calypso_mpi
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
!
      character(len=kchara), parameter                                  &
     &                      :: MHD_ctl_name = 'control_MHD_dup'
!
!
      call write_control_file_sph_SGS_MHD(MHD_ctl_name, MHD_ctl_C,      &
     &                                    add_SSMHD_ctl_C)
!
      end subroutine c_write_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_read_control_sph_MHD()                               &
     &          bind(C, NAME = 'c_read_control_sph_MHD')
!
      use bcast_control_sph_MHD
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
      type(buffer_for_control) :: c_buf1
!
      call read_control_4_sph_MHD_w_psf(MHD_ctl_name, MHD_ctl_C,        &
     &                                  add_SMHD_ctl_C, c_buf1)
!
      end subroutine c_read_control_sph_MHD
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
