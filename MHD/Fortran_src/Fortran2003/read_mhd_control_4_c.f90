!
      module read_mhd_control_4_c
!
      use iso_c_binding
      use t_ctl_data_SGS_MHD
      use t_ctl_data_MHD
!
      implicit none
!
      type(sph_sgs_mhd_control) :: MHD_ctl
      type(DNS_mhd_simulation_control) :: DNS_MHD_ctl
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
      use calypso_mpi
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
!
      call calypso_MPI_init
!
      call read_control_4_sph_SGS_MHD(MHD_ctl_name, MHD_ctl)
!
      end subroutine c_read_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_write_control_sph_SGS_MHD()                          &
     &          bind(C, NAME = 'c_write_control_sph_SGS_MHD')
!
      use calypso_mpi
!
      character(len=kchara), parameter                                  &
     &                      :: MHD_ctl_name = 'control_MHD_dup'
!
      character(len=kchara) :: hd_block1 = "Aho"
      character(len=kchara) :: hd_block2 = "Baka"
      character(len=kchara) :: hd_block3 = "Manu"
!
      integer(kind = kint) :: level = 0
!
      open(id_ctl, file = MHD_ctl_name)
      call write_control_platforms                                      &
     &   (id_ctl, hd_block1, MHD_ctl%plt, level)
      call write_phys_data_control                                      &
     &   (id_ctl, hd_block2, MHD_ctl%model_ctl%fld_ctl, level)
      call write_control_time_step_data                                 &
     &   (id_ctl, hd_block3, MHD_ctl%smctl_ctl%tctl, level)
      close(id_ctl)
!
      call calypso_MPI_finalize
!
      end subroutine c_write_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_read_control_sph_MHD()                               &
     &          bind(C, NAME = 'c_read_control_sph_MHD')
!
      use calypso_mpi
      use t_ctl_data_SGS_MHD
      use t_ctl_data_sph_MHD_psf
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
!
      call calypso_MPI_init
!
      call read_control_4_sph_MHD_w_psf(MHD_ctl_name, DNS_MHD_ctl)
      call calypso_MPI_finalize
!
      end subroutine c_read_control_sph_MHD
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
