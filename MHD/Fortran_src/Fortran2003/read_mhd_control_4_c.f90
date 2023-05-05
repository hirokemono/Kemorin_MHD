!
      module read_mhd_control_4_c
!
      use iso_c_binding
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_surfacings
      use t_control_data_dynamo_vizs
!
      implicit none
!
      type(mhd_simulation_control), save :: MHD_ctl_C
      type(add_sgs_sph_mhd_ctl), save, private :: add_SSMHD_ctl_C
      type(mhd_simulation_control), save :: DNS_MHD_ctl
      type(surfacing_controls), save :: surfacing_ctls_C
      type(sph_dynamo_viz_controls), save :: zm_ctls_C
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
      use ctl_file_SGS_MHD_IO
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
!
      call read_control_4_sph_SGS_MHD(MHD_ctl_name,                     &
     &    MHD_ctl_C, add_SSMHD_ctl_C)
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
      use bcast_control_sph_MHD
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
!
      call calypso_MPI_init
!
      call load_control_4_sph_MHD_w_psf(MHD_ctl_name, DNS_MHD_ctl,      &
     &                                  surfacing_ctls_C, zm_ctls_C)
      call calypso_MPI_finalize
!
      end subroutine c_read_control_sph_MHD
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
