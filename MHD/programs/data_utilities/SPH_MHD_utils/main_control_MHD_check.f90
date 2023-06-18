!>@file   main_control_MHD_check.f90
!!@brief  program kemorin_control_MHD_check
!!
!!@author H. Matsui
!!@date Programmed by by H. Matsui in July 2023
!
!>@brief  Main program to check control file for SPH_MHD
!!         with visualizers
!!         Input ontrol file: control_snapshot
!
      program kemorin_control_MHD_check
!
      use m_precision
!
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
!
      implicit none
!
!>      File name for control file
      character(len=kchara) :: MHD_ctl_name
      character(len=kchara), parameter                                  &
     &                    :: hd_mhd_ctl = 'MHD_control_Check'
!
      type(mhd_simulation_control) :: MHD_ctl1
      type(add_sgs_sph_mhd_ctl) :: add_SSMHD_ctl1
      integer(kind = kint) :: level1 = 0
!
!
      read(*,*) MHD_ctl_name
      call read_control_4_sph_SGS_MHD(MHD_ctl_name,                     &
     &                                MHD_ctl1, add_SSMHD_ctl1)
!
      call write_sph_mhd_control_data(6, hd_mhd_ctl,                    &
     &                                MHD_ctl1, add_SSMHD_ctl1, level1)
!
      stop '***** program finished *****'
!
      end program kemorin_control_MHD_check
