!>@file   main_control_MHD_check.f90
!!@brief  program kemorin_control_SGS_MHD_check
!!
!!@author H. Matsui
!!@date Programmed by by H. Matsui in July 2023
!
!>@brief  Main program to check control file for SPH_MHD
!!         with visualizers
!!         Input ontrol file: control_snapshot
!
      program kemorin_control_SGS_MHD_check
!
      use m_precision
!
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_tracers
      use t_control_data_dynamo_vizs
      use write_control_elements
      use ctl_data_SGS_SPH_MHD_IO
!
      implicit none
!
!>      File name for control file
      character(len=kchara) :: MHD_ctl_name
!
      type(mhd_simulation_control) :: MHD_ctl3
!>        Structures for SGS controls
      type(SGS_model_control) :: sgs_ctl3
!>        Structures of visualization controls
      type(visualization_controls) :: viz_ctls3
!>        Structures of tracer
      type(tracers_control) :: tracer_ctls3
!>        Structures of zonal mean controls
      type(sph_dynamo_viz_controls) :: zm_ctls3
!
      type(buffer_for_control) :: c_buf1
!
!
      if(iargc_kemo() .le. 0) then
        write(*,*) 'check_control_mhd CONTROL_FILE_NAME'
        stop
      end if
      call getarg_k(1, MHD_ctl_name)
!
      c_buf1%level = 0
      call read_control_4_sph_SGS_MHD(MHD_ctl_name, MHD_ctl3, sgs_ctl3, &
     &    tracer_ctls3, viz_ctls3, zm_ctls3, c_buf1)
      if(c_buf1%iend .gt. 0) stop 'Error in control file'
!
!
      write(id_monitor,'(a)') '!  '
      write(id_monitor,'(a)') '!  Checked control data'
      write(id_monitor,'(a)') '!  '
      call write_sph_mhd_control_data(id_monitor, MHD_ctl3, sgs_ctl3,   &
     &    tracer_ctls3, viz_ctls3,  zm_ctls3, c_buf1%level)
!
      stop '***** program finished *****'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      end program kemorin_control_SGS_MHD_check
