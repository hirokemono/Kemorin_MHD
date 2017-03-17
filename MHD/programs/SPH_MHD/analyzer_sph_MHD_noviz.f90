!>@file   analyzer_sph_MHD_noviz.f90
!!@brief  module analyzer_sph_MHD_noviz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!        without cross sectioning routines
!!
!!@verbatim
!!      subroutine initialize_sph_MHD_noviz
!!      subroutine evolution_sph_MHD_noviz
!!@endverbatim
!
      module analyzer_sph_MHD_noviz
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_MHD_step_parameter
      use m_mesh_data
      use m_sph_trans_arrays_MHD
      use m_MHD_step_parameter
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_MHD_noviz
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_SGS_control_parameter
      use m_spheric_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_cal_max_indices
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz(MHD_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_mesh'
      call input_control_SPH_mesh                                       &
     &   (MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1,     &
     &    pwr1, SGS_par1, trns_WK1%dynamic_SPH,                         &
     &    mesh1, group1, ele_mesh1)
      call end_eleps_time(4)
!
!        Initialize FEM mesh data for field data IO
      call start_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHD_step1, mesh1, group1, ele_mesh1,  &
     &    iphys, nod_fld1, range)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD(iphys, MHD_step1)
!
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_MHD_noviz
!
      use m_spheric_parameter
      use m_node_phys_data
      use output_viz_file_control
!
      integer(kind = kint) :: visval, iflag_finish
      integer(kind = kint) :: iflag
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time =       time_init
      i_step_MHD = i_step_init
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        time = time + time_d1%dt
        i_step_MHD = i_step_MHD + 1
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(i_step_MHD, iflag_finish, MHD_step1)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(4)
        iflag = lead_field_data_flag(i_step_MHD, MHD_step1,             &
     &                               SGS_par1%sgs_step)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (sph1%sph_params, sph1%sph_rtp, trns_WK1,                  &
     &        mesh1, iphys, nod_fld1)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(i_step_MHD, SGS_par1, mesh1,           &
     &      nod_fld1, MHD_step1, visval)
!
        call end_eleps_time(4)
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_step1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_noviz
