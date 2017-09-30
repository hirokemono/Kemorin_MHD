!>@file   analyzer_sph_MHD.f90
!!@brief  module analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd
!!      subroutine evolution_sph_mhd
!!@endverbatim
!
      module analyzer_sph_MHD
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_SPH_MHD_model_data
      use m_SPH_mesh_field_data
      use m_work_time
      use m_mesh_data
      use m_node_phys_data
      use m_jacobians_VIZ
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
!
      use SPH_analyzer_SGS_MHD
      use visualizer_all
      use init_sph_MHD_elapsed_label
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_SGS_MHD
      use m_mesh_data
      use m_node_phys_data
      use m_bc_data_list
      use input_control_sph_SGS_MHD
      use FEM_analyzer_sph_MHD_w_viz
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_elapsed_time(1)
      call start_elapsed_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(MHD_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamo'
      call input_control_SPH_dynamo(MHD_files1,                         &
     &    bc_sph_IO1, MHD_ctl1, SPH_MHD1%sph, SPH_MHD1%comms,           &
     &    SPH_MHD1%groups, SPH_MHD1%fld, nod_fld1, SPH_SGS1,            &
     &    MHD_step1, SPH_model1%MHD_prop, MHD_BC1,                      &
     &    SPH_WK1%trns_WK, SPH_WK1%monitor, femmesh1, ele_mesh1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!    IO elapsed end
!    precondition elaps start
!
      call start_elapsed_time(2)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz(MHD_files1, MHD_step1,                  &
     &    femmesh1%mesh, femmesh1%group, ele_mesh1,                     &
     &    iphys_nod1, nod_fld1, next_tbl_VIZ1, jacobians_VIZ1,          &
     &    range1, fem_ucd1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_SGS_MHD'
      call SPH_initialize_SGS_MHD                                       &
     &   (MHD_files1, bc_sph_IO1, iphys_nod1, MHD_step1, SPH_model1,    &
     &    sph_MHD_bc1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(femmesh1, ele_mesh1, nod_fld1)
!
      call calypso_MPI_barrier
!
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd
!
      use FEM_analyzer_sph_MHD
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
!
      integer(kind = kint) :: visval, iflag_finish
      integer(kind = kint) :: iflag
!
!     ---------------------
!
      call start_elapsed_time(3)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHD_step1%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_MHD'
        call SPH_analyze_SGS_MHD(MHD_step1%time_d%i_time_step,          &
     &      MHD_files1, SPH_model1, sph_MHD_bc1,                        &
     &      iflag_finish, MHD_step1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        call start_elapsed_time(4)
        iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,      &
     &                               MHD_step1)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SPH_SGS1%SGS_par, SPH_MHD1%sph,                           &
     &        SPH_WK1%trns_WK, femmesh1%mesh, iphys_nod1, nod_fld1)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      femmesh1%mesh, nod_fld1, MHD_step1, visval, fem_ucd1)
!
        call end_elapsed_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all', my_rank
          call start_elapsed_time(12)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        femmesh1, ele_mesh1, nod_fld1,                            &
     &        next_tbl_VIZ1%neib_ele, jacobians_VIZ1)
          call end_elapsed_time(12)
        end if
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, range1, fem_ucd1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SPH_MHD1%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD
