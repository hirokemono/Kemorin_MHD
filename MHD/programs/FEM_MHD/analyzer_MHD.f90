!
!      module analyzer_MHD
!
!      Written by H. Matsui and H. Okuda
!      modified by H. Matsui on June, 2005 
!
!      subroutine initialization_MHD
!      subroutine evolution_MHD
!
      module analyzer_MHD
!
      use m_precision
      use calypso_mpi
      use m_work_time
!
      use m_MHD_step_parameter
      use m_FEM_MHD_model_data
!
      use FEM_analyzer_MHD
      use t_viz_sections
!
      implicit none
!
      type(surfacing_modules), save :: MHD_viz_psfs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialization_MHD
!
      use input_control
!
!
      total_start = MPI_WTIME()
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      num_elapsed = 68
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                 '
      elapse_labels(2) = 'Initialization time        '
      elapse_labels(3) = 'Time evolution loop time   '
      elapse_labels(4) = 'Data IO time               '
      elapse_labels(5) = 'Linear solver time         '
      elapse_labels(6) = 'Communication for RHS      '
      elapse_labels(7) = 'Communication time         '
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
!
!     --------------------- 
!
      call start_elapsed_time(1)
!
      call start_elapsed_time(4)
      call input_control_4_FEM_MHD(MHD_files1, FEM_model1%FEM_prm,      &
     &    FEM_SGS1%SGS_par, MHD_step1, FEM_model1%MHD_prop,             &
     &    FEM_model1%MHD_BC, FEM_MHD1%geofem, FEM_MHD1%ele_mesh,        &
     &    FEM_MHD1%field, SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    viz_ctls_F)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
      call start_elapsed_time(2)
      call FEM_initialize_MHD(MHD_files1, flex_MHD1, MHD_step1,         &
     &    FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,           &
     &    FEM_MHD1%field, FEM_model1, MHD_CG1, FEM_SGS1, SGS_MHD_wk1,   &
     &    MHD_IO1, fem_sq1, FEM_MHD1%label_sim)
!
      call init_visualize_surface                                       &
     &   (FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,           &
     &    viz_ctls_F%psf_ctls, viz_ctls_F%iso_ctls, MHD_viz_psfs)
      call end_elapsed_time(2)
!
      end subroutine initialization_MHD
!
! ----------------------------------------------------------------------
!
      subroutine evolution_MHD
!
      integer(kind=kint ) :: visval
      integer(kind=kint ) :: retval
!
!
      retval = 1
      call start_elapsed_time(3)
!
      do
!  Time evolution
        call FEM_analyze_MHD(MHD_files1,                                &
     &      FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,         &
     &      FEM_model1, flex_MHD1, MHD_step1, visval, retval, MHD_CG1,  &
     &      FEM_SGS1, SGS_MHD_wk1, FEM_MHD1%field, MHD_IO1, fem_sq1)
!
!     ---------------------
!
!  Visualization
        if (visval.eq.0) then
          call start_elapsed_time(4)
          call visualize_surface(MHD_step1%viz_step, MHD_step1%time_d,  &
     &        FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,       &
     &        MHD_viz_psfs)
          call end_elapsed_time(4)
        end if
!
        if (retval .eq. 0) exit
      end do
!
      call end_elapsed_time(3)
!
!  time evolution end
!
      call FEM_finalize_MHD(MHD_files1, MHD_step1, MHD_IO1)
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      call output_elapsed_times
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_MHD
!
! ----------------------------------------------------------------------
!
      end module analyzer_MHD
