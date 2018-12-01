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
      use m_elapsed_labels_4_MHD
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
      use m_elapsed_labels_4_VIZ
!
!
      total_start = MPI_WTIME()
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      call init_elapse_time_by_TOTAL
      call elapsed_label_4_MHD
      call elapsed_label_4_FEM_MHD
      call elpsed_label_4_VIZ
      call append_COMM_TIME_to_elapsed
!
!     --------------------- 
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call input_control_4_FEM_MHD(MHD_files1, FEM_model1%FEM_prm,      &
     &    FEM_SGS1%SGS_par, MHD_step1, FEM_model1%MHD_prop,             &
     &    FEM_model1%MHD_BC, FEM_MHD1%geofem, FEM_MHD1%ele_mesh,        &
     &    FEM_MHD1%field, SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    viz_ctls_F)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      call FEM_initialize_MHD(MHD_files1, flex_MHD1, MHD_step1,         &
     &    FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,           &
     &    FEM_MHD1%field, FEM_model1, MHD_CG1, FEM_SGS1, SGS_MHD_wk1,   &
     &    MHD_IO1, fem_sq1, FEM_MHD1%label_sim)
!
      call init_visualize_surface                                       &
     &   (FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,           &
     &    viz_ctls_F%psf_ctls, viz_ctls_F%iso_ctls, MHD_viz_psfs)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
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
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
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
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
          call visualize_surface(MHD_step1%viz_step, MHD_step1%time_d,  &
     &        FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,       &
     &        MHD_viz_psfs)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        end if
!
        if (retval .eq. 0) exit
      end do
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
!  time evolution end
!
      call FEM_finalize_MHD(MHD_files1, MHD_step1, MHD_IO1)
!
      call copy_COMM_TIME_to_elaps
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
      return
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_MHD
!
! ----------------------------------------------------------------------
!
      end module analyzer_MHD
