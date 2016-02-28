!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_vizs
!      subroutine FEM_analyze_vizs(i_step,                              &
!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      module FEM_analyzer_viz
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_visualization
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_vizs
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if( (i_step_output_fline+i_step_output_pvr) .gt. 0) then
        call element_normals_4_VIZ
      end if
!
!     --------------------- 
!
      call deallocate_edge_geom_type(elemesh_VIZ%edge)
!
!     ---------------------
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_vizs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_vizs(i_step,                               &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use t_ucd_data
      use m_control_params_2nd_files
      use set_exit_flag_4_visualizer
!
      integer (kind =kint), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
!
      call set_flag_to_visualization(i_step,                            &
     &        istep_psf, istep_iso, istep_pvr, istep_fline, visval)
      call set_field_data_4_VIZ(visval, i_step)
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
