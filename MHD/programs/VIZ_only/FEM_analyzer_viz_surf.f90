!FEM_analyzer_viz_surf.f90
!
!      module FEM_analyzer_viz_surf
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_surface
!!      subroutine FEM_analyze_surface(i_step, viz_step)
!!        type(VIZ_step_params), intent(inout) :: viz_step
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_visualization
      use t_IO_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_surface
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     ---------------------
!
      call deallocate_surface_geom_type(elemesh_VIZ%surf)
      call deallocate_edge_geom_type(elemesh_VIZ%edge)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, viz_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer (kind =kint) :: visval, iflag
!
!
      visval = ione
      call accum_flag_to_visualization(i_step, viz_step%PSF_t, visval)
      call accum_flag_to_visualization(i_step, viz_step%ISO_t, visval)
!
      iflag = viz_step%PSF_t%istep_file * viz_step%ISO_t%istep_file
      call set_field_data_4_VIZ(iflag, i_step)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
