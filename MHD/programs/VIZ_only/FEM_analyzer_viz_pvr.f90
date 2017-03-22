!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_pvr
!!      subroutine FEM_analyze_pvr(i_step, t_VIZ, pvr_step)
!!        type(time_step_param), intent(in) :: t_VIZ
!!        type(IO_step_param), intent(inout)  :: pvr_step
!
      module FEM_analyzer_viz_pvr
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!      use m_visualization
      use t_step_parameter
      use t_VIZ_step_parameter
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
      subroutine FEM_initialize_pvr
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     --------------------- init for PVR
!
      call element_normals_4_VIZ
!
!     ---------------------
!
      call deallocate_edge_geom_type(elemesh_VIZ%edge)
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, t_VIZ, pvr_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(time_step_param), intent(in) :: t_VIZ
      type(IO_step_param), intent(inout)  :: pvr_step
!
      integer (kind =kint) :: visval
!
!
      visval = ione
      call accum_flag_to_visualization(i_step, pvr_step, visval)
      call set_field_data_4_VIZ                                         &
     &   (pvr_step%istep_file, i_step, t_VIZ%time_d)
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
