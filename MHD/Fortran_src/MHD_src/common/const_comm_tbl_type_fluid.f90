!const_comm_tbl_type_fluid.f90
!      module const_comm_tbl_type_fluid
!
!     Programmed by H.Matsui on Dec., 2008
!
!      subroutine s_const_comm_tbl_type_fluid(solver_C, mesh, MHD_mesh)
!        type(mpi_4_solver), intent(in) ::       solver_C
!        type(mesh_geometry),    intent(in) :: mesh
!
!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
      module const_comm_tbl_type_fluid
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_type_fluid(solver_C, mesh, MHD_mesh)
!
      use t_vector_for_solver
      use t_mesh_data
      use t_geometry_data_MHD
      use const_comm_table_fluid
!
      type(mpi_4_solver), intent(in) :: solver_C
      type(mesh_geometry),    intent(in) :: mesh
!
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      if (mesh%node%numnod .eq. 0) then
        call set_empty_comm_table_fluid(MHD_mesh%nod_fl_comm)
      else
        call s_const_comm_table_fluid                                   &
     &   (solver_C%nprocs, MHD_mesh%fluid%istack_ele_fld_smp,           &
     &    mesh%node, mesh%ele, mesh%nod_comm, MHD_mesh%nod_fl_comm)
      end if
!
      end subroutine s_const_comm_tbl_type_fluid
!
!------------------------------------------------------------------
!
      end module const_comm_tbl_type_fluid
