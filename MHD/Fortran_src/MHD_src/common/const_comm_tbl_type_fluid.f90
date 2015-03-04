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
      private :: set_empty_comm_tbl_type_fluid
      private :: set_comm_tbl_type_fluid
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_type_fluid(solver_C, mesh, MHD_mesh)
!
      use m_control_parameter
      use t_vector_for_solver
      use t_mesh_data
      use t_geometry_data_MHD
!
      type(mpi_4_solver), intent(in) :: solver_C
      type(mesh_geometry),    intent(in) :: mesh
!
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      if (mesh%node%numnod .eq. 0) then
        call set_empty_comm_tbl_type_fluid(MHD_mesh%nod_fl_comm)
      else
        call set_comm_tbl_type_fluid(solver_C, mesh, MHD_mesh%fluid,    &
     &      MHD_mesh%nod_fl_comm)
      end if
!
      end subroutine s_const_comm_tbl_type_fluid
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_empty_comm_tbl_type_fluid(nod_fl_comm)
!
      use t_comm_table
!
      type(communication_table), intent(inout) :: nod_fl_comm
!
!
      nod_fl_comm%num_neib = 0
      call allocate_type_comm_tbl_num(nod_fl_comm)
!
      nod_fl_comm%ntot_import = 0
      nod_fl_comm%ntot_export = 0
      call allocate_type_comm_tbl_item(nod_fl_comm)
!
      end subroutine set_empty_comm_tbl_type_fluid
!
!------------------------------------------------------------------
!
      subroutine set_comm_tbl_type_fluid(solver_C, mesh, fluid,         &
     &          nod_fl_comm)
!
      use calypso_mpi
      use m_machine_parameter
      use t_vector_for_solver
      use t_mesh_data
      use t_geometry_data_MHD
      use solver_SR_int
      use set_comm_table_fluid
!
      type(mesh_geometry), intent(in) :: mesh
      type(mpi_4_solver), intent(in) ::       solver_C
      type(field_geometry_data), intent(in) :: fluid
!
      type(communication_table), intent(inout) :: nod_fl_comm
!
!
      call allocate_flags_reduced_comm                                  &
     &   (solver_C%nprocs, mesh%node%numnod)
!
      write(*,*) 'fluid%istack_ele_fld_smp(0)', fluid%istack_ele_fld_smp
      call mark_4_fluid_nod_by_ele(mesh%ele%numele,                     &
     &    mesh%ele%nnod_4_ele, mesh%ele%ie,                             &
     &    fluid%istack_ele_fld_smp(0),                                  &
     &    fluid%istack_ele_fld_smp(np_smp) )
!
      call solver_send_recv_i(mesh%node%numnod, mesh%nod_comm%num_neib, &
     &    mesh%nod_comm%id_neib, mesh%nod_comm%istack_import,           &
     &    mesh%nod_comm%item_import, mesh%nod_comm%istack_export,       &
     &    mesh%nod_comm%item_export, iflag_nod)
!
!
      call mark_reduced_neib_domain(mesh%nod_comm%num_neib,             &
     &    mesh%nod_comm%ntot_import, mesh%nod_comm%ntot_export,         &
     &    mesh%nod_comm%id_neib, mesh%nod_comm%istack_import,           &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_import,       &
     &    mesh%nod_comm%item_export)
!
!
      call count_reduced_neib_domain(mesh%nod_comm%num_neib,            &
     &    mesh%nod_comm%id_neib, nod_fl_comm%num_neib)
!
      call allocate_type_comm_tbl_num(nod_fl_comm)
!
      call set_reduced_neib_domain(mesh%nod_comm%num_neib,              &
     &    mesh%nod_comm%id_neib, nod_fl_comm%num_neib,                  &
     &    nod_fl_comm%id_neib)
!
      call count_reduced_comm_stack(mesh%nod_comm%num_neib,             &
     &    mesh%nod_comm%ntot_import, mesh%nod_comm%id_neib,             &
     &    mesh%nod_comm%istack_import, mesh%nod_comm%item_import,       &
     &    nod_fl_comm%num_neib, nod_fl_comm%ntot_import,                &
     &    nod_fl_comm%num_import, nod_fl_comm%istack_import)
      call count_reduced_comm_stack(mesh%nod_comm%num_neib,             &
     &    mesh%nod_comm%ntot_export, mesh%nod_comm%id_neib,             &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_export,       &
     &    nod_fl_comm%num_neib, nod_fl_comm%ntot_export,                &
     &    nod_fl_comm%num_export, nod_fl_comm%istack_export)
!
      call allocate_type_comm_tbl_item(nod_fl_comm)
!
      call set_reduced_comm_item(mesh%nod_comm%num_neib,                &
     &    mesh%nod_comm%ntot_import, mesh%nod_comm%id_neib,             &
     &    mesh%nod_comm%istack_import, mesh%nod_comm%item_import,       &
     &    nod_fl_comm%num_neib, nod_fl_comm%ntot_import,                &
     &    nod_fl_comm%istack_import, nod_fl_comm%item_import)
      call set_reduced_comm_item(mesh%nod_comm%num_neib,                &
     &    mesh%nod_comm%ntot_export, mesh%nod_comm%id_neib,             &
     &    mesh%nod_comm%istack_export, mesh%nod_comm%item_export,       &
     &    nod_fl_comm%num_neib, nod_fl_comm%ntot_export,                &
     &    nod_fl_comm%istack_export, nod_fl_comm%item_export)
!
      call deallocate_flags_reduced_comm
!
      end subroutine set_comm_tbl_type_fluid
!
!------------------------------------------------------------------
!
      end module const_comm_tbl_type_fluid
