!>@file   analyzer_fix_repart_by_mesh.f90
!!@brief  module analyzer_fix_repart_by_mesh
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_fix_repart_by_mesh
!!      subroutine analyze_fix_repart_by_mesh
!!@endverbatim
!
      module analyzer_fix_repart_by_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_repartition
      use t_vector_for_solver
      use t_mesh_SR
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &     :: repart_test_name = 'repart_check.dat'
!
      type(vol_partion_prog_param), save ::  part_p1
      type(mesh_data), save :: fem_T
      type(communication_table) :: ele_comm_T
      type(mesh_SR), save :: m_SR_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_fix_repart_by_mesh
!
      use t_next_node_ele_4_node
      use t_ctl_file_volume_grouping
      use t_const_comm_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
      use const_element_comm_tables
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      call elapsed_label_4_ele_comm_tbl
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      call read_ctl_file_new_partition(part_tctl1)
      call set_control_param_repartition(part_tctl1, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call init_nod_send_recv(fem_T%mesh,                               &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_i, m_SR_T%SR_il)
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
!      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node, fem_T%mesh%nod_comm,   &
     &                          fem_T%mesh%ele, ele_comm_T, m_SR_T)
!
      end subroutine initialize_fix_repart_by_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_fix_repart_by_mesh
!
      use t_solver_SR
!
      use t_repart_double_numberings
      use t_para_double_numbering
      use t_interpolate_table
!
      use calypso_mpi_int
      use m_file_format_switch
      use parallel_FEM_mesh_init
      use load_repartition_table
!
      use mpi_load_mesh_data
      use const_element_comm_tables
      use compare_mesh_structures
      use repartiton_by_volume
      use mesh_repartition_by_volume
      use copy_repart_and_itp_table
      use itrplte_tbl_coef_IO_select
      use para_itrplte_table_IO_sel
!
      type(calypso_comm_table), save :: part_nod_tbl1
      type(calypso_comm_table), save :: part_ele_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(interpolate_table), save :: itp_nod_tbl_IO
      type(mesh_data), save :: new_fem_f, new_fem3
      integer(kind = kint) :: new_numele
!
      integer(kind = kint) :: icount_error, icou_error_gl, ierr
!
!
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%repart_p%viz_mesh_file,               &
     &                    nprocs, new_fem_f)
      call calypso_mpi_barrier
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem_f%mesh, new_fem_f%group,     &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_ele_comm_table(new_fem_f%mesh%node,                    &
     &    new_fem_f%mesh%nod_comm, new_fem_f%mesh%ele,                  &
     &    new_ele_comm2, m_SR_T)
!
!
!  --  read old interpolation table
      call sel_mpi_read_interpolate_table                               &
     &   (my_rank, nprocs, part_p1%repart_p%trans_tbl_file,             &
     &    itp_nod_tbl_IO, ierr)
!
      call copy_itp_table_to_repart_tbl(my_rank,                        &
     &    fem_T%mesh, new_fem_f%mesh, itp_nod_tbl_IO, part_nod_tbl1)
      call dealloc_itp_tbl_after_write(itp_nod_tbl_IO)
      call calypso_MPI_barrier
!
!  --  Construct element repartitioned mesh
      call reconstruct_reparted_element(fem_T%mesh, ele_comm_T,         &
     &   new_fem_f%mesh%node, new_fem_f%mesh%nod_comm, part_nod_tbl1,   &
     &   part_ele_tbl2, m_SR_T)
!
!  --  Construct element repartitioned mesh
      new_numele = new_fem_f%mesh%ele%numele
      call const_repart_mesh_by_table                                   &
     &   (fem_T%mesh, fem_T%group, ele_comm_T,                          &
     &    part_nod_tbl1, part_ele_tbl2, new_ele_comm2,                  &
     &    new_numele, new_fem3%mesh, new_fem3%group, m_SR_T)
!
!
!
      call compare_node_comm_types(my_rank, new_fem_f%mesh%nod_comm,    &
     &                             new_fem3%mesh%nod_comm)
!
      call compare_node_position(my_rank, new_fem_f%mesh%node,          &
     &                           new_fem3%mesh%node, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare node: ', icou_error_gl
!      write(*,*) my_rank, 'Compare node: ', icount_error
!
      call compare_ele_connect(my_rank, new_fem_f%mesh%ele,             &
     &    new_fem3%mesh%ele, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare element: ', icou_error_gl
!      write(*,*) my_rank, 'Compare element: ', icount_error
!
      call compare_mesh_groups(my_rank, new_fem_f%group,                &
     &                         new_fem3%group)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_fix_repart_by_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine reconstruct_reparted_element                           &
     &         (mesh, ele_comm, new_node, new_nod_comm, repart_nod_tbl, &
     &          repart_ele_tbl, m_SR)
!
      use t_control_param_vol_grping
      use t_repart_double_numberings
      use t_repartition_by_volume
!
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use check_data_for_repartition
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: new_nod_comm
      type(node_data), intent(in) :: new_node
      type(calypso_comm_table), intent(in) :: repart_nod_tbl
!
      type(calypso_comm_table), intent(inout) :: repart_ele_tbl
      type(mesh_SR), intent(inout) :: m_SR
!
      type(node_ele_double_number) :: new_ids_on_org
      type(element_data) :: new_ele
!
!
!       Re-partitioning
      call alloc_double_numbering(mesh%node%numnod, new_ids_on_org)
      call node_dbl_numbering_to_repart(mesh%nod_comm, mesh%node,       &
     &    repart_nod_tbl, new_ids_on_org, m_SR%SR_sig, m_SR%SR_i)
      call check_repart_node_transfer                                   &
     &   (mesh%nod_comm, mesh%node, new_nod_comm, new_node,             &
     &    repart_nod_tbl, new_ids_on_org, m_SR%SR_sig, m_SR%SR_i)
!
      call s_const_repart_ele_connect                                   &
     &   (mesh, ele_comm, repart_nod_tbl, new_ids_on_org,               &
     &    new_nod_comm, new_node, new_ele,                              &
     &    repart_ele_tbl, m_SR%SR_sig, m_SR%SR_i, m_SR%SR_il)
      call dealloc_double_numbering(new_ids_on_org)
      call dealloc_ele_connect(new_ele)
!
!
      end subroutine reconstruct_reparted_element
!
! ----------------------------------------------------------------------
!
      end module analyzer_fix_repart_by_mesh
