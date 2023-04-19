!>@file   analyzer_check_repart.f90
!!@brief  module analyzer_check_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_check_reapart_mesh
!!      subroutine analyze_check_reapart_mesh
!!@endverbatim
!
      module analyzer_check_repart
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
      type(mesh_SR), save :: m_SR_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_check_reapart_mesh
!
      use t_next_node_ele_4_node
      use t_ctl_file_volume_grouping
      use t_const_comm_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
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
      call read_control_new_partition(part_tctl1)
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
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
      end subroutine initialize_check_reapart_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_check_reapart_mesh
!
      use t_solver_SR
      use t_interpolate_table
      use t_work_for_comm_check
!
      use m_file_format_switch
      use copy_repart_and_itp_table
      use const_element_comm_tables
      use const_surface_comm_table
      use check_data_for_repartition
      use mesh_send_recv_check
      use write_diff_4_comm_test
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use load_repartition_table
      use mesh_repartition_by_volume
!
      type(mesh_data), save :: new_fem
!
      type(calypso_comm_table), save :: part_nod_tbl2
      type(calypso_comm_table), save :: part_ele_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(communication_table), save :: T_ele_comm
      type(communication_table), save :: T_surf_comm
      type(communication_table), save :: T_edge_comm
!
      type(work_for_comm_check), save :: nod_check
      type(work_for_comm_check), save :: ele_check
      type(work_for_comm_check), save :: surf_check
      type(work_for_comm_check), save :: edge_check
!
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: i, ierr, new_numele
!
!
      call set_repart_table_from_file                                   &
     &   (part_p1%repart_p%trans_tbl_file, new_numele,                  &
     &    part_nod_tbl2, part_ele_tbl2, new_fem%mesh%nod_comm,          &
     &    new_ele_comm2)
      call calypso_mpi_barrier
      new_fem%mesh%ele%numele = new_numele
      write(*,*) my_rank, 'new_fem%mesh%ele%numele',                    &
     &                   new_fem%mesh%ele%numele
!
      call const_repart_mesh_by_table(fem_T%mesh, fem_T%group,          &
     &    part_nod_tbl2, part_ele_tbl2, new_ele_comm2,                  &
     &    new_numele, new_fem%mesh, new_fem%group, m_SR_T)
      call calypso_mpi_barrier
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group,         &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
      if(iflag_debug.gt.0) write(*,*) ' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_surf_comm,        &
     &    new_fem%mesh%surf, m_SR_T)
!
      if(iflag_debug.gt.0) write(*,*) ' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_edge_comm,        &
     &    new_fem%mesh%edge, m_SR_T)
!
!
      if(my_rank .eq. 0) write(*,*) 'check communication table...'
      call node_transfer_test(fem_T%mesh%node, new_fem%mesh%node,       &
     &    new_fem%mesh%nod_comm, part_nod_tbl2, nod_check,              &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_il)
!
      call ele_send_recv_test(new_fem%mesh%ele, new_ele_comm2,          &
     &                        ele_check, m_SR_T%SR_sig, m_SR_T%SR_r)
      call surf_send_recv_test(new_fem%mesh%surf, T_surf_comm,          &
     &                         surf_check, m_SR_T%SR_sig, m_SR_T%SR_r)
      call edge_send_recv_test(new_fem%mesh%edge, T_edge_comm,          &
     &                         edge_check, m_SR_T%SR_sig, m_SR_T%SR_r)
!
      call output_diff_mesh_comm_test(repart_test_name,                 &
     &    nod_check, ele_check, surf_check, edge_check)
!
      call dealloc_ele_comm_test_IO(nod_check)
      call dealloc_ele_comm_test_IO(ele_check)
      call dealloc_ele_comm_test_IO(surf_check)
      call dealloc_ele_comm_test_IO(edge_check)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_check_reapart_mesh
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_repart
