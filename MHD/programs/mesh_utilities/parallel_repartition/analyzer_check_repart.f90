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
!
      implicit none
!
      character(len=kchara), parameter                                          &
     &     :: repart_test_name = 'repart_check.dat'
!
      type(vol_partion_prog_param), save ::  part_p1
      type(mesh_data), save :: fem_T
      type(mesh_data), save :: new_fem
!
      type(calypso_comm_table), save :: org_to_new_tbl
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
      use repartiton_by_volume
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use int_volume_of_single_domain
      use field_to_new_partition
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
      call read_control_new_partition(part_tctl1)
      call set_control_param_repartition(part_tctl1, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call init_nod_send_recv(fem_T%mesh)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh for target'
      call mpi_input_mesh(part_p1%repart_p%viz_mesh_file,               &
     &                    nprocs, new_fem)
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
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
      use const_element_comm_tables
      use check_data_for_repartition
      use mesh_send_recv_check
      use write_diff_4_comm_test
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
!
      type(interpolate_table) :: itp_tbl_IO2
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
      integer(kind = kint) :: i, ierr
!
!
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_p1%repart_p%trans_tbl_file, itp_tbl_IO2, ierr)
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_tbl_IO2, org_to_new_tbl)
      call calypso_MPI_barrier
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group)
      if(iflag_debug.gt.0) write(*,*) ' const_ele_comm_table'
      call const_ele_comm_table                                         &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm,                     &
     &    T_ele_comm, new_fem%mesh%ele)
!
      if(iflag_debug.gt.0) write(*,*) ' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm,                     &
     &    T_surf_comm, new_fem%mesh%surf)
!
      if(iflag_debug.gt.0) write(*,*) ' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm,                     &
     &    T_edge_comm, new_fem%mesh%edge)
!
!
      if(my_rank .eq. 0) write(*,*) 'check communication table...'
      call node_transfer_test                                           &
     &   (fem_T%mesh%node, new_fem%mesh%node,  new_fem%mesh%nod_comm,   &
     &    org_to_new_tbl, nod_check)
!
      call ele_send_recv_test(new_fem%mesh%node, new_fem%mesh%ele,      &
     &    T_ele_comm, ele_check)
      call surf_send_recv_test(new_fem%mesh%node, new_fem%mesh%surf,    &
     &    T_surf_comm, surf_check)
      call edge_send_recv_test(new_fem%mesh%node, new_fem%mesh%edge,    &
     &    T_edge_comm, edge_check)
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
