!>@file   analyzer_repart_by_volume.f90
!!@brief  module analyzer_repart_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_reapart_by_vol
!!      subroutine analyze_reapart_by_vol
!!@endverbatim
!
      module analyzer_repart_by_volume
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_REPART
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
!>      Structure for communicatiors for solver
      type(vectors_4_solver), save :: v_sol_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_reapart_by_vol
!
      use t_next_node_ele_4_node
      use t_ctl_file_volume_grouping
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
      use repartiton_by_volume
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use int_volume_of_single_domain
      use field_to_new_partition
      use parallel_sleeve_extension
      use const_element_comm_table
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
      if(iflag_debug .gt. 0) write(*,*) 'const_new_partition_mesh'
      call const_new_partition_mesh(part_p1%repart_p, fem_T,            &
     &                              new_fem, org_to_new_tbl)
!
      end subroutine initialize_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_reapart_by_vol
!
      use t_solver_SR
      use t_interpolate_table
      use t_work_for_comm_check
      use t_belonged_element_4_node
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
      type(calypso_comm_table) :: part_tbl_2
      type(interpolate_table) :: itp_tbl_IO2
!
      type(send_recv_status), save :: SR_sig_t
!
      type(belonged_table), save :: blng
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
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_p1%repart_p%trans_tbl_file, itp_tbl_IO2, ierr)
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_tbl_IO2, part_tbl_2)
      call calypso_MPI_barrier
!
      if(my_rank .eq. 0) write(*,*) 'check table reading...'
      call compare_calypso_comm_tbls(org_to_new_tbl, part_tbl_2)
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'check table reading end!'
!
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group)
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call const_ele_comm_tbl                                           &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, blng,               &
     &    T_ele_comm, new_fem%mesh%ele)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, blng,               &
     &    T_surf_comm, new_fem%mesh%surf)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, blng,               &
     &    T_edge_comm, new_fem%mesh%edge)
!
!
      if(my_rank .eq. 0) write(*,*) 'check communication table...'
      call node_transfer_test                                           &
     &   (fem_T%mesh%node, new_fem%mesh%node,  new_fem%mesh%nod_comm,   &
     &    org_to_new_tbl, nod_check, v_sol_T)
      call collect_failed_comm(nod_check, SR_sig_t)
!
      call alloc_iccg_int8_vector(new_fem%mesh%node%numnod, v_sol_T)
      call FEM_comm_initialization(new_fem%mesh, v_sol_T)
      call ele_send_recv_test(new_fem%mesh%node, new_fem%mesh%ele,      &
     &    T_ele_comm, ele_check, v_sol_T)
      call collect_failed_comm(ele_check, SR_sig_t)
      call surf_send_recv_test(new_fem%mesh%node, new_fem%mesh%surf,    &
     &    T_surf_comm, surf_check, v_sol_T)
      call collect_failed_comm(surf_check, SR_sig_t)
      call edge_send_recv_test(new_fem%mesh%node, new_fem%mesh%edge,    &
     &    T_edge_comm, edge_check, v_sol_T)
      call collect_failed_comm(surf_check, SR_sig_t)
!
      call dealloc_SR_flag(SR_sig_t)
      call dealloc_iccgN_vec_type(v_sol_T)
      call dealloc_iccg_int8_vector(v_sol_T)
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
      end subroutine analyze_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_repart_by_volume
