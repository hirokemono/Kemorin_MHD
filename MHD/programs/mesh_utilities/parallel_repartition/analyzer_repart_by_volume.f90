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
      type(mesh_data), save :: new_fem
      type(mesh_SR), save :: m_SR_T
!
      type(calypso_comm_table), save :: repart_nod_tbl1
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
      use t_const_comm_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
      use repartiton_by_volume
      use const_element_comm_tables
      use set_element_id_4_node
      use int_volume_of_single_domain
      use field_to_new_partition
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(communication_table), save :: ele_comm1
      type(jacobians_type), save :: jacobians1
      type(shape_finctions_at_points), save :: spfs1
      type(next_nod_ele_table), save :: next_tbl1
      type(sleeve_extension_work), save :: sleeve_exp_WK1
!
      type(masking_parameter), allocatable, target :: masking1(:)
      real(kind = kreal), allocatable :: d_mask_org1(:,:)
      real(kind = kreal), allocatable :: vect_ref1(:,:)
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
!  -------------------------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
!
!  -----  Const Element communication table
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node,                        &
     &    fem_T%mesh%nod_comm, fem_T%mesh%ele, ele_comm1, m_SR_T)
!
!  -----  Const volume of each element
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs1, jacobians1)
      call finalize_jac_and_single_vol                                  &
     &   (fem_T%mesh, spfs1, jacobians1)
!
!  -----  Const Neighboring information
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl1%neib_ele, next_tbl1%neib_nod)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
!  -----  Re-partitioning
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
      if(iflag_debug .gt. 0) write(*,*) 's_repartiton_by_volume'
      allocate(masking1(0))
      allocate(d_mask_org1(fem_T%mesh%node%numnod,1))
      allocate(vect_ref1(fem_T%mesh%node%numnod,3))
      call s_repartiton_by_volume((.TRUE.), part_p1%repart_p,           &
     &    fem_T%mesh, fem_T%group, ele_comm1, next_tbl1,                &
     &    izero, masking1, vect_ref1(1,1), d_mask_org1, vect_ref1,      &
     &    new_fem%mesh, new_fem%group, repart_nod_tbl1,                 &
     &    sleeve_exp_WK1, m_SR_T)
      deallocate(d_mask_org1, vect_ref1, masking1)
!
      call dealloc_comm_table(ele_comm1)
      call dealloc_next_nod_ele_table(next_tbl1)
      call dealloc_mesh_infomations(fem_T%mesh, fem_T%group)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
!
      call dealloc_node_param_smp(new_fem%mesh%node)
      call dealloc_ele_param_smp(new_fem%mesh%ele)
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
!
      use m_file_format_switch
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
      use copy_repart_ele_and_itp_tbl
      use const_element_comm_tables
      use const_surface_comm_table
      use check_data_for_repartition
      use mesh_send_recv_check
      use write_diff_4_comm_test
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use itrplte_tbl_coef_IO_select
!
      type(calypso_comm_table) :: part_nod_tbl2
      type(interpolate_table) :: itp_nod_tbl_IO
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
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!
      call sel_mpi_read_interpolate_table                               &
     &   (my_rank, nprocs, part_p1%repart_p%trans_tbl_file,             &
     &    itp_nod_tbl_IO, ierr)
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    fem_T%mesh, new_fem%mesh, itp_nod_tbl_IO, part_nod_tbl2)
      call dealloc_itp_tbl_after_write(itp_nod_tbl_IO)
      call calypso_MPI_barrier
!
      if(my_rank .eq. 0) write(*,*) 'check table reading...'
      call compare_calypso_comm_tbls(repart_nod_tbl1, part_nod_tbl2)
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'check table reading end!'
!
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group,         &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_global_numele_list(new_fem%mesh%ele)
      call const_ele_comm_table(new_fem%mesh%node,                      &
     &    new_fem%mesh%nod_comm, new_fem%mesh%ele, T_ele_comm, m_SR_T)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_surf_comm,        &
     &    new_fem%mesh%surf, m_SR_T)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_edge_comm,        &
     &    new_fem%mesh%edge, m_SR_T)
!
!
      if(my_rank .eq. 0) write(*,*) 'check communication table...'
      call node_transfer_test(fem_T%mesh%node, new_fem%mesh%node,       &
     &    new_fem%mesh%nod_comm, repart_nod_tbl1, nod_check,            &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_il)
!
!
      call ele_send_recv_test(new_fem%mesh%ele, T_ele_comm,             &
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
      end subroutine analyze_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_repart_by_volume
