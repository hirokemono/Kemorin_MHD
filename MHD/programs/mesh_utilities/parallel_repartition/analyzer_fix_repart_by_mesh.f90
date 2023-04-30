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
      use m_file_format_switch
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
      type(calypso_comm_table), save :: repart_ele_tbl1
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
      use t_jacobians
      use t_shape_functions
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
      use const_element_comm_tables
      use int_volume_of_single_domain
      use repartiton_by_volume
      use set_element_id_4_node
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(jacobians_type), save :: jacobians1
      type(shape_finctions_at_points), save :: spfs1
      type(next_nod_ele_table), save :: next_tbl1
      type(sleeve_extension_work), save :: sleeve_exp_WK1
      type(calypso_comm_table), save :: repart_nod_tbl0
!
      type(mesh_data) :: new_fem0
!
      type(masking_parameter), allocatable, target :: masking1(:)
      real(kind = kreal), allocatable :: d_mask_org1(:,:)
      real(kind = kreal), allocatable :: vect_ref1(:,:)
      integer :: id_tmp(3)
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
      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node, fem_T%mesh%nod_comm,   &
     &                          fem_T%mesh%ele, ele_comm_T, m_SR_T)
!
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
      id_tmp(1) = part_p1%repart_p%sleeve_exp_p%iflag_expand_mode
      id_tmp(2) = part_p1%repart_p%viz_mesh_file%iflag_format
      id_tmp(3) = part_p1%repart_p%trans_tbl_file%iflag_format
      part_p1%repart_p%sleeve_exp_p%iflag_expand_mode = iflag_turn_off
      part_p1%repart_p%viz_mesh_file%iflag_format =    id_no_file
      part_p1%repart_p%trans_tbl_file%iflag_format =    id_no_file
      call s_repartiton_by_volume((.TRUE.), part_p1%repart_p,           &
     &   fem_T%mesh, fem_T%group, ele_comm_T, next_tbl1,                &
     &   izero, masking1, vect_ref1(1,1), d_mask_org1, vect_ref1,       &
     &   new_fem0%mesh, new_fem0%group, repart_nod_tbl0,                &
     &   repart_ele_tbl1, sleeve_exp_WK1, m_SR_T)
      part_p1%repart_p%sleeve_exp_p%iflag_expand_mode = id_tmp(1)
      part_p1%repart_p%viz_mesh_file%iflag_format =     id_tmp(2)
      part_p1%repart_p%trans_tbl_file%iflag_format =    id_tmp(3)
      deallocate(d_mask_org1, vect_ref1, masking1)
      call calypso_mpi_barrier
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
      use compare_calypso_comm_tables
!
      type(mesh_data), save :: new_fem2
      type(calypso_comm_table), save :: part_nod_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(interpolate_table), save :: itp_nod_tbl_IO
!
      integer(kind = kint) :: ierr
!
!
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!  --  read repartitioned geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%repart_p%viz_mesh_file,               &
     &                    nprocs, new_fem2)
      call calypso_mpi_barrier
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem2%mesh, new_fem2%group,       &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_global_numele_list(new_fem2%mesh%ele)
      call const_ele_comm_table(new_fem2%mesh%node,                     &
     &    new_fem2%mesh%nod_comm, new_fem2%mesh%ele,                    &
     &    new_ele_comm2, m_SR_T)
      call calypso_mpi_barrier
!
!  --  read old interpolation table
!
      call sel_mpi_read_interpolate_table                               &
     &   (my_rank, nprocs, part_p1%repart_p%trans_tbl_file,             &
     &    itp_nod_tbl_IO, ierr)
!
      call copy_itp_table_to_repart_tbl(my_rank,                        &
     &    fem_T%mesh, new_fem2%mesh, itp_nod_tbl_IO, part_nod_tbl2)
      call dealloc_itp_tbl_after_write(itp_nod_tbl_IO)
      call calypso_MPI_barrier
!
!  --  load new repartition table
!
      if (iflag_debug.gt.0) write(*,*) 'output_repart_table'
      call output_repart_table                                          &
     &   (part_p1%repart_p%trans_tbl_file, new_fem2%mesh%ele%numele,    &
     &    part_nod_tbl2, repart_ele_tbl1, new_fem2%mesh%nod_comm,       &
     &    new_ele_comm2)
      call calypso_MPI_barrier
!
      end subroutine analyze_fix_repart_by_mesh
!
! ----------------------------------------------------------------------
!
      end module analyzer_fix_repart_by_mesh
