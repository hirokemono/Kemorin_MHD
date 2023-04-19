!>@file   analyzer_chk_repart_by_mesh.f90
!!@brief  module analyzer_repart_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_chk_repart_by_mesh
!!      subroutine analyze_chk_repart_by_mesh
!!@endverbatim
!
      module analyzer_chk_repart_by_mesh
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
      subroutine initialize_chk_repart_by_mesh
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
      if(iflag_debug .gt. 0) write(*,*) 's_repartiton_by_volume2'
      allocate(masking1(0))
      allocate(d_mask_org1(fem_T%mesh%node%numnod,1))
      allocate(vect_ref1(fem_T%mesh%node%numnod,3))
      call s_repartiton_by_volume2((.TRUE.), part_p1%repart_p,          &
     &    fem_T%mesh, fem_T%group, ele_comm1, next_tbl1,                &
     &    izero, masking1, vect_ref1(1,1), d_mask_org1, vect_ref1,      &
     &    new_fem%mesh, new_fem%group, repart_nod_tbl1,                 &
     &    sleeve_exp_WK1, m_SR_T)
      deallocate(d_mask_org1, vect_ref1, masking1)
!
      call dealloc_comm_table(ele_comm1)
      call dealloc_next_nod_ele_table(next_tbl1)
!      call dealloc_mesh_infomations(fem_T%mesh, fem_T%group)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
!
      call dealloc_node_param_smp(new_fem%mesh%node)
      call dealloc_ele_param_smp(new_fem%mesh%ele)
!
      end subroutine initialize_chk_repart_by_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_chk_repart_by_mesh
!
      use t_solver_SR
      use t_interpolate_table
      use t_work_for_comm_check
!
      use t_repart_double_numberings
      use t_para_double_numbering
!
      use m_file_format_switch
      use para_itrplte_table_IO_sel
      use copy_repart_and_itp_table
      use check_data_for_repartition
      use mesh_send_recv_check
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use load_repartition_table
!
      use compare_mesh_structures
      use mesh_repartition_by_volume
!
      type(calypso_comm_table), save :: part_nod_tbl2
      type(calypso_comm_table), save :: part_ele_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(mesh_data), save :: new_fem2
      integer(kind = kint) :: new_numele
!
      type(node_ele_double_number), save :: new_ids_on_org1
      type(node_ele_double_number), save :: new_iele_dbl1
!
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: icount_error
!
!
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!
      call set_repart_table_from_file                                   &
     &   (part_p1%repart_p%trans_tbl_file, new_numele,                  &
     &    part_nod_tbl2, part_ele_tbl2, new_fem2%mesh%nod_comm,         &
     &    new_ele_comm2)
      new_fem2%mesh%ele%numele = new_numele
      write(*,*) my_rank, 'new_fem2%mesh%ele%numele', new_fem2%mesh%ele%numele
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group,         &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
!
      call const_repart_mesh_by_table(fem_T%mesh, fem_T%group,          &
     &    part_nod_tbl2, part_ele_tbl2, new_ele_comm2,                  &
     &    new_numele, new_fem2%mesh, new_fem2%group, m_SR_T)
!
      if(my_rank .eq. 0) write(*,*) 'Compare read comm tables...'
      call compare_calypso_comm_tbls(repart_nod_tbl1, part_nod_tbl2)
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'Compareing end!'
!
      call compare_node_comm_types(my_rank, new_fem%mesh%nod_comm,      &
     &                             new_fem2%mesh%nod_comm)
      call compare_node_position(my_rank, new_fem%mesh%node,            &
     &                           new_fem2%mesh%node, icount_error)
      write(*,*) my_rank, 'Compare node: ', icount_error
      call compare_ele_connect(my_rank, new_fem%mesh%ele,               &
     &    new_fem2%mesh%ele, icount_error)
      write(*,*) my_rank, 'Compare element: ', icount_error
!
      call compare_mesh_groups(my_rank, new_fem%group, new_fem2%group)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_chk_repart_by_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_repartiton_by_volume2(flag_lic_dump, part_param,     &
     &          mesh, group, ele_comm, next_tbl, num_mask, masking,     &
     &          ref_repart, d_mask, ref_vect_sleeve_ext,                &
     &          new_mesh, new_group, repart_nod_tbl,                    &
     &          sleeve_exp_WK, m_SR)
!
      use t_next_node_ele_4_node
      use t_interpolate_table

      use t_para_double_numbering
      use calypso_SR_type
      use solver_SR_type
      use reverse_SR_int
      use quicksort
      use cal_minmax_and_stacks
!
      use m_file_format_switch
      use m_elapsed_labels_4_REPART
      use m_work_time
      use m_error_IDs
!
      use sleeve_extend
      use parallel_FEM_mesh_init
      use mesh_repartition_by_volume
      use mesh_MPI_IO_select
      use set_nnod_4_ele_by_type
      use copy_mesh_structures
      use copy_repart_and_itp_table
      use const_element_comm_tables
      use nod_and_ele_derived_info
      use const_same_domain_grouping
      use load_repartition_table
      use calypso_mpi_int
!
      logical, intent(in) :: flag_lic_dump
      integer(kind = kint), intent(in) :: num_mask
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
      real(kind = kreal), intent(in)                                    &
     &                   :: d_mask(mesh%node%numnod,num_mask)
      real(kind = kreal), intent(in)                                    &
     &                   :: ref_vect_sleeve_ext(mesh%node%numnod,3)
!
      type(mesh_geometry), intent(inout) :: new_mesh
      type(mesh_groups), intent(inout) :: new_group
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
      type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!
      type(mesh_SR), intent(inout) :: m_SR
!
      type(communication_table) :: new_ele_comm
      type(calypso_comm_table) :: repart_ele_tbl
      integer(kind = kint) :: ierr
!
!  -------------------------------
!
      call calypso_mpi_barrier
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+2)
      if(part_param%iflag_repart_ref .eq. i_NO_REPARTITION) then
        new_mesh%ele%first_ele_type                                     &
     &     = set_cube_eletype_from_num(mesh%ele%nnod_4_ele)
        call copy_mesh_and_group(mesh, group, new_mesh, new_group)
        call const_trans_tbl_to_same_mesh(my_rank, mesh%node,           &
     &                                    repart_nod_tbl, ierr)
!
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr_repart,                           &
     &                         'Failed repatition table loading')
        end if
      else
        call s_mesh_repartition_by_volume                               &
     &     (mesh, group, ele_comm, next_tbl%neib_nod, part_param,       &
     &      num_mask, masking, ref_repart, d_mask, new_mesh, new_group, &
     &      new_ele_comm, repart_nod_tbl, repart_ele_tbl, m_SR)
      end if
      call calypso_mpi_barrier
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+2)
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+7)
      call set_nod_and_ele_infos(new_mesh%node, new_mesh%ele)
      call const_ele_comm_table(new_mesh%node, new_mesh%nod_comm,       &
     &                          new_mesh%ele, new_ele_comm, m_SR)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+7)
!
! Increase sleeve size
      if(part_param%sleeve_exp_p%iflag_expand_mode                      &
     &                         .ne. iflag_turn_off) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+3)
        call calypso_mpi_barrier
        call sleeve_extension_for_new_mesh                              &
     &     (flag_lic_dump, part_param%sleeve_exp_p,                     &
     &      mesh, ref_vect_sleeve_ext, repart_nod_tbl,                  &
     &      new_mesh, new_group, new_ele_comm, sleeve_exp_WK, m_SR)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+3)
      end if
      call calypso_mpi_barrier
!
!  ----------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+7)
      call set_nod_and_ele_infos(new_mesh%node, new_mesh%ele)
      call const_ele_comm_table(new_mesh%node, new_mesh%nod_comm,       &
     &                          new_mesh%ele, new_ele_comm, m_SR)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+7)
!
!       Output new mesh file
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
      if(part_param%viz_mesh_file%iflag_format .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &          'No repartitioned mesh data output'
      else
        call sel_mpi_write_mesh_file(part_param%viz_mesh_file,          &
     &                               new_mesh, new_group)
      end if
      call calypso_MPI_barrier
      return
!
      end subroutine s_repartiton_by_volume2
!
! ----------------------------------------------------------------------
!
      end module analyzer_chk_repart_by_mesh
