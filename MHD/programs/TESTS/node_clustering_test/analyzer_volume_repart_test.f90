!>@file   analyzer_volume_repart_test.f90
!!@brief  module analyzer_volume_repart_test
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_volume_repartition
!!      subroutine analyze_volume_grouping
!!@endverbatim
!
      module analyzer_volume_repart_test
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_volume_repartition
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_volume_grouping
      use t_control_param_vol_grping
      use t_1d_repartitioning_work
      use t_repartition_by_volume
      use set_istack_4_domain_block
      use repart_in_xyz_by_volume
      use external_group_4_new_part
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
!
      use mpi_load_mesh_data
      use mesh_file_IO
      use copy_mesh_structures
      use append_group_data
!
      use calypso_mpi_int
      use calypso_mpi_real
      use const_jacobians_3d
      use parallel_FEM_mesh_init
      use output_test_mesh
      use set_table_4_RHS_assemble
      use nod_phys_send_recv
      use solver_SR_type
      use transfer_to_long_integers
!
      use set_parallel_file_name
      use int_volume_of_single_domain
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(group_data) :: part_grp
      type(group_data) :: ext_grp
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!      call elapsed_label_4_ele_comm_tbl
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call s_set_ctl_params_4_test_mesh(part_tctl1, T_meshes)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_meshes%mesh_file_IO, nprocs, fem_T)
!
!  -------------------------------
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs_T, jacobians_T)
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
!       Re-partitioning
      call s_repartition_by_volume(fem_T%mesh, T_meshes, part_grp)
!
!       Re-partitioning for external node
      call const_external_grp_4_new_part                                &
     &   (fem_T%mesh%node, next_tbl_T%neib_nod, T_meshes,               &
     &    part_grp, ext_grp)
!
!       Append group data
      call s_append_group_data(part_grp, fem_T%group%nod_grp)
!
      call const_comm_tbls_for_new_part                                 &
     &   (fem_T%mesh%nod_comm, fem_T%mesh%node, part_grp, ext_grp)
!
      end subroutine initialize_volume_repartition
!
! ----------------------------------------------------------------------
!
      subroutine analyze_volume_repartition
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_volume_repartition'
!
      end subroutine analyze_volume_repartition
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_comm_tbls_for_new_part                           &
     &         (nod_comm, node, part_grp, ext_grp)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
      use set_comm_tbl_to_new_part
      use const_comm_tbl_to_new_mesh
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(group_data), intent(in) :: part_grp, ext_grp
!
      integer(kind = kint) :: nloop
      type(calypso_comm_table), allocatable :: part_tbl(:)
      type(calypso_comm_table), allocatable :: ext_tbl(:)
!
      type(node_data), allocatable :: new_node(:)
!
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:,:)
!
      integer(kind = kint) :: iloop
!
!
      nloop = (part_grp%num_grp-1) / nprocs + 1
      allocate(part_tbl(nloop))
      allocate(ext_tbl(nloop))
      allocate(new_node(nloop))
!
      allocate(num_send_tmp(part_grp%num_grp))
      allocate(num_recv_tmp(nprocs,nloop))
!
      call gather_num_trans_for_repart                                  &
     &         (nloop, part_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (part_grp, nloop, num_send_tmp, num_recv_tmp, part_tbl)
!
      call set_istack_import_repart(part_tbl, nloop, num_recv_tmp)
      call send_back_istack_import_repart                               &
     &   (part_grp, nloop, num_recv_tmp, num_send_tmp)
      call set_new_subdomain_id                                         &
     &   (nod_comm, node, part_grp, num_send_tmp)
!
!
      call gather_num_trans_for_repart                                  &
     &         (nloop, ext_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_grp, nloop, num_send_tmp, num_recv_tmp, ext_tbl)
!
!
      call set_ext_istack_import_repart                                 &
     &   (part_tbl, ext_tbl, nloop, num_recv_tmp)
      call send_back_istack_import_repart                               &
     &   (ext_grp, nloop, num_recv_tmp, num_send_tmp)
!
!
      do iloop = 1, nloop
        new_node(iloop)%internal_node =    part_tbl(iloop)%ntot_import
        new_node(iloop)%numnod                                          &
     &       = ext_tbl(iloop)%ntot_import + part_tbl(iloop)%ntot_import
      end do
!
!
      deallocate(num_send_tmp, num_recv_tmp)
      deallocate(part_tbl, ext_tbl, new_node)
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_new_subdomain_id                                   &
     &         (nod_comm, node, part_grp, num_send_tmp)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint), allocatable :: idomain_new(:)
      integer(kind = kint), allocatable :: inod_new(:)
!
      integer(kind = kint) :: inum, inod, ist, num, jst
      integer(kind = kint) :: igrp
!
!
      allocate(idomain_new(node%numnod))
!$omp parallel workshare
      idomain_new(1:node%numnod) = 0
!$omp end parallel workshare
!
      do igrp = 1, part_grp%num_grp
        jst = num_send_tmp(igrp)
        ist = part_grp%istack_grp(igrp-1)
        num = part_grp%istack_grp(igrp  ) - ist
!$omp parallel do private(inum,inod)
        do inum = 1, num
          if(jst .lt. 0) write(*,*)                                     &
     &            'Wring in recieved stack', igrp, jst
          inod = part_grp%item_grp(ist+inum)
          idomain_new(inod) = igrp
          inod_new(inod) =    jst + inum
        end do
!$omp end parallel do
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new)
      deallocate(idomain_new, inod_new)
!
!
      end subroutine set_new_subdomain_id
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
