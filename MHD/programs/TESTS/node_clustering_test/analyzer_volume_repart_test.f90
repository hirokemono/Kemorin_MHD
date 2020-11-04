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
      call const_comm_tbl_to_new_part(part_grp)
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
      subroutine const_comm_tbl_to_new_part(part_grp)
!
      use t_calypso_comm_table
!
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint) :: nloop
      type(calypso_comm_table), allocatable :: part_tbl(:)
!
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:,:)
!
      integer(kind = kint) :: iloop
!
!
      nloop = (part_grp%num_grp-1) / nprocs + 1
      allocate(part_tbl(nloop))
!
      allocate(num_send_tmp(part_grp%num_grp))
      allocate(num_recv_tmp(nprocs,nloop))
!
      call gather_num_trans_for_repart                                  &
     &         (nloop, part_grp, num_send_tmp, num_recv_tmp)
!
      do iloop = 1, nloop
        part_tbl(iloop)%iflag_self_copy = 0
      end do
      call count_num_export_for_repart                                  &
     &   (my_rank, part_grp%num_grp, num_send_tmp,                      &
     &    part_tbl(1)%iflag_self_copy, part_tbl(1)%nrank_export)
!
      do iloop = 1, nloop
        call alloc_calypso_export_num(part_tbl(iloop))
      end do
      call set_istack_export_for_repart                                 &
     &   (my_rank, part_grp%num_grp, num_send_tmp,                      &
     &    part_tbl(1)%nrank_export, part_tbl(1)%ntot_export,            &
     &    part_tbl(1)%irank_export, part_tbl(1)%num_export,             &
     &    part_tbl(1)%istack_export)
!
      do iloop = 1, nloop
        call alloc_calypso_export_item(part_tbl(iloop))
      end do
!
      call set_export_item_for_repart(my_rank, part_grp, num_send_tmp,  &
     &    part_tbl(1)%nrank_export, part_tbl(1)%ntot_export,            &
     &    part_tbl(1)%istack_export, part_tbl(1)%item_export)
!
!
      do iloop = 1, nloop
        call count_num_import_for_repart                                &
     &     (iloop, nprocs, part_grp%num_grp, num_recv_tmp(1,iloop),     &
     &      part_tbl(iloop)%nrank_import)
        call alloc_calypso_import_num(part_tbl(iloop))
!
        call set_istack_import_for_repart(iloop,                        &
     &      my_rank, nprocs, part_grp%num_grp, num_recv_tmp(1,iloop),   &
     &      part_tbl(iloop)%nrank_import, part_tbl(iloop)%ntot_import,  &
     &      part_tbl(iloop)%irank_import, part_tbl(iloop)%num_import,   &
     &      part_tbl(iloop)%istack_import)
!
        call alloc_calypso_import_item                                  &
     &     (part_tbl(iloop)%ntot_import, part_tbl(iloop))
        call set_import_item_for_repart                                 &
     &     (part_tbl(iloop)%ntot_import, part_tbl(iloop)%ntot_import,   &
     &      part_tbl(iloop)%item_import, part_tbl(iloop)%irev_import)
      end do
!
!
!
      call send_back_istack_import_repart(part_grp, part_tbl,           &
     &    nloop, num_recv_tmp, num_send_tmp)
!
!
!
      deallocate(num_send_tmp, num_recv_tmp)
      deallocate(part_tbl)
!
      end subroutine const_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gather_num_trans_for_repart                            &
     &         (nloop, part_grp, num_send_tmp, num_recv_tmp)
!
      use calypso_mpi
      use calypso_mpi_int
      use t_group_data
!
      integer(kind = kint), intent(in) :: nloop
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
      integer(kind = kint), intent(inout) :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint) :: i, iloop
      integer :: irank_recv
!
      do i = 1, part_grp%num_grp
        iloop = 1 + (i-1) / nprocs
        irank_recv = mod(i-1,nprocs)
        num_send_tmp(i)                                                 &
     &      = part_grp%istack_grp(i) - part_grp%istack_grp(i-1)
        call calypso_mpi_gather_one_int                                 &
     &     (num_send_tmp(i), num_recv_tmp(1,iloop), irank_recv)
      end do
!
      end subroutine gather_num_trans_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine send_back_istack_import_repart(part_grp, part_tbl,     &
     &          nloop, num_recv_tmp, num_send_tmp)
!
      use calypso_mpi
      use calypso_mpi_int
      use t_calypso_comm_table
!
      type(group_data), intent(in) :: part_grp
      type(calypso_comm_table), intent(in) :: part_tbl(nloop)
!
      integer(kind = kint), intent(in) :: nloop
!
      integer(kind = kint), intent(inout) :: num_recv_tmp(nprocs,nloop)
      integer(kind = kint), intent(inout)                               &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint) :: i, iloop, ip
      integer :: irank_recv
!
!
!$omp parallel
      do iloop = 1, nloop
!$omp do private(ip)
        do ip = 1, nprocs
          num_recv_tmp(ip,iloop) = -1
        end do
!$omp end do
!$omp do private(i,ip)
        do i = 1, part_tbl(iloop)%nrank_import
          ip = part_tbl(iloop)%irank_import(i)
          num_recv_tmp(ip,iloop)                                        &
     &      = part_tbl(iloop)%istack_import(ip-1)
        end do
!$omp end do
      end do
!$omp end parallel
!
      do i = 1, part_grp%num_grp
        iloop = 1 + (i-1) / nprocs
        irank_recv = mod(i-1,nprocs)
        call calypso_mpi_scatter_one_int                                &
     &     (num_recv_tmp(1,iloop), num_send_tmp(i), irank_recv)
      end do
!
      end subroutine send_back_istack_import_repart
!
! ----------------------------------------------------------------------
!
      subroutine count_num_export_for_repart                            &
     &         (my_rank, num_grp, num_send_tmp,                         &
     &          iflag_self_copy, nrank_export)
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: num_send_tmp(num_grp)
!
      integer(kind = kint), intent(inout) :: iflag_self_copy
      integer(kind = kint), intent(inout) :: nrank_export
!
      integer(kind = kint) :: i
!
      if(num_send_tmp(my_rank+1) .gt. 0) iflag_self_copy = 1
!
      nrank_export = 0
      do i = 1, num_grp
        if(num_send_tmp(i) .gt. 0) nrank_export = nrank_export + 1
      end do
!
      end subroutine count_num_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine count_num_import_for_repart                            &
     &         (iloop, nprocs, num_grp, num_recv_tmp, nrank_import)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: iloop
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs)
!
      integer(kind = kint), intent(inout) :: nrank_import
!
      integer(kind = kint) :: i, inum
!
!
        nrank_import = 0
        do inum = 1, nprocs
          i = inum + (iloop-1) * nprocs
          if(i .gt. num_grp) cycle
          if(num_recv_tmp(i) .gt. 0)  nrank_import = nrank_import + 1
        end do
!
      end subroutine count_num_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_istack_export_for_repart(my_rank, num_grp,         &
     &          num_send_tmp, nrank_export, ntot_export, irank_export,  &
     &          num_export, istack_export)
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: num_send_tmp(num_grp)
      integer(kind = kint), intent(in) :: nrank_export
!
      integer(kind = kint), intent(inout) :: ntot_export
      integer(kind = kint), intent(inout) :: irank_export(nrank_export)
      integer(kind = kint), intent(inout) :: num_export(nrank_export)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_export(0:nrank_export)
!
      integer(kind = kint) :: i, ip, icou
!
      icou = 0
      istack_export(0) = 0
      do i = 1, num_grp
        ip = 1 + mod(i+my_rank,num_grp)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          irank_export(icou) =  ip
          num_export(icou) =    num_send_tmp(ip)
          istack_export(icou) = istack_export(icou-1)                   &
     &                          + num_send_tmp(ip)
        end if
      end do
      ntot_export = istack_export(nrank_export)
!
      end subroutine set_istack_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_istack_import_for_repart(iloop,                    &
     &          my_rank, nprocs, num_grp, num_recv_tmp, nrank_import,   &
     &          ntot_import, irank_import, num_import, istack_import)
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: iloop
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs)
!
      integer(kind = kint), intent(in) :: nrank_import
!
      integer(kind = kint), intent(inout) :: ntot_import
      integer(kind = kint), intent(inout) :: irank_import(nrank_import)
      integer(kind = kint), intent(inout) :: num_import(nrank_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_import(0:nrank_import)
!
      integer(kind = kint) :: i, inum, icou, id_rank, ip
!
!
      icou = 0
      istack_import(0) = 0
      id_rank = my_rank + (iloop-1) * nprocs
      do inum = 1, nprocs
        i = inum + (iloop-1) * nprocs
        ip = 1 + mod(i+id_rank,nprocs)
        if(i .gt. num_grp) cycle
        if(num_recv_tmp(ip) .gt. 0) then
          icou = icou + 1
          irank_import(icou) =  ip
          num_import(icou) = num_recv_tmp(ip)
          istack_import(icou) = istack_import(icou-1)                   &
     &                         + num_import(icou)
        end if
      end do
      ntot_import = istack_import(nrank_import)
!
      end subroutine set_istack_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_export_item_for_repart                             &
     &         (my_rank, part_grp, num_send_tmp,                        &
     &          nrank_export, ntot_export, istack_export, item_export)
!
      use t_group_data
!
      type(group_data), intent(in) :: part_grp
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint), intent(in) :: nrank_export, ntot_export
      integer(kind = kint), intent(in) :: istack_export(0:nrank_export)
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      integer(kind = kint) :: i, ip, inum, icou, ist, num, jst
!
      icou = 0
      do i = 1, part_grp%num_grp
        ip = 1 + mod(i+my_rank,part_grp%num_grp)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          ist = part_grp%istack_grp(ip-1)
          jst = istack_export(icou-1)
          num = istack_export(icou) - istack_export(icou-1)
!$omp parallel do
          do inum = 1, num
            item_export(jst+inum) = part_grp%item_grp(ist+inum)
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine set_export_item_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_import_item_for_repart                             &
     &         (NP, ntot_import, item_import, irev_import)
!
      integer(kind = kint), intent(in) :: NP, ntot_import
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer(kind = kint), intent(inout) :: irev_import(NP)
!
      integer(kind = kint) :: inum
!
!
!$omp parallel do
      do inum = 1, ntot_import
        item_import(inum) = inum
        irev_import(inum) = inum
      end do
!$omp end parallel do
!
      end subroutine set_import_item_for_repart
!
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
