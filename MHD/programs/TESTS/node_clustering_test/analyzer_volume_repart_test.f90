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
      use set_repartition_group_name
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
!       Append group data
      call s_append_group_data(part_grp, fem_T%group%nod_grp)
!
      call const_comm_tbl_to_new_part(part_grp)
!
      call const_external_grp_4_new_part                                &
     &  (fem_T%mesh%nod_comm, fem_T%mesh%node, next_tbl_T%neib_nod,     &
     &   T_meshes, part_grp, ext_grp)
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
      call count_num_export_for_repart                                  &
     &   (my_rank, nloop, part_grp%num_grp, num_send_tmp, part_tbl)
!
      do iloop = 1, nloop
        call alloc_calypso_export_num(part_tbl(iloop))
      end do
      call set_istack_export_for_repart                                 &
     &   (my_rank, nloop, part_grp%num_grp, num_send_tmp, part_tbl)
!
      do iloop = 1, nloop
        call alloc_calypso_export_item(part_tbl(iloop))
      end do
!
      call set_export_item_for_repart                                   &
     &   (my_rank, nloop, part_grp, num_send_tmp, part_tbl)
!
!
      call count_num_import_for_repart                                  &
     &   (nprocs, nloop, part_grp%num_grp, num_recv_tmp, part_tbl)
!
      do iloop = 1, nloop
        call alloc_calypso_import_num(part_tbl(iloop))
      end do
!
      call set_istack_import_for_repart(my_rank, nprocs,                &
     &    nloop, part_grp%num_grp, num_recv_tmp, part_tbl)
!
      do iloop = 1, nloop
        call alloc_calypso_import_item                                  &
     &     (part_tbl(iloop)%ntot_import, part_tbl(iloop))
      end do
!
      call set_import_item_for_repart(nloop, part_tbl)
!
      deallocate(num_send_tmp, num_recv_tmp)
      deallocate(part_tbl)
!
      end subroutine const_comm_tbl_to_new_part
!
! ----------------------------------------------------------------------
!
      subroutine const_external_grp_4_new_part                          &
     &         (nod_comm, node, neib_nod, part_param,                   &
     &          part_grp, ext_grp)
!
      use t_comm_table
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_control_param_vol_grping
!
      use solver_SR_type
      use set_repartition_group_name
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      type(group_data), intent(inout) :: ext_grp
!
      integer(kind = kint), allocatable :: idomain_new(:)
      integer(kind = kint), allocatable :: iflag_nod(:)
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: igrp, icou
!
!
      allocate(idomain_new(node%numnod))
      allocate(iflag_nod(node%numnod))
!$omp parallel workshare
      idomain_new(1:node%numnod) = 0
      iflag_nod(1:node%numnod) = 1
!$omp end parallel workshare
!
      do igrp = 1, part_grp%num_grp
        ist = part_grp%istack_grp(igrp-1) + 1
        ied = part_grp%istack_grp(igrp)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = part_grp%num_item
          idomain_new(inod) = igrp
        end do
!$omp end parallel do
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new)
!
      ext_grp%num_grp = part_grp%num_grp
      call alloc_group_num(ext_grp)
!
      call set_xyz_domain_grp_name                                      &
     &   (part_param, ext_grp%num_grp, ext_grp%grp_name)
!
!
      call count_external_grp_4_new_part(node, neib_nod, part_grp,      &
     &    ext_grp%num_grp, ext_grp%num_item, ext_grp%istack_grp,        &
     &    iflag_nod)
      call alloc_group_item(ext_grp)
!
      call set_external_grp_4_new_part(node, neib_nod, part_grp,        &
     &    ext_grp%num_grp, ext_grp%num_item, ext_grp%istack_grp,        &
     &    ext_grp%item_grp, iflag_nod)
!
      deallocate(idomain_new, iflag_nod)
!
      end subroutine const_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      subroutine count_external_grp_4_new_part                          &
     &         (node, neib_nod, part_grp, num_ext_grp,                  &
     &          ntot_ext_grp, istack_ext_grp, iflag_nod)
!
      use t_geometry_data
      use t_group_data
      use t_next_node_ele_4_node
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in) :: num_ext_grp
!
      integer(kind = kint), intent(inout) :: ntot_ext_grp
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_ext_grp(0:num_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: igrp, icou
!
!
      istack_ext_grp(0) = 0
      do igrp = 1, part_grp%num_grp
!$omp parallel workshare
        iflag_nod(1:node%internal_node) = 1
!$omp end parallel workshare
        if(node%numnod .gt. node%internal_node) then
!$omp parallel workshare
          iflag_nod(node%internal_node+1:node%numnod) = 0
!$omp end parallel workshare
        end if
!
        icou = istack_ext_grp(igrp-1)
        ist = part_grp%istack_grp(igrp-1) + 1
        ied = part_grp%istack_grp(igrp)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          iflag_nod(inod) = 0
        end do
!$omp end parallel do
!
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(jnod .le. node%internal_node                             &
     &          .and. iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              iflag_nod(jnod) = 0
            end if
          end do
        end do
        istack_ext_grp(igrp) = icou
      end do
!
      ntot_ext_grp = istack_ext_grp(part_grp%num_grp)
!
      end subroutine count_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      subroutine set_external_grp_4_new_part(node, neib_nod, part_grp,  &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp,              &
     &          item_ext_grp, iflag_nod)
!
      use t_geometry_data
      use t_group_data
      use t_next_node_ele_4_node
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in) :: num_ext_grp
      integer(kind = kint), intent(in) :: ntot_ext_grp
      integer(kind = kint), intent(in) :: istack_ext_grp(0:num_ext_grp)
!
      integer(kind = kint), intent(inout) :: item_ext_grp(ntot_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: igrp, icou
!
!
      do igrp = 1, part_grp%num_grp
!$omp parallel workshare
        iflag_nod(1:node%internal_node) = 1
!$omp end parallel workshare
        if(node%numnod .gt. node%internal_node) then
!$omp parallel workshare
          iflag_nod(node%internal_node+1:node%numnod) = 0
!$omp end parallel workshare
        end if
!
        icou = istack_ext_grp(igrp-1)
        ist = part_grp%istack_grp(igrp-1) + 1
        ied = part_grp%istack_grp(igrp)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          iflag_nod(inod) = 0
        end do
!$omp end parallel do
!
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(jnod .le. node%internal_node                             &
     &          .and. iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              item_ext_grp(icou) = jnod
              iflag_nod(jnod) =    0
            end if
          end do
        end do
      end do
!
      end subroutine set_external_grp_4_new_part
!
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
      integer(kind = kint), intent(inout)                               &
     &                     :: num_recv_tmp(nprocs,nloop)
!
      integer(kind = kint) :: i, iloop, irank_recv
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
      subroutine count_num_export_for_repart                            &
     &         (my_rank, nloop, num_grp, num_send_tmp, part_tbl)
!
      use t_calypso_comm_table
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: num_grp, nloop
      integer(kind = kint), intent(in) :: num_send_tmp(num_grp)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: i, iloop
!
      do iloop = 1, nloop
        part_tbl(iloop)%iflag_self_copy = 0
      end do
      if(num_send_tmp(my_rank+1) .gt. 0) then
        part_tbl(1)%iflag_self_copy = 1
      end if
!
      part_tbl(1)%nrank_export = 0
      do i = 1, num_grp
        if(num_send_tmp(i) .gt. 0) then
          part_tbl(1)%nrank_export = part_tbl(1)%nrank_export + 1
        end if
      end do
!
      end subroutine count_num_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine count_num_import_for_repart                            &
     &         (nprocs, nloop, num_grp, num_recv_tmp, part_tbl)
!
      use t_calypso_comm_table
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_grp, nloop
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs,nloop)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: i, iloop, inum
!
!
      do iloop = 1, nloop
        part_tbl(iloop)%nrank_import = 0
        do inum = 1, nprocs
          i = inum + (iloop-1) * nprocs
          if(i .gt. num_grp) cycle
          if(num_recv_tmp(i,iloop) .gt. 0) then
            part_tbl(iloop)%nrank_import                                &
     &          = part_tbl(iloop)%nrank_import + 1
          end if
        end do
      end do
!
      end subroutine count_num_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_istack_export_for_repart                           &
     &         (my_rank, nloop, num_grp, num_send_tmp, part_tbl)
!
      use t_calypso_comm_table
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: num_grp, nloop
      integer(kind = kint), intent(in) :: num_send_tmp(num_grp)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: i, ip, icou
!
      icou = 0
      do i = 1, num_grp
        ip = 1 + mod(i+my_rank,num_grp)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          part_tbl(1)%irank_export(icou) =  ip
          part_tbl(1)%num_export(icou) =    num_send_tmp(ip)
          part_tbl(1)%istack_export(icou)                               &
     &       = part_tbl(1)%istack_export(icou-1) + num_send_tmp(ip)
        end if
      end do
      icou = part_tbl(1)%nrank_export
      part_tbl(1)%ntot_export = part_tbl(1)%istack_export(icou)
!
      end subroutine set_istack_export_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_istack_import_for_repart(my_rank, nprocs,          &
     &          nloop, num_grp, num_recv_tmp, part_tbl)
!
      use t_calypso_comm_table
!
      integer, intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: num_grp, nloop
      integer(kind = kint), intent(in)  :: num_recv_tmp(nprocs,nloop)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: i, iloop, inum, icou, id_rank, ip
!
!
      do iloop = 1, nloop
        icou = 0
        id_rank = my_rank + (iloop-1) * nprocs
        do inum = 1, nprocs
          i = inum + (iloop-1) * nprocs
          ip = 1 + mod(i+id_rank,nprocs)
          if(i .gt. num_grp) cycle
          if(num_recv_tmp(ip,iloop) .gt. 0) then
            icou = icou + 1
            part_tbl(iloop)%irank_import(icou) =  ip
            part_tbl(iloop)%num_import(icou) = num_recv_tmp(ip,iloop)
            part_tbl(iloop)%istack_import(icou)                         &
     &       = part_tbl(iloop)%istack_import(icou-1)                    &
     &        + num_recv_tmp(ip,iloop)
          end if
        end do
        icou = part_tbl(iloop)%nrank_import
        part_tbl(iloop)%ntot_import                                     &
     &       = part_tbl(iloop)%istack_import(icou)
      end do
!
      end subroutine set_istack_import_for_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_export_item_for_repart                           &
     &         (my_rank, nloop, part_grp, num_send_tmp, part_tbl)
!
      use t_group_data
      use t_calypso_comm_table
!
      type(group_data), intent(in) :: part_grp
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nloop
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: i, ip, inum, icou, ist, jst
!
      icou = 0
      do i = 1, part_grp%num_grp
        ip = 1 + mod(i+my_rank,part_grp%num_grp)
        if(num_send_tmp(ip) .gt. 0) then
          icou = icou + 1
          ist = part_grp%istack_grp(ip-1)
          jst = part_tbl(1)%istack_export(icou-1)
          do inum = 1, part_tbl(1)%num_export(icou)
            part_tbl(1)%item_export(jst+inum)                           &
     &            = part_grp%item_grp(ist+inum)
          end do
        end if
      end do
!
      end subroutine set_export_item_for_repart
!
! ----------------------------------------------------------------------
!
      subroutine set_import_item_for_repart(nloop, part_tbl)
!
      use t_calypso_comm_table
!
      integer(kind = kint), intent(in) :: nloop
!
      type(calypso_comm_table), intent(inout) :: part_tbl(nloop)
!
      integer(kind = kint) :: iloop, inum
!
!
      do iloop = 1, nloop
        do inum = 1, part_tbl(iloop)%ntot_import
          part_tbl(iloop)%item_import(inum) = inum
          part_tbl(iloop)%irev_import(inum) = inum
        end do
      end do
!
      end subroutine set_import_item_for_repart
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
