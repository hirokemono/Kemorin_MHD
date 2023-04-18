!>@file   repartiton_by_volume.f90
!!@brief  module repartiton_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Dec., 2020
!!
!>@brief Repartitioning based on volume
!!
!!@verbatim
!!      subroutine s_repartiton_by_volume(flag_lic_dump, part_param,    &
!!     &          mesh, group, ele_comm, next_tbl, num_mask, masking,   &
!!     &          ref_repart, d_mask, ref_vect_sleeve_ext,              &
!!     &          new_mesh, new_group, repart_nod_tbl,                  &
!!     &          sleeve_exp_WK, m_SR)
!!        logical, intent(in) :: flag_lic_dump
!!        integer(kind = kint), intent(in) :: num_mask
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(masking_parameter), intent(in) :: masking(num_mask)
!!        real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: d_mask(mesh%node%numnod,num_mask)
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: ref_vect_sleeve_ext(mesh%node%numnod,3)
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!        type(mesh_groups), intent(inout) :: new_group
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!        type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine load_repartitoned_file                               &
!!     &         (part_param, geofem, new_fem, repart_nod_tbl)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!@endverbatim
!
      module repartiton_by_volume
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
      use t_control_param_vol_grping
      use t_ctl_param_sleeve_extend
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_repartiton_by_volume(flag_lic_dump, part_param,      &
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
      type(calypso_comm_table) :: repart_ele_tbl, repart_ele_tbl_2
      integer(kind = kint) :: ierr
      integer :: nnod_tot_org, nele_tot_org, nnod_tot_new, nele_tot_new
      integer :: nele_ele_tbl
!
      type(node_ele_double_number) :: inod_dbl_org, iele_dbl_org
      type(node_ele_double_number) :: inod_dbl_new, iele_dbl_new
      type(node_ele_double_number) :: inod_dbl_org_on_new
      type(node_ele_double_number) :: iele_dbl_org_on_new
      integer(kind = kint), allocatable :: ip_recv_tmp(:)
      integer(kind = kint), allocatable :: ip_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:)
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: iele_sort(:)
      integer(kind = kint_gl), allocatable :: iele_gl_org(:)
      integer(kind = kint) :: iele, ist, ied, num, ip, irank, inum
      integer(kind = kint) :: iele_org, irank_org
      integer(kind = kint) ::icou_recv, icou_send
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
      call alloc_double_numbering(mesh%node%numnod, inod_dbl_org)
      call alloc_double_numbering(mesh%ele%numele, iele_dbl_org)
      call set_node_ele_double_address                                  &
     &   (mesh%node, mesh%ele, mesh%nod_comm, ele_comm,                 &
     &    inod_dbl_org, iele_dbl_org, m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_double_numbering(new_mesh%node%numnod, inod_dbl_new)
      call alloc_double_numbering(new_mesh%ele%numele, iele_dbl_new)
      call set_node_ele_double_address                                  &
     &   (new_mesh%node, new_mesh%ele, new_mesh%nod_comm, new_ele_comm, &
     &    inod_dbl_new, iele_dbl_new, m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_double_numbering(new_mesh%node%numnod,                 &
     &                            inod_dbl_org_on_new)
      call calypso_SR_type_int(iflag_import_item, repart_nod_tbl,       &
     &    mesh%node%numnod, new_mesh%node%numnod,                       &
     &    inod_dbl_org%index(1), inod_dbl_org_on_new%index(1),          &
     &    m_SR%SR_sig, m_SR%SR_i)
      call calypso_SR_type_int(iflag_import_item, repart_nod_tbl,       &
     &    mesh%node%numnod, new_mesh%node%numnod,                       &
     &    inod_dbl_org%irank(1), inod_dbl_org_on_new%irank(1),          &
     &    m_SR%SR_sig, m_SR%SR_i)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm,                      &
     &    m_SR%SR_sig, m_SR%SR_i, inod_dbl_org_on_new%index(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm,                      &
     &    m_SR%SR_sig, m_SR%SR_i, inod_dbl_org_on_new%irank(1))
!
      call alloc_double_numbering(new_mesh%ele%numele,                  &
     &                            iele_dbl_org_on_new)
      call calypso_SR_type_int(iflag_import_item, repart_ele_tbl,       &
     &    mesh%ele%numele, new_mesh%ele%numele,                         &
     &    iele_dbl_org%index(1), iele_dbl_org_on_new%index(1),          &
     &    m_SR%SR_sig, m_SR%SR_i)
      call calypso_SR_type_int(iflag_import_item, repart_ele_tbl,       &
     &    mesh%ele%numele, new_mesh%ele%numele,                         &
     &    iele_dbl_org%irank(1), iele_dbl_org_on_new%irank(1),          &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%ele%numele, new_ele_comm,                            &
     &    m_SR%SR_sig, m_SR%SR_i, iele_dbl_org_on_new%index(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%ele%numele, new_ele_comm,                            &
     &    m_SR%SR_sig, m_SR%SR_i, iele_dbl_org_on_new%irank(1))
      call dealloc_comm_table(new_ele_comm)
      call calypso_mpi_barrier
!
      allocate(num_recv_tmp(0:nprocs-1))
      num_recv_tmp(0:nprocs-1) = 0
      do iele = 1, new_mesh%ele%numele
        irank = iele_dbl_org_on_new%irank(iele)
        num_recv_tmp(irank) = num_recv_tmp(irank) + 1
      end do
!
      allocate(num_send_tmp(0:nprocs-1))
      num_send_tmp(0:nprocs-1) = 0
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_recv_tmp(0), num_send_tmp(0))
!
      icou_recv = 0
      icou_send = 0
      do irank = 0, nprocs-1
        if(num_recv_tmp(irank) .gt. 0) icou_recv = icou_recv + 1
        if(num_send_tmp(irank) .gt. 0) icou_send = icou_send + 1
      end do
!
      repart_ele_tbl_2%nrank_import = icou_recv
      call alloc_calypso_import_num(repart_ele_tbl_2)
!
      repart_ele_tbl_2%nrank_export = icou_send
      call alloc_calypso_export_num(repart_ele_tbl_2)
!
      allocate(ip_send_tmp(0:nprocs-1))
      allocate(ip_recv_tmp(0:nprocs-1))
      ip_send_tmp(0:nprocs-1) = -1
      ip_recv_tmp(0:nprocs-1) = -1
      icou_recv = 0
      icou_send = 0
      do ip = 1, nprocs
        irank = mod(my_rank+ip,nprocs)
        if(num_recv_tmp(irank) .gt. 0) then
          icou_recv = icou_recv + 1
          repart_ele_tbl_2%irank_import(icou_recv) = irank
          repart_ele_tbl_2%num_import(icou_recv) = num_recv_tmp(irank)
          ip_recv_tmp(irank) = icou_recv
        end if
        if(num_send_tmp(irank) .gt. 0) then
          icou_send = icou_send + 1
          repart_ele_tbl_2%irank_export(icou_send) = irank
          repart_ele_tbl_2%num_export(icou_recv) = num_send_tmp(irank)
          ip_send_tmp(irank) = icou_send
        end if
      end do
      repart_ele_tbl_2%iflag_self_copy = 0
      if(num_recv_tmp(my_rank) .gt. 0) then
        repart_ele_tbl_2%iflag_self_copy = 1
      end if
!
      call s_cal_total_and_stacks(repart_ele_tbl_2%nrank_import,        &
     &                            repart_ele_tbl_2%num_import, izero,   &
     &                            repart_ele_tbl_2%istack_import,       &
     &                            repart_ele_tbl_2%ntot_import)
      call s_cal_total_and_stacks(repart_ele_tbl_2%nrank_export,        &
     &                            repart_ele_tbl_2%num_export, izero,   &
     &                            repart_ele_tbl_2%istack_export,       &
     &                            repart_ele_tbl_2%ntot_export)
!
      go to 100
      call alloc_calypso_import_item(repart_ele_tbl_2)
      call alloc_calypso_import_rev(new_mesh%ele%numele,                &
     &                              repart_ele_tbl_2)
      call alloc_calypso_export_item(repart_ele_tbl_2)
!
!      write(*,*) my_rank, 'repart_ele_tbl_2%ntot_import', &
!     &  repart_ele_tbl_2%ntot_import, new_mesh%ele%numele
!
      allocate(iele_sort(repart_ele_tbl_2%ntot_import))
      iele_sort(1:repart_ele_tbl_2%ntot_import) = 0
      num_recv_tmp(0:nprocs-1) = 0
      do iele = 1, new_mesh%ele%numele
        iele_org =  iele_dbl_org_on_new%irank(iele)
        irank_org = iele_dbl_org_on_new%irank(iele)
        num_recv_tmp(irank_org) = num_recv_tmp(irank_org) + 1
        ip = ip_recv_tmp(irank_org)
        inum = repart_ele_tbl_2%istack_import(ip-1)                     &
     &        + num_recv_tmp(irank_org)
        repart_ele_tbl_2%item_import(inum) = iele
        iele_sort(inum) = iele_org
      end do
!
      do ip = 1, repart_ele_tbl_2%nrank_import
        ist = repart_ele_tbl_2%istack_import(ip-1) + 1
        ied = repart_ele_tbl_2%istack_import(ip  )
        if((ied-ist) .gt. 0) then
          call quicksort_w_index(repart_ele_tbl_2%ntot_import,          &
     &        iele_sort, ist, ied, repart_ele_tbl_2%item_import)
        end if
      end do
!
      call calypso_mpi_barrier
      call comm_items_send_recv                                         &
     &   (repart_ele_tbl_2%nrank_import, repart_ele_tbl_2%irank_import, &
     &    repart_ele_tbl_2%istack_import, iele_sort,                    &
     &    repart_ele_tbl_2%nrank_export, repart_ele_tbl_2%irank_export, &
     &    repart_ele_tbl_2%istack_export,                               &
     &    repart_ele_tbl_2%iflag_self_copy,                             &
     &    repart_ele_tbl_2%item_export, m_SR%SR_sig)
      call calypso_mpi_barrier
!
      allocate(iele_gl_org(new_mesh%ele%numele))
      iele_gl_org(1:new_mesh%ele%numele) = 0
      call calypso_SR_type_int8(iflag_import_item, repart_ele_tbl_2,    &
     &                          mesh%ele%numele, new_mesh%ele%numele,   &
     &                          mesh%ele%iele_global, iele_gl_org,      &
     &                          m_SR%SR_sig, m_SR%SR_il)
      call calypso_mpi_barrier
!

      inum = 0
      do iele = 1, new_mesh%ele%numele
        if(iele_gl_org(iele) .ne. new_mesh%ele%iele_global(iele)) inum = inum+1
      end do
      write(*,*) my_rank, 'errror table: ', inum
 100  continue
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
!
!       Output data transfer table
      if(part_param%trans_tbl_file%iflag_format .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &          'No transfer table output for repartition'
      else
          call calypso_mpi_reduce_one_int                               &
     &       (mesh%node%internal_node, nnod_tot_org, MPI_SUM, 0)
          call calypso_mpi_reduce_one_int                               &
     &       (mesh%ele%internal_ele, nele_tot_org, MPI_SUM, 0)
          call calypso_mpi_reduce_one_int                               &
     &       (new_mesh%node%internal_node, nnod_tot_new, MPI_SUM, 0)
          call calypso_mpi_reduce_one_int                               &
     &       (new_mesh%ele%internal_ele, nele_tot_new, MPI_SUM, 0)
          call calypso_mpi_reduce_one_int                               &
     &       (repart_ele_tbl%ntot_import, nele_ele_tbl, MPI_SUM, 0)
        if(my_rank .eq. 0) write(*,*) 'Total: ', &
     &     nnod_tot_org, nele_tot_org, nnod_tot_new, nele_tot_new, &
     &     nele_ele_tbl
        write(*,*) my_rank, 'old nums: ', &
     &             mesh%node%numnod, mesh%node%internal_node, &
     &     repart_nod_tbl%ntot_export, mesh%nod_comm%ntot_import
        write(*,*) my_rank, 'old element: ', &
     &     mesh%ele%numele, mesh%ele%internal_ele, &
     &     repart_ele_tbl%ntot_export, ele_comm%ntot_import
        write(*,*) my_rank, 'new node: ', &
     &     new_mesh%node%numnod, new_mesh%node%internal_node, &
     &     repart_nod_tbl%ntot_import, new_mesh%nod_comm%ntot_import
        write(*,*) my_rank, 'new element: ', &
     &     new_mesh%ele%numele, new_mesh%ele%internal_ele, &
     &     repart_ele_tbl%ntot_import, new_ele_comm%ntot_import, &
     &     maxval(repart_ele_tbl%item_import), &
     &     maxval(new_ele_comm%item_import), &
     &     max(maxval(repart_ele_tbl%item_import),                      &
     &     maxval(new_ele_comm%item_import))
!
        call output_repart_table(part_param%trans_tbl_file,             &
     &      new_mesh%ele%numele, repart_nod_tbl, repart_ele_tbl,        &
     &      new_mesh%nod_comm, new_ele_comm)
      end if
!
      call dealloc_calypso_comm_table(repart_ele_tbl)
      call calypso_MPI_barrier
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
!
      end subroutine s_repartiton_by_volume
!
! ----------------------------------------------------------------------
!
      subroutine load_repartitoned_file                                 &
     &         (part_param, geofem, new_fem, repart_nod_tbl)
!
      use t_interpolate_table
      use m_file_format_switch
!
      use mpi_load_mesh_data
      use para_itrplte_table_IO_sel
      use copy_repart_and_itp_table
      use itrplte_tbl_coef_IO_select
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!
      type(interpolate_table) :: itp_nod_tbl_IO
!
      integer(kind= kint) :: irank_read, ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh for new mesh'
      call mpi_input_mesh(part_param%viz_mesh_file, nprocs, new_fem)
!
      if (iflag_debug.gt.0) write(*,*) 'sel_mpi_read_dbl_itp_table'
      call sel_mpi_read_interpolate_table                               &
     &   (my_rank, nprocs, part_param%trans_tbl_file,                   &
     &    itp_nod_tbl_IO, ierr)
      call calypso_MPI_barrier
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    geofem%mesh, new_fem%mesh, itp_nod_tbl_IO, repart_nod_tbl)
      call dealloc_itp_tbl_after_write(itp_nod_tbl_IO)
      call calypso_MPI_barrier
!
      end subroutine load_repartitoned_file
!
! ----------------------------------------------------------------------
!
      end module repartiton_by_volume
