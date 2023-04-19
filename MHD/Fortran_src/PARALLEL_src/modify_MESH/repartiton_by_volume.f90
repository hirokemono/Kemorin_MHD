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
      type(calypso_comm_table) :: repart_ele_tbl
      integer(kind = kint) :: ierr
      integer :: nnod_tot_org, nele_tot_org, nnod_tot_new, nele_tot_new
      integer :: nele_ele_tbl
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
!
      call dealloc_comm_table(new_ele_comm)
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
!
!       Output data transfer table
      if(part_param%trans_tbl_file%iflag_format .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &          'No transfer table output for repartition'
      else
        if(i_debug .gt. 0) then
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
          if(my_rank .eq. 0) write(*,*) 'Total: ',                      &
     &     nnod_tot_org, nele_tot_org, nnod_tot_new, nele_tot_new,      &
     &     nele_ele_tbl
          write(*,*) my_rank, 'old nums: ',                             &
     &             mesh%node%numnod, mesh%node%internal_node,           &
     &     repart_nod_tbl%ntot_export, mesh%nod_comm%ntot_import
          write(*,*) my_rank, 'old element: ',                          &
     &     mesh%ele%numele, mesh%ele%internal_ele,                      &
     &     repart_ele_tbl%ntot_export, ele_comm%ntot_import
          write(*,*) my_rank, 'new node: ',                             &
     &     new_mesh%node%numnod, new_mesh%node%internal_node,           &
     &     repart_nod_tbl%ntot_import, new_mesh%nod_comm%ntot_import
          write(*,*) my_rank, 'new element: ',                          &
     &     new_mesh%ele%numele, new_mesh%ele%internal_ele,              &
     &     repart_ele_tbl%ntot_import, new_ele_comm%ntot_import,        &
     &     maxval(repart_ele_tbl%item_import),                          &
     &     maxval(new_ele_comm%item_import),                            &
     &     max(maxval(repart_ele_tbl%item_import),                      &
     &     maxval(new_ele_comm%item_import))
        end if
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
