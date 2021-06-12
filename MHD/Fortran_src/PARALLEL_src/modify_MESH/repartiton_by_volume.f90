!>@file   repartiton_by_volume.f90
!!@brief  module repartiton_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Dec., 2020
!!
!>@brief Repartitioning based on volume
!!
!!@verbatim
!!      subroutine s_repartiton_by_volume                               &
!!     &         (part_param, geofem, ele_comm, next_tbl,               &
!!     &          new_fem, repart_nod_tbl, repart_WK)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!        type(volume_partioning_work), intent(inout) :: repart_WK
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
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_repartiton_by_volume                                 &
     &         (part_param, geofem, ele_comm, next_tbl,                 &
     &          new_fem, repart_nod_tbl, repart_WK)
!
      use t_next_node_ele_4_node
      use t_interpolate_table
!
      use m_solver_SR
      use m_file_format_switch
      use m_elapsed_labels_4_REPART
      use m_work_time
!
      use sleeve_extend
      use parallel_FEM_mesh_init
      use mesh_repartition_by_volume
      use mesh_MPI_IO_select
      use parallel_itp_tbl_IO_select
      use itp_table_file_IO_select
      use copy_repart_and_itp_table
      use copy_repart_ele_and_itp_tbl
      use const_element_comm_tables
      use nod_and_ele_derived_info
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
      type(volume_partioning_work), intent(inout) :: repart_WK
!
      type(interpolate_table) :: itp_nod_tbl_IO
      type(communication_table) :: new_ele_comm
!
!  -------------------------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+2)
      call s_mesh_repartition_by_volume                                 &
     &   (geofem, ele_comm, next_tbl%neib_nod, part_param,              &
     &    new_fem%mesh, new_fem%group, repart_nod_tbl, repart_WK)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+2)
!
! Increase sleeve size
      if(part_param%sleeve_exp_p%iflag_expand_mode                      &
     &                         .ne. iflag_turn_off) then
        call set_nod_and_ele_infos(new_fem%mesh%node, new_fem%mesh%ele)
!
        call const_ele_comm_table(new_fem%mesh%node,                    &
     &      new_fem%mesh%nod_comm, new_fem%mesh%ele, new_ele_comm,      &
     &      SR_sig1, SR_r1, SR_i1, SR_il1)
!
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+3)
        call sleeve_extension_loop(part_param%sleeve_exp_p,             &
     &      new_fem%mesh, new_fem%group, new_ele_comm,                  &
     &      repart_WK%sleeve_exp_WK, SR_sig1, SR_r1, SR_i1, SR_il1)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+3)
!
        call dealloc_comm_table(new_ele_comm)
      end if
!
!       Output new mesh file
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
      if(part_param%viz_mesh_file%iflag_format .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &          'No repartitioned mesh data output'
      else
        call sel_mpi_write_mesh_file(part_param%viz_mesh_file,          &
     &                               new_fem%mesh, new_fem%group)
      end if
      call calypso_MPI_barrier
!
!       Output data transfer table
      if(part_param%trans_tbl_file%iflag_format .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &          'No transfer table output for repartition'
      else
        call copy_repart_tbl_to_itp_table(geofem%mesh,                  &
     &      next_tbl%neib_ele, repart_nod_tbl, itp_nod_tbl_IO)
        call sel_mpi_write_interpolate_table(my_rank,                   &
     &      part_param%trans_tbl_file, itp_nod_tbl_IO)
        call dealloc_itp_tbl_after_write(itp_nod_tbl_IO)
      end if
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
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
      use copy_repart_ele_and_itp_tbl
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!
      type(interpolate_table) :: itp_nod_tbl_IO
!
      integer(kind= kint) :: irank_read
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
