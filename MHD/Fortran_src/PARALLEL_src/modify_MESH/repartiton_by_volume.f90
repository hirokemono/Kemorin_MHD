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
!!     &         (part_param, geofem, ele_comm_T, next_tbl_T,           &
!!     &          new_fem, org_to_new_tbl)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm_T
!!        type(next_nod_ele_table), intent(in) :: next_tbl_T
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: org_to_new_tbl
!!      subroutine load_repartitoned_file(part_param, geofem, new_fem,  &
!!     &                                  org_to_new_tbl)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: org_to_new_tbl
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
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_repartiton_by_volume                                 &
     &         (part_param, geofem, ele_comm_T, next_tbl_T,             &
     &          new_fem, org_to_new_tbl)
!
      use t_next_node_ele_4_node
      use t_interpolate_table
!
      use m_file_format_switch
      use m_elapsed_labels_4_REPART
      use m_work_time
!
      use parallel_sleeve_extension
      use parallel_FEM_mesh_init
      use mesh_repartition_by_volume
      use mesh_MPI_IO_select
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm_T
      type(next_nod_ele_table), intent(in) :: next_tbl_T
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: org_to_new_tbl
!
      integer(kind = kint) :: i_level
      type(interpolate_table) :: itp_tbl_IO
!
!  -------------------------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+2)
      call s_mesh_repartition_by_volume                                 &
     &   (geofem, ele_comm_T, next_tbl_T%neib_nod,                      &
     &    part_param, new_fem, org_to_new_tbl)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+2)
!
! Increase sleeve size
      if(part_param%num_FEM_sleeve .le. 1) return
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+3)
      call sleeve_extension_loop(part_param%num_FEM_sleeve,             &
     &                           new_fem%mesh, new_fem%group)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+3)
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
     &      next_tbl_T%neib_ele, org_to_new_tbl, itp_tbl_IO)
        call sel_mpi_write_interpolate_table(my_rank,                   &
     &      part_param%trans_tbl_file, itp_tbl_IO)
        call dealloc_itp_tbl_for_repart(itp_tbl_IO)
      end if
      call calypso_MPI_barrier
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
!
      end subroutine s_repartiton_by_volume
!
! ----------------------------------------------------------------------
!
      subroutine load_repartitoned_file(part_param, geofem, new_fem,    &
     &                                  org_to_new_tbl)
!
      use t_interpolate_table
      use m_file_format_switch
!
      use mpi_load_mesh_data
      use parallel_itp_tbl_IO_select
      use copy_repart_and_itp_table
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: org_to_new_tbl
!
      type(interpolate_table) :: itp_tbl_IO
!
      integer(kind= kint) :: irank_read
!
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh for new mesh'
      call mpi_input_mesh(part_param%viz_mesh_file, nprocs, new_fem)
!
      if (iflag_debug.gt.0) write(*,*) 'sel_mpi_read_interpolate_table'
      call sel_mpi_read_interpolate_table(my_rank, nprocs,              &
     &    part_param%trans_tbl_file, itp_tbl_IO, ierr)
      call calypso_MPI_barrier
!
      irank_read = my_rank
      call copy_itp_table_to_repart_tbl(irank_read,                     &
     &    geofem%mesh, new_fem%mesh, itp_tbl_IO, org_to_new_tbl)
      call dealloc_itp_tbl_for_repart(itp_tbl_IO)
      call calypso_MPI_barrier
!
      end subroutine load_repartitoned_file
!
! ----------------------------------------------------------------------
!
      end module repartiton_by_volume
