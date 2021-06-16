!> @file  udt_to_new_partition.f90
!!      module udt_to_new_partition
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 2020
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine init_udt_to_new_partition                            &
!!     &         (new_ucd_file, new_mesh, new_ucd)
!!      subroutine udt_field_to_new_partition                           &
!!     &         (iflag_recv, istep_ucd, new_ucd_file, t_IO,            &
!!     &          new_mesh, org_to_new_tbl, org_ucd, new_ucd,           &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine finalize_udt_to_new_partition(new_ucd)
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(calypso_comm_table), intent(in) :: org_to_new_tbl
!!        type(ucd_data), intent(in) :: org_ucd
!!        type(ucd_data), intent(inout) :: new_ucd
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module udt_to_new_partition
!
      use m_precision
      use calypso_mpi
      use t_mesh_data
      use t_calypso_comm_table
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
      use t_vector_for_solver
      use t_solver_SR
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_udt_to_new_partition                              &
     &         (new_ucd_file, new_mesh, new_ucd)
!
      use transfer_to_new_partition
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
!
      type(mesh_geometry), intent(in) :: new_mesh
      type(field_IO_params), intent(in) :: new_ucd_file
      type(ucd_data), intent(inout) :: new_ucd
!
!
      call init_vector_for_repart(n_vector, new_mesh%node%numnod)
!
      call link_local_mesh_2_ucd(new_mesh%node, new_mesh%ele, new_ucd)
      call link_nnod_stacks_2_ucd(nprocs, new_mesh%node, new_ucd)
      call sel_write_parallel_ucd_mesh(new_ucd_file, new_ucd)
!
      end subroutine init_udt_to_new_partition
!
! ----------------------------------------------------------------------
!
      subroutine udt_field_to_new_partition                             &
     &         (iflag_recv, istep_ucd, new_ucd_file, t_IO,              &
     &          new_mesh, org_to_new_tbl, org_ucd, new_ucd,             &
     &          v_sol, SR_sig, SR_r)
!
      use transfer_to_new_partition
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: new_ucd_file
      type(mesh_geometry), intent(in) :: new_mesh
      type(calypso_comm_table), intent(in) :: org_to_new_tbl
      type(ucd_data), intent(in) :: org_ucd
      type(time_data), intent(in) :: t_IO
!
      type(ucd_data), intent(inout) :: new_ucd
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      new_ucd%num_field =  org_ucd%num_field
      new_ucd%ntot_comp =  org_ucd%ntot_comp
      new_ucd%num_comp =>  org_ucd%num_comp
      new_ucd%phys_name => org_ucd%phys_name
!
      call allocate_ucd_phys_data(new_ucd)
      call tensor_to_new_partition(iflag_recv,                          &
     &    org_to_new_tbl, new_mesh%nod_comm, new_ucd%ntot_comp,         &
     &    int(org_ucd%nnod), int(new_ucd%nnod),                         &
     &    org_ucd%d_ucd, new_ucd%d_ucd, v_sol, SR_sig, SR_r)
!
      call sel_write_parallel_ucd_file                                  &
         (istep_ucd, new_ucd_file, t_IO, new_ucd)
      call deallocate_ucd_phys_data(new_ucd)
      call disconnect_ucd_phys_name(new_ucd)
!
      end subroutine udt_field_to_new_partition
!
! ----------------------------------------------------------------------
!
      subroutine finalize_udt_to_new_partition(new_ucd)
!
      use transfer_to_new_partition
!
      type(ucd_data), intent(inout) :: new_ucd
!
!
      call deallocate_ucd_ele(new_ucd)
      call deallocate_ucd_node(new_ucd)
      call deallocate_vector_for_repart
      call unlink_merged_ucd_nod_stack(new_ucd)
!
      end subroutine finalize_udt_to_new_partition
!
! ----------------------------------------------------------------------
!
      end module udt_to_new_partition
