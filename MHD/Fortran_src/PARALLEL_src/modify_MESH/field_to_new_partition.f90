!> @file  field_to_new_partition.f90
!!      module field_to_new_partition
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 2020
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine const_new_partition_mesh(part_param, geofem, new_fem,&
!!     &                                    org_to_new_tbl, v_sol)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(inout) :: geofem
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: org_to_new_tbl
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!      subroutine init_fld_to_new_partition(new_mesh, org_fld, new_fld)
!!      subroutine nod_field_to_new_partition(iflag_recv,               &
!!     &          new_mesh, org_to_new_tbl, org_fld, new_fld, v_sol)
!!      subroutine finalize_fld_to_new_partition(new_fld)
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(calypso_comm_table), intent(in) :: org_to_new_tbl
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module field_to_new_partition
!
      use m_precision
      use calypso_mpi
      use t_mesh_data
      use t_calypso_comm_table
      use t_phys_data
      use t_control_param_vol_grping
      use t_vector_for_solver
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_new_partition_mesh(part_param, geofem, new_fem,  &
     &                                    org_to_new_tbl, v_sol)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use calypso_mpi_logical
      use repartiton_by_volume
      use mesh_file_name_by_param
      use set_interpolate_file_name
!
      type(volume_partioning_param), intent(in) ::  part_param
!
      type(mesh_data), intent(inout) :: geofem
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: org_to_new_tbl
      type(vectors_4_solver), intent(inout) :: v_sol
!
      logical :: flag
!
!
      if(my_rank .eq. 0) then
        flag = (check_exist_mesh(my_rank, part_param%viz_mesh_file))    &
     &   .and. (check_exist_interpolate_file(my_rank,                   &
     &                                      part_param%trans_tbl_file))
      end if
      call calypso_MPI_barrier
      call calypso_mpi_bcast_one_logical(flag, 0)
!
      if(flag) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
        call load_repartitoned_file(part_param, geofem, new_fem,        &
     &                              org_to_new_tbl)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
      else
        write(e_message,*)                                              &
     &      'Construct repartitioned mesh and transfer table'
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
        call s_repartiton_by_volume(part_param, geofem, new_fem,        &
     &                              org_to_new_tbl, v_sol)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
      end if
!
      end subroutine const_new_partition_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_fld_to_new_partition(new_mesh, org_fld, new_fld)
!
      use transfer_to_new_partition
!
      type(mesh_geometry), intent(in) :: new_mesh
      type(phys_data), intent(in) :: org_fld
      type(phys_data), intent(inout) :: new_fld
!
!
      call init_vector_for_repart(n_vector, new_mesh%node%numnod)
!
      new_fld%num_phys =   org_fld%num_phys_viz
      new_fld%ntot_phys =  org_fld%ntot_phys_viz
      new_fld%num_phys_viz =   org_fld%num_phys_viz
      new_fld%ntot_phys_viz =  org_fld%ntot_phys_viz
!
      call alloc_phys_name_type(new_fld)
!
      new_fld%num_component(1:new_fld%num_phys)                         &
     &             = org_fld%num_component(1:new_fld%num_phys)
      new_fld%phys_name(1:new_fld%num_phys)                             &
     &             = org_fld%phys_name(1:new_fld%num_phys)
      new_fld%flag_monitor(1:new_fld%num_phys)                          &
     &             = org_fld%flag_monitor(1:new_fld%num_phys)
      new_fld%iorder_eletype(1:new_fld%num_phys)                        &
     &             = org_fld%iorder_eletype(1:new_fld%num_phys)
      new_fld%istack_component(0:new_fld%num_phys)                      &
     &             = org_fld%istack_component(0:new_fld%num_phys)
!
      call alloc_phys_data_type(new_mesh%node%numnod, new_fld)
!
      end subroutine init_fld_to_new_partition
!
! ----------------------------------------------------------------------
!
      subroutine nod_field_to_new_partition(iflag_recv,                 &
     &          new_mesh, org_to_new_tbl, org_fld, new_fld, v_sol)
!
      use transfer_to_new_partition
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(mesh_geometry), intent(in) :: new_mesh
      type(calypso_comm_table), intent(in) :: org_to_new_tbl
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, new_fld%num_phys_viz
        i_comp = new_fld%istack_component(i_fld) + 1
        if     (new_fld%num_component(i_fld) .eq. n_scalar) then
          call scalar_to_new_partition(iflag_recv, org_to_new_tbl,      &
     &        new_mesh%nod_comm, org_fld%n_point, new_fld%n_point,      &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp), v_sol)
        else if(new_fld%num_component(i_fld) .eq. n_vector) then
          call vector_to_new_partition(iflag_recv, org_to_new_tbl,      &
     &        new_mesh%nod_comm, org_fld%n_point, new_fld%n_point,      &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp), v_sol)
        else
          call tensor_to_new_partition(iflag_recv, org_to_new_tbl,      &
     &        new_mesh%nod_comm, new_fld%num_component(i_fld),          &
     &        org_fld%n_point, new_fld%n_point,                         &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp), v_sol)
        end if
      end do
!
      end subroutine nod_field_to_new_partition
!
! ----------------------------------------------------------------------
!
      subroutine finalize_fld_to_new_partition(new_fld)
!
      use transfer_to_new_partition
!
      type(phys_data), intent(inout) :: new_fld
!
!
      call deallocate_vector_for_repart
      call dealloc_phys_data_type(new_fld)
!
      end subroutine finalize_fld_to_new_partition
!
! ----------------------------------------------------------------------
!
      end module field_to_new_partition
