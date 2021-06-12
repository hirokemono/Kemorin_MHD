!> @file  field_to_new_partition.f90
!!      module field_to_new_partition
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 2020
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine load_or_const_new_partition(part_param,              &
!!     &          geofem, next_tbl, new_fem, repart_nod_tbl, repart_WK, &
!!     &          SR_sig, SR_r, SR_i, SR_il)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!        type(volume_partioning_work), intent(inout) :: repart_WK
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine init_fld_to_new_partition(new_mesh, org_fld, new_fld)
!!      subroutine nod_field_to_new_partition(iflag_recv,               &
!!     &          new_mesh, repart_nod_tbl, org_fld, new_fld,           &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine finalize_fld_to_new_partition(new_fld)
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(calypso_comm_table), intent(in) :: repart_nod_tbl
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use t_next_node_ele_4_node
      use t_jacobians
      use t_shape_functions
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_or_const_new_partition(part_param,                &
     &          geofem, next_tbl, new_fem, repart_nod_tbl, repart_WK,   &
     &          SR_sig, SR_r, SR_i, SR_il)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use calypso_mpi_logical
      use repartiton_by_volume
      use mesh_file_name_by_param
      use set_interpolate_file_name
      use const_element_comm_tables
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
      type(volume_partioning_work), intent(inout) :: repart_WK
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      type(communication_table) :: ele_comm_T
!
      logical :: flag
!
!
      if(my_rank .eq. 0) then
        if(part_param%viz_mesh_file%iflag_format .eq. id_no_file        &
     &    .or. part_param%trans_tbl_file%iflag_format .eq. id_no_file)  &
     &   then
        flag = .FALSE.
        else
          flag = (check_exist_mesh(my_rank, part_param%viz_mesh_file))  &
     &     .and. (check_exist_interpolate_file(my_rank,                 &
     &                                      part_param%trans_tbl_file))
        end if
      end if
      call calypso_MPI_barrier
      call calypso_mpi_bcast_one_logical(flag, 0)
!
      if(flag) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
        call load_repartitoned_file(part_param, geofem, new_fem,        &
     &                              repart_nod_tbl)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
      else
!  -----  Const Element communication table
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
        write(e_message,*)                                              &
     &      'Construct repartitioned mesh and transfer table'
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_ele_comm_table                                       &
     &     (geofem%mesh%node, geofem%mesh%nod_comm, geofem%mesh%ele,    &
     &      ele_comm_T, SR_sig, SR_r, SR_i, SR_il)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
!  -----  Re-partitioning
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
        if(iflag_debug .gt. 0) write(*,*) 's_repartiton_by_volume'
        call s_repartiton_by_volume                                     &
     &     (part_param, geofem, ele_comm_T, next_tbl,                   &
     &      new_fem, repart_nod_tbl, repart_WK)
        call dealloc_comm_table(ele_comm_T)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
      end if
!
      end subroutine load_or_const_new_partition
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
      call alloc_phys_name(new_fld)
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
      call alloc_phys_data(new_mesh%node%numnod, new_fld)
!
      end subroutine init_fld_to_new_partition
!
! ----------------------------------------------------------------------
!
      subroutine nod_field_to_new_partition(iflag_recv,                 &
     &          new_mesh, repart_nod_tbl, org_fld, new_fld,             &
     &          v_sol, SR_sig, SR_r)
!
      use transfer_to_new_partition
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(mesh_geometry), intent(in) :: new_mesh
      type(calypso_comm_table), intent(in) :: repart_nod_tbl
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i_fld, i_comp
!
!
      do i_fld = 1, new_fld%num_phys_viz
        i_comp = new_fld%istack_component(i_fld-1) + 1
        if     (new_fld%num_component(i_fld) .eq. n_scalar) then
          call scalar_to_new_partition(iflag_recv, repart_nod_tbl,      &
     &        new_mesh%nod_comm, org_fld%n_point, new_fld%n_point,      &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp),         &
     &        v_sol, SR_sig, SR_r)
        else if(new_fld%num_component(i_fld) .eq. n_vector) then
          call vector_to_new_partition(iflag_recv, repart_nod_tbl,      &
     &        new_mesh%nod_comm, org_fld%n_point, new_fld%n_point,      &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp),         &
     &        v_sol, SR_sig, SR_r)
        else
          call tensor_to_new_partition(iflag_recv, repart_nod_tbl,      &
     &        new_mesh%nod_comm, new_fld%num_component(i_fld),          &
     &        org_fld%n_point, new_fld%n_point,                         &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp),         &
     &        v_sol, SR_sig, SR_r)
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
      call dealloc_phys_data(new_fld)
!
      end subroutine finalize_fld_to_new_partition
!
! ----------------------------------------------------------------------
!
      end module field_to_new_partition
