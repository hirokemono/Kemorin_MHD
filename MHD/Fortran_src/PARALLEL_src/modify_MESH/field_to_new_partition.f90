!> @file  field_to_new_partition.f90
!!      module field_to_new_partition
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 2020
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine load_or_const_new_partition                          &
!!     &         (flag_lic_dump, part_param, geofem, ele_comm, next_tbl,&
!!     &          num_mask, masking, ref_repart, d_mask,                &
!!     &          ref_vect_sleeve_ext, new_fem, new_ele_comm,           &
!!     &          repart_nod_tbl, repart_ele_tbl, sleeve_exp_WK, m_SR)
!!        logical, intent(in) :: flag_lic_dump
!!        integer(kind = kint), intent(in) :: num_mask
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(masking_parameter), intent(in) :: masking(num_mask)
!!        real(kind = kreal), intent(in)                                &
!!     &             :: ref_repart(geofem%mesh%node%numnod)
!!        real(kind = kreal), intent(in)                                &
!!     &             :: d_mask(geofem%mesh%node%numnod,num_mask)
!!        real(kind = kreal), intent(in)                                &
!!     &             :: ref_vect_sleeve_ext(geofem%mesh%node%numnod,3)
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!        type(calypso_comm_table), intent(inout) :: repart_ele_tbl
!!        type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine init_fld_to_new_partition(new_mesh, org_fld, new_fld)
!!      subroutine nod_field_to_new_partition(iflag_recv,               &
!!     &          new_mesh, repart_nod_tbl, org_fld, new_fld, m_SR)
!!      subroutine finalize_fld_to_new_partition(new_fld)
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(calypso_comm_table), intent(in) :: repart_nod_tbl
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine scalar_to_original_partition                         &
!!     &         (transfer_tbl, org_nod_comm, nnod_new, nnod_org,       &
!!     &          vec_new, vec_org, v_sol, SR_sig, SR_r)
!!        type(calypso_comm_table), intent(in) :: transfer_tbl
!!        type(communication_table), intent(in) :: org_nod_comm
!!        integer(kind = kint), intent(in) :: nnod_org, nnod_new
!!        real(kind = kreal), intent(in) :: vec_new(nnod_new)
!!        real(kind = kreal), intent(inout) :: vec_org(nnod_org)
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine vector_to_original_partition                         &
!!     &         (transfer_tbl, org_nod_comm, nnod_new, nnod_org,       &
!!     &          vec_new, vec_org, v_sol, SR_sig, SR_r)
!!        type(calypso_comm_table), intent(in) :: transfer_tbl
!!        type(communication_table), intent(in) :: org_nod_comm
!!        integer(kind = kint), intent(in) :: nnod_org, nnod_new
!!        real(kind = kreal), intent(in) :: vec_new(3*nnod_new)
!!        real(kind = kreal), intent(inout) :: vec_org(3*nnod_org)
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
      use t_next_node_ele_4_node
      use t_jacobians
      use t_shape_functions
      use t_mesh_SR
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_or_const_new_partition                            &
     &         (flag_lic_dump, part_param, geofem, ele_comm, next_tbl,  &
     &          num_mask, masking, ref_repart, d_mask,                  &
     &          ref_vect_sleeve_ext, new_fem, new_ele_comm,             &
     &          repart_nod_tbl, repart_ele_tbl, sleeve_exp_WK, m_SR)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use calypso_mpi_logical
      use repartiton_by_volume
      use mesh_file_name_by_param
      use sel_repartition_table_IO
!
      logical, intent(in) :: flag_lic_dump
      integer(kind = kint), intent(in) :: num_mask
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in)                                    &
     &           :: ref_repart(geofem%mesh%node%numnod)
      real(kind = kreal), intent(in)                                    &
     &           :: d_mask(geofem%mesh%node%numnod,num_mask)
      real(kind = kreal), intent(in)                                    &
     &           :: ref_vect_sleeve_ext(geofem%mesh%node%numnod,3)
!
      type(mesh_data), intent(inout) :: new_fem
      type(communication_table), intent(inout) :: new_ele_comm
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
      type(calypso_comm_table), intent(inout) :: repart_ele_tbl
      type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
      type(mesh_SR), intent(inout) :: m_SR
!
      logical :: flag_t, flag_m
!
!
      if(my_rank .eq. 0) then
        if(part_param%trans_tbl_file%iflag_format .eq. id_no_file) then
          flag_t = .FALSE.
        else
          flag_t = check_exist_repart_table_file(my_rank,               &
     &                                      part_param%trans_tbl_file)
        end if
        if(part_param%viz_mesh_file%iflag_format .eq. id_no_file) then
          flag_m = .FALSE.
        else
          flag_m = check_exist_mesh(my_rank, part_param%viz_mesh_file)
        end if
      end if
      call calypso_MPI_barrier
      call calypso_mpi_bcast_one_logical(flag_t, 0)
      call calypso_mpi_bcast_one_logical(flag_m, 0)
!
!
      if(flag_t) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+6)
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'load_repartitoned_table_mesh'
        call load_repartitoned_table_mesh(flag_m,                       &
     &      part_param, geofem, ele_comm, new_fem, new_ele_comm,        &
     &      repart_nod_tbl, repart_ele_tbl, m_SR)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+6)
      else
!
!  -----  Re-partitioning
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
        if(iflag_debug .gt. 0) write(*,*) 's_repartiton_by_volume'
        call s_repartiton_by_volume(flag_lic_dump, part_param,          &
     &     geofem%mesh, geofem%group, ele_comm, next_tbl,               &
     &     num_mask, masking, ref_repart, d_mask, ref_vect_sleeve_ext,  &
     &     new_fem%mesh, new_fem%group, repart_nod_tbl, repart_ele_tbl, &
     &     sleeve_exp_WK, m_SR)
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
     &          new_mesh, repart_nod_tbl, org_fld, new_fld, m_SR)
!
      use transfer_to_new_partition
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(mesh_geometry), intent(in) :: new_mesh
      type(calypso_comm_table), intent(in) :: repart_nod_tbl
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
      type(mesh_SR), intent(inout) :: m_SR
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
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else if(new_fld%num_component(i_fld) .eq. n_vector) then
          call vector_to_new_partition(iflag_recv, repart_nod_tbl,      &
     &        new_mesh%nod_comm, org_fld%n_point, new_fld%n_point,      &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp),         &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        else
          call tensor_to_new_partition(iflag_recv, repart_nod_tbl,      &
     &        new_mesh%nod_comm, new_fld%num_component(i_fld),          &
     &        org_fld%n_point, new_fld%n_point,                         &
     &        org_fld%d_fld(1,i_comp), new_fld%d_fld(1,i_comp),         &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
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
! ----------------------------------------------------------------------
!
      subroutine scalar_to_original_partition                           &
     &         (transfer_tbl, org_nod_comm, nnod_new, nnod_org,         &
     &          vec_new, vec_org, v_sol, SR_sig, SR_r)
!
      use calypso_SR_type
      use solver_SR_type
      use calypso_reverse_send_recv
!
      type(calypso_comm_table), intent(in) :: transfer_tbl
      type(communication_table), intent(in) :: org_nod_comm
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real(kind = kreal), intent(in) :: vec_new(nnod_new)
!
      real(kind = kreal), intent(inout) :: vec_org(nnod_org)
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call verify_iccgN_vec_type(n_scalar, nnod_new, v_sol)
      call calypso_reverse_SR_1(transfer_tbl,                           &
     &    nnod_new, nnod_org, vec_new(1), vec_org(1), SR_sig, SR_r)
      call SOLVER_SEND_RECV_type(nnod_org, org_nod_comm,                &
     &                           SR_sig, SR_r, vec_org(1))
!
      end subroutine scalar_to_original_partition
!
!-----------------------------------------------------------------------
!
      subroutine vector_to_original_partition                           &
     &         (transfer_tbl, org_nod_comm, nnod_new, nnod_org,         &
     &          vec_new, vec_org, v_sol, SR_sig, SR_r)
!
      use calypso_SR_type
      use solver_SR_type
      use calypso_reverse_send_recv
!
      type(calypso_comm_table), intent(in) :: transfer_tbl
      type(communication_table), intent(in) :: org_nod_comm
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real(kind = kreal), intent(in) :: vec_new(3*nnod_new)
!
      real(kind = kreal), intent(inout) :: vec_org(3*nnod_org)
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call verify_iccgN_vec_type(n_vector, nnod_new, v_sol)
      call calypso_reverse_SR_3(transfer_tbl,                           &
     &    nnod_new, nnod_org, vec_new(1), vec_org(1), SR_sig, SR_r)
      call SOLVER_SEND_RECV_3_type(nnod_org, org_nod_comm,              &
     &                           SR_sig, SR_r, vec_org(1))
!
      end subroutine vector_to_original_partition
!
!-----------------------------------------------------------------------
!
      end module field_to_new_partition
