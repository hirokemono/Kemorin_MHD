!>@file   xyz_block_id_by_nod_vol.f90
!!@brief  module xyz_block_id_by_nod_vol
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_volume_at_node(part_param, mesh,                 &
!!     &          num_mask, masking, ref_repart, d_mask,                &
!!     &          volume_nod, volume_nod_tot, volume_min_gl,            &
!!     &          SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: num_mask
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(masking_parameter), intent(in) :: masking(num_mask)
!!        real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: d_mask(mesh%node%numnod,num_mask)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: volume_nod(mesh%node%numnod)
!!        real(kind = kreal), intent(inout) :: volume_nod_tot
!!        real(kind = kreal), intent(inout) :: volume_min_gl
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine set_xyz_block_id_by_nod_vol                          &
!!     &         (node, part_param, id_block)
!!        type(node_data), intent(in) :: node
!!        type(volume_partioning_param), intent(in) :: part_param
!!        subroutine vector_by_masking(node, num_mask, masking,         &
!!     &                             d_mask, vector_nod)
!!
!!      subroutine const_single_domain_list(sub_z)
!!        type(grouping_1d_work), intent(inout) :: sub_z
!!      subroutine const_z_subdomain_list(part_param, z_part_grp, sub_y)
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(group_data), intent(in) :: z_part_grp
!!        type(grouping_1d_work), intent(inout) :: sub_y
!!      subroutine const_yz_subdomain_list                              &
!!     &         (part_param, sub_y, yz_part_grp, sub_x)
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(grouping_1d_work), intent(in) :: sub_y
!!        type(group_data), intent(in) :: yz_part_grp
!!        type(grouping_1d_work), intent(inout) :: sub_x
!!@endverbatim
!
      module xyz_block_id_by_nod_vol
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
!
      implicit none
!
      private :: weighting_by_masking
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_volume_at_node(part_param, mesh,                   &
     &          num_mask, masking, ref_repart, d_mask,                  &
     &          volume_nod, volume_nod_tot, volume_min_gl,              &
     &          SR_sig, SR_r)
!
      use t_solver_SR
      use calypso_mpi_real
      use int_volume_of_single_domain
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: num_mask
      type(volume_partioning_param), intent(in) :: part_param
      type(mesh_geometry), intent(in) :: mesh
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
      real(kind = kreal), intent(in)                                    &
     &                     :: d_mask(mesh%node%numnod,num_mask)
!
      real(kind = kreal), intent(inout) :: volume_nod(mesh%node%numnod)
      real(kind = kreal), intent(inout) :: volume_nod_tot
      real(kind = kreal), intent(inout) :: volume_min_gl
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal) :: vol_lc
      integer(kind = kint) :: inod
!
!
      if(part_param%iflag_repart_ref .eq. i_NODE_BASED) then
!$omp parallel workshare
        volume_nod(1:mesh%node%internal_node) = 1.0d0
!$omp end parallel workshare
!
      else if(part_param%iflag_repart_ref .eq. i_TIME_BASED) then
!$omp parallel workshare
        volume_nod(1:mesh%node%internal_node)                           &
     &      = ref_repart(1:mesh%node%internal_node)
!$omp end parallel workshare
!
      else
        call cal_node_volue_w_power(part_param%vol_power,               &
     &      mesh%node, mesh%ele, volume_nod)
      end if
!
      if(part_param%iflag_repart_ref .ne. i_TIME_BASED                  &
     &   .and. num_mask .gt. 0) then
          call weighting_by_masking(mesh%node, part_param%shrink,       &
     &        num_mask, masking, d_mask, volume_nod)
      end if
!
      call SOLVER_SEND_RECV_type(mesh%node%numnod, mesh%nod_comm,       &
     &                           SR_sig, SR_r, volume_nod)
!
      vol_lc = 0.0d0
!$omp parallel do reduction(+:vol_lc)
      do inod = 1, mesh%node%internal_node
        vol_lc = vol_lc + volume_nod(inod)
      end do
!$omp end parallel do
!
      call calypso_mpi_allreduce_one_real                               &
     &   (vol_lc, volume_nod_tot, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (vol_lc, volume_min_gl, MPI_MIN)
!
      if(my_rank .eq. 0) then
        write(*,*) 'volume_nod_tot', volume_nod_tot
        write(*,*) 'volume_min_gl', volume_min_gl
      end if
!
      end subroutine set_volume_at_node
!
! ----------------------------------------------------------------------
!
      subroutine weighting_by_masking(node, shrink, num_mask, masking,  &
     &                                d_mask, volume_nod)
!
      use t_ctl_param_masking
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: shrink
      integer(kind = kint), intent(in) :: num_mask
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: d_mask(node%numnod,num_mask)
!
      real(kind = kreal), intent(inout) :: volume_nod(node%numnod)
!
      real(kind = kreal), allocatable :: value(:,:)
      integer(kind = kint) :: ip, ist, ied, inod
!
!
      allocate(value(num_mask,np_smp))
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node%istack_internal_smp(ip-1) + 1
        ied = node%istack_internal_smp(ip)
        do inod = ist, ied
          value(1:num_mask,ip) = d_mask(inod,1:num_mask)
!
          if(multi_mask_flag(num_mask, masking, value(1,ip))            &
     &                                      .eqv. .FALSE.) then
            volume_nod(inod) = shrink * volume_nod(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      deallocate(value)
!
      end subroutine weighting_by_masking
!
! ----------------------------------------------------------------------
!
      subroutine vector_by_masking(node, num_mask, masking,             &
     &                             d_mask, vector_nod)
!
      use t_ctl_param_masking
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_mask
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: d_mask(node%numnod,num_mask)
!
      real(kind = kreal), intent(inout) :: vector_nod(node%numnod,3)
!
      real(kind = kreal), allocatable :: value(:,:)
      integer(kind = kint) :: ip, ist, ied, inod
!
!
      allocate(value(num_mask,np_smp))
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node%istack_internal_smp(ip-1) + 1
        ied = node%istack_internal_smp(ip)
        do inod = ist, ied
          value(1:num_mask,ip) = d_mask(inod,1:num_mask)
!
          if(multi_mask_flag(num_mask, masking, value(1,ip))            &
     &                                      .eqv. .FALSE.) then
            vector_nod(inod,1) = 0.0d0
            vector_nod(inod,2) = 0.0d0
            vector_nod(inod,3) = 0.0d0
          end if
        end do
      end do
!$omp end parallel do
!
      deallocate(value)
!
      end subroutine vector_by_masking
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_xyz_block_id_by_nod_vol                            &
     &         (node, part_param, id_block)
!
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      integer(kind = kint), intent(inout)                               &
     &                    :: id_block(node%numnod,3)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      integer(kind = kint) :: inod, nd
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'xyz_min_gl', node%xyz_min_gl(1:3)
        write(*,*) 'xyz_max_gl', node%xyz_max_gl(1:3)
      end if
!
      size_gl(1:3) = node%xyz_max_gl(1:3) - node%xyz_min_gl(1:3)
      size_blk(1:3) = size_gl(1:3) / dble(part_param%ndivide_eb(1:3))
      do nd = 1, 3
!$omp parallel do private(inod)
        do inod = 1, node%numnod
          id_block(inod,nd) = int((node%xx(inod,nd)                     &
     &                       - node%xyz_min_gl(nd)) / size_blk(nd))
          id_block(inod,nd)                                             &
     &          = min(id_block(inod,nd)+1,part_param%ndivide_eb(nd))
        end do
!$omp end parallel do
      end do
!
      end subroutine set_xyz_block_id_by_nod_vol
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_single_domain_list(sub_z)
!
      type(grouping_1d_work), intent(inout) :: sub_z
!
!
      sub_z%ndomain_done = 1
      call alloc_new_domain_list(sub_z)
      sub_z%idomain_done = 1
!
      end subroutine const_single_domain_list
!
! ----------------------------------------------------------------------
!
      subroutine const_z_subdomain_list(part_param, z_part_grp, sub_y)
!
      use set_istack_4_domain_block
!
      type(volume_partioning_param), intent(in) :: part_param
      type(group_data), intent(in) :: z_part_grp
!
      type(grouping_1d_work), intent(inout) :: sub_y
!
!
      sub_y%ndomain_done = count_z_subdomain_num(part_param,            &
     &                       z_part_grp%num_grp, z_part_grp%istack_grp)
      call alloc_new_domain_list(sub_y)
!
      call set_z_subdomain_list                                         &
     &   (part_param, z_part_grp%num_grp, z_part_grp%istack_grp,        &
     &    sub_y%ndomain_done, sub_y%idomain_done)
!
      end subroutine const_z_subdomain_list
!
! ----------------------------------------------------------------------
!
      subroutine const_yz_subdomain_list                                &
     &         (part_param, sub_y, yz_part_grp, sub_x)
!
      use set_istack_4_domain_block
!
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_y
      type(group_data), intent(in) :: yz_part_grp
!
      type(grouping_1d_work), intent(inout) :: sub_x
!
!
      sub_x%ndomain_done = count_yz_subdomain_num(part_param,           &
     &                    sub_y%ndomain_done, sub_y%idomain_done,       &
     &                    yz_part_grp%num_grp, yz_part_grp%istack_grp)
      call alloc_new_domain_list(sub_x)
!
      call set_yz_subdomain_list                                        &
     &   (part_param, sub_y%ndomain_done, sub_y%idomain_done,           &
     &    yz_part_grp%num_grp, yz_part_grp%istack_grp,                  &
     &    sub_x%ndomain_done, sub_x%idomain_done)
!
      end subroutine const_yz_subdomain_list
!
! ----------------------------------------------------------------------
!
      end module xyz_block_id_by_nod_vol
