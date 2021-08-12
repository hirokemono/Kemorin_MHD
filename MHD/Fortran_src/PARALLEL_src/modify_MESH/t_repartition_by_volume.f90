!>@file   t_repartition_by_volume.f90
!!@brief  module t_repartition_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine grouping_by_volume                                   &
!!     &         (mesh, part_param, num_mask, masking,                  &
!!     &          ref_repart, d_mask, part_grp, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: num_mask
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(masking_parameter), intent(in) :: masking(num_mask)
!!        real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
!!        real(kind = kreal), intent(in)                                &
!!     &        :: d_mask(mesh%node%numnod,num_mask)
!!        type(group_data), intent(inout) :: part_grp
!!       type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module t_repartition_by_volume
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
      use t_solver_SR
!
      implicit none
!
      type node_volume_and_sorting
        integer(kind = kint) :: numnod
        integer(kind = kint), allocatable :: id_block(:,:)
        real(kind = kreal), allocatable :: volume_nod(:)
        real(kind = kreal) :: volume_nod_tot
        real(kind = kreal) :: volume_min_gl
!
        real(kind = kreal), allocatable :: sub_volume(:)
!
        integer(kind = kint), allocatable :: inod_sort(:)
      end type node_volume_and_sorting
!
      character(len = kchara), parameter, private                       &
     &                        :: base_name = 'new_domain'
!
      private :: alloc_node_volume_and_sort, alloc_vol_sort_sub_volume
      private :: dealloc_node_volume_and_sort
      private :: dealloc_vol_sort_sub_volume
      private :: const_istack_z_domain_block
      private :: const_istack_xyz_domain_block
      private :: const_z_div_domain_group_data
      private :: const_newdomain_group_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine grouping_by_volume                                     &
     &         (mesh, part_param, num_mask, masking,                    &
     &          ref_repart, d_mask, part_grp, SR_sig, SR_r)
!
      use xyz_block_id_by_nod_vol
!
      integer(kind = kint), intent(in) :: num_mask
      type(mesh_geometry), intent(in) :: mesh
      type(volume_partioning_param), intent(in) :: part_param
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
      real(kind = kreal), intent(in)                                    &
     &                    :: d_mask(mesh%node%numnod,num_mask)
!
      type(group_data), intent(inout) :: part_grp
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      type(group_data) :: z_part_grp
      type(group_data) :: yz_part_grp
!
      type(node_volume_and_sorting) :: vol_sort
      type(grouping_1d_work) :: sub_z
      type(grouping_1d_work) :: sub_y
      type(grouping_1d_work) :: sub_x
!
      integer(kind = kint) :: ndomain_yz
      integer(kind = kint) :: k, iy, iz
!
!
      call alloc_node_volume_and_sort(mesh%node, vol_sort)
!
      call set_volume_at_node(part_param, mesh, num_mask, masking,      &
     &    ref_repart, d_mask, vol_sort%volume_nod,                      &
     &    vol_sort%volume_nod_tot, vol_sort%volume_min_gl,              &
     &    SR_sig, SR_r)
!
      call set_xyz_block_id_by_nod_vol(mesh%node, part_param,           &
     &                                 vol_sort%id_block)
!
      call const_single_domain_list(sub_z)
!
!    For z direction
!
      call alloc_vol_sort_sub_volume(ione, vol_sort)
      vol_sort%sub_volume(1)                                            &
     &     = vol_sort%volume_nod_tot / dble(part_param%ndomain_eb(3))
!
      call const_istack_z_domain_block                                  &
     &   (mesh, part_param, vol_sort, sub_z)
      call dealloc_vol_sort_sub_volume(vol_sort)
!
      call const_z_div_domain_group_data                                &
     &   (mesh, part_param, sub_z, vol_sort, z_part_grp)
!
      call const_z_subdomain_list(part_param, z_part_grp, sub_y)
!
!
      call alloc_vol_sort_sub_volume                                    &
     &   (part_param%ndomain_eb(3), vol_sort)
!$omp parallel do private(iz)
      do iz = 1, part_param%ndomain_eb(3)
        vol_sort%sub_volume(iz) = sub_z%vol_grp(iz,1)                   &
     &                           / dble(part_param%ndomain_eb(2))
      end do
!$omp end parallel do
      call dealloc_grouping_1d_work(sub_z)
!
!   For y direction
!
      ndomain_yz = part_param%ndomain_eb(2) * part_param%ndomain_eb(3)
      call const_istack_xyz_domain_block(itwo, mesh, part_param,        &
     &    z_part_grp, vol_sort, sub_y)
!
      call const_newdomain_group_data(itwo, ndomain_yz, mesh,           &
     &    part_param, z_part_grp, sub_y, vol_sort, yz_part_grp)
      call dealloc_vol_sort_sub_volume(vol_sort)
!
      call const_yz_subdomain_list                                      &
     &   (part_param, sub_y, yz_part_grp, sub_x)
      call dealloc_group(z_part_grp)
!
      call alloc_vol_sort_sub_volume(ndomain_yz, vol_sort)
!$omp parallel do private(iz,iy,k)
      do iz = 1, part_param%ndomain_eb(3)
        do iy = 1, part_param%ndomain_eb(2)
          k = iy + (iz-1)*part_param%ndomain_eb(2)
          vol_sort%sub_volume(k) = sub_y%vol_grp(iy,iz)                 &
     &                            / dble(part_param%ndomain_eb(1))
        end do
      end do
!$omp end parallel do
      call dealloc_grouping_1d_work(sub_y)
!
!   For x direction
!
      call const_istack_xyz_domain_block(ione, mesh, part_param,        &
     &    yz_part_grp, vol_sort, sub_x)
!
      call const_newdomain_group_data(ione, part_param%new_nprocs,      &
     &    mesh, part_param, yz_part_grp, sub_x, vol_sort, part_grp)
      call dealloc_grouping_1d_work(sub_x)
      call dealloc_group(yz_part_grp)
!
      call dealloc_vol_sort_sub_volume(vol_sort)
      call dealloc_node_volume_and_sort(vol_sort)
!
      end subroutine grouping_by_volume 
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_node_volume_and_sort(node, vol_sort)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(node_volume_and_sorting), intent(inout) :: vol_sort
!
!
      allocate(vol_sort%volume_nod(node%numnod))
      allocate(vol_sort%id_block(node%numnod,3))
      allocate(vol_sort%inod_sort(node%numnod))
!
!$omp parallel workshare
      vol_sort%volume_nod(node%numnod) = 0.0d0
      vol_sort%id_block(node%numnod,3) = 0
      vol_sort%id_block(node%numnod,3) = 0
      vol_sort%id_block(node%numnod,3) = 0
      vol_sort%inod_sort(node%numnod) =  0
!$omp end parallel workshare
!
      end subroutine alloc_node_volume_and_sort
!
! ----------------------------------------------------------------------
!
      subroutine alloc_vol_sort_sub_volume(num_grp, vol_sort)
!
      integer(kind = kint), intent(in) :: num_grp
      type(node_volume_and_sorting), intent(inout) :: vol_sort
!
      allocate(vol_sort%sub_volume(num_grp))
!
      if(num_grp .le. 0) return
!$omp parallel workshare
        vol_sort%sub_volume(1:num_grp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_vol_sort_sub_volume
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_node_volume_and_sort(vol_sort)
!
      type(node_volume_and_sorting), intent(inout) :: vol_sort
!
!
      deallocate(vol_sort%volume_nod)
      deallocate(vol_sort%id_block)
      deallocate(vol_sort%inod_sort)
!
      end subroutine dealloc_node_volume_and_sort
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_vol_sort_sub_volume(vol_sort)
!
      type(node_volume_and_sorting), intent(inout) :: vol_sort
!
      deallocate(vol_sort%sub_volume)
!
      end subroutine dealloc_vol_sort_sub_volume
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_istack_z_domain_block                            &
     &         (mesh, part_param, vol_sort, sub_z)
!
      use repart_in_xyz_by_volume
      use set_istack_4_domain_block
!
      type(mesh_geometry), intent(in) :: mesh
      type(volume_partioning_param), intent(in) :: part_param
!
      type(node_volume_and_sorting), intent(inout) :: vol_sort
      type(grouping_1d_work), intent(inout) :: sub_z
!
      integer(kind = kint) :: istack_intnod(0:1)
!
!
      call alloc_grouping_1d_work(ione,                                 &
     &    part_param%ndivide_eb(3), part_param%ndomain_eb(3), sub_z)
      call set_z_sorted_node_and_stack                                  &
     &   (mesh%node, vol_sort%id_block(1,3),                            &
     &    sub_z%n_block, sub_z%istack_block(0,1), vol_sort%inod_sort)
!
      istack_intnod(0) = 0
      istack_intnod(1) = mesh%node%internal_node
!
      call set_istack_xyz_domain_block(mesh%node, vol_sort%inod_sort,   &
     &    vol_sort%id_block(1,3), vol_sort%volume_nod,                  &
     &    sub_z%n_block, vol_sort%sub_volume(1), sub_z%n_domain,        &
     &    ione, istack_intnod, sub_z%istack_vol, sub_z%vol_grp)
!
      if(iflag_debug .eq. 0) return
!      call check_blocks_4_z_domain(my_rank, mesh%node,                 &
!          part_param, vol_sort%inod_sort, vol_sort%id_block, sub_z)
      if(my_rank .eq. 0) then
        write(*,*) 'vol_grp_z', vol_sort%volume_nod_tot
        call check_z_divided_volumes(part_param, sub_z,                 &
     &                               vol_sort%sub_volume(1))
      end if
!
      end subroutine const_istack_z_domain_block
!
! ----------------------------------------------------------------------
!
      subroutine const_istack_xyz_domain_block(nd, mesh, part_param,    &
     &          prev_part_grp, vol_sort, part_1d)
!
      use repart_in_xyz_by_volume
      use set_istack_4_domain_block
!
      integer(kind = kint), intent(in) :: nd
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: prev_part_grp
      type(volume_partioning_param), intent(in) :: part_param
!
      type(node_volume_and_sorting), intent(inout) :: vol_sort
      type(grouping_1d_work), intent(inout) :: part_1d
!
!
      call alloc_grouping_1d_work(prev_part_grp%num_grp,                &
     &   part_param%ndivide_eb(nd), part_param%ndomain_eb(nd), part_1d)
      call set_sorted_node_and_stack                                    &
     &   (nd, mesh%node, vol_sort%id_block(1,nd),                       &
     &    part_1d%ndomain_done, part_1d%idomain_done, part_1d%n_block,  &
     &    prev_part_grp%num_grp, prev_part_grp%istack_grp,              &
     &    part_1d%istack_block, vol_sort%inod_sort)
!
      call set_istack_xyz_domain_block(mesh%node, vol_sort%inod_sort,   &
     &    vol_sort%id_block(1,nd), vol_sort%volume_nod,                 &
     &    part_1d%n_block, vol_sort%sub_volume(1), part_1d%n_domain,    &
     &    prev_part_grp%num_grp, prev_part_grp%istack_grp,              &
     &    part_1d%istack_vol, part_1d%vol_grp)
!
      if(iflag_debug .eq. 0) return
!
      if(nd .eq. 1) then
!        call check_blocks_4_xyz_domain(my_rank, mesh%node,             &
!     &      part_param, vol_sort%inod_sort, vol_sort%id_block, part_1d)
        if(my_rank .eq. 0) then
          write(*,*) 'vol_grp_x', vol_sort%volume_nod_tot
          call check_xyz_divided_volumes(part_param, part_1d,           &
     &        prev_part_grp%num_grp, vol_sort%sub_volume(1))
        end if
      else if(nd .eq. 2) then
!        call check_blocks_4_yz_domain(my_rank, mesh%node,              &
!     &      part_param, vol_sort%inod_sort, vol_sort%id_block, part_1d)
        if(my_rank .eq. 0) then
          write(*,*) 'vol_grp_y', vol_sort%volume_nod_tot
          call check_yz_divided_volumes(part_param, part_1d,            &
     &        prev_part_grp%num_grp, vol_sort%sub_volume(1))
        end if
      end if
!
      end subroutine const_istack_xyz_domain_block
!
! ----------------------------------------------------------------------
!
      subroutine const_z_div_domain_group_data                          &
     &         (mesh, part_param, sub_z, vol_sort, z_part_grp)
!
      use repart_in_xyz_by_volume
      use set_repartition_group_name
!
      type(mesh_geometry), intent(in) :: mesh
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_z
      type(node_volume_and_sorting), intent(in) :: vol_sort
!
      type(group_data), intent(inout) :: z_part_grp
!
!
      z_part_grp%num_grp = sub_z%n_domain
      call alloc_group_num(z_part_grp)
      call set_z_domain_grp_name(part_param, base_name,                 &
     &    z_part_grp%num_grp, z_part_grp%grp_name)
      call set_z_domain_grp_stack(sub_z%n_block, sub_z%n_domain,        &
     &    sub_z%istack_block(0,1), sub_z%istack_vol,                    &
     &    z_part_grp%num_grp, z_part_grp%istack_grp)
!      call check_stacks_4_z_domain                                     &
!     &   (my_rank, mesh%node, part_param, vol_sort%inod_sort,          &
!     &    z_part_grp%num_grp, z_part_grp%istack_grp)
!
      z_part_grp%num_item = z_part_grp%istack_grp(z_part_grp%num_grp)
      call alloc_group_item(z_part_grp)
      call set_domain_grp_item(mesh%node, vol_sort%inod_sort,           &
     &    z_part_grp%num_grp, z_part_grp%num_item,                      &
     &    z_part_grp%istack_grp, z_part_grp%item_grp)
!
      end subroutine const_z_div_domain_group_data
!
! ----------------------------------------------------------------------
!
      subroutine const_newdomain_group_data                             &
     &         (nd, num_group, mesh, part_param,                        &
     &          prev_part_grp, part_1d, vol_sort, part_grp)
!
      use repart_in_xyz_by_volume
      use set_repartition_group_name
!
      integer(kind = kint), intent(in) :: nd, num_group
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: prev_part_grp
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: part_1d
      type(node_volume_and_sorting), intent(in) :: vol_sort
!
      type(group_data), intent(inout) :: part_grp
!
      part_grp%num_grp = num_group
      call alloc_group_num(part_grp)
!
      call set_newdomain_grp_stack                                      &
     &   (part_1d%n_block, part_1d%n_domain, prev_part_grp%num_grp,     &
     &    part_1d%ndomain_done, part_1d%idomain_done,                   &
     &    part_1d%istack_block, part_1d%istack_vol, part_grp%num_grp,   &
     &    part_grp%istack_grp)
!      call check_stacks_4_new_domain                                   &
!     &   (my_rank, mesh%node, part_param, vol_sort%inod_sort,          &
!     &    part_1d%ndomain_done, part_1d%idomain_done,                  &
!     &    part_param%new_nprocs, part_grp%istack_grp)
!
      if(nd .eq. 1) then
        call set_xyz_domain_grp_name(part_param, base_name,             &
     &      part_grp%num_grp, part_grp%grp_name)
      else if(nd .eq. 2) then
        call set_yz_domain_grp_name(part_param, base_name,              &
     &      part_grp%num_grp, part_grp%grp_name)
      end if
!
      part_grp%num_item = part_grp%istack_grp(part_grp%num_grp)
      call alloc_group_item(part_grp)
      call set_domain_grp_item(mesh%node, vol_sort%inod_sort,           &
     &                         part_grp%num_grp, part_grp%num_item,     &
     &                         part_grp%istack_grp, part_grp%item_grp)
!
      end subroutine const_newdomain_group_data
!
! ----------------------------------------------------------------------
!
      end module t_repartition_by_volume
