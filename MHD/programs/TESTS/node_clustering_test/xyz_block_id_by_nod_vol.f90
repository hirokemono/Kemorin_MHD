!>@file   xyz_block_id_by_nod_vol.f90
!!@brief  module xyz_block_id_by_nod_vol
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_volume_at_node(mesh, volume_nod, volume_nod_tot)
!!        type(mesh_geometry), intent(in) :: mesh
!!      subroutine set_xyz_block_id_by_nod_vol                          &
!!     &         (node, part_param, id_block)
!!        type(node_data), intent(in) :: node
!!        type(mesh_test_files_param), intent(in) :: part_param
!!
!!      subroutine const_single_domain_list(sub_z)
!!        type(grouping_1d_work), intent(inout) :: sub_z
!!      subroutine const_z_subdomain_list(part_param, z_part_grp, sub_y)
!!        type(mesh_test_files_param), intent(in) :: part_param
!!        type(group_data), intent(in) :: z_part_grp
!!        type(grouping_1d_work), intent(inout) :: sub_y
!!      subroutine const_yz_subdomain_list                              &
!!     &         (part_param, sub_y, yz_part_grp, sub_x)
!!        type(mesh_test_files_param), intent(in) :: part_param
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_volume_at_node(mesh, volume_nod, volume_nod_tot)
!
      use calypso_mpi_real
      use int_volume_of_single_domain
      use solver_SR_type
!
      type(mesh_geometry), intent(in) :: mesh
      real(kind = kreal), intent(inout) :: volume_nod(mesh%node%numnod)
      real(kind = kreal), intent(inout) :: volume_nod_tot
!
      real(kind = kreal) :: vol_lc
      integer(kind = kint) :: inod
!
!
      call cal_node_volue(mesh%node, mesh%ele, volume_nod)
      call SOLVER_SEND_RECV_type                                        &
     &   (mesh%node%numnod, mesh%nod_comm, volume_nod)
!
      vol_lc = 0.0d0
      do inod = 1, mesh%node%internal_node
        vol_lc = vol_lc + volume_nod(inod)
      end do
      call calypso_mpi_allreduce_one_real                               &
     &   (vol_lc, volume_nod_tot, MPI_SUM)
!
      end subroutine set_volume_at_node
!
! ----------------------------------------------------------------------
!
      subroutine set_xyz_block_id_by_nod_vol                            &
     &         (node, part_param, id_block)
!
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: part_param
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
      type(mesh_test_files_param), intent(in) :: part_param
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
      type(mesh_test_files_param), intent(in) :: part_param
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
