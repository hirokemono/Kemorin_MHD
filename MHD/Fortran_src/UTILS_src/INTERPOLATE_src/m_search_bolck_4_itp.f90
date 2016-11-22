!m_search_bolck_4_itp.f90
!     module m_search_bolck_4_itp
!
!     Written by H. Matsui on March, 2015
!
!!      subroutine set_all_block_points_4_itp                           &
!!     &         (nblock_ref, nnod, xx, nprocs_org, para_mesh)
!!      subroutine dealloc_interpolate_blocks(nprocs_org)
!!      subroutine check_block_points_4_itp(id_file, nprocs_org)
!
      module m_search_bolck_4_itp
!
      use m_precision
      use m_constants
      use t_group_data
!
      implicit none
!
      integer(kind = kint) :: num_xyz_block(3) = (/1, 1, 1/)
!
      type block_4_interpolate
        integer(kind = kint) :: ntot_itp_block
        integer(kind = kint) :: num_itp_block(3) = (/1, 1, 1/)
!
        real(kind = kreal) :: xdomain_min(3)
        real(kind = kreal) :: xdomain_max(3)
        real(kind = kreal), allocatable :: x_block(:)
        real(kind = kreal), allocatable :: y_block(:)
        real(kind = kreal), allocatable :: z_block(:)
!
        type(group_data) :: ele_list_by_ctr
        type(group_data) :: ele_list_by_rng
!
        integer(kind = kint), allocatable :: iblock_tgt_node(:,:)
!
        real(kind = kreal), allocatable :: xele_min(:,:)
        real(kind = kreal), allocatable :: xele_max(:,:)
      end type block_4_interpolate
!
!
      type(block_4_interpolate), save, allocatable :: org_blocks(:)
      type(block_4_interpolate), save :: dest_block
!
      private :: alloc_interpolate_blocks, allocate_block_points
      private :: deallocate_block_points, set_block_points_4_itp
      private :: check_block_points
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_all_block_points_4_itp                             &
     &         (nblock_ref, nnod, xx, nprocs_org, para_mesh)
!
      use t_mesh_data
      use t_geometry_data
      use cal_minmax_and_stacks
      use set_ele_list_4_itp_table
!
      integer(kind = kint), intent(in) :: nblock_ref(3)
      integer(kind = kint), intent(in) :: nnod, nprocs_org
      real(kind = kreal), intent(in) :: xx(nnod,3)
      type(mesh_data), intent(inout) :: para_mesh(nprocs_org)
!
      integer(kind = kint) :: i
!
!
      call alloc_interpolate_blocks(nprocs_org)
!
      call set_block_points_4_itp(nblock_ref, nnod, xx, dest_block)
      do i = 1, nprocs_org
        call set_block_points_4_itp                                     &
     &     (nblock_ref, para_mesh(i)%mesh%node%numnod,                  &
     &      para_mesh(i)%mesh%node%xx, org_blocks(i))
      end do
!
      do i = 1, nprocs_org
        call allocate_block_list                                        &
     &     (nnod, para_mesh(i)%mesh%ele%numele, org_blocks(i))
!
        call set_element_range                                          &
     &    (para_mesh(i)%mesh%node%numnod, para_mesh(i)%mesh%ele%numele, &
     &     para_mesh(i)%mesh%ele%nnod_4_ele, para_mesh(i)%mesh%node%xx, &
     &     para_mesh(i)%mesh%ele%ie,                                    &
     &     org_blocks(i)%xele_min, org_blocks(i)%xele_max)
        call set_block_list_4_target(nnod, xx(1,1),                     &
     &      org_blocks(i)%xdomain_min, org_blocks(i)%xdomain_max,       &
     &      org_blocks(i)%num_itp_block,                                &
     &      org_blocks(i)%iblock_tgt_node)
!
        org_blocks(i)%ele_list_by_ctr%num_grp                           &
     &      = org_blocks(i)%ntot_itp_block
        call allocate_grp_type_num(org_blocks(i)%ele_list_by_ctr)
!
!        write(*,*) 'org_blocks(i)%ele_list_by_ctr%num_grp', i,         &
!       &   org_blocks(i)%ele_list_by_ctr%num_grp
        call count_ele_list_by_center                                   &
     &     (para_mesh(i)%mesh%ele%numele, para_mesh(i)%mesh%ele%x_ele,  &
     &      org_blocks(i)%xdomain_min, org_blocks(i)%xdomain_max,       &
     &      org_blocks(i)%num_itp_block,                                &
     &      org_blocks(i)%ele_list_by_ctr%num_grp,                      &
     &      org_blocks(i)%ele_list_by_ctr%nitem_grp)
        call s_cal_total_and_stacks                                     &
     &     (org_blocks(i)%ele_list_by_ctr%num_grp,                      &
     &      org_blocks(i)%ele_list_by_ctr%nitem_grp, izero,             &
     &      org_blocks(i)%ele_list_by_ctr%istack_grp,                   &
     &      org_blocks(i)%ele_list_by_ctr%num_item)
!
!        write(*,*) 'ele_list_by_ctr', i,                               &
!       &   org_blocks(i)%ele_list_by_ctr%num_item,                     &
!       &   maxval(org_blocks(i)%ele_list_by_ctr%nitem_grp)
!
        call allocate_grp_type_item(org_blocks(i)%ele_list_by_ctr)
!        write(*,*) 'set_ele_list_by_center'
        call set_ele_list_by_center                                     &
     &     (para_mesh(i)%mesh%ele%numele, para_mesh(i)%mesh%ele%x_ele,  &
     &      org_blocks(i)%xdomain_min, org_blocks(i)%xdomain_max,       &
     &      org_blocks(i)%num_itp_block,                                &
     &      org_blocks(i)%ele_list_by_ctr%num_grp,                      &
     &      org_blocks(i)%ele_list_by_ctr%num_item,                     &
     &      org_blocks(i)%ele_list_by_ctr%nitem_grp,                    &
     &      org_blocks(i)%ele_list_by_ctr%istack_grp,                   &
     &      org_blocks(i)%ele_list_by_ctr%item_grp)
!
!
        org_blocks(i)%ele_list_by_rng%num_grp                           &
     &      = org_blocks(i)%ntot_itp_block
        call allocate_grp_type_num(org_blocks(i)%ele_list_by_rng)
!
!        write(*,*) 'count_ele_list_with_range'
        call count_ele_list_with_range(para_mesh(i)%mesh%ele%numele,    &
     &     org_blocks(i)%xdomain_min, org_blocks(i)%xdomain_max,        &
     &     org_blocks(i)%xele_min, org_blocks(i)%xele_max,              &
     &     org_blocks(i)%num_itp_block,                                 &
     &     org_blocks(i)%ele_list_by_rng%num_grp,                       &
     &     org_blocks(i)%ele_list_by_rng%nitem_grp)
        call s_cal_total_and_stacks                                     &
     &     (org_blocks(i)%ele_list_by_rng%num_grp,                      &
     &      org_blocks(i)%ele_list_by_rng%nitem_grp, izero,             &
     &      org_blocks(i)%ele_list_by_rng%istack_grp,                   &
     &      org_blocks(i)%ele_list_by_rng%num_item)
!
        call allocate_grp_type_item(org_blocks(i)%ele_list_by_rng)
!        write(*,*) 'set_ele_list_with_range', i, &
!       & org_blocks(i)%ele_list_by_rng%num_item, &
!       & maxval(org_blocks(i)%ele_list_by_rng%nitem_grp)
        call set_ele_list_with_range(para_mesh(i)%mesh%ele%numele,      &
     &     org_blocks(i)%xdomain_min, org_blocks(i)%xdomain_max,        &
     &     org_blocks(i)%xele_min, org_blocks(i)%xele_max,              &
     &     org_blocks(i)%num_itp_block,                                 &
     &     org_blocks(i)%ele_list_by_rng%num_grp,                       &
     &     org_blocks(i)%ele_list_by_rng%num_item,                      &
     &     org_blocks(i)%ele_list_by_rng%nitem_grp,                     &
     &     org_blocks(i)%ele_list_by_rng%istack_grp,                    &
     &     org_blocks(i)%ele_list_by_rng%item_grp)
      end do
!
      end subroutine set_all_block_points_4_itp
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_interpolate_blocks(nprocs_org)
!
      integer(kind = kint), intent(in) :: nprocs_org
!
      integer(kind = kint) :: i
!
!
      do i = 1, nprocs_org
        call deallocate_block_points(org_blocks(i))
      end do
      deallocate(org_blocks)
!
      end subroutine dealloc_interpolate_blocks
!
!  ---------------------------------------------------------------------
!
      subroutine check_block_points_4_itp(id_file, nprocs_org)
!
      integer(kind = kint), intent(in) :: id_file, nprocs_org
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) '#  Block for destination grid'
      call check_block_points(id_file, dest_block)

      do i = 1, nprocs_org
        write(id_file,*) '#  Block for original mesh for ', i
        call check_block_points(id_file, org_blocks(i))
      end do
!
      end subroutine check_block_points_4_itp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_interpolate_blocks(nprocs_org)
!
      integer(kind = kint), intent(in) :: nprocs_org
!
!
      allocate(org_blocks(nprocs_org))
!
      end subroutine alloc_interpolate_blocks
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_block_points(block, nblock_ref)
!
!
      integer(kind = kint), intent(in) :: nblock_ref(3)
      type(block_4_interpolate), intent(inout) :: block
!
!
      block%num_itp_block(1:3) = nblock_ref(1:3)
      block%ntot_itp_block = block%num_itp_block(1)                     &
     &                     * block%num_itp_block(2)                     &
     &                     * block%num_itp_block(3)
!
      allocate( block%x_block(0:block%num_itp_block(1)) )
      allocate( block%y_block(0:block%num_itp_block(2)) )
      allocate( block%z_block(0:block%num_itp_block(3)) )
!
      block%x_block = 0.0d0
      block%y_block = 0.0d0
      block%z_block = 0.0d0
!
      end subroutine allocate_block_points
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_block_list(nnod_tgt, nele_org, block)
!
      integer(kind = kint), intent(in) :: nnod_tgt, nele_org
      type(block_4_interpolate), intent(inout) :: block
!
!
      allocate( block%iblock_tgt_node(nnod_tgt,4) )
      allocate( block%xele_min(nele_org,3) )
      allocate( block%xele_max(nele_org,3) )
!
      if(nnod_tgt .gt. 0) block%iblock_tgt_node = 0
      if(nele_org .gt. 0) block%xele_min = 0.0d0
      if(nele_org .gt. 0) block%xele_max = 0.0d0
!
      end subroutine allocate_block_list
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_block_points(block)
!
      type(block_4_interpolate), intent(inout) :: block
!
      deallocate(block%x_block, block%y_block, block%z_block)
!
      end subroutine deallocate_block_points
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_block_list(block)
!
      type(block_4_interpolate), intent(inout) :: block
!
!
      deallocate(block%iblock_tgt_node)
      deallocate(block%xele_max, block%xele_min)
!
      end subroutine deallocate_block_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_block_points_4_itp(nblock_ref, nnod, xx, block)
!
      use set_ele_list_4_itp_table
!
      integer(kind = kint), intent(in) :: nnod, nblock_ref(3)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      type(block_4_interpolate), intent(inout) :: block
!
!
      call allocate_block_points(block, nblock_ref)
!
      call set_block_boundary(nnod, xx(1,1), block%num_itp_block(1),    &
     &    block%xdomain_min(1), block%xdomain_max(1), block%x_block)
      call set_block_boundary(nnod, xx(1,2), block%num_itp_block(2),    &
     &    block%xdomain_min(2), block%xdomain_max(2), block%y_block)
      call set_block_boundary(nnod, xx(1,3), block%num_itp_block(3),    &
     &    block%xdomain_min(3), block%xdomain_max(3), block%z_block)
!
      end subroutine set_block_points_4_itp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_block_points(id_file, block)
!
      integer(kind = kint), intent(in) :: id_file
      type(block_4_interpolate), intent(in) :: block
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of blocks (x, y, z)'
      write(id_file,*) '#'
!
      write(id_file,'(3i16)')  block%num_itp_block(1:3)
      close(id_file)
!
      write(id_file,*) '#'
      write(id_file,*) '#  x-grid'
      write(id_file,*) '#'
!
      do i = 0, block%num_itp_block(1)
        write(id_file,'(i16,1pE25.15e3)') i, block%x_block(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  y-grid'
      write(id_file,*) '#'
!
      do i = 0, block%num_itp_block(2)
        write(id_file,'(i16,1pE25.15e3)') i, block%y_block(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  z-grid'
      write(id_file,*) '#'
!
      do i = 0, block%num_itp_block(3)
        write(id_file,'(i16,1pE25.15e3)') i, block%z_block(i)
      end do
!
      end subroutine check_block_points
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module m_search_bolck_4_itp
