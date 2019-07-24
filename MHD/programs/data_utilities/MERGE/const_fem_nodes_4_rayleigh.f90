!>@file   const_fem_nodes_4_rayleigh.f90
!!@brief  module const_fem_nodes_4_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_FEM_rayleigh
!!      subroutine analyze_FEM_rayleigh
!!@endverbatim
!
      module const_fem_nodes_4_rayleigh
!
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_fem_nodes_4_rayleigh(nri, nth, mphi,            &
     &          kst, ked, lst, led, radius, theta, mesh, group)
!
      integer(kind = kint), intent(in) :: nri, nth, mphi
      integer(kind = kint), intent(in) :: kst, ked, lst, led
      real(kind - kreal), intent(in) :: radius(nri)
      real(kind - kreal), intent(in) :: theta(nth)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      call const_fem_nodes_4_rayleigh(nri, nth, mphi,                   &
     &          kst, ked, lst, led, radius, theta, mesh, group)
!
      mesh_file%file_prefix = sph_file_head
      call mpi_output_mesh(mesh_file, mesh, group)
!
      end subroutine output_fem_nodes_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine s_const_fem_nodes_4_rayleigh(nri, nth, mphi,           &
     &          kst, ked, lst, led, radius, theta, mesh, group)
!
      integer(kind = kint), intent(in) :: nri, nth, mphi
      integer(kind = kint), intent(in) :: kst, ked, lst, led
      real(kind - kreal), intent(in) :: radius(nri)
      real(kind - kreal), intent(in) :: theta(nth)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      mesh%node%numnod = (ked - kst + 1) * (led - lst + 1) * mphi
      mesh%node%internal_node = 0
      call alloc_node_geometry_base(mesh%node)
!
      call nodes_4_rayleigh(nri, nth, mphi, kst, ked, lst, led,         &
     &    radius, theta, mesh%node)
!
      mesh%ele%numele = 0
      mesh%ele%nnod_4_ele = num_t_linear
      call allocate_ele_connect_type(mesh%ele)
!
      call empty_comm_table(mesh%comm_tbl)
!
!
      group%nod_grp%num_grp = 0
      call alloc_group_num(group%nod_grp)
      call alloc_group_item(group%nod_grp)
!
      group%ele_grp%num_grp = 0
      call alloc_group_num(group%ele_grp)
      call alloc_group_item(group%ele_grp)
!
      group%surf_grp%num_grp = 0
      call alloc_sf_group_num(group%surf_grp)
      call alloc_sf_group_item(group%surf_grp)
!
      end subroutine s_const_fem_nodes_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine nodes_4_rayleigh(nri, nth, mphi, kst, ked, lst, led,   &
     &          radius, theta, node)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: nri, nth, mphi
      integer(kind = kint), intent(in) :: kst, ked, lst, led
      real(kind - kreal), intent(in) :: radius(nri)
      real(kind - kreal), intent(in) :: theta(nth)
!
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: k, l, m, k_to_out, inod
!
!
      allocate(r(node%numnod), t(node%numnod), p(node%numnod))
!
!$omp parallel do private(k,l,m,k_to_out,inod)
      do k = kst, ked
        do l = lst, led
          do m = 1, mphi
            k_to_out = nri - ked + 1
            inod = inod + 1
            node%inod_global(inod) = k_to_out + (l-1) * nth             &
     &                                   + (m-1) * nth * nri
            r(inod) = radius(k)
            t(inod) = theta(l)
            p(inod) = two * pi * dble(m) / dble(mphi)
          end do
        end do
      end do
!$omp end parallel do
      deallocate(r, t, p)
!
      call position_2_xyz(node%numnod, r, t, p,                         &
     &    node%xx(1,1), node%xx(1,2), node%xx(1,3))
!
      end subroutine nodes_4_rayleigh
!
! ----------------------------------------------------------------------
!
      end module const_fem_nodes_4_rayleigh
