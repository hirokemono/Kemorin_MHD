!>@file   copy_node_data.f90
!!@brief  module copy_node_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief Routines to copy element data
!!
!!@verbatim
!!      subroutine dup_node_data(nprocs, org_node, new_node)
!!      subroutine dup_derived_node_data(nprocs, org_node, new_node)
!!        integer, intent(in) :: nprocs
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(inout) :: new_node
!!
!!      subroutine copy_node_position(org_node, numnod, inod_global, xx)
!!        type(node_data), intent(in) :: org_node
!!        integer(kind=kint), intent(in)  ::  numnod
!!        integer(kind=kint_gl), intent(inout) :: inod_global(numnod)
!!        real(kind = kreal), intent(inout) :: xx(numnod,3)
!!      subroutine copy_sph_node_position(org_node, numnod,             &
!!     &          rr, a_r, theta, phi, ss, a_s)
!!       type(node_data), intent(in) :: org_node
!!       integer(kind=kint), intent(in)  ::  numnod
!!       real(kind = kreal), intent(inout) :: rr(numnod),    a_r(numnod)
!!       real(kind = kreal), intent(inout) :: theta(numnod), phi(numnod)
!!       real(kind = kreal), intent(inout) :: ss(numnod),    a_s(numnod)
!!      subroutine copy_position_range(org_node,                        &
!!     &          xyz_min_lc, xyz_max_lc, xyz_min_gl, xyz_max_gl)
!!       type(node_data), intent(in) :: org_node
!!       real(kind = kreal), intent(inout) :: xyz_min_lc(3),xyz_max_lc(3)
!!       real(kind = kreal), intent(inout) :: xyz_min_gl(3),xyz_max_gl(3)
!!      subroutine copy_global_numnod_list                              &
!!     &         (org_node, nprocs, istack_numnod, istack_internod)
!!       type(node_data), intent(in) :: org_node
!!       integer, intent(in) :: nprocs
!!       integer(kind=kint_gl), intent(inout)                          &
!!      &                      :: istack_numnod(0:nprocs)
!!       integer(kind=kint_gl), intent(inout)                          &
!!      &                      :: istack_internod(0:nprocs)
!!@endverbatim
!
      module copy_node_data
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dup_node_data(nprocs, org_node, new_node)
!
      integer, intent(in) :: nprocs
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
!
      new_node%numnod =        org_node%numnod
      new_node%internal_node = org_node%internal_node
      call alloc_node_geometry_base(new_node)
      call copy_node_position(org_node, new_node%numnod,                &
     &                        new_node%inod_global, new_node%xx)
!
      call dup_derived_node_data(nprocs, org_node, new_node)
!
      end subroutine dup_node_data
!
! ----------------------------------------------------------------------
!
      subroutine dup_derived_node_data(nprocs, org_node, new_node)
!
      integer, intent(in) :: nprocs
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
!
      call alloc_sph_node_geometry(new_node)
      call copy_sph_node_position(org_node, new_node%numnod,            &
     &    new_node%rr, new_node%a_r, new_node%theta, new_node%phi,      &
     &    new_node%ss, new_node%a_s)
!
      call alloc_node_param_smp(new_node)
      call count_node_4_smp_mesh(new_node)
!
      call alloc_numnod_stack(nprocs, new_node)
      call copy_global_numnod_list(org_node, nprocs,                    &
     &    new_node%istack_numnod, new_node%istack_internod)
      call copy_position_range(org_node,                                &
     &    new_node%xyz_min_lc, new_node%xyz_max_lc,                     &
     &    new_node%xyz_min_gl, new_node%xyz_max_gl)
!
      end subroutine dup_derived_node_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_node_position(org_node, numnod, inod_global, xx)
!
      type(node_data), intent(in) :: org_node
      integer(kind=kint), intent(in)  ::  numnod
      integer(kind=kint_gl), intent(inout) :: inod_global(numnod)
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
!$omp parallel workshare
      inod_global(1:numnod) = org_node%inod_global(1:numnod)
      xx(1:numnod,1) = org_node%xx(1:numnod,1)
      xx(1:numnod,2) = org_node%xx(1:numnod,2)
      xx(1:numnod,3) = org_node%xx(1:numnod,3)
!$omp end parallel workshare
!
      end subroutine copy_node_position
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_position(org_node, numnod,               &
     &          rr, a_r, theta, phi, ss, a_s)
!
      type(node_data), intent(in) :: org_node
      integer(kind=kint), intent(in)  ::  numnod
      real(kind = kreal), intent(inout) :: rr(numnod),    a_r(numnod)
      real(kind = kreal), intent(inout) :: theta(numnod), phi(numnod)
      real(kind = kreal), intent(inout) :: ss(numnod),    a_s(numnod)
!
!$omp parallel workshare
      rr(1:numnod) =    org_node%rr(1:numnod)
      theta(1:numnod) = org_node%theta(1:numnod)
      phi(1:numnod) =   org_node%phi(1:numnod)
      a_r(1:numnod) =   org_node%a_r(1:numnod)
      ss(1:numnod) =    org_node%ss(1:numnod)
      a_s(1:numnod) =   org_node%a_s(1:numnod)
!$omp end parallel workshare
!
      end subroutine copy_sph_node_position
!
! ----------------------------------------------------------------------
!
      subroutine copy_position_range(org_node,                          &
     &          xyz_min_lc, xyz_max_lc, xyz_min_gl, xyz_max_gl)
!
      type(node_data), intent(in) :: org_node
!
      real(kind = kreal), intent(inout) :: xyz_min_lc(3), xyz_max_lc(3)
      real(kind = kreal), intent(inout) :: xyz_min_gl(3), xyz_max_gl(3)
!
      xyz_max_lc(1:3) = org_node%xyz_max_lc(1:3)
      xyz_min_lc(1:3) = org_node%xyz_min_lc(1:3)
!
      xyz_max_gl(1:3) = org_node%xyz_max_gl(1:3)
      xyz_min_gl(1:3) = org_node%xyz_min_gl(1:3)
!
      end subroutine copy_position_range
!
! ----------------------------------------------------------------------
!
      subroutine copy_global_numnod_list                                &
     &         (org_node, nprocs, istack_numnod, istack_internod)
!
      type(node_data), intent(in) :: org_node
      integer, intent(in) :: nprocs
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_numnod(0:nprocs)
      integer(kind=kint_gl), intent(inout)                              &
     &                      :: istack_internod(0:nprocs)
!
!
!$omp parallel workshare
      istack_numnod(0:nprocs) = org_node%istack_numnod(0:nprocs)
      istack_internod(0:nprocs) = org_node%istack_internod(0:nprocs)
!$omp end parallel workshare
!
      end subroutine copy_global_numnod_list
!
!-----------------------------------------------------------------------
!
      end module copy_node_data
