!>@file   t_z_filter_moment_work.f90
!!        module t_z_filter_moment_work
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM integration for vertical moments for filteres
!!
!!@verbatim
!!      subroutine alloc_int_commute_filter(ndep_filter, node_z,        &
!!     &                                    z_mom_WK)
!!      subroutine dealloc_int_commute_filter(z_mom_WK)
!!        integer(kind = kint), intent(in) :: ndep_filter
!!        type(node_data), intent(in) :: node_z
!!        type(z_filter_moment_work), intent(inout) :: z_mom_WK
!!
!!      subroutine check_int_commutative_filter(id_file, node_z,        &
!!     &                                        z_mom_WK)
!!        integer, intent(in) :: id_file
!!        type(node_data), intent(in) :: node_z
!!        type(z_filter_moment_work), intent(in) :: z_mom_WK
!!@endverbatim
!
      module t_z_filter_moment_work
!
      use m_precision
!
      implicit none
!
      type z_filter_moment_work
        integer(kind = kint) :: nside
        integer(kind = kint) :: ndep_filter
!
        real(kind = kreal), allocatable :: c_filter(:,:)
!
        real(kind = kreal), allocatable :: xmom_h_x(:,:)
        real(kind = kreal), allocatable :: xmom_h_y(:,:)
        real(kind = kreal), allocatable :: xmom_ht_x(:)
        real(kind = kreal), allocatable :: xmom_ht_y(:)
        real(kind = kreal), allocatable :: xmom_ht_z(:)
!
        real(kind = kreal), allocatable :: xmom_int_org(:,:,:)
        real(kind = kreal), allocatable :: xmom_int(:,:,:)
!
        real(kind = kreal), allocatable :: xmom_int_t(:,:)
        real(kind = kreal), allocatable :: xmom_int_to(:,:)
!
        real(kind = kreal), allocatable :: xmom_dt(:,:)
        real(kind = kreal), allocatable :: xmom_dot(:,:)
      end type z_filter_moment_work
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_int_commute_filter(ndep_filter, node_z,          &
     &                                    z_mom_WK)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ndep_filter
      type(node_data), intent(in) :: node_z
!
      type(z_filter_moment_work), intent(inout) :: z_mom_WK
!
!
      z_mom_WK%ndep_filter = ndep_filter
      z_mom_WK%nside = (z_mom_WK%ndep_filter-1) / 2
!
      allocate(z_mom_WK%c_filter(z_mom_WK%ndep_filter,node_z%numnod))
!
      allocate(z_mom_WK%xmom_h_x(z_mom_WK%ndep_filter,0:2))
      allocate(z_mom_WK%xmom_h_y(z_mom_WK%ndep_filter,0:2))
      allocate(z_mom_WK%xmom_ht_x(0:3))
      allocate(z_mom_WK%xmom_ht_y(0:3))
      allocate(z_mom_WK%xmom_ht_z(0:2))
!
      allocate(z_mom_WK%xmom_int_org(node_z%numnod,ndep_filter,0:2))
      allocate(z_mom_WK%xmom_int(node_z%numnod,ndep_filter,0:2))
      allocate(z_mom_WK%xmom_int_t(node_z%numnod,0:2))
      allocate(z_mom_WK%xmom_int_to(node_z%numnod,0:2))
      allocate(z_mom_WK%xmom_dt(node_z%numnod,0:2))
      allocate(z_mom_WK%xmom_dot(node_z%numnod,0:2))
!
      z_mom_WK%c_filter = 0.0d0
!
      z_mom_WK%xmom_h_x =  0.0d0
      z_mom_WK%xmom_h_y =  0.0d0
      z_mom_WK%xmom_ht_x = 0.0d0
      z_mom_WK%xmom_ht_y = 0.0d0
      z_mom_WK%xmom_ht_z = 0.0d0
!
      z_mom_WK%xmom_int_org = 0.0d0
      z_mom_WK%xmom_int =     0.0d0
      z_mom_WK%xmom_int_t =   0.0d0
      z_mom_WK%xmom_int_to =  0.0d0
      z_mom_WK%xmom_dt =      0.0d0
      z_mom_WK%xmom_dot =     0.0d0
!
      end subroutine alloc_int_commute_filter
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_int_commute_filter(z_mom_WK)
!
      type(z_filter_moment_work), intent(inout) :: z_mom_WK
!
!
      deallocate(z_mom_WK%c_filter)
!
      deallocate(z_mom_WK%xmom_h_x, z_mom_WK%xmom_h_y)
      deallocate(z_mom_WK%xmom_ht_x, z_mom_WK%xmom_ht_y)
      deallocate(z_mom_WK%xmom_ht_z)
!
      deallocate(z_mom_WK%xmom_int_org)
      deallocate(z_mom_WK%xmom_int, z_mom_WK%xmom_int_t)
      deallocate(z_mom_WK%xmom_int_to)
      deallocate(z_mom_WK%xmom_dt, z_mom_WK%xmom_dot)
!
!
      end subroutine dealloc_int_commute_filter
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_int_commutative_filter(id_file, node_z,          &
     &                                        z_mom_WK)
!
      use t_geometry_data
!
      integer, intent(in) :: id_file
      type(node_data), intent(in) :: node_z
      type(z_filter_moment_work), intent(in) :: z_mom_WK
!
      integer(kind = kint) :: i, k
!
!
      do i = 1, node_z%numnod
        write(id_file,'(a,i6)')  'c_filter (inod) = ', i
        write(id_file,'(1p5e16.8)')                                    &
     &            z_mom_WK%c_filter(1:z_mom_WK%ndep_filter,i)
      end do
!
      do k = 0, 2
        do i = 1, node_z%numnod
          write(id_file,'(a,2i6)')                                     &
     &            'xmom_int_org (order, inod) = ', k, i
          write(id_file,'(1p5e16.8)')                                  &
     &             z_mom_WK%xmom_int_org(i,1:z_mom_WK%ndep_filter,k)
        end do
      end do
!
      do k = 0, 2
        do i = 1, node_z%numnod
          write(id_file,'(a,2i6)')                                     &
     &              'xmom_int (order, inod) = ', k, i
          write(id_file,'(1p5e16.8)')                                  &
     &              z_mom_WK%xmom_int(i,1:z_mom_WK%ndep_filter,k)
        end do
      end do
!
      write(id_file,'(a)')  'xmom_int_t '
      do i = 1, node_z%numnod
        write(id_file,'(1p5e16.8)') z_mom_WK%xmom_int_t(i,0:2)
      end do
!
      end subroutine check_int_commutative_filter
!
!  ---------------------------------------------------------------------
!
      end module t_z_filter_moment_work
