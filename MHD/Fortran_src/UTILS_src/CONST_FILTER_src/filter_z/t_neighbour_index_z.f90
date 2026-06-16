!>@file   t_neighbour_index_z.f90
!!        module t_neighbour_index_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2008
!
!>@brief Neighboring data to construct vertical filter
!!
!!@verbatim
!!      subroutine alloc_z_neib_index(numnod, nfilter, zfilter_wk)
!!      subroutine dealloc_z_neib_index(zfilter_wk)
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: nfilter
!!        type(z_filter_work), intent(inout) :: zfilter_wk
!!      subroutine check_z_neib_index(id_rank, numnod, zfilter_wk)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numnod
!!        type(z_filter_work), intent(in) :: zfilter_wk
!!      subroutine check_difference_of_position(id_rank, nele,          &
!!     &                                        neib_z, zfilter_wk)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: nele
!!        integer(kind = kint), intent(in) :: internal_node
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(z_filter_work), intent(in) :: zfilter_wk
!!
!!      subroutine s_set_neib_connect_z(totalele, nfilter2_1,           &
!!     &                                nneib_ele, jdx)
!!        integer(kind = kint), intent(in) :: totalele
!!        integer(kind = kint), intent(in) :: nfilter2_1
!!        integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: jdx(totalele,nfilter2_1,3)
!!      subroutine s_set_neib_connect_z(totalele, nfilter2_1,           &
!!     &                                nneib_ele, jdx)
!!        integer(kind = kint), intent(in) :: totalele
!!        integer(kind = kint), intent(in) :: nfilter2_1
!!        integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: jdx(totalele,nfilter2_1,3)
!!@endverbatim
!!
      module t_neighbour_index_z
!
      use m_precision
!
      implicit none
!
      type z_filter_work
        integer(kind = kint), allocatable :: ncomp_z_st(:)
!
        integer(kind = kint) :: nfil_jdx
        integer(kind = kint), allocatable :: jdx_z(:,:,:)
        real(kind = kreal), allocatable :: alpha(:,:,:)
      end type z_filter_work
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_z_neib_index(numnod, nfilter, zfilter_wk)
!
      use m_commute_filter_z
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nfilter
      type(z_filter_work), intent(inout) :: zfilter_wk
!
!
      zfilter_wk%nfil_jdx = nfilter
      allocate(zfilter_wk%ncomp_z_st(numnod))
      allocate(zfilter_wk%jdx_z(totalele,zfilter_wk%nfil_jdx,3))
      allocate(zfilter_wk%alpha(totalele,0:zfilter_wk%nfil_jdx,2))
!
      if(numnod .gt. 0) zfilter_wk%ncomp_z_st(1:numnod) = 0
      if(totalele .gt. 0) then
        zfilter_wk%jdx_z(1:totalele,1:zfilter_wk%nfil_jdx,1:3) = 0
        zfilter_wk%alpha(1:totalele,0:zfilter_wk%nfil_jdx,1:2) = 0.0d0
      end if
!
      end subroutine alloc_z_neib_index
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_z_neib_index(zfilter_wk)
!
      type(z_filter_work), intent(inout) :: zfilter_wk
!
      deallocate(zfilter_wk%ncomp_z_st)
      deallocate(zfilter_wk%jdx_z)
      deallocate(zfilter_wk%alpha)
!
      end subroutine dealloc_z_neib_index
!
!  ---------------------------------------------------------------------
!
      subroutine check_z_neib_index(id_rank, nnod, nele, zfilter_wk)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(z_filter_work), intent(in) :: zfilter_wk
!
      integer(kind = kint) :: i, k
!
!
      write(50+id_rank,*) 'ncomp_z_st'
      write(50+id_rank,'(10i16)') zfilter_wk%ncomp_z_st(1:nnod)
!
      do k = 1, 2
        do i = 1, nele
          write(50+id_rank,*) 'k, i, jdx_z'
          write(50+id_rank,'(10i16)') k, i,                             &
     &              zfilter_wk%jdx_z(i,1:zfilter_wk%nfil_jdx,k)
        end do
      end do
!
      end subroutine check_z_neib_index
!
!  ---------------------------------------------------------------------
!
      subroutine check_difference_of_position(id_rank, nele,            &
     &                                        neib_z, zfilter_wk)
!
      use t_neighbour_data_z
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nele
      type(neighbour_data_z), intent(in) :: neib_z
      type(z_filter_work), intent(in) :: zfilter_wk
!
      integer(kind = kint) :: i, j, k
!
!
      write(50+id_rank,*) 'element, direction, distance, alpha'
      do i = 1, nele
        do k = 1, 2
          do j = 0, neib_z%nneib_ele(i,k)
            write(50+id_rank,*) i, k, j, zfilter_wk%alpha(i,j,k)
          end do
        end do
      end do
!
      end subroutine check_difference_of_position
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_neib_connect_z(totalele, nfilter2_1,             &
     &                                nneib_ele, jdx)
!
      integer(kind = kint), intent(in) :: totalele
      integer(kind = kint), intent(in) :: nfilter2_1
      integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: jdx(totalele,nfilter2_1,3)
!
      integer(kind = kint) :: i, j, j1
!
!
      do i = 1, totalele
        j1 = 1
        do j = nneib_ele(i,1), 1, -1
          jdx(i,j1,1) = j
          jdx(i,j1,2) = 1
          j1 = j1 + 1
        end do
        jdx(i,j1,1) = 0
        jdx(i,j1,2) = 2
        j1 = j1 + 1
        do j = 1, nneib_ele(i,2)
          jdx(i,j1,1) = j
          jdx(i,j1,2) = 2
          j1 = j1 + 1
        end do
      end do
!
      end subroutine s_set_neib_connect_z
!
!-----------------------------------------------------------------------
!
      subroutine set_difference_of_position(node, edge,                 &
     &          nneib_ele, ineib_ele, alpha)
!
      use t_geometry_data
      use t_edge_data
!
      use m_commute_filter_z
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
!      integer(kind = kint), intent(in) :: totalele, nfilter2_1
      integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
      integer(kind = kint), intent(in)                                  &
     &                     :: ineib_ele(totalele,nfilter2_1,2)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: alpha(totalele,0:nfilter2_1,2)
!
      real(kind = kreal) :: dz0, dzeta
      integer(kind = kint) :: i, j, inod1, inod2, jnod1, jnod2, jele
!
!
      write(*,*) size(alpha,1), size(alpha,2), size(alpha,3)
!
      do i = 1, totalele
        inod1 = edge%ie_edge(i,1)
        inod2 = edge%ie_edge(i,2)
        jnod1 = edge%ie_edge(i,1)
        jnod2 = edge%ie_edge(i,2)
        dz0 =   node%xx(inod2,3) - node%xx(inod1,3)
        dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
        alpha(i,0,1) = dz0/dzeta - 1
        alpha(i,0,2) = alpha(i,1,1)
!
        do j = 1, nneib_ele(i,1)
          jele = ineib_ele(i,j,1)
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
          alpha(i,j,1) = dz0/dzeta - 1
        end do
!
        do j = 1, nneib_ele(i,2)
          jele = ineib_ele(i,j,2)
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
          alpha(i,j,2) = dz0/dzeta - 1
        end do
      end do
!
      end subroutine set_difference_of_position
!
!   --------------------------------------------------------------------
!
      end module t_neighbour_index_z
