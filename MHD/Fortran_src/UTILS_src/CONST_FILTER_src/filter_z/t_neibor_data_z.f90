!>@file   t_neibor_data_z.f90
!!        module t_neibor_data_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2008
!
!>@brief Neighboring data to construct vertical filter
!!
!!@verbatim
!!      subroutine alloc_z_neib_nod(nnod, ndepth, numfilter, neib_z)
!!      subroutine alloc_z_neib_ele(nele, ndepth, numfilter, neib_z)
!!      subroutine dealloc_z_neib_nod(neib_z)
!!      subroutine dealloc_z_neib_ele(neib_z)
!!        integer(kind = kint), intent(in) :: nnod
!!        integer(kind = kint), intent(in) :: nele
!!        integer(kind = kint), intent(in) :: ndepth
!!        integer(kind = kint), intent(in) :: numfilter
!!        type(neighbour_data_z), intent(inout) :: neib_z
!!
!!      subroutine check_z_neib_nod(id_rank, nnod_z, neib_z)
!!      subroutine check_z_neib_ele(id_rank, numele_z, neib_z)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: nnod_z
!!        integer(kind = kint), intent(in) ::  numele_z
!!        type(neighbour_data_z), intent(in) :: neib_z
!!@endverbatim
!!
      module t_neibor_data_z
!
      use m_precision
!
      implicit none
!
      type neighbour_data_z
        integer(kind = kint), allocatable :: nneib_nod(:,:)
        integer(kind = kint), allocatable :: ineib_nod(:,:,:)
!
        integer(kind = kint), allocatable :: nneib_ele(:,:)
        integer(kind = kint), allocatable :: ineib_ele(:,:,:)
      end type neighbour_data_z
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_z_neib_nod(nnod, ndepth, numfilter, neib_z)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ndepth
      integer(kind = kint), intent(in) :: numfilter
      type(neighbour_data_z), intent(inout) :: neib_z
!
!
      allocate(neib_z%nneib_nod(nnod,2))
      allocate(neib_z%ineib_nod(nnod,ndepth,2))

      neib_z%nneib_nod(1:nnod,1:2) = numfilter
      neib_z%ineib_nod(1:nnod,1:ndepth,1:2) = -1
!
      end subroutine alloc_z_neib_nod
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_z_neib_ele(nele, ndepth, numfilter, neib_z)
!
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: ndepth
      integer(kind = kint), intent(in) :: numfilter
      type(neighbour_data_z), intent(inout) :: neib_z
!
!
      allocate(neib_z%nneib_ele(nele,2))
      allocate(neib_z%ineib_ele(nele,ndepth,2))
!
      neib_z%nneib_ele(1:nele,1:2) = numfilter
      neib_z%ineib_ele(1:nele,1:ndepth,1:2) = -1
!
      end subroutine alloc_z_neib_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_z_neib_nod(neib_z)
!
      type(neighbour_data_z), intent(inout) :: neib_z
!
      deallocate(neib_z%nneib_nod, neib_z%ineib_nod)
!
      end subroutine dealloc_z_neib_nod
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_z_neib_ele(neib_z)
!
      type(neighbour_data_z), intent(inout) :: neib_z
!
      deallocate(neib_z%nneib_ele, neib_z%ineib_ele)
!
      end subroutine dealloc_z_neib_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_z_neib_nod(id_rank, nnod_z, neib_z)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_z
      type(neighbour_data_z), intent(in) :: neib_z
!
      integer(kind = kint) :: i, k
!
!
      write(50+id_rank,'(10i16)') neib_z%nneib_nod(1:nnod_z,1)
      write(50+id_rank,'(10i16)') neib_z%nneib_nod(1:nnod_z,2)
      write(50+id_rank,*) 'direction, inod, ineib_nod'
      do k = 1, 2
        do i = 1, nnod_z
          write(50+id_rank,'(10i16)') k, i,                             &
     &            neib_z%ineib_nod(i,neib_z%nneib_nod(i,k),k)
        end do
      end do
!
      end subroutine check_z_neib_nod
!
!  ---------------------------------------------------------------------
!
      subroutine check_z_neib_ele(id_rank, numele_z, neib_z)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) ::  numele_z
      type(neighbour_data_z), intent(in) :: neib_z
!
      integer(kind = kint) :: i, k
!
      write(50+id_rank,*) 'nneib_ele'
      write(50+id_rank,'(10i16)') (neib_z%nneib_ele(i,1),i=1,numele_z)
      write(50+id_rank,'(10i16)') (neib_z%nneib_ele(i,2),i=1,numele_z)
      write(50+id_rank,*) 'direction, iele, ineib_ele'
      do k = 1, 2
        do i = 1, numele_z
          write(50+id_rank,'(10i16)') k, i,                             &
     &            neib_z%ineib_ele(i,1:neib_z%nneib_ele(i,k),k)
        end do
      end do
!
      end subroutine check_z_neib_ele
!
!  ---------------------------------------------------------------------
!
      end module t_neibor_data_z
