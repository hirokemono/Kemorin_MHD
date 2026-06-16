!>@file   t_neighbour_data_z.f90
!!        module t_neighbour_data_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2008
!
!>@brief Neighboring data to construct vertical filter
!!
!!@verbatim
!!      subroutine init_z_neighbour(nnod, nele, ndepth_nod, ndepth_ele, &
!!     &                            numfilter_nod, numfilter_ele, neib_z)
!!      subroutine dealloc_z_neighbour(neib_z)
!!        integer(kind = kint), intent(in) :: nnod, nele
!!        integer(kind = kint), intent(in) :: ndepth_nod, ndepth_ele
!!        integer(kind = kint), intent(in) :: numfilter_nod
!!        integer(kind = kint), intent(in) :: numfilter_ele
!!        type(neighbour_data_z), intent(inout) :: neib_z
!!      subroutine check_z_neighbour(id_rank, nnod_z, numele_z, neib_z)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: nnod_z, numele_z
!!        type(neighbour_data_z), intent(in) :: neib_z
!!@endverbatim
!!
      module t_neighbour_data_z
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
      private :: alloc_z_neib_nod, alloc_z_neib_ele
      private :: check_z_neib_nod, check_z_neib_ele
      private :: s_set_neib_nod_z, s_set_neib_ele_z
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_z_neighbour(nnod, nele, ndepth_nod, ndepth_ele,   &
     &                            numfilter_nod, numfilter_ele, neib_z)
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: ndepth_nod, ndepth_ele
      integer(kind = kint), intent(in) :: numfilter_nod
      integer(kind = kint), intent(in) :: numfilter_ele
      type(neighbour_data_z), intent(inout) :: neib_z
!
!
      call alloc_z_neib_nod(nnod, ndepth_nod, numfilter_nod, neib_z)
      call s_set_neib_nod_z(nnod, ndepth_nod, numfilter_nod,            &
     &                      neib_z%nneib_nod, neib_z%ineib_nod)
!
      call alloc_z_neib_ele(nele, ndepth_ele, numfilter_ele, neib_z)
      call s_set_neib_ele_z(nele, ndepth_ele, numfilter_ele,            &
     &                      neib_z%nneib_ele, neib_z%ineib_ele)
!
      end subroutine init_z_neighbour
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_z_neighbour(neib_z)
!
      type(neighbour_data_z), intent(inout) :: neib_z
!
      deallocate(neib_z%nneib_nod, neib_z%ineib_nod)
      deallocate(neib_z%nneib_ele, neib_z%ineib_ele)
!
      end subroutine dealloc_z_neighbour
!
!  ---------------------------------------------------------------------
!
      subroutine check_z_neighbour(id_rank, nnod_z, numele_z, neib_z)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_z, numele_z
      type(neighbour_data_z), intent(in) :: neib_z
!
!
      call check_z_neib_nod(id_rank, nnod_z, neib_z)
      call check_z_neib_ele(id_rank, numele_z, neib_z)
!
      end subroutine check_z_neighbour
!
!  ---------------------------------------------------------------------
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
!  ---------------------------------------------------------------------
!
      subroutine s_set_neib_nod_z(numnod, nsize, numfilter,             &
     &                            nneib_nod, ineib_nod)
!
      integer(kind = kint), intent(in) :: numnod, nsize, numfilter
      integer(kind = kint), intent(inout) :: nneib_nod(numnod,2)
      integer(kind = kint), intent(inout)                               &
     &                      :: ineib_nod(numnod,nsize,2)
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, numfilter
        do i = 1, numnod
          ineib_nod(i,j,1) = i-j;
          ineib_nod(i,j,2) = i+j;
        end do
      end do
!
      do j = numfilter, 1, -1
        do i = 1, numnod
          if (ineib_nod(i,j,1) .lt. 1) then
            nneib_nod(i,1) = nneib_nod(i,1) - 1
            nneib_nod(i,2) = nneib_nod(i,2) + 1
            ineib_nod(i,j,1) = -1
            ineib_nod(i,nneib_nod(i,2),2)                               &
     &            = ineib_nod(i,nneib_nod(i,2)-1,2) + 1
           end if
          if (ineib_nod(i,j,2) .gt. numnod) then
            nneib_nod(i,2) = nneib_nod(i,2) - 1
            nneib_nod(i,1) = nneib_nod(i,1) + 1
            ineib_nod(i,j,2) = -1
            ineib_nod(i,nneib_nod(i,1),1)                               &
     &            = ineib_nod(i,nneib_nod(i,1)-1,1) - 1
           end if
         end do
       end do
!
!
      end subroutine s_set_neib_nod_z
!
!-----------------------------------------------------------------------
!
      subroutine s_set_neib_ele_z(numele, nsize, numfilter,             &
     &                            nneib_ele, ineib_ele)
!
      integer(kind = kint), intent(in) :: numele, numfilter, nsize
!
      integer(kind = kint), intent(inout) :: nneib_ele(numele,2)
        integer(kind = kint), intent(inout)                             &
     &                       :: ineib_ele(numele,nsize,2)
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, numfilter
        do i = 1, numele
          ineib_ele(i,j,1) = i-j;
          ineib_ele(i,j,2) = i+j;
        end do
      end do
!
      do j = numfilter, 1, -1
        do i = 1, numele
          if (ineib_ele(i,j,1) .lt. 1) then
            nneib_ele(i,1) = nneib_ele(i,1) - 1
            nneib_ele(i,2) = nneib_ele(i,2) + 1
            ineib_ele(i,j,1) = -1
            ineib_ele(i,nneib_ele(i,2),2)                               &
     &            = ineib_ele(i,nneib_ele(i,2)-1,2) + 1
           end if
          if (ineib_ele(i,j,2) .gt. numele) then
            nneib_ele(i,2) = nneib_ele(i,2) - 1
            nneib_ele(i,1) = nneib_ele(i,1) + 1
            ineib_ele(i,j,2) = -1
            ineib_ele(i,nneib_ele(i,1),1)                               &
     &            = ineib_ele(i,nneib_ele(i,1)-1,1) - 1
           end if
         end do
       end do
!
!
      end subroutine s_set_neib_ele_z
!
!-----------------------------------------------------------------------
!
      end module t_neighbour_data_z
