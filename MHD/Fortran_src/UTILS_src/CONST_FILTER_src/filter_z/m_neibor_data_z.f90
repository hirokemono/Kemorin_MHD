!>@file   m_neibor_data_z.f90
!!        module m_neibor_data_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2008
!
!>@brief Neighboring data to construct vertical filter
!!
!!@verbatim
!!      subroutine alloc_z_neib_index(numnod, zfilter_wk)
!!      subroutine dealloc_z_neib_index(zfilter_wk)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(z_filter_work), intent(inout) :: zfilter_wk
!!      subroutine check_z_neib_index(id_rank, numnod, zfilter_wk)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numnod
!!        type(z_filter_work), intent(in) :: zfilter_wk
!!      subroutine check_difference_of_position(id_rank,                &
!!     &                                        neib_z, zfilter_wk)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: internal_node
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(z_filter_work), intent(in) :: zfilter_wk
!!@endverbatim
!!
      module m_neibor_data_z
!
      use m_precision
!
      implicit none
!
      type z_filter_work
        integer(kind = kint), allocatable :: ncomp_z_st(:)
!
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
      subroutine alloc_z_neib_index(numnod, zfilter_wk)
!
      use m_commute_filter_z
!
      integer(kind = kint), intent(in) :: numnod
      type(z_filter_work), intent(inout) :: zfilter_wk
!
!
      allocate(zfilter_wk%ncomp_z_st(numnod))
      allocate(zfilter_wk%jdx_z(totalele,nfilter2_1,3))
      allocate(zfilter_wk%alpha(totalele,0:nfilter2_1,2))
!
      if(numnod .gt. 0) zfilter_wk%ncomp_z_st(1:numnod) = 0
      if(totalele .gt. 0) then
        zfilter_wk%jdx_z(1:totalele,1:nfilter2_1,1:3) = 0
        zfilter_wk%alpha(1:totalele,0:nfilter2_1,1:2) = 0.0d0
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
      subroutine check_z_neib_index(id_rank, numnod, zfilter_wk)
!
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod
      type(z_filter_work), intent(in) :: zfilter_wk
!
      integer(kind = kint) :: i, k
!
!
      write(50+id_rank,*) 'ncomp_z_st'
      write(50+id_rank,'(10i16)') zfilter_wk%ncomp_z_st(1:numnod)
!
      do k = 1, 2
        do i = 1, totalele
          write(50+id_rank,*) 'k, i, jdx_z'
          write(50+id_rank,'(10i16)') k, i,                             &
     &              zfilter_wk%jdx_z(i,1:nfilter2_1,k)
        end do
      end do
!
      end subroutine check_z_neib_index
!
!  ---------------------------------------------------------------------
!
      subroutine check_difference_of_position(id_rank,                  &
     &                                        neib_z, zfilter_wk)
!
      use t_neibor_data_z
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      type(neighbour_data_z), intent(in) :: neib_z
      type(z_filter_work), intent(in) :: zfilter_wk
!
      integer(kind = kint) :: i, j, k
!
!
      write(50+id_rank,*) 'element, direction, distance, alpha'
      do i = 1, totalele
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
!
      end module m_neibor_data_z
