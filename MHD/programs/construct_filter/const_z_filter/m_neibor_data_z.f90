!
!      module m_neibor_data_z
!
!      Written by Kemorin
!
!      subroutine allocate_neib_nod(numnod, internal_node)
!      subroutine allocate_neib_ele
!      subroutine check_neib_nod(id_rank, numnod, internal_node)
!      subroutine check_neib_ele(id_rank)
!
      module m_neibor_data_z
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nummat
      integer(kind = kint), dimension(:,:), allocatable :: nneib_nod
      integer(kind = kint), dimension(:,:,:), allocatable :: ineib_nod
!
      integer(kind = kint), dimension(:), allocatable :: ncomp_st
!
      integer(kind = kint), dimension(:,:), allocatable :: nneib_ele
      integer(kind = kint), dimension(:,:,:), allocatable :: ineib_ele
!
      integer(kind = kint), dimension(:,:,:), allocatable :: jdx
!
      real(kind = kreal), dimension(:,:,:), allocatable :: alpha
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_neib_nod(numnod, internal_node)
!
      use m_commute_filter_z
!
      integer(kind = kint), intent(in) :: numnod, internal_node
!
      allocate( nneib_nod(internal_node,2) )
      allocate( ineib_nod(internal_node,nfilter2_2,2) )
      
      allocate( ncomp_st(numnod) )

      nneib_nod = numfilter + 1
      ineib_nod = -1
!
      ncomp_st = 0
!
      end subroutine allocate_neib_nod
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_neib_ele
!
      use m_commute_filter_z
!
      allocate( nneib_ele(totalele,2) )
      allocate( ineib_ele(totalele,nfilter2_1,2) )
      allocate( jdx(totalele,nfilter2_1,3) )
!
      allocate( alpha(totalele,0:nfilter2_1,2) )
!
      nneib_ele = numfilter
      ineib_ele = -1
!
      jdx = 0
!
       alpha = 0.0d0
!
      end subroutine allocate_neib_ele
!
!  ---------------------------------------------------------------------
!
      subroutine check_neib_nod(id_rank, numnod, internal_node)
!
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod, internal_node
!
      integer(kind = kint) :: i, j, k
!
      write(50+id_rank,*) 'nummat', nummat
      write(50+id_rank,*) 'nneib_nod'
      write(50+id_rank,'(10i16)') (nneib_nod(i,1),i=1,internal_node)
      write(50+id_rank,'(10i16)') (nneib_nod(i,2),i=1,internal_node)
      write(50+id_rank,*) 'direction, inod, ineib_nod'
      do k = 1, 2
        do i = 1, internal_node
          write(50+id_rank,'(10i16)') k, i,                             &
     &            (ineib_nod(i,j,k),j=1,nneib_nod(i,k))
        end do
      end do
!
      write(50+id_rank,*) 'ncomp_st'
      write(50+id_rank,'(10i16)') (ncomp_st(i),i=1,numnod)
!
      end subroutine check_neib_nod
!
!  ---------------------------------------------------------------------
!
      subroutine check_neib_ele(id_rank)
!
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: i, j, k
!
      write(50+id_rank,*) 'nneib_ele'
      write(50+id_rank,'(10i16)') (nneib_ele(i,1),i=1,totalele)
      write(50+id_rank,'(10i16)') (nneib_ele(i,2),i=1,totalele)
      write(50+id_rank,*) 'direction, iele, ineib_ele'
      do k = 1, 2
        do i = 1, totalele
          write(50+id_rank,'(10i16)') k, i,                             &
     &            (ineib_ele(i,j,k),j=1,nneib_ele(i,k))
        end do
      end do
!
      do k = 1, 2
        do i = 1, totalele
          write(50+id_rank,*) 'k, i, jdx'
          write(50+id_rank,'(10i16)') k, i,                             &
     &              (jdx(i,j,k),j=1,nfilter2_1)
        end do
      end do
!
      end subroutine check_neib_ele
!
!  ---------------------------------------------------------------------
!
      subroutine check_difference_of_position(id_rank)
!
      use m_commute_filter_z
!
      integer(kind = kint) :: id_rank
      integer(kind = kint) :: i, j, k
!
      write(50+id_rank,*) 'element, direction, distance, alpha'
      do i = 1, totalele
        do k = 1, 2
          do j = 0, nneib_ele(i,k)
            write(50+id_rank,*) i, k, j, alpha(i,j,k)
          end do
        end do
      end do
!
!
      end subroutine check_difference_of_position
!
!  ---------------------------------------------------------------------
!
      end module m_neibor_data_z
