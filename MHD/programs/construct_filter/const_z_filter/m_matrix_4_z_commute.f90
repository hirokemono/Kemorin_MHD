!
!      module m_matrix_4_z_commute
!
!      Written by Kemorin
!
!      subroutine allocate_matrix_4_commutation(numnod)
!      subroutine check_nod_normalize_matrix(my_rank, numnod)
!
      module m_matrix_4_z_commute
!
      use m_precision
!
      implicit none
!
!
      real(kind=kreal), dimension(:,:,:), allocatable:: d_norm_nod
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_matrix_4_commutation(numnod)
!
      use m_commute_filter_z
      use m_crs_matrix
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( d_norm_nod(numnod,nfilter2_3,0:nfilter2_3) )
!
      d_norm_nod =   0.0d0
!
      end subroutine allocate_matrix_4_commutation
!
! -----------------------------------------------------------------------
!
      subroutine check_nod_normalize_matrix(my_rank, numnod)
!
      use m_commute_filter_z
      use m_crs_matrix
!
      integer (kind = kint), intent(in) :: my_rank, numnod
      integer (kind = kint) :: i, j, k
!
!
      do k = 0, nfilter2_3
        do i = 1, numnod
        write(my_rank+60,*) 'd_norm_nod (node_id,order) = ', i, k
        write(my_rank+60,'(1p5e16.8)')                                  &
     &        (d_norm_nod(i,j,k), j=1, nfilter2_3)
        end do
      end do
!
      end subroutine check_nod_normalize_matrix
!
! -----------------------------------------------------------------------
!
      end module m_matrix_4_z_commute
      