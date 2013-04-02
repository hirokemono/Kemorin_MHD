!
!      module m_matrix_4_commutation
!
      module m_matrix_4_commutation
!
!      Written by Kemorin
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
      subroutine allocate_matrix_4_commutation
!
      use m_geometry_parameter
      use m_commute_filter_z
      use m_crs_connect
      use m_crs_matrix
!
      allocate( d_norm_nod(numnod,nfilter2_3,0:nfilter2_3) )
!
      d_norm_nod =   0.0d0
!
      end subroutine allocate_matrix_4_commutation
!
! -----------------------------------------------------------------------
!
      subroutine check_nod_normalize_matrix(my_rank)
!
      use m_geometry_parameter
      use m_commute_filter_z
      use m_crs_matrix
!
      integer (kind = kint) :: my_rank
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
      end module m_matrix_4_commutation
      