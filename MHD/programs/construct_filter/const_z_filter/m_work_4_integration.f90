!
!      module m_work_4_integration
!
!      Written by Kemorin
!
      module m_work_4_integration
!
      use m_precision
!
      implicit none
!
      integer (kind = kint) :: num_idx
!
      real(kind=kreal), dimension(:), allocatable:: xi_norm0
      real(kind=kreal), dimension(:,:,:), allocatable:: sk_norm
!
      real(kind=kreal), dimension(:), allocatable:: sk_norm_n
!
      real(kind=kreal), dimension(:,:), allocatable :: c_momentum
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_work_4_commute
!
       use m_commute_filter_z
!
       integer (kind= kint) :: i, j
!
       num_idx = 2*(nfilter2_1+3)*totalele*nfilter2_1
!
       allocate( xi_norm0(0:nfilter6_1) )
       allocate( sk_norm(num_idx,2,2) )
!
       allocate( sk_norm_n(0:nfilter6_1) )
       allocate( c_momentum(0:nfilter2_3,nfilter2_3) )
!
       xi_norm0 = 0.0d0
       sk_norm = 0.0d0
       sk_norm_n = 0.0d0
!
       c_momentum = 0.0d0
!
       c_momentum(0,1) = 1.0d0
       c_momentum(1,1) = 1.0d0
       do i = 2, nfilter2_3
        c_momentum(0,i) = 1.0d0
        do j = 1, i
         c_momentum(j,i) = c_momentum(j-1,i-1) + c_momentum(j,i-1)
        end do
       end do
!
       end subroutine allocate_work_4_commute
!
! -----------------------------------------------------------------------
!
!
       subroutine check_work_4_norm_ele(id_rank)
!
       use m_commute_filter_z
!
       integer :: id_rank
       integer(kind = kint) :: k ,is, idx, i, kfact
       integer(kind = kint) :: iele, jele, idx1, ia
!
!
       write(id_rank+50,*) 'jele, iele, kfact, i, sk_norm'
          do idx = 1, num_idx
!
             jele = mod(idx-1,nfilter2_1) + 1
             idx1 = (idx-jele)/nfilter2_1
             iele = mod( idx1, totalele ) + 1
             idx1 = (idx1-iele+1)/totalele
             kfact = mod( idx1, (nfilter2_3+1) )
             i = (idx1-kfact) / (nfilter2_3+1) + 1
!
             write(id_rank+50,'(4i4, 1p4e16.8)') jele, iele, kfact, i,  &
     &             ((sk_norm(idx,ia,is),ia=1,2), is=1,2 )
!
          end do
!
       end subroutine check_work_4_norm_ele
!
! -----------------------------------------------------------------------
!
      end module m_work_4_integration
