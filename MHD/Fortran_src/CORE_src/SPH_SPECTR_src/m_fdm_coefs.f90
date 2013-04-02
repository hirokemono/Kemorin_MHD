!
!      module m_fdm_coefs
!
!     Written by H. Matsui on Jan, 2010
!
!     define of differences
!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!
!      dfdr =    d1nod_mat_fdm_2(-1) * d_nod(k-1)
!              + d1nod_mat_fdm_2( 0) * d_nod(k  )
!              + d1nod_mat_fdm_2( 1) * d_nod(k+1)
!      d2fdr2 =  d2nod_mat_fdm_2(-1) * d_nod(k-1)
!              + d2nod_mat_fdm_2( 0) * d_nod(k  )
!              + d2nod_mat_fdm_2( 1) * d_nod(k+1)
!
!    derivatives on node by element field
!      d_nod =   d_nod_mat_fdm_2e(0) * d_ele(k  )
!              + d_nod_mat_fdm_2e(1) * d_ele(k+1)
!      dfdr =    d1nod_mat_fdm_2e(0) * d_ele(k  )
!              + d1nod_mat_fdm_2e(1) * d_ele(k+1)
!
!      subroutine allocate_fdm_coefs(nri)
!      subroutine deallocate_fdm_coefs
!
!      subroutine check_fdm_2_coefs(nri, r)
!      subroutine check_fdm_2e_coefs(nri, r)
!
      module m_fdm_coefs
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: d1nod_mat_fdm_2(:,:)
      real(kind = kreal), allocatable :: d2nod_mat_fdm_2(:,:)
!
      real(kind = kreal), allocatable :: d_nod_mat_fdm_2e(:,:)
      real(kind = kreal), allocatable :: d1nod_mat_fdm_2e(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_coefs(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( d1nod_mat_fdm_2(nri,-1:1) )
      allocate( d2nod_mat_fdm_2(nri,-1:1) )
!
      allocate( d_nod_mat_fdm_2e(nri,0:1) )
      allocate( d1nod_mat_fdm_2e(nri,0:1) )
!
      d1nod_mat_fdm_2 = 0.0d0
      d2nod_mat_fdm_2 = 0.0d0
!
      d_nod_mat_fdm_2e = 0.0d0
      d1nod_mat_fdm_2e = 0.0d0
!
      end subroutine allocate_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_coefs
!
!
      deallocate( d1nod_mat_fdm_2, d2nod_mat_fdm_2 )
      deallocate( d_nod_mat_fdm_2e, d1nod_mat_fdm_2e )
!
      end subroutine deallocate_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2_coefs(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, df_n1, df_0, df_p1'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), d1nod_mat_fdm_2(kr,-1:1)
      end do
      write(50,*) 'kr, r, d2f_n1, d2f_0, d2f_p1'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), d2nod_mat_fdm_2(kr,-1:1)
      end do
!
      end subroutine check_fdm_2_coefs
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2e_coefs(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, dnod_n1, dnod_p1'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr),                           &
     &           d_nod_mat_fdm_2e(kr,0:1)
      end do
!
      write(50,*) 'kr, r, dfe_n1, dfe_p1'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr),                           &
     &           d1nod_mat_fdm_2e(kr,0:1)
      end do
!
      end subroutine check_fdm_2e_coefs
!
! -----------------------------------------------------------------------
!
      end module m_fdm_coefs
