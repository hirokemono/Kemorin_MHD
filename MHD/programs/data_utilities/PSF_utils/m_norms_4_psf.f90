!
!      module m_norms_4_psf
!
      module m_norms_4_psf
!
!      Written by H. Matsui
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: norm_ele_psf(:,:)
      real(kind = kreal), allocatable :: norm_nod_psf(:,:)
      real(kind = kreal), allocatable :: area_psf(:)
      real(kind = kreal), allocatable :: w_4_norm_nod(:)
      real(kind = kreal) :: area_total_psf
!
      real(kind = kreal) :: psf_min(3), psf_max(3)
!
!      subroutine allocate_norms_4_psf
!      subroutine deallocate_norms_4_psf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_norms_4_psf
!
      use m_psf_results
!
      allocate ( norm_ele_psf(numele_psf,3) )
      allocate ( area_psf(numele_psf) )
!
      allocate ( w_4_norm_nod(numnod_psf) )
      allocate ( norm_nod_psf(numnod_psf,3) )
!
      norm_ele_psf = 0.0d0
      area_psf = 0.0d0
      w_4_norm_nod = 0.0d0
      norm_nod_psf = 0.0d0
!
      end subroutine allocate_norms_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_norms_4_psf
!
      deallocate ( norm_ele_psf )
      deallocate ( area_psf )
      deallocate ( w_4_norm_nod )
      deallocate ( norm_nod_psf )
!
      end subroutine deallocate_norms_4_psf
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_ele_normal_psf
!
      use m_psf_results
!
      integer(kind = kint) :: iele, i1, i2, i3
!
      write(50,*) 'iele, area_psf(iele), norm_ele_psf(iele,1:3)'
      do iele = 1, numele_psf
        write(50,*) iele, area_psf(iele), norm_ele_psf(iele,1:3)
      end do
!
      end subroutine check_ele_normal_psf
!
!-----------------------------------------------------------------------
!
      subroutine check_nod_normal_psf
!
      use m_psf_results
!
      integer(kind = kint) :: inod
!
      write(50,*) 'inod, norm_nod_psf(inod,1:3)'
      do inod = 1, numnod_psf
        write(50,*) inod, norm_nod_psf(inod,1:3)
      end do
!
      end subroutine check_nod_normal_psf
!
!-----------------------------------------------------------------------
!
      end module  m_norms_4_psf
