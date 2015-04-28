!>@file   t_sph_matrices.f90
!!@brief  module t_sph_matrices
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief structures of radial matrices
!!
!!@verbatim
!!      subroutine alloc_band_mat_sph(nband, nri, jmax, smat)
!!      subroutine dealloc_band_mat_sph(smat)
!!@endverbatim
!
      module t_sph_matrices
!
      use m_precision
!
      implicit none
!
!>      Structure for band matrices
      type band_matrix_type
!>        name of matrices
        character(len = kchara) :: mat_name
!
!>        Length of matrices for spectr data
        integer(kind = kint) :: n_vect
!
!>        Band width of matrices for spectr data
        integer(kind = kint) :: n_band
!
!>        Band width of LU-decompositted matrices for spectr data
        integer(kind = kint) :: n_band_lu
!
!>        number of components of matrices for spectr data
        integer(kind = kint) :: n_comp
!
!>        Band matrices for spectr data
        real(kind = kreal), allocatable :: mat(:,:,:)
!
!>        LU-decompositted matrices for spectr data
        real(kind = kreal), allocatable :: lu(:,:,:)
!
!>        Determinant of matrices for spectr data
        real(kind = kreal), allocatable :: det(:,:)
!
!>        Pivot information for matrices for spectr data
        integer(kind = kint), allocatable :: i_pivot(:,:)
      end type band_matrix_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
!      call alloc_band_mat_sph(nband, nidx_rj(1), nidx_rj(2), smat)
      subroutine alloc_band_mat_sph(nband, nri, jmax, smat)
!
      integer(kind = kint), intent(in) :: nband, nri, jmax
      type(band_matrix_type), intent(inout) :: smat
!
      integer(kind = kint) :: l_diag
!
!
      smat%n_vect =      nri
      smat%n_comp =      jmax
      smat%n_band =      nband
      smat%n_band_lu = 2*nband - 1
!
!
      allocate( smat%mat(smat%n_band,smat%n_vect,smat%n_comp) )
      allocate( smat%lu(smat%n_band_lu ,smat%n_vect,smat%n_comp) )
      allocate( smat%det(smat%n_vect,smat%n_comp) )
      allocate( smat%i_pivot(smat%n_vect,smat%n_comp) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      l_diag = (smat%n_band + 1) / 2
      smat%mat(l_diag,1:smat%n_vect,1:smat%n_comp) = 1.0d0
!
      end subroutine alloc_band_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_band_mat_sph(smat)
!
      type(band_matrix_type), intent(inout) :: smat
!
!
      deallocate( smat%mat, smat%lu )
      deallocate( smat%det, smat%i_pivot )
!
      end subroutine dealloc_band_mat_sph
!
! -----------------------------------------------------------------------
!
      end module t_sph_matrices
