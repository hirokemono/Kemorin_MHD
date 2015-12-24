!>@file   m_psf_results.f90
!!@brief  module m_psf_results
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief set edge information for PSF results
!!
!!@verbatim
!!
!!@endverbatim
!
      module m_psf_results
!
      use m_precision
      use m_field_file_format
!
      use t_psf_results
      use t_norms_4_psf
!
      implicit none
!
!
!>      structure for section data
      type(psf_results), save :: psf1
!
!>      structure for normals
      type(psf_normals), save :: psf_norm1
!
!>      structure for averages
      type(psf_averages), save :: psf_ave1
!psf_aves%ntot_comp
!
      character(len=kchara) :: psf_file_header
      integer(kind = kint) :: iflag_psf_fmt = iflag_udt
!
!
      end module  m_psf_results
