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
      use t_file_IO_parameter
!
      implicit none
!
!
!>      structure for section data
      type(psf_results), save :: psf_u
!
!>      structure for normals
      type(psf_normals), save :: psf_normal
!
!>      structure for averages
      type(psf_averages), save :: psf_average
!
      type(field_IO_params), save :: psf_file_param
!
!
      end module  m_psf_results
