!>@file   m_rms_4_sph_spectr.f90
!!@brief  module m_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine allocate_num_spec_layer
!!      subroutine allocate_rms_name_sph_spec
!!      subroutine allocate_rms_4_sph_spectr(my_rank, l_truncation)
!!      subroutine allocate_ave_4_sph_spectr(idx_rj_degree_zero, nri_rj)
!!      subroutine deallocate_rms_4_sph_spectr(my_rank)
!!      subroutine deallocate_ave_4_sph_spectr(idx_rj_degree_zero)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module m_rms_4_sph_spectr
!
      use m_precision
      use t_rms_4_sph_spectr
!
      implicit none
!
!>      Structure of mean square data
      type(sph_mean_squares), save :: pwr1
!
      end module m_rms_4_sph_spectr
