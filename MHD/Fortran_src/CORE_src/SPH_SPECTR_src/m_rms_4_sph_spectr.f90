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
!pwr1%nri_rms
!
!>      Number of field for mean square
      integer (kind=kint) :: num_rms_rj
!
!>      Number of component for mean square
      integer (kind=kint) :: ntot_rms_rj
!
!>      Field ID for mean square
      integer (kind=kint), allocatable :: ifield_rms_rj(:)
!
!>      Number of each component for mean square
      integer (kind=kint), allocatable :: num_rms_comp_rj(:)
!
!>      End ID of each field for mean square
      integer (kind=kint), allocatable :: istack_rms_comp_rj(:)
!
!>      Field name for mean square
      character (len=kchara), allocatable :: rms_name_rj(:)
!
!
!>      Number of radial points for mean square
!      integer(kind=kint) :: nri_rms = 0
!
!>      Radial ID from layered mean square
!      integer(kind=kint), allocatable :: kr_for_rms(:)
!
!>      Radius from layered mean square
!      real(kind = kreal), allocatable :: r_for_rms(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_name_sph_spec
!
!
      pwr1%num_rms_rj = num_rms_rj
      allocate(ifield_rms_rj(num_rms_rj))
      allocate(num_rms_comp_rj(num_rms_rj))
      allocate(istack_rms_comp_rj(0:num_rms_rj))
      allocate(rms_name_rj(num_rms_rj))
!
      if (num_rms_rj .gt. 0) then
        num_rms_comp_rj = 0
        ifield_rms_rj =   0
      end if
      istack_rms_comp_rj = 0
!
      end subroutine allocate_rms_name_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_4_sph_spectr(my_rank, l_truncation)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
!
!
      pwr1%ntot_rms_rj = ntot_rms_rj
      call alloc_rms_4_sph_spectr(my_rank, l_truncation, pwr1)
!
      end subroutine allocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_4_sph_spectr(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      deallocate(pwr1%r_4_rms, pwr1%kr_4_rms)
!
      if(my_rank .gt. 0) return
      deallocate(pwr1%shl_l, pwr1%shl_m, pwr1%shl_lm)
      deallocate(pwr1%shl_sq, pwr1%shl_m0)
!
      deallocate(pwr1%vol_l, pwr1%vol_m, pwr1%vol_lm)
      deallocate(pwr1%vol_sq, pwr1%vol_m0, pwr1%ratio_vol_m0)
!
      deallocate(num_rms_comp_rj, istack_rms_comp_rj)
      deallocate(rms_name_rj, ifield_rms_rj)
!
      end subroutine deallocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module m_rms_4_sph_spectr
