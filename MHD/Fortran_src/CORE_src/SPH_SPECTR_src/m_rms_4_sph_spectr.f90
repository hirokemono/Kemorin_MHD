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
!!      subroutine allocate_rms_4_sph_spectr(my_rank)
!!      subroutine allocate_ave_4_sph_spectr(nri_ave)
!!      subroutine deallocate_rms_4_sph_spectr(my_rank)
!!      subroutine deallocate_ave_4_sph_spectr
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
!
      implicit none
!
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
      integer(kind=kint) :: nri_rms = 0
!
!>      Radial ID from layered mean square
      integer(kind=kint), allocatable :: kr_for_rms(:)
!
!>      Radius from layered mean square
      real(kind = kreal), allocatable :: r_for_rms(:)
!
!>      Mean square spectrum for degree on spheres
      real(kind = kreal), allocatable :: rms_sph_l(:,:,:)
!
!>      Mean square spectrum for order on spheres
      real(kind = kreal), allocatable :: rms_sph_m(:,:,:)
!
!>      Mean square spectrum for l-m on spheres
      real(kind = kreal), allocatable :: rms_sph_lm(:,:,:)
!
!>       Mean square on spheres
      real(kind = kreal), allocatable :: rms_sph(:,:)
!>       Mean square of axis-symmetric component on spheres
      real(kind = kreal), allocatable :: rms_sph_m0(:,:)
!>      Ratio of axis-symmetric componbent to total mean square
      real(kind = kreal), allocatable :: ratio_sph_m0(:,:)
!
!
!>      Volume mean square spectrum for degree
      real(kind = kreal), allocatable :: rms_sph_vol_l(:,:)
!
!>      Volume mean square spectrum for order
      real(kind = kreal), allocatable :: rms_sph_vol_m(:,:)
!
!>      Volume mean square spectrum for l-m
      real(kind = kreal), allocatable :: rms_sph_vol_lm(:,:)
!
!>      Volume mean square
      real(kind = kreal), allocatable :: rms_sph_vol(:)
!>      Volume mean square of axis-symmetric component
      real(kind = kreal), allocatable :: rms_sph_vol_m0(:)
!>      Ratio of axis-symmetric componbent to total mean square
      real(kind = kreal), allocatable :: ratio_sph_vol_m0(:)
!
!
!>      Number of radial point for average
      integer(kind = kint) :: nri_ave
!
!>      Average over single sphere
      real(kind = kreal), allocatable :: ave_sph(:,:)
!
!>      Volume average
      real(kind = kreal), allocatable :: ave_sph_vol(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_spec_layer
!
!
      allocate( kr_for_rms(nri_rms) )
      allocate( r_for_rms(nri_rms) )
      if(nri_rms .gt. 0) then
        kr_for_rms = 0
        r_for_rms =  0.0d0
      end if
!
      end subroutine allocate_num_spec_layer
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_name_sph_spec
!
!
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
      subroutine allocate_rms_4_sph_spectr(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank .gt. 0) return
!
      allocate( rms_sph_l(nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_m(nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_lm(nri_rms,0:l_truncation,ntot_rms_rj) )
!
      allocate( rms_sph(nri_rms,ntot_rms_rj) )
      allocate( rms_sph_m0(nri_rms,ntot_rms_rj) )
      allocate( ratio_sph_m0(nri_rms,ntot_rms_rj) )
      if(nri_rms .gt. 0) then
        rms_sph =    0.0d0
        rms_sph_m0 = 0.0d0
        ratio_sph_m0 = 0.0d0
!
        rms_sph_l =  0.0d0
        rms_sph_m =  0.0d0
        rms_sph_lm = 0.0d0
      end if
!
      allocate( rms_sph_vol_l(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_m(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_lm(0:l_truncation,ntot_rms_rj) )
!
      allocate( rms_sph_vol(ntot_rms_rj) )
      allocate( rms_sph_vol_m0(ntot_rms_rj) )
      allocate( ratio_sph_vol_m0(ntot_rms_rj) )
      rms_sph_vol_l = 0.0d0
      rms_sph_vol_m = 0.0d0
      rms_sph_vol_lm = 0.0d0
!
      rms_sph_vol =      0.0d0
      rms_sph_vol_m0 =   0.0d0
      ratio_sph_vol_m0 = 0.0d0
!
!
      end subroutine allocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ave_4_sph_spectr
!
      use m_spheric_parameter
!
      if(idx_rj_degree_zero .eq. 0) return
!
!
      nri_ave = nidx_rj(1)
      allocate(ave_sph_vol(ntot_rms_rj))
      allocate(ave_sph(0:nri_ave,ntot_rms_rj))
!
      if(nri_ave*ntot_rms_rj .gt. 0) then
        ave_sph=     0.0d0
        ave_sph_vol = 0.0d0
      end if
!
      end subroutine allocate_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_4_sph_spectr(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      deallocate(r_for_rms, kr_for_rms)
!
      if(my_rank .gt. 0) return
      deallocate(rms_sph_l, rms_sph_m, rms_sph_lm, rms_sph, rms_sph_m0)
!
      deallocate(rms_sph_vol_l, rms_sph_vol_m, rms_sph_vol_lm)
      deallocate(rms_sph_vol, rms_sph_vol_m0, ratio_sph_vol_m0)
!
      deallocate(num_rms_comp_rj, istack_rms_comp_rj)
      deallocate(rms_name_rj, ifield_rms_rj)
!
      end subroutine deallocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ave_4_sph_spectr
!
      use m_spheric_parameter
!
      if(idx_rj_degree_zero .eq. 0) return
      deallocate(ave_sph, ave_sph_vol)
!
      end subroutine deallocate_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module m_rms_4_sph_spectr
