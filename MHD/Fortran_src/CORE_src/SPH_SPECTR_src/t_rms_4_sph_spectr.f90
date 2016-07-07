!>@file   t_rms_4_sph_spectr.f90
!!@brief  module t_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine alloc_num_spec_layer(nri_in, pwr)
!!      subroutine alloc_rms_name_sph_spec(nfld_in, pwr)
!!      subroutine alloc_rms_4_sph_spectr(my_rank, ltr, pwr)
!!      subroutine alloc_ave_4_sph_spectr                               &
!!     &         (idx_rj_degree_zero, nri_rj, pwr)
!!
!!      subroutine dealloc_rms_4_sph_spectr(my_rank, pwr)
!!      subroutine dealloc_ave_4_sph_spectr(idx_rj_degree_zero, pwr)
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
      module t_rms_4_sph_spectr
!
      use m_precision
!
      implicit none
!
!
!>      Structure of mean square data
      type sph_mean_squares
!>        Number of field for mean square
        integer (kind=kint) :: num_fld_sq
!>        Number of component for mean square
        integer (kind=kint) :: ntot_comp_sq
!
!>        Field ID for mean square
        integer (kind=kint), pointer :: id_field(:)
!>        Number of each component for mean square
        integer (kind=kint), pointer :: num_comp_sq(:)
!>        End ID of each field for mean square
        integer (kind=kint), pointer :: istack_comp_sq(:)
!>        Field name for mean square
        character (len=kchara), pointer :: pwr_name(:)
!
!
!>        Number of radial points for mean square
        integer(kind=kint) :: nri_rms = 0
!
!>        Radial ID from layered mean square
        integer(kind=kint), pointer :: kr_4_rms(:)
!>        Radius from layered mean square
        real(kind = kreal), pointer :: r_4_rms(:)
!
!>        Mean square spectrum for degree on spheres
        real(kind = kreal), pointer :: shl_l(:,:,:)
!>      Mean square spectrum for order on spheres
        real(kind = kreal), pointer :: shl_m(:,:,:)
!>        Mean square spectrum for l-m on spheres
        real(kind = kreal), pointer :: shl_lm(:,:,:)
!
!>         Mean square on spheres
        real(kind = kreal), pointer :: shl_sq(:,:)
!>         Mean square of axis-symmetric component on spheres
        real(kind = kreal), pointer :: shl_m0(:,:)
!>        Ratio of axis-symmetric componbent to total mean square
        real(kind = kreal), pointer :: ratio_shl_m0(:,:)
!
!
!>        Volume mean square spectrum for degree
        real(kind = kreal), pointer :: vol_l(:,:)
!>        Volume mean square spectrum for order
        real(kind = kreal), pointer :: vol_m(:,:)
!>        Volume mean square spectrum for l-m
        real(kind = kreal), pointer :: vol_lm(:,:)
!
!>        Volume mean square
        real(kind = kreal), pointer :: vol_sq(:)
!>        Volume mean square of axis-symmetric component
        real(kind = kreal), pointer :: vol_m0(:)
!>        Ratio of axis-symmetric componbent to total mean square
        real(kind = kreal), pointer :: ratio_vol_m0(:)
!
!
!>        Number of radial point for average
        integer(kind = kint) :: nri_ave
!>        Average over single sphere
        real(kind = kreal), pointer :: shl_ave(:,:)
!>        Volume average
        real(kind = kreal), pointer :: vol_ave(:)
      end type sph_mean_squares
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_spec_layer(nri_in, pwr)
!
      integer(kind = kint), intent(in) :: nri_in
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%nri_rms = nri_in
!
      allocate( pwr%kr_4_rms(pwr%nri_rms) )
      allocate( pwr%r_4_rms(pwr%nri_rms) )
      if(pwr%nri_rms .gt. 0) then
        pwr%kr_4_rms = 0
        pwr%r_4_rms =  0.0d0
      end if
!
      end subroutine alloc_num_spec_layer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rms_name_sph_spec(nfld_in, pwr)
!
      integer(kind = kint), intent(in) :: nfld_in
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%num_fld_sq = nfld_in
      allocate(pwr%id_field(pwr%num_fld_sq))
      allocate(pwr%num_comp_sq(pwr%num_fld_sq))
      allocate(pwr%istack_comp_sq(0:pwr%num_fld_sq))
      allocate(pwr%pwr_name(pwr%num_fld_sq))
!
      if (pwr%num_fld_sq .gt. 0) then
        pwr%num_comp_sq = 0
        pwr%id_field =   0
      end if
      pwr%istack_comp_sq = 0
!
      end subroutine alloc_rms_name_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rms_4_sph_spectr(my_rank, ltr, pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ltr
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%ntot_comp_sq = pwr%istack_comp_sq(pwr%num_fld_sq)
      if(my_rank .gt. 0) return
!
      allocate(pwr%shl_l(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
      allocate(pwr%shl_m(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
      allocate(pwr%shl_lm(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
!
      allocate( pwr%shl_sq(pwr%nri_rms,pwr%ntot_comp_sq) )
      allocate( pwr%shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
      allocate( pwr%ratio_shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
      if(pwr%nri_rms .gt. 0) then
        pwr%shl_sq =       0.0d0
        pwr%shl_m0 =       0.0d0
        pwr%ratio_shl_m0 = 0.0d0
!
        pwr%shl_l =  0.0d0
        pwr%shl_m =  0.0d0
        pwr%shl_lm = 0.0d0
      end if
!
      allocate( pwr%vol_l(0:ltr,pwr%ntot_comp_sq) )
      allocate( pwr%vol_m(0:ltr,pwr%ntot_comp_sq) )
      allocate( pwr%vol_lm(0:ltr,pwr%ntot_comp_sq) )
!
      allocate( pwr%vol_sq(pwr%ntot_comp_sq) )
      allocate( pwr%vol_m0(pwr%ntot_comp_sq) )
      allocate( pwr%ratio_vol_m0(pwr%ntot_comp_sq) )
      pwr%vol_l = 0.0d0
      pwr%vol_m =  0.0d0
      pwr%vol_lm = 0.0d0
!
      pwr%vol_sq =       0.0d0
      pwr%vol_m0 =       0.0d0
      pwr%ratio_vol_m0 = 0.0d0
!
!
      end subroutine alloc_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ave_4_sph_spectr                                 &
     &         (idx_rj_degree_zero, nri_rj, pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri_rj
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
!
      pwr%nri_ave = nri_rj
      allocate(pwr%vol_ave(pwr%ntot_comp_sq))
      allocate(pwr%shl_ave(0:pwr%nri_ave,pwr%ntot_comp_sq))
!
      if(pwr%nri_ave*pwr%ntot_comp_sq .gt. 0) then
        pwr%shl_ave = 0.0d0
        pwr%vol_ave = 0.0d0
      end if
!
      end subroutine alloc_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rms_4_sph_spectr(my_rank, pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      deallocate(pwr%r_4_rms, pwr%kr_4_rms)
!
      if(my_rank .gt. 0) return
      deallocate(pwr%shl_l, pwr%shl_m, pwr%shl_lm)
      deallocate(pwr%shl_sq, pwr%shl_m0, pwr%ratio_shl_m0)
!
      deallocate(pwr%vol_l, pwr%vol_m, pwr%vol_lm)
      deallocate(pwr%vol_sq, pwr%vol_m0, pwr%ratio_vol_m0)
!
      deallocate(pwr%num_comp_sq, pwr%istack_comp_sq)
      deallocate(pwr%pwr_name, pwr%id_field)
!
      end subroutine dealloc_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ave_4_sph_spectr(idx_rj_degree_zero, pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      if(idx_rj_degree_zero .eq. 0) return
      deallocate(pwr%shl_ave, pwr%vol_ave)
!
      end subroutine dealloc_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module t_rms_4_sph_spectr
