!>@file   t_read_sph_spectra.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine alloc_sph_espec_name(sph_IN)
!!      subroutine alloc_sph_spectr_data(ltr, sph_IN)
!!      subroutine dealloc_sph_espec_data(sph_IN)
!!        integer(kind = kint), intent(in) :: ltr
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!!      subroutine copy_read_ene_step_data(sph_IN, sph_OUT)
!!      subroutine copy_ene_spectr_data_to_IO(sph_IN, sph_OUT)
!!      subroutine copy_part_ene_spectr_to_IO(nri_dat, ltr_sph, ncomp,  &
!!     &          spectr_l, sph_OUT)
!!        integer(kind = kint), intent(in) :: nri_dat, ltr_sph
!!        integer(kind = kint), intent(in) :: ncomp
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_dat)
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!
!!      subroutine check_sph_spectr_name(sph_IN)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
      module t_read_sph_spectra
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type read_sph_spectr_data
        character(len = kchara) :: hdr_num_field, hdr_num_comp
        integer(kind = kint) :: nfield_sph_spec
        integer(kind = kint) :: ntot_sph_spec
        integer(kind = kint) :: num_time_labels
        integer(kind = kint) :: num_labels
        integer(kind = kint), allocatable :: ncomp_sph_spec(:)
        character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
        character(len = kchara) :: hdr_nri, hdr_ltr
        integer(kind = kint) :: ltr_sph, nri_sph, nri_dat
!
        character(len = kchara) :: hdr_ICB_id, hdr_CMB_id
        integer(kind = kint) :: kr_ICB, kr_CMB
!
        character(len = kchara) :: hdr_kr_in, hdr_kr_out
        integer(kind = kint) :: kr_inner, kr_outer
!
        character(len = kchara) :: hdr_r_in, hdr_r_out
        real(kind = kreal) :: r_inner, r_outer
!
        integer(kind = kint), allocatable :: i_mode(:)
        integer(kind = kint), allocatable :: kr_sph(:)
        real(kind = kreal), allocatable :: r_sph(:)
!
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: spectr_IO(:,:,:)
      end type read_sph_spectr_data
!
      logical, parameter :: flag_current_fmt = .FALSE.
      logical, parameter :: spectr_on =        .TRUE.
      logical, parameter :: spectr_off =       .FALSE.
      logical, parameter :: volume_on =        .TRUE.
      logical, parameter :: volume_off =       .FALSE.
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_espec_name(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      sph_IN%num_labels = sph_IN%ntot_sph_spec + sph_IN%num_time_labels
      allocate( sph_IN%ene_sph_spec_name(sph_IN%num_labels) )
      allocate( sph_IN%ncomp_sph_spec(sph_IN%nfield_sph_spec))
      sph_IN%ncomp_sph_spec = 0
!
      end subroutine alloc_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_spectr_data(ltr, sph_IN)
!
      integer(kind = kint), intent(in) :: ltr
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: ncomp
!
!
      allocate(sph_IN%kr_sph(sph_IN%nri_dat))
      allocate(sph_IN%r_sph(sph_IN%nri_dat))
!
      ncomp = sph_IN%ntot_sph_spec
      allocate(sph_IN%i_mode(0:ltr))
      allocate(sph_IN%spectr_IO(ncomp,0:ltr,sph_IN%nri_dat))
!
!$omp parallel workshare
      sph_IN%kr_sph(1:sph_IN%nri_dat) = izero
      sph_IN%r_sph(1:sph_IN%nri_dat) = zero
!$omp end parallel workshare
!$omp parallel workshare
      sph_IN%i_mode(0:ltr) = izero
!$omp end parallel workshare
!$omp parallel workshare
      sph_IN%spectr_IO(1:ncomp,0:ltr,1:sph_IN%nri_dat) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      deallocate(sph_IN%ene_sph_spec_name, sph_IN%ncomp_sph_spec)
      deallocate(sph_IN%kr_sph, sph_IN%r_sph)
      deallocate(sph_IN%i_mode, sph_IN%spectr_IO)
!
      end subroutine dealloc_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_head_params(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%ltr_sph = sph_IN%ltr_sph
      sph_OUT%nri_sph = sph_IN%nri_sph
      sph_OUT%nri_dat = sph_IN%nri_dat
      sph_OUT%kr_ICB = sph_IN%kr_ICB
      sph_OUT%kr_CMB = sph_IN%kr_CMB
      sph_OUT%kr_inner = sph_IN%kr_inner
      sph_OUT%kr_outer = sph_IN%kr_outer
      sph_OUT%r_inner = sph_IN%r_inner
      sph_OUT%r_outer = sph_IN%r_outer
      sph_OUT%nfield_sph_spec = sph_IN%nfield_sph_spec
      sph_OUT%ntot_sph_spec = sph_IN%ntot_sph_spec
      sph_OUT%num_time_labels = sph_IN%num_time_labels
!
      sph_OUT%hdr_num_field = sph_IN%hdr_num_field
      sph_OUT%hdr_num_comp =  sph_IN%hdr_num_comp
      sph_OUT%hdr_nri =    sph_IN%hdr_nri
      sph_OUT%hdr_ltr =    sph_IN%hdr_ltr
      sph_OUT%hdr_ICB_id = sph_IN%hdr_ICB_id
      sph_OUT%hdr_CMB_id = sph_IN%hdr_CMB_id
      sph_OUT%hdr_kr_in =  sph_IN%hdr_kr_in
      sph_OUT%hdr_kr_out = sph_IN%hdr_kr_out
      sph_OUT%hdr_r_in =   sph_IN%hdr_r_in
      sph_OUT%hdr_r_out =  sph_IN%hdr_r_out
!
      end subroutine copy_read_ene_head_params
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_name_params(n_fld, ntot_spec,            &
     &          num_time_labels, sph_IN, sph_OUT)
!
      integer(kind= kint), intent(in) :: n_fld, ntot_spec
      integer(kind= kint), intent(in) :: num_time_labels
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i
!
!
      sph_OUT%ncomp_sph_spec(1:n_fld) = sph_IN%ncomp_sph_spec(1:n_fld)
      sph_OUT%ene_sph_spec_name(1:num_time_labels)                      &
     &         = sph_IN%ene_sph_spec_name(1:num_time_labels)
!
      do i = 1, ntot_spec
        sph_OUT%ene_sph_spec_name(i+num_time_labels)                    &
     &          = sph_IN%ene_sph_spec_name(i+num_time_labels)
      end do
!
      end subroutine copy_read_ene_name_params
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      call copy_read_ene_head_params(sph_IN, sph_OUT)
      sph_OUT%num_time_labels = sph_OUT%num_time_labels - 1
!
      call alloc_sph_espec_name(sph_OUT)
      call copy_read_ene_name_params                                    &
     &   (sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec,               &
     &    sph_OUT%num_time_labels, sph_IN, sph_OUT)
!
      call alloc_sph_spectr_data(izero, sph_OUT)
!
      sph_OUT%kr_sph(1:sph_OUT%nri_dat)                                 &
     &                 = sph_IN%kr_sph(1:sph_OUT%nri_dat)
      sph_OUT%r_sph(1:sph_OUT%nri_dat)                                  &
     &                 = sph_IN%r_sph(1:sph_OUT%nri_dat)
      sph_OUT%i_mode(0:sph_OUT%ltr_sph)                                 &
     &                 = sph_IN%i_mode(0:sph_OUT%ltr_sph)
!
      end subroutine copy_read_ene_params_4_sum
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_step_data(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%time = sph_IN%time
      sph_OUT%i_step = sph_IN%i_step
!$omp parallel workshare
      sph_OUT%kr_sph(1:sph_OUT%nri_dat)                                 &
     &      = sph_IN%kr_sph(1:sph_OUT%nri_dat)
      sph_OUT%r_sph(1:sph_OUT%nri_dat)                                  &
     &      = sph_IN%r_sph(1:sph_OUT%nri_dat)
!$omp end parallel workshare
!$omp parallel workshare
      sph_OUT%i_mode(0:sph_OUT%ltr_sph)                                 &
     &                 = sph_IN%i_mode(0:sph_OUT%ltr_sph)
!$omp end parallel workshare
!
      end subroutine copy_read_ene_step_data
!
!   --------------------------------------------------------------------
!
      subroutine copy_ene_spectr_data_to_IO(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: nri_dat, ltr_sph
      integer(kind = kint) :: ncomp
!
!
      nri_dat = sph_OUT%nri_dat
      ltr_sph = sph_OUT%ltr_sph
      ncomp = sph_OUT%ntot_sph_spec
!$omp parallel workshare
      sph_OUT%spectr_IO(1:ncomp,0:ltr_sph,1:nri_dat)                    &
     &        = sph_in%spectr_IO(1:ncomp,0:ltr_sph,1:nri_dat)
!$omp end parallel workshare
!
      end subroutine copy_ene_spectr_data_to_IO
!
!   --------------------------------------------------------------------
!
      subroutine copy_part_ene_spectr_to_IO(nri_dat, ltr_sph, ncomp,    &
     &          spectr_l, sph_OUT)
!
      integer(kind = kint), intent(in) :: nri_dat, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_dat)
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
!$omp parallel workshare
      sph_OUT%spectr_IO(1:ncomp,0:ltr_sph,1:nri_dat)                    &
     &        = spectr_l(1:ncomp,0:ltr_sph,1:nri_dat)
!$omp end parallel workshare
!
      end subroutine copy_part_ene_spectr_to_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine check_sph_spectr_name(sph_IN)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint) :: i
!
!
      write(*,*) "Number of components: ", sph_IN%ntot_sph_spec
      do i = sph_IN%num_time_labels+1, sph_IN%num_labels
        write(*,*) trim(sph_IN%ene_sph_spec_name(i))
      end do
!
      end subroutine check_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      subroutine sph_mean_squre_header_labels(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      sph_IN%hdr_nri =       'radial_layers'
      sph_IN%hdr_ltr =       'truncation'
      sph_IN%hdr_ICB_id =    'ICB_id'
      sph_IN%hdr_CMB_id =    'CMB_id'
      sph_IN%hdr_kr_in =     'Lower_boundary_ID'
      sph_IN%hdr_kr_out =    'Lower_boundary_radius'
      sph_IN%hdr_r_in =      'Upper_boundary_ID'
      sph_IN%hdr_r_out =     'Upper_boundary_radius'
      sph_IN%hdr_num_field = 'Number_of_field'
      sph_IN%hdr_num_comp =  'Number_of_components'
!
      end subroutine sph_mean_squre_header_labels
!
!   --------------------------------------------------------------------
!
      end module t_read_sph_spectra
