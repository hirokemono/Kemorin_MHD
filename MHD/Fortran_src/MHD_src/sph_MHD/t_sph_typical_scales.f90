!>@file   t_sph_typical_scales.f90
!!@brief      module t_sph_typical_scales
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine open_typical_scale_file                              &
!!     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,            &
!!     &          kr_in, kr_out, r_in, r_out, tsl, flag_gzip_lc, zbuf)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: r_in, r_out
!!        type(typical_scale_data), intent(in) :: tsl
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!@endverbatim
!
      module t_sph_typical_scales
!
      use m_precision
      use m_constants
!
      use t_field_labels
!
      implicit none
!
!>        Field label for Elsasser number
!!         @f$ B^{2} / \rho \mu_{0} \eta \Omega @f$
      type(field_def), parameter :: Elsasser_number                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Elsasser_number',                         &
     &                math = '$B^{2} / \rho \mu_{0} \eta \Omega $')
!
!>        Field label for dynamic Elsasser number
!!         @f$ B^{2}/ \rho \mu_{0} \Omega u \ell_{B} @f$
      type(field_def), parameter :: dynamic_Elsasser_number             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'dynamic_Elsasser_number',                 &
     &                math = '$B^{2}/ \rho \mu_{0} \Omega u \ell_{B}$')
!
!>        Field label for dynamic Alfven number
!!         @f$ u \sqrt{\rho \mu_{0}} / B @f$
      type(field_def), parameter :: Alfven_number                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Alfven_number',                           &
     &                math = '$u \sqrt{\rho \mu_{0}} / B$')
!
!>        Field label for dynamic Lehnert number
!!         @f$ B / \ell_{B} \Omega \sqrt{\rho \mu_{0}} @f$
      type(field_def), parameter :: Lehnert_number                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lehnert_number',                          &
     &              math = '$B / \ell_{B} \Omega \sqrt{\rho \mu_{0}}$')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{u}} @f$
      type(field_def), parameter :: flow_degree                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_degree',                             &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{u}} @f$
      type(field_def), parameter :: flow_order                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_order',                              &
     &                math = '$ \bar{m_{u}} $')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{B}} @f$
      type(field_def), parameter :: magne_degree                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_degree',                            &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{B}} @f$
      type(field_def), parameter :: magne_order                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_order',                             &
     &                math = '$ \bar{m_{u}} $')
!
!
      type typical_scale_data
!>        Integer flag for typical scales
        integer(kind = kint) :: iflag_ub_scales = 0
!>        File prefix for dipolarity data
        character(len = kchara) :: scale_prefix = 'typical_scales'
!
!>        kinetic energy address
        integer(kind = kint) :: icomp_kene = 0
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!
!>        number of output component
        integer(kind = kint) :: num_lscale = 0
!>        number of output component
        integer(kind = kint), allocatable :: ncomp_lscale(:)
!>        number of output component
        character(len = kchara), allocatable :: lscale_name(:)
!>        magnetic length scale
        real(kind = kreal) :: dl_mag
!>        magnetic zonal length scale
        real(kind = kreal) :: dm_mag
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_mag
!
!>        kinetic length scale
        real(kind = kreal) :: dl_kin
!>        kinetic zonal length scale
        real(kind = kreal) :: dm_kin
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_kin
      end type typical_scale_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_typical_scale_data(num, tsl)
!
      integer(kind = kint), intent(in) :: num
      type(typical_scale_data), intent(inout) :: tsl
!
      tsl%num_lscale = num
      allocate(tsl%lscale_name(tsl%num_lscale))
      allocate(tsl%ncomp_lscale(tsl%num_lscale))
!
      if(tsl%num_lscale .gt. 0) then
        tsl%ncomp_lscale(1:tsl%num_lscale) = 1
      end if
!
      end subroutine alloc_typical_scale_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_typical_scale_data(tsl)
!
      type(typical_scale_data), intent(inout) :: tsl
!
      if(allocated(tsl%ncomp_lscale) .eqv. .FALSE.) return
      deallocate(tsl%ncomp_lscale, tsl%lscale_name)
!
      end subroutine dealloc_typical_scale_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_typical_scale_file                                &
     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,              &
     &          kr_in, kr_out, r_in, r_out, tsl, flag_gzip_lc, zbuf)
!
      use t_buffer_4_gzip
      use t_read_sph_spectra
!
      use set_parallel_file_name
      use write_field_labels
      use sph_power_spectr_data_text
      use select_gz_stream_file_IO
      use gz_open_sph_monitor_file
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      type(typical_scale_data), intent(in) :: tsl
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: file_name, base_name
      type(read_sph_spectr_data) :: sph_OUT
      logical :: flag_miss
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      base_name = add_dat_extension(tsl%scale_prefix)
      call check_gzip_or_ascii_file(base_name, file_name,               &
     &                              flag_gzip_lc, flag_miss)
      if(flag_miss) go to 99
!
      open(id_file, file=file_name, status='old', position='append',    &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      return
!
   99 continue
!
      open(id_file, file=file_name, FORM='UNFORMATTED',ACCESS='STREAM')
      call dup_typ_scale_head_params(ltr, nri, nlayer_ICB, nlayer_CMB,  &
     &    kr_in, kr_out, r_in, r_out, tsl, sph_OUT)
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT,           &
     &                               len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_file,              &
     &    sph_vol_spectr_header_text(len_tot, len_each,                 &
     &                               sph_pwr_labels, sph_OUT),          &
     &    zbuf)
      call dealloc_sph_espec_name(sph_OUT)
!
      end subroutine open_typical_scale_file
!
! -----------------------------------------------------------------------
!
      subroutine dup_typ_scale_head_params                              &
     &         (ltr, nri, nlayer_ICB, nlayer_CMB,                       &
     &          kr_in, kr_out, r_in, r_out, tsl, sph_OUT)
!
      use t_read_sph_spectra
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      type(typical_scale_data), intent(in) :: tsl
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB = nlayer_ICB
      sph_OUT%kr_CMB = nlayer_CMB
      sph_OUT%kr_inner = kr_in
      sph_OUT%kr_outer = kr_out
      sph_OUT%r_inner = r_in
      sph_OUT%r_outer = r_out
      sph_OUT%nfield_sph_spec = tsl%num_lscale
      sph_OUT%ntot_sph_spec =   tsl%num_lscale
      sph_OUT%num_time_labels = 2
!
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ncomp_sph_spec(1:tsl%num_lscale)                          &
     &               = tsl%ncomp_lscale(1:tsl%num_lscale)
!
      sph_OUT%ene_sph_spec_name(1) = 't_step'
      sph_OUT%ene_sph_spec_name(2) = 'time'
      do i = 1, tsl%num_lscale
        sph_OUT%ene_sph_spec_name(i+sph_OUT%num_time_labels)            &
     &                                        = tsl%lscale_name(i)
      end do
!
      end subroutine dup_typ_scale_head_params
!
!   --------------------------------------------------------------------
!
      end module t_sph_typical_scales
