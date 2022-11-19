!>@file   t_pickup_sph_spectr_data.f90
!!@brief  module t_pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine alloc_pick_sph_mode(list_pick)
!!      subroutine alloc_pick_sph_l(list_pick)
!!      subroutine alloc_pick_sph_m(list_pick)
!!      subroutine alloc_num_pick_layer(picked)
!!      subroutine alloc_pickup_sph_spec_local(nprocs, picked)
!!        integer, intent(in) :: nprocs
!!        type(picked_spectrum_data), intent(inout) :: picked
!!      subroutine dealloc_pickup_sph_spec_local(picked)
!!      subroutine alloc_pick_sph_monitor(picked)
!!
!!      subroutine alloc_gauss_coef_monitor_lc(my_rank, nprocs, gauss)
!!      subroutine dealloc_gauss_coef_monitor_lc(gauss)
!!        integer, intent(in) :: my_rank, nprocs
!!        type(picked_spectrum_data), intent(inout) :: gauss
!!
!!      subroutine dealloc_pick_sph_mode(list_pick)
!!      subroutine dealloc_num_pick_layer(picked)
!!      subroutine dealloc_pick_sph_monitor(picked)
!!
!!      subroutine write_pick_sph_file_header                           &
!!     &         (id_file, nlayer_ICB, nlayer_CMB, picked)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module t_pickup_sph_spectr_data
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
!
      implicit  none
!
!>        Structure for pickup list
      type pickup_mode_list
!>        Number of modes of monitoring spectrum to be evaluated
        integer(kind = kint) :: num_modes = 0
!>        Degree and Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_mode(:,:)
!>        Number of degrees of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_degree = 0
!>        Degree ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_l(:)
!>        Number of orders of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_order = 0
!>        Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_m(:)
      end type pickup_mode_list
!
!
!>        Structure for picked spectr data
      type picked_spectrum_data
!>        File prefix for spectr monitoring file
        character(len = kchara) :: file_prefix =  'picked_ene_spec'
!
!>        Number of radial layer for monitoring spectrum
        integer(kind = kint) :: num_layer = 0
!>        Radial ID for monitoring spectrum
        integer(kind = kint), allocatable :: id_radius(:)
!>        Radius for monitoring spectrum
        real(kind = kreal), allocatable :: radius_gl(:)
!
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_sph_mode =  0
!
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: ntot_pick_spectr_lc = 0
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: num_sph_mode_lc =  0
!>        Stack of modes for monitoring spectrum
        integer(kind = kint), allocatable :: istack_picked_spec_lc(:)
!>        Stack of modes for monitoring spectrum
!!          (global, l, global m, Global lm, local lm)
        integer(kind = kint), allocatable :: idx_out(:,:)
!>        Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_mode_name_lc(:)
!>        Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_mode_name_out(:)
!>        Number of components (all to be 1)
        integer(kind = kint), allocatable :: ncomp_gauss_out(:)
!
!>        Number of fields for monitoring output
!!         @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) ::  num_field_rj =  0
!>        Total number of component for monitoring spectrum
        integer(kind = kint) :: ntot_comp_rj =  0
!>        Number of component for monitoring spectrum
        integer(kind = kint), allocatable :: istack_comp_rj(:)
!>        Field  address for monitoring of @f$ f(r,j) @f$
        integer(kind = kint), allocatable :: ifield_monitor_rj(:)

!>        Name of  monitoring spectrum
        character(len=kchara), allocatable :: spectr_name(:)
      end type picked_spectrum_data
!
      type(sph_spectr_head_labels), parameter, private                  &
     &           :: pick_spectr_labels = sph_spectr_head_labels(        &
     &                           hdr_nri = 'Num_Radial_layers',         &
     &                           hdr_ltr = 'Num_spectr',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Not_used',               &
     &                           hdr_kr_out = 'Not_used',               &
     &                           hdr_r_out =  'Not_used',               &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate(list_pick%idx_pick_mode(list_pick%num_modes,2) )
      if(list_pick%num_modes .gt. 0) list_pick%idx_pick_mode = 0
!
      end subroutine alloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_l(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_l(list_pick%num_degree) )
      if(list_pick%num_degree .gt. 0) list_pick%idx_pick_l = 0
!
      end subroutine alloc_pick_sph_l
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_m(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_m(list_pick%num_order) )
      if(list_pick%num_order .gt. 0) list_pick%idx_pick_m = 0
!
      end subroutine alloc_pick_sph_m
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      allocate( picked%id_radius(picked%num_layer) )
      allocate( picked%radius_gl(picked%num_layer) )
      if(picked%num_layer .gt. 0) then
        picked%id_radius = 0
        picked%radius_gl = 0.0d0
      end if
!
      end subroutine alloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pickup_sph_spec_local(nprocs, picked)
!
      integer, intent(in) :: nprocs
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      picked%ntot_pick_spectr_lc                                        &
     &      = picked%num_sph_mode_lc * picked%num_layer
!
      allocate( picked%istack_picked_spec_lc(0:nprocs) )
!
      allocate( picked%idx_out(0:picked%num_sph_mode_lc,4) )
!
      picked%istack_picked_spec_lc = 0
!$omp parallel workshare
      picked%idx_out =  -1
!$omp end parallel workshare
!
      end subroutine alloc_pickup_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pickup_sph_spec_local(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate( picked%istack_picked_spec_lc)
      deallocate( picked%idx_out)
!
      end subroutine dealloc_pickup_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      allocate( picked%spectr_name(picked%ntot_comp_rj) )
      allocate( picked%istack_comp_rj(0:picked%num_field_rj) )
      allocate( picked%ifield_monitor_rj(picked%num_field_rj) )
!
      if(picked%num_field_rj .gt. 0) then
        picked%ifield_monitor_rj = 0
        picked%istack_comp_rj =    0
      end if
!
      end subroutine alloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_monitor_lc(my_rank, nprocs, gauss)
!
      integer, intent(in) :: my_rank, nprocs
      type(picked_spectrum_data), intent(inout) :: gauss
      integer(kind = kint) :: num
!
!
      allocate(gauss%gauss_mode_name_lc(gauss%num_sph_mode_lc))
!
      if(my_rank .eq. 0) then
        num = gauss%istack_picked_spec_lc(nprocs)
        allocate(gauss%gauss_mode_name_out(num))
        allocate(gauss%ncomp_gauss_out(num))
!
        gauss%ncomp_gauss_out(1:num) = 1
      else
        allocate(gauss%gauss_mode_name_out(0))
      end if
!
      end subroutine alloc_gauss_coef_monitor_lc
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_monitor_lc(gauss)
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
!
      deallocate(gauss%gauss_mode_name_lc)
      deallocate(gauss%gauss_mode_name_out, gauss%ncomp_gauss_out)
!
      end subroutine dealloc_gauss_coef_monitor_lc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      deallocate( list_pick%idx_pick_mode )
      deallocate( list_pick%idx_pick_l, list_pick%idx_pick_m )
!
      end subroutine dealloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate( picked%id_radius, picked%radius_gl)
!
      end subroutine dealloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate(picked%spectr_name)
      deallocate(picked%istack_comp_rj, picked%ifield_monitor_rj)
!
      end subroutine dealloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_pick_sph_file_header                             &
     &         (id_file, nlayer_ICB, nlayer_CMB, picked)
!
      use sph_power_spectr_data_text
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call dup_pick_sph_file_header(nlayer_ICB, nlayer_CMB,             &
     &                              picked, sph_OUT)
      call len_sph_layer_spectr_header(pick_spectr_labels, sph_OUT,     &
     &                                 len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &      sph_layer_spectr_header_text(len_tot, len_each,             &
     &                                   pick_spectr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
!
      end subroutine write_pick_sph_file_header
!
! -----------------------------------------------------------------------
!
      subroutine write_each_pick_sph_file_header                        &
     &         (id_file, nlayer_ICB, nlayer_CMB, picked)
!
      use sph_power_spectr_data_text
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT)
      call len_sph_layer_spectr_header(pick_spectr_labels, sph_OUT,     &
     &                                 len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &      sph_layer_spectr_header_text(len_tot, len_each,             &
     &                                   pick_spectr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
!
      end subroutine write_each_pick_sph_file_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_pick_sph_file_header(nlayer_ICB, nlayer_CMB,       &
     &                                    picked, sph_OUT)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%ltr_sph = picked%num_sph_mode
      call dup_pick_sph_file_header_base(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT)
!
      end subroutine dup_pick_sph_file_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_each_pick_sph_file_header(nlayer_ICB, nlayer_CMB,  &
     &                                    picked, sph_OUT)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%ltr_sph = 1
      call dup_pick_sph_file_header_base(nlayer_ICB, nlayer_CMB,        &
     &                                   picked, sph_OUT)
!
      end subroutine dup_each_pick_sph_file_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_pick_sph_file_header_base(nlayer_ICB, nlayer_CMB,  &
     &                                         picked, sph_OUT)
!
      use m_time_labels
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i, icou
!
      sph_OUT%nri_sph = picked%num_layer
      sph_OUT%nri_dat = picked%num_layer
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
!
      sph_OUT%nfield_sph_spec = picked%num_field_rj
      sph_OUT%ntot_sph_spec =   picked%ntot_comp_rj
      sph_OUT%num_time_labels = 6
      call alloc_sph_espec_name(sph_OUT)
      call alloc_sph_spectr_data(picked%num_sph_mode, sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Radius_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Radius'
      sph_OUT%ene_sph_spec_name(5) = 'Degree'
      sph_OUT%ene_sph_spec_name(6) = 'Order'
!
      icou = sph_OUT%num_time_labels
      do i = 1, picked%num_field_rj
        sph_OUT%ncomp_sph_spec(i) = picked%istack_comp_rj(i)            &
     &                             - picked%istack_comp_rj(i-1)
      end do
      do i = 1, picked%ntot_comp_rj
        sph_OUT%ene_sph_spec_name(icou+i) = picked%spectr_name(i)
      end do
!
      end subroutine dup_pick_sph_file_header_base
!
! -----------------------------------------------------------------------
!
      end module t_pickup_sph_spectr_data
