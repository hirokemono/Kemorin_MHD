!>@file   t_radial_reference_temp.f90
!!@brief  module t_radial_reference_temp
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine init_reft_rj_data(sph_rj, ipol, refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(reference_field), intent(inout) :: refs
!!      subroutine output_reference_field(refs)
!!        type(reference_field), intent(in) :: refs
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module t_radial_reference_temp
!
      use m_precision
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_file_IO_parameter
!
      implicit  none
!
      character(len = kchara), parameter, private :: radius_name = 'radius'
!
!>      Structure of reference temperature
      type reference_field
!>        file name to read radial reference data
        type(field_IO_params) :: ref_input_IO
!>        file name to write radial reference data
        type(field_IO_params) :: ref_output_IO
!
!>        Address of radius
        integer(kind = kint) :: iref_radius
!>        Address of reference field
        type(base_field_address) :: iref_base
!>        Address of gradient of reference field
        type(gradient_field_address) :: iref_grad
!>        Structure of reference field (include center at the end)
        type(phys_data) :: ref_field
      end type reference_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_reft_rj_data(sph_rj, ipol, refs)
!
      use m_base_field_labels
      use m_grad_field_labels
      use append_phys_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(reference_field), intent(inout) :: refs
!
!
      refs%ref_field%num_phys =   0
      refs%ref_field%ntot_phys =  0
      call alloc_phys_name(refs%ref_field)

      refs%iref_radius = refs%ref_field%ntot_phys + 1
      call append_field_name_list(radius_name,                          &
     &    ione, .TRUE., .FALSE., izero, refs%ref_field)
!
      if(ipol%base%i_heat_source .gt. 0) then
        refs%iref_base%i_heat_source = refs%ref_field%ntot_phys + 1
        call append_field_name_list(heat_source%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
      if(ipol%base%i_light_source .gt. 0) then
        refs%iref_base%i_light_source = refs%ref_field%ntot_phys + 1
        call append_field_name_list(composition_source%name,            &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
!
      if(ipol%base%i_temp .gt. 0) then
        refs%iref_base%i_temp = refs%ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
!
        refs%iref_grad%i_grad_temp = refs%ref_field%ntot_phys + 1
        call append_field_name_list(grad_temp%name,                     &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
      if(ipol%base%i_light .gt. 0) then
        refs%iref_base%i_light = refs%ref_field%ntot_phys + 1
        call append_field_name_list(composition%name,                   &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
!
        refs%iref_grad%i_grad_composit = refs%ref_field%ntot_phys + 1
        call append_field_name_list(grad_composition%name,              &
     &      ione, .TRUE., .FALSE., izero, refs%ref_field)
      end if
!
      call alloc_phys_data((sph_rj%nidx_rj(1)+1), refs%ref_field)
!
      refs%ref_field%d_fld(1,refs%iref_radius) = 0.0d0
!$omp parallel workshare
      refs%ref_field%d_fld(2:sph_rj%nidx_rj(1)+1,refs%iref_radius)      &
     &                    = sph_rj%radius_1d_rj_r(1:sph_rj%nidx_rj(1))
!$omp end parallel workshare
!
      end subroutine init_reft_rj_data
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine output_reference_field(refs)
!
      use calypso_mpi
      use t_time_data
      use t_field_data_IO
      use field_file_IO
!
      use copy_rj_phys_data_4_IO
      use set_sph_extensions
!
      type(reference_field), intent(in) :: refs
!
      type(field_IO) :: sph_out_IO
      type(time_data) :: time_IO
!
!
      if(my_rank .ne. 0) return
!
      time_IO%i_time_step = izero
      time_IO%time = zero
      time_IO%dt = zero
!
      call copy_rj_phys_name_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, sph_out_IO)
      call alloc_phys_data_IO(sph_out_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (refs%ref_field%num_phys_viz, refs%ref_field, sph_out_IO)
!
      call write_step_field_file(refs%ref_output_IO%file_prefix,        &
     &                           my_rank, time_IO, sph_out_IO)
!
      call dealloc_phys_data_IO(sph_out_IO)
      call dealloc_phys_name_IO(sph_out_IO)
!
      end subroutine output_reference_field
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_reference_data(sph_rj, ipol, rj_fld, refs)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use t_time_data
      use t_field_data_IO
      use t_file_IO_parameter
      use t_sph_radial_interpolate
      use field_file_IO
      use r_interpolate_sph_data
      use radial_interpolation
      use fill_scalar_field
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
      type(reference_field), intent(inout) :: refs
!
      type(sph_radial_interpolate) :: r_itp
      type(time_data) :: time_IO
      type(field_IO) :: ref_fld_IO
      integer(kind = kint_gl) :: num64
!
      integer :: k, i
!
      refs%ref_field%iflag_update(1:refs%ref_field%ntot_phys) = 0
      if(my_rank .eq. 0) then
      if(refs%ref_input_IO%iflag_IO .eq. 0) return
        call read_and_alloc_step_field(refs%ref_input_IO%file_prefix,   &
     &                                  my_rank, time_IO, ref_fld_IO)
!
        call copy_reference_radius_from_IO                              &
     &     (ref_fld_IO, refs%ref_field%n_point, radius_name, r_itp)
        call copy_cmb_icb_radial_point                                 &
     &     (ione, refs%ref_field%n_point, r_itp)
        call const_radial_itp_table                                    &
     &     (r_itp%nri_source, r_itp%source_radius,                     &
     &      refs%ref_field%n_point,                                    &
     &      refs%ref_field%d_fld(1,refs%iref_radius),                  &
     &      r_itp%kr_target_inside, r_itp%kr_target_outside,           &
     &      r_itp%k_inter, r_itp%coef_old2new_in)
        call interepolate_reference_data                                &
     &     (radius_name, ref_fld_IO, r_itp, refs%ref_field)
!
      write(*,*) 'r_itp%kr_target_inside', r_itp%kr_target_inside
      write(*,*) 'r_itp%kr_target_outside', r_itp%kr_target_outside
      do k = 1, refs%ref_field%n_point
        write(*,'(i5,1pe16.8,2i5,1p3e16.8)') k,       &
     &         refs%ref_field%d_fld(k,refs%iref_radius),  &
     &         r_itp%k_inter(k,1:2),               &
     &         r_itp%source_radius(r_itp%k_inter(k,1)),                          &
     &         r_itp%source_radius(r_itp%k_inter(k,2)),                         &
     &         r_itp%coef_old2new_in(k)
      end do
!
      do k = 1, refs%ref_field%num_phys
        write(*,*) k, refs%ref_field%iflag_update(k), &
     &        refs%ref_field%phys_name(k)
      end do
      do k = 1, refs%ref_field%n_point
        write(*,*) k, refs%ref_field%d_fld(k,:)
      end do
!
        call dealloc_phys_data_IO(ref_fld_IO)
        call dealloc_phys_name_IO(ref_fld_IO)
      end if
!
      num64 = refs%ref_field%ntot_phys
      call calypso_mpi_bcast_int(refs%ref_field%iflag_update, num64, 0)
      num64 = refs%ref_field%n_point * refs%ref_field%ntot_phys
      call calypso_mpi_bcast_real(refs%ref_field%d_fld, num64, 0)
!
      do i = 1, refs%ref_field%num_phys
        if(refs%ref_field%iflag_update(i) .eq. 0) cycle
        if((ipol%base%i_heat_source*refs%iref_base%i_heat_source) .gt. 0) then
          write(*,*) 'Overwritte heat source from radial field file'
          call copy_degree0_comps_from_sol                              &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,         &
     &        refs%ref_field%d_fld(1,refs%iref_base%i_heat_source),     &
     &        ipol%base%i_heat_source, rj_fld%n_point,                  &
     &        rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if((ipol%base%i_light_source*refs%iref_base%i_light_source) .gt. 0) then
          write(*,*) 'Overwritte composition source from radial field file'
          call copy_degree0_comps_from_sol                              &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,         &
     &        refs%ref_field%d_fld(1,refs%iref_base%i_light_source),    &
     &        ipol%base%i_light_source, rj_fld%n_point,                 &
     &        rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      end do
!
      end subroutine read_alloc_sph_reference_data
!
! -----------------------------------------------------------------------
!
      end module t_radial_reference_temp
