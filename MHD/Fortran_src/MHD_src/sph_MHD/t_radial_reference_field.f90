!>@file   t_radial_reference_field.f90
!!@brief  module t_radial_reference_field
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine dealloc_reference_field(refs)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(radial_reference_field), intent(inout) :: refs
!!      subroutine set_default_reference_file_name(refs)
!!        type(radial_reference_field), intent(in) :: refs
!!      subroutine append_reference_field_names                         &
!!     &         (radius_name, ipol_base, iref_radius,                  &
!!     &          iref_base, iref_grad, iref_cmp, ref_field)
!!        character(len = kchara), intent(in) :: radius_name
!!        type(base_field_address), intent(in) :: ipol_base
!!        integer(kind = kint), intent(inout) :: iref_radius
!!        type(base_field_address), intent(inout) :: iref_base
!!        type(gradient_field_address), intent(inout) :: iref_grad
!!        type(field_component_address), intent(inout) :: iref_cmp
!!        type(phys_data), intent(inout) :: ref_field
!!
!!      subroutine overwrite_sources_by_reference                       &
!!     &         (sph_rj, iref_base, ipol_base, ref_field, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: iref_base
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(phys_data), intent(inout) :: ref_field
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module t_radial_reference_field
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_field_component_labels
      use t_file_IO_parameter
      use t_sph_radial_interpolate
      use t_field_data_IO
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &     :: default_input_reference_file = 'input_reference.dat'
      character(len = kchara), parameter, private                       &
     &     :: default_output_reference_file = 'reference_fields.dat'
!
!>      Structure of reference temperature
      type radial_reference_field
!>        Address of radius
        integer(kind = kint) :: iref_radius
!>        Address of reference field
        type(base_field_address) :: iref_base
!>        Address of gradient of reference field
        type(gradient_field_address) :: iref_grad
!>        Address of reference vector components
        type(field_component_address) :: iref_cmp
!>        Structure of reference field (include center at the end)
        type(phys_data) :: ref_field
!
!>        file name to read radial reference data
        type(field_IO_params) :: ref_input_IO
!>        file name to write radial reference data
        type(field_IO_params) :: ref_output_IO
!>        file name to write radial reference data
        type(field_IO) :: ref_fld_IO
!
!>        Interpolation table from radial data input 
        type(sph_radial_interpolate) :: r_itp
      end type radial_reference_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_reference_field(refs)
!
      type(radial_reference_field), intent(inout) :: refs
!
      call dealloc_phys_data(refs%ref_field)
      call dealloc_phys_name(refs%ref_field)
!
      end subroutine dealloc_reference_field
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_default_reference_file_name(refs)
!
      type(radial_reference_field), intent(inout) :: refs
!
      refs%ref_output_IO%file_prefix = default_output_reference_file
!
      end subroutine set_default_reference_file_name
!
!  --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_reference_field_names                           &
     &         (radius_name, ipol_base, iref_radius,                    &
     &          iref_base, iref_grad, iref_cmp, ref_field)
!
      use m_base_field_labels
      use m_grad_field_labels
      use m_field_component_labels
      use append_phys_data
!
      character(len = kchara), intent(in) :: radius_name
      type(base_field_address), intent(in) :: ipol_base
!
      integer(kind = kint), intent(inout) :: iref_radius
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(field_component_address), intent(inout) :: iref_cmp
      type(phys_data), intent(inout) :: ref_field
!
!
      iref_radius = ref_field%ntot_phys + 1
      call append_field_name_list(radius_name,                          &
     &    ione, .TRUE., .FALSE., izero, ref_field)
!
      if(ipol_base%i_heat_source .gt. 0) then
        iref_base%i_heat_source = ref_field%ntot_phys + 1
        call append_field_name_list(heat_source%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light_source .gt. 0) then
        iref_base%i_light_source = ref_field%ntot_phys + 1
        call append_field_name_list(composition_source%name,            &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        iref_base%i_temp = ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_temp = ref_field%ntot_phys + 1
        call append_field_name_list(grad_temp%name,                     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light .gt. 0) then
        iref_base%i_light = ref_field%ntot_phys + 1
        call append_field_name_list(composition%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_composit = ref_field%ntot_phys + 1
        call append_field_name_list(grad_composition%name,              &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(ipol_base%i_back_B .gt. 0) then
        iref_cmp%i_magne_y = ref_field%ntot_phys + 1
        call append_field_name_list(y_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
        iref_cmp%i_magne_z = ref_field%ntot_phys + 1
        call append_field_name_list(z_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
        iref_cmp%i_magne_x = ref_field%ntot_phys + 1
        call append_field_name_list(x_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
      end if
!
      end subroutine append_reference_field_names
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_sources_by_reference                         &
     &         (sph_rj, iref_base, ipol_base, ref_field, rj_fld)
!
      use interpolate_reference_data
      use init_external_magne_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: iref_base
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: ref_field
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
      call overwrite_each_field_by_ref(sph_rj,                          &
     &    iref_base%i_heat_source, ipol_base%i_heat_source,             &
     &    ref_field, rj_fld)
      call overwrite_each_field_by_ref(sph_rj,                          &
     &    iref_base%i_light_source, ipol_base%i_light_source,           &
     &    ref_field, rj_fld)
!
!      call overwrite_dipole_by_ref(sph_rj,                             &
!     &    iref_base%i_back_B, ipol_base%i_back_B,                      &
!     &    ref_field, rj_fld)
!
      end subroutine overwrite_sources_by_reference
!
! -----------------------------------------------------------------------
!
      end module t_radial_reference_field
