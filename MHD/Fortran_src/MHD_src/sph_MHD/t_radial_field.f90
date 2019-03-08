!>@file   t_radial_field.f90
!!@brief  module t_radial_field
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  Radial field data f(r)
!!
!!@verbatim
!!      subroutine alloc_radial_field(nri_rj,                           &
!!     &          fl_prop, cd_prop, ref_param_T, ref_param_C,           &
!!     &          r_file, r_field)
!!      subroutine deallocate_radial_field
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!@endverbatim
!
      module t_radial_field
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_phys_data
      use t_phys_address
      use t_physical_property
      use t_reference_scalar_param
!
      implicit none
!
      type radial_field_type
!>         Structure of radial field
        type(phys_data) :: d_rad
!>         Structure of radial field address
        type(phys_address) :: irad
      end type radial_field_type
!
      private :: each_radial_field_num_by_model
      private :: each_radial_field_name_by_model
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_radial_field(nri_rj,                             &
     &          fl_prop, cd_prop, ref_param_T, ref_param_C,             &
     &          r_file, r_field)
!
      use t_radial_parameter_input
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: nri_rj
      type(radial_parameter_file), intent(in) :: r_file
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
!
      type(radial_field_type), intent(inout) :: r_field
!
      integer(kind = kint) :: icou, i
!
!
      r_field%d_rad%num_phys = 0
      call each_radial_field_num_by_file(r_file, r_field%d_rad)
      call each_radial_field_num_by_model                               &
     &   (fl_prop, cd_prop, ref_param_T, ref_param_C, r_field%d_rad)
!
      call alloc_phys_name_type(r_field%d_rad)
!
      icou = 0
      do i = 1, r_file%num_r_param_ctl
        call each_radial_field_name_by_file                             &
     &     (r_file%rfld_ctls(i), r_field%d_rad, r_field%irad, icou)
      end do
!
      call each_radial_field_name_by_model                              &
     &   (fl_prop, cd_prop, ref_param_T, ref_param_C,                   &
     &    r_field%d_rad, r_field%irad, icou)
!
      r_field%d_rad%ntot_phys                                           &
     &      = r_field%d_rad%istack_component(r_field%d_rad%num_phys)
      call alloc_phys_data_type(nri_rj, r_field%d_rad)
!
      icou = 0
      do i = 1, r_file%num_r_param_ctl
        call each_radial_field_by_file                                  &
     &     (r_file%rfld_ctls(i), r_field%d_rad, icou)
      end do
!
      end subroutine alloc_radial_field
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_radial_field(r_field)
!
      type(radial_field_type), intent(inout) :: r_field
!
!
      call dealloc_phys_data_type(r_field%d_rad)
      call dealloc_phys_name_type(r_field%d_rad)
!
      end subroutine dealloc_radial_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine each_radial_field_num_by_file(r_file, d_rad)
!
      use t_radial_parameter_input
!
      type(radial_parameter_file), intent(in) :: r_file
      type(phys_data), intent(inout) :: d_rad
!
!
      if(r_file%iflag_radial_param_file .eq. id_read_radial_file) then
        d_rad%num_phys = d_rad%num_phys + r_file%num_r_param_ctl
      end if
!
      end subroutine each_radial_field_num_by_file
!
! ----------------------------------------------------------------------
!
      subroutine each_radial_field_num_by_model                         &
     &         (fl_prop, cd_prop, ref_param_T, ref_param_C, d_rad)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
!
      type(phys_data), intent(inout) :: d_rad
!
!
      if(fl_prop%iflag_4_coriolis .eq. id_turn_ON)                      &
     &       d_rad%num_phys = d_rad%num_phys + 1
      if(cd_prop%iflag_magneto_cv .eq. id_turn_ON)                      &
     &       d_rad%num_phys = d_rad%num_phys + 1
      if(ref_param_T%iflag_reference .eq. id_sphere_ref_temp)           &
     &       d_rad%num_phys = d_rad%num_phys + 1
      if(ref_param_C%iflag_reference .eq. id_sphere_ref_temp)           &
     &       d_rad%num_phys = d_rad%num_phys + 1
!
      end subroutine each_radial_field_num_by_model
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine each_radial_field_name_by_file                         &
     &         (rfld, d_rad, irad, icou)
!
      use m_phys_labels
      use t_radial_parameter_input
      use skip_comment_f
!
      type(each_radial_parameter), intent(in) :: rfld
      type(phys_data), intent(inout) :: d_rad
      type(phys_address), intent(inout) :: irad
      integer(kind = kint), intent(inout) :: icou
!
!
        icou = icou + 1
        d_rad%phys_name(icou) = rfld%r_param_name
        d_rad%num_component(icou) = rfld%ncomp_r_param
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
!
      if(cmp_no_case(rfld%r_param_name, fhd_K_viscosity))               &
     &      irad%i_K_viscosity =   d_rad%istack_component(icou-1) + 1
      if(cmp_no_case(rfld%r_param_name, fhd_T_diffusivity))             &
     &      irad%i_T_diffusivity = d_rad%istack_component(icou-1) + 1
      if(cmp_no_case(rfld%r_param_name, fhd_C_diffusivity))             &
     &      irad%i_C_diffusivity = d_rad%istack_component(icou-1) + 1
      if(cmp_no_case(rfld%r_param_name, fhd_B_diffusivity))             &
     &      irad%i_B_diffusivity = d_rad%istack_component(icou-1) + 1
      if(cmp_no_case(rfld%r_param_name, fhd_ref_density))               &
     &      irad%i_ref_density =   d_rad%istack_component(icou-1) + 1
!
      end subroutine each_radial_field_name_by_file
!
! ----------------------------------------------------------------------
!
      subroutine each_radial_field_name_by_model                        &
     &         (fl_prop, cd_prop, ref_param_T, ref_param_C,             &
     &          d_rad, irad, icou)
!
      use m_phys_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(phys_data), intent(inout) :: d_rad
      type(phys_address), intent(inout) :: irad
      integer(kind = kint), intent(inout) :: icou
!
!
      if(fl_prop%iflag_4_coriolis .eq. id_turn_ON) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_omega
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_omega = d_rad%istack_component(icou-1) + 1
      end if
      if(cd_prop%iflag_magneto_cv .eq. id_turn_ON) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_back_B
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_back_B = d_rad%istack_component(icou-1) + 1
      end if
      if(ref_param_T%iflag_reference .eq. id_sphere_ref_temp) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_ref_temp
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_ref_t = d_rad%istack_component(icou-1) + 1
      end if
      if(ref_param_C%iflag_reference .eq. id_sphere_ref_temp) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_ref_light
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_ref_c = d_rad%istack_component(icou-1) + 1
      end if
!
      end subroutine each_radial_field_name_by_model
!
! ----------------------------------------------------------------------
!
      subroutine each_radial_field_by_file(rfld, d_rad, icou)
!
      use t_radial_parameter_input
!
      type(each_radial_parameter), intent(in) :: rfld
      type(phys_data), intent(inout) :: d_rad
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: ncomp, j, ist, k, inum
!
!
      do j = 1, d_rad%ntot_phys
        if(d_rad%phys_name(j) .eq. rfld%r_param_name) then
          ist = d_rad%istack_component(icou-1)
          ncomp = d_rad%num_component(icou)
          do inum = 1, rfld%nri_param
            k = int(rfld%kr_param(inum), KIND(k))
            d_rad%d_fld(k,ist+1:ist+ncomp) = rfld%r_param(inum,1:ncomp)
          end do
          exit
        end if
      end do
!
      end subroutine each_radial_field_by_file
!
! ----------------------------------------------------------------------
!
      end module t_radial_field
