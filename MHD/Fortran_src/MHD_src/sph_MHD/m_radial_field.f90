!>@file   m_radial_field.f90
!!@brief  module m_radial_field
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  Radial field data f(r)
!!
!!@verbatim
!!      subroutine allocate_radial_field(nri_rj)
!!      subroutine deallocate_radial_field
!!@endverbatim
!
      module m_radial_field
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
!
      implicit none
!
!>       Structure of radial field
      type(phys_data), save :: d_rad
!>       Structure of radial field address
      type(phys_address), save :: irad
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_radial_field(nri_rj)
!
      use m_phys_constants
      use m_control_parameter
      use m_phys_labels
      use m_radial_parameter_input
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: nri_rj
      integer(kind = kint) :: icou, ncomp, i, j, ist, k, inum
!
!
      d_rad%num_phys = 0
!
      if(iflag_radial_param_file .eq. id_read_boundary_file) then
        d_rad%num_phys = d_rad%num_phys + num_r_param_ctl
      end if
!
      if(iflag_4_coriolis .eq. id_turn_ON)                              &
     &       d_rad%num_phys = d_rad%num_phys + 1
      if(iflag_magneto_cv .eq. id_turn_ON)                              &
     &       d_rad%num_phys = d_rad%num_phys + 1
      if(iflag_4_ref_temp .eq. id_sphere_ref_temp)                      &
     &       d_rad%num_phys = d_rad%num_phys + 1
!
      call alloc_phys_name_type(d_rad)
!
      icou = 0
      do i = 1, num_r_param_ctl
        icou = icou + 1
        d_rad%phys_name(icou) = rfld_ctls(i)%r_param_name
        d_rad%num_component(icou) = rfld_ctls(i)%ncomp_r_param
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
!
        if(cmp_no_case(rfld_ctls(i)%r_param_name, fhd_K_viscosity))     &
     &      irad%i_K_viscosity =   d_rad%istack_component(icou-1) + 1
        if(cmp_no_case(rfld_ctls(i)%r_param_name, fhd_T_diffusivity))   &
     &      irad%i_T_diffusivity = d_rad%istack_component(icou-1) + 1
        if(cmp_no_case(rfld_ctls(i)%r_param_name, fhd_C_diffusivity))   &
     &      irad%i_C_diffusivity = d_rad%istack_component(icou-1) + 1
        if(cmp_no_case(rfld_ctls(i)%r_param_name, fhd_B_diffusivity))   &
     &      irad%i_B_diffusivity = d_rad%istack_component(icou-1) + 1
        if(cmp_no_case(rfld_ctls(i)%r_param_name, fhd_ref_density))     &
     &      irad%i_ref_density =   d_rad%istack_component(icou-1) + 1
      end do
!
      if(iflag_4_coriolis .eq. id_turn_ON) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_omega
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_omega = d_rad%istack_component(icou-1) + 1
      end if
      if(iflag_magneto_cv .eq. id_turn_ON) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_back_B
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_back_B = d_rad%istack_component(icou-1) + 1
      end if
      if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        icou = icou + 1
        d_rad%phys_name(icou) = fhd_ref_temp
        d_rad%num_component(icou) = n_vector
        d_rad%istack_component(icou) = d_rad%istack_component(icou-1)   &
     &                                + d_rad%num_component(icou)
        irad%i_ref_t = d_rad%istack_component(icou-1) + 1
      end if
      d_rad%ntot_phys = d_rad%istack_component(d_rad%num_phys)
!
      call alloc_phys_data_type(nri_rj, d_rad)
!
      do i = 1, num_r_param_ctl
        do j = 1, d_rad%ntot_phys
          if(d_rad%phys_name(j) .eq. rfld_ctls(i)%r_param_name) then
            ist = d_rad%istack_component(icou-1)
            ncomp = d_rad%num_component(icou)
            do inum = 1, rfld_ctls(i)%nri_param
              k = int(rfld_ctls(i)%kr_param(inum))
              d_rad%d_fld(k,ist+1:ist+ncomp)                            &
     &          = rfld_ctls(i)%r_param(inum,1:ncomp)
            end do
            exit
          end if
        end do
      end do
!
      end subroutine allocate_radial_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_radial_field
!
!
      call dealloc_phys_data_type(d_rad)
      call dealloc_phys_name_type(d_rad)
!
      end subroutine deallocate_radial_field
!
! ----------------------------------------------------------------------
!
      end module m_radial_field
