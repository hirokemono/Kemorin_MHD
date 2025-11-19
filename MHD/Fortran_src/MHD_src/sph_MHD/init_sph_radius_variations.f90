!>@file   init_sph_radius_variations.f90
!!@brief  module init_sph_radius_variations
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Set radial variations for densityuy and diffusivities
!!
!!@verbatim
!!      subroutine init_radius_variations_sph_mhd                       &
!!     &         (sph_rj, r_2nd, MHD_prop, radial_variation)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(phys_data), intent(inout) :: radial_variation
!!@endverbatim
!!
!
      module init_sph_radius_variations
!
      use m_precision
      use m_constants
      use t_control_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_radial_interpolate
      use t_field_data_IO
      use t_fdm_coefs
!
      implicit none
!
      private :: check_r_variation_data_list
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_radius_variations_sph_mhd                         &
     &         (sph_rj, r_2nd, MHD_prop, radial_variation)
!
      use calypso_mpi_real
      use transfer_to_long_integers
      use set_sph_radial_variations
      use m_base_field_labels
      use m_diffusion_term_labels
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd

      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(phys_data), intent(inout) :: radial_variation
!
      character(len=kchara), parameter :: radius_name = 'radius'
      type(sph_radial_interpolate) :: r_itp
      type(field_IO) :: fld_IO
!
      integer(kind = kint) :: k
!
!
!      if(iflag_debug .gt. 0) then
        call check_r_variation_data_list(MHD_prop, radial_variation)
!      end if
      return
!
      if(my_rank .eq. 0) then
        call set_sph_radial_density(my_rank, radius_name,               &
     &      density%name, MHD_prop%fl_prop%ir_rho,                      &
     &      sph_rj, r_2nd, MHD_prop%polytrope_param,                    &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      kinetic_viscosity%name, MHD_prop%fl_prop%ir_nu,             &
     &      sph_rj, r_2nd, MHD_prop%val_viscous_param,                  &
     &      radial_variation, r_itp, fld_IO)
!
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      magnetic_diffusivity%name, MHD_prop%cd_prop%ir_eta,         &
     &      sph_rj, r_2nd, MHD_prop%val_mag_diffuse_param,              &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      thermal_diffusivity%name, MHD_prop%ht_prop%ir_kappa,        &
     &      sph_rj, r_2nd, MHD_prop%val_thermal_diffuse_param,          &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      chemical_diffusivity%name, MHD_prop%cp_prop%ir_kappa,       &
     &      sph_rj, r_2nd, MHD_prop%val_comp_diffuse_param,             &
     &      radial_variation, r_itp, fld_IO)
      end if
!
      do k = 1, radial_variation%ntot_phys
        call calypso_mpi_bcast_real(radial_variation%d_fld(1,k),        &
     &      cast_long(radial_variation%n_point), 0)
      end do
!
      end subroutine init_radius_variations_sph_mhd
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine check_r_variation_data_list(MHD_prop,                  &
     &                                       radial_variation)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_data), intent(in) :: radial_variation
      integer(kind = kint) :: icou_ref
!
      write(*,*) 'ir_rho', MHD_prop%fl_prop%ir_rho
      write(*,*) 'ir_drho_norm',  MHD_prop%fl_prop%ir_drho_norm
      write(*,*) 'ir_d2rho_norm', MHD_prop%fl_prop%ir_d2rho_norm
!
      write(*,*) 'ir_nu', MHD_prop%fl_prop%ir_nu
      write(*,*) 'ir_dnu_norm', MHD_prop%fl_prop%ir_dnu_norm
      write(*,*) 'ir_eta', MHD_prop%cd_prop%ir_eta
      write(*,*) 'ir_deta_norm', MHD_prop%cd_prop%ir_deta_norm
      write(*,*) 'ir_kappa', MHD_prop%ht_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%ht_prop%ir_dkappa_norm
      write(*,*) 'ir_kappa', MHD_prop%cp_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%cp_prop%ir_dkappa_norm
      do icou_ref = 1, radial_variation%num_phys
        write(*,*) icou_ref, trim(radial_variation%phys_name(icou_ref))
      end do
!
      end subroutine check_r_variation_data_list
!
! -----------------------------------------------------------------------
!
      end module init_sph_radius_variations
