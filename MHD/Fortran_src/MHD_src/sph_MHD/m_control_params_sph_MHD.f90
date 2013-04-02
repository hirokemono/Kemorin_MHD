!m_control_params_sph_MHD.f90
!      module m_control_params_sph_MHD
!
!     Written by H. Matsui on Oct., 2009
!
!      subroutine allocate_vsp_bc_array(jmax)
!      subroutine allocate_temp_bc_array(jmax)
!      subroutine allocate_dscalar_bc_array(jmax)
!
!      subroutine deallocate_vsp_bc_array
!      subroutine deallocate_temp_bc_array
!      subroutine deallocate_dscalar_bc_array
!
      module m_control_params_sph_MHD
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: iflag_rigid_icb =    1
      integer(kind = kint) :: iflag_free_icb =     0
      integer(kind = kint) :: iflag_rotatable_ic = 0
!
      integer(kind = kint) :: iflag_rigid_cmb =  1
      integer(kind = kint) :: iflag_free_cmb =   0
!
      real(kind= kreal), allocatable :: vp_ICB_bc(:)
      real(kind= kreal), allocatable :: vt_ICB_bc(:)
      real(kind= kreal), allocatable :: vp_CMB_bc(:)
      real(kind= kreal), allocatable :: vt_CMB_bc(:)
!
      integer(kind = kint) :: iflag_hflux_icb = 0
      integer(kind = kint) :: iflag_hflux_cmb = 0
!
      real(kind= kreal), allocatable :: temp_ICB_bc(:)
      real(kind= kreal), allocatable :: temp_CMB_bc(:)
      real(kind= kreal), allocatable :: h_flux_ICB_bc(:)
      real(kind= kreal), allocatable :: h_flux_CMB_bc(:)
!
!
      integer(kind = kint) :: iflag_center_b = 0
      integer(kind = kint) :: iflag_ins_icb =  1
      integer(kind = kint) :: iflag_ins_cmb =  1
      integer(kind = kint) :: iflag_p_vacume_icb =  0
      integer(kind = kint) :: iflag_p_vacume_cmb =  0
!
!
      integer(kind = kint) :: iflag_cflux_icb = 0
      integer(kind = kint) :: iflag_cflux_cmb = 0
!
      real(kind= kreal), allocatable :: composition_ICB_bc(:)
      real(kind= kreal), allocatable :: composition_CMB_bc(:)
      real(kind= kreal), allocatable :: c_flux_ICB_bc(:)
      real(kind= kreal), allocatable :: c_flux_CMB_bc(:)
!
      integer(kind = kint) :: iflag_sph_coriolis_file = 0
!
      integer(kind = kint) :: kr_rj_fluid_start =    1
      integer(kind = kint) :: kr_rj_fluid_end =      1
      integer(kind = kint) :: kr_rj_thermal_start =  1
      integer(kind = kint) :: kr_rj_thermal_end =    1
      integer(kind = kint) :: kr_rj_conduct_start =  1
      integer(kind = kint) :: kr_rj_conduct_end =    1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_vsp_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(vp_ICB_bc(jmax))
      allocate(vt_ICB_bc(jmax))
      allocate(vp_CMB_bc(jmax))
      allocate(vt_CMB_bc(jmax))
      vp_ICB_bc = 0.0d0
      vt_ICB_bc = 0.0d0
      vp_CMB_bc = 0.0d0
      vt_CMB_bc = 0.0d0
!
      end subroutine allocate_vsp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine allocate_temp_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(temp_ICB_bc(jmax))
      allocate(temp_CMB_bc(jmax))
      allocate(h_flux_ICB_bc(jmax))
      allocate(h_flux_CMB_bc(jmax))
      temp_ICB_bc = 0.0d0
      temp_CMB_bc = 0.0d0
      h_flux_ICB_bc = 0.0d0
      h_flux_CMB_bc = 0.0d0
!
      end subroutine allocate_temp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine allocate_dscalar_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(composition_ICB_bc(jmax))
      allocate(composition_CMB_bc(jmax))
      allocate(c_flux_ICB_bc(jmax))
      allocate(c_flux_CMB_bc(jmax))
      composition_ICB_bc = 0.0d0
      composition_CMB_bc = 0.0d0
      c_flux_ICB_bc = 0.0d0
      c_flux_CMB_bc = 0.0d0
!
      end subroutine allocate_dscalar_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_vsp_bc_array
!
      deallocate(vp_ICB_bc, vt_ICB_bc)
      deallocate(vp_CMB_bc, vt_CMB_bc)
!
      end subroutine deallocate_vsp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_temp_bc_array
!
      deallocate(temp_ICB_bc,   temp_CMB_bc)
      deallocate(h_flux_ICB_bc, h_flux_CMB_bc)
!
      end subroutine deallocate_temp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_dscalar_bc_array
!
      deallocate(composition_ICB_bc, composition_CMB_bc)
      deallocate(c_flux_ICB_bc,  c_flux_CMB_bc)
!
      end subroutine deallocate_dscalar_bc_array
!
! -----------------------------------------------------------------------
!
      end module m_control_params_sph_MHD
