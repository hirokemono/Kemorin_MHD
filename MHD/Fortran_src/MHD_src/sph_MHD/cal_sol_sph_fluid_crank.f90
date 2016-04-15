!>@file   cal_sol_sph_fluid_crank.f90
!!@brief  module cal_sol_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine cal_sol_velo_by_vort_sph_crank(ntot_phys_rj, d_rj)
!!        Input address:    ipol%i_vort, itor%i_vort
!!        Solution address: ipol%i_velo, itor%i_velo
!!
!!      subroutine cal_sol_pressure_by_div_v(ntot_phys_rj, d_rj)
!!        Solution address: ipol%i_press
!!
!!
!!      subroutine cal_sol_magne_sph_crank(ntot_phys_rj, d_rj)
!!        Input address:    ipol%i_magne, itor%i_magne
!!        Solution address: ipol%i_magne, itor%i_magne
!!
!!      subroutine cal_sol_temperature_sph_crank(ntot_phys_rj, d_rj)
!!        Input address:    ipol%i_temp
!!        Solution address: ipol%i_temp
!!      subroutine cal_sol_composition_sph_crank(ntot_phys_rj, d_rj)
!!        Input address:    ipol%i_light
!!        Solution address: ipol%i_light
!!@endverbatim
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module cal_sol_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_phys_address
      use m_radial_matrices_sph
      use set_reference_sph_mhd
!
      use lubksb_357band_mul
!
      implicit none
!
      private :: cal_sol_scalar_sph_crank
      private :: copy_degree0_comps_to_sol, copy_degree0_comps_from_sol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_by_vort_sph_crank(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!       integer(kind = kint) :: k, j
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo) = d_rj(inod,itor%i_vort)
        d_rj(inod,itor%i_velo) = d_rj(inod,ipol%i_vort)
      end do
!$omp end parallel do
!
      call delete_zero_degree_comp(ipol%i_velo,                         &
     &    sph_rj1%idx_rj_degree_zero,     &
     &    nnod_rj, nidx_rj, ntot_phys_rj, d_rj)
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2                                 &
     &     (nnod_rj, nidx_rj(2), sph_bc_U%kr_in,                        &
     &      fdm2_free_vp_ICB, ipol%i_velo, ntot_phys_rj, d_rj)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (sph_rj1%idx_rj_degree_zero, sph_rj1%idx_rj_degree_one,      &
     &      nnod_rj, nidx_rj,    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_rj1%radius_1d_rj_r,     &
     &      vt_ICB_bc, ipol%i_velo, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_rigid_velo2(nnod_rj, nidx_rj(2),           &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, ipol%i_velo,     &
     &      ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2                                 &
     &     (nnod_rj, nidx_rj(2), sph_bc_U%kr_out,                       &
     &      fdm2_free_vp_CMB, ipol%i_velo, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_rigid_velo2(nnod_rj, nidx_rj(2),           &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, vt_CMB_bc, ipol%i_velo,    &
     &      ntot_phys_rj, d_rj)
      end if
!
!      write(my_rank+70,*) 'k, j, inod, vp_rhs, vt_rhs'
!      do j = 1, nidx_rj(2)
!        j = 3
!        do k = 1, sph_bc_U%kr_out
!          inod = (k-1)*nidx_rj(2) + j
!          write(my_rank+70,*) k, j, inod,                              &
!     &                 d_rj(inod,ipol%i_velo), d_rj(inod,itor%i_velo)
!        end do
!      end do
!
      call lubksb_5band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vp_evo_lu,                            &
     &    i_vp_pivot, d_rj(1,ipol%i_velo) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vt_evo_lu,                            &
     &    i_vt_pivot, d_rj(1,itor%i_velo) )
!
!
      end subroutine cal_sol_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_by_div_v(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use set_reference_sph_mhd
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), p_poisson_lu, i_p_pivot,              &
     &    d_rj(1,ipol%i_press) )
!
      call adjust_by_ave_pressure_on_CMB                                &
     &   (sph_bc_U%kr_in, sph_bc_U%kr_out, sph_rj1%idx_rj_degree_zero,  &
     &    nnod_rj, nidx_rj, ntot_phys_rj, d_rj)
!
      end subroutine cal_sol_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_sph_crank(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      call delete_zero_degree_comp(ipol%i_magne,                        &
     &    sph_rj1%idx_rj_degree_zero,    &
     &    nnod_rj, nidx_rj, ntot_phys_rj, d_rj)
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call cal_sph_nod_icb_ins_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in,                        &
     &      sph_bc_B%r_ICB, ipol%i_magne, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in,                        &
     &      ipol%i_magne, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      ipol%i_magne, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      sph_bc_B%r_CMB, ipol%i_magne, ntot_phys_rj, d_rj)
      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bs_evo_lu, i_bs_pivot,                &
     &    d_rj(1,ipol%i_magne) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bt_evo_lu, i_bt_pivot,                &
     &    d_rj(1,itor%i_magne) )
!
      end subroutine cal_sol_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temperature_sph_crank(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_t_int_parameter
      use m_physical_property
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      call cal_sol_scalar_sph_crank(nidx_rj(1), nidx_rj(2),             &
     &    idx_rj_smp_stack, sph_bc_T, coef_temp, coef_d_temp,           &
     &    coef_imp_t, temp_evo_mat, temp_evo_lu, i_temp_pivot,          &
     &    t00_evo_lu, i_t00_pivot, ipol%i_temp,                         &
     &    ntot_phys_rj, d_rj, t00_solution)
!
      end subroutine cal_sol_temperature_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_composition_sph_crank(ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_t_int_parameter
      use m_physical_property
      use m_boundary_params_sph_MHD
      use m_radial_mat_sph_w_center
!
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      call cal_sol_scalar_sph_crank(nidx_rj(1), nidx_rj(2),             &
     &    idx_rj_smp_stack, sph_bc_C, coef_light, coef_d_light,         &
     &    coef_imp_c, composit_evo_mat, composit_evo_lu,                &
     &    i_composit_pivot, c00_evo_lu, i_c00_pivot, ipol%i_light,      &
     &    ntot_phys_rj, d_rj, c00_solution)
!
      end subroutine cal_sol_composition_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine check_temperature(l, m, is_field, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: l, m, is_field
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: j,k,inod
      integer(kind = 4) :: l4, m4
!
!
      l4 = int(l)
      m4 = int(m)
      j = find_local_sph_address(sph_rj1, l4, m4)
      if(j .eq. 0) return
!
      write(*,*) 'field ID, l, m: ', is_field, l, m
      do k = 1, nidx_rj(1)
        inod = j + (k-1)*nidx_rj(2)
        write(*,*) k, d_rj(inod,is_field)
      end do
!
      end subroutine check_temperature
!
! -----------------------------------------------------------------------
!
      subroutine check_NaN_temperature(is_field, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) ::  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do inod = 1, nnod_rj
        if(d_rj(inod,is_field) .ne. d_rj(inod,is_field)) then
          j = sph_rj1%idx_global_rj(inod,2)
          k = sph_rj1%idx_global_rj(inod,1)
          call get_dgree_order_by_full_j(j, l, m)
          write(50+my_rank,*) 'Broken', inod, k, j, l, m,  &
     &              d_rj(inod,is_field)
        end if
      end do
!
end subroutine check_NaN_temperature
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_scalar_sph_crank(nri, jmax, idx_rj_smp_stack,  &
     &          sph_bc, coef_f, coef_d, coef_imp, evo_mat, evo_lu,      &
     &          i_pivot, s00_evo_lu, i_s00_pivot, is_field,             &
     &          ntot_phys_rj, d_rj, sol_00)
!
      use m_spheric_parameter
      use m_t_int_parameter
      use t_boundary_params_sph_MHD
      use set_scalar_boundary_sph
      use cal_sph_exp_center
      use lubksb_357band
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: idx_rj_smp_stack(0:np_smp,2)
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: evo_mat(3,nri,jmax)
      real(kind = kreal), intent(in) :: evo_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_pivot(nri,jmax)
      real(kind = kreal), intent(in) :: s00_evo_lu(5,0:nri)
      integer(kind = kint), intent(in) :: i_s00_pivot(0:nri)
!
      integer(kind = kint), intent(in) :: is_field, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
!
!   Set RHS vector for CMB
      if (sph_bc%iflag_cmb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(nnod_rj, jmax, sph_bc%kr_out,         &
     &      nri, is_field, sph_bc%CMB_fld, ntot_phys_rj, d_rj)
      else if(coef_f .ne. 0.0d0) then
        call adjust_out_fixed_flux_sph(nnod_rj, jmax,                   &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, coef_d, coef_imp, dt, is_field,            &
     &      ntot_phys_rj, d_rj)
      else
        call poisson_out_fixed_flux_sph(nnod_rj, jmax,                  &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, is_field, ntot_phys_rj, d_rj)
      end if
!
!   Set RHS vector for ICB
      if (sph_bc%iflag_icb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(nnod_rj, jmax, ione, sph_bc%kr_in,    &
     &      is_field, sph_bc%ICB_fld, ntot_phys_rj, d_rj)
      else if (sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center(inod_rj_center, nnod_rj,              &
     &      sph_bc%CTR_fld, is_field, ntot_phys_rj, d_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &     .and. coef_f .ne. 0.0d0) then
        call adjust_in_fixed_flux_sph(nnod_rj, jmax,                    &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, coef_d, coef_imp, dt, is_field,            &
     &      ntot_phys_rj, d_rj)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call poisson_in_fixed_flux_sph(nnod_rj, jmax,                   &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, is_field, ntot_phys_rj, d_rj)
      end if
!
      if(inod_rj_center .gt. 0) then
        call copy_degree0_comps_to_sol(nnod_rj, nri, jmax,              &
     &      inod_rj_center, sph_rj1%idx_rj_degree_zero, is_field,       &
     &      ntot_phys_rj, d_rj, sol_00)
      end if
!
!      j = find_local_sph_address(sph_rj1, 30,-23)
!      if(j.gt.0) then
!        write(*,*) 'matrix'
!        call check_single_radial_3band_mat(my_rank, nri,               &
!     &      sph_rj1%radius_1d_rj_r, evo_mat(1,1,j))
!      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    jmax, nri, evo_lu, i_pivot, d_rj(1,is_field) )
!
!       write(*,*) 'solution'
!       call check_temperature(30,-23, is_field, ntot_phys_rj, d_rj)
!       write(*,*) 'check_NaN_temperature'
!       call check_NaN_temperature(is_field, ntot_phys_rj, d_rj)
!
!   Solve l=m=0 including center
      if(inod_rj_center .eq. 0) return
      call lubksb_3band(nri+1, s00_evo_lu, i_s00_pivot, sol_00)
      call copy_degree0_comps_from_sol(nnod_rj, nri, jmax,              &
     &    inod_rj_center, sph_rj1%idx_rj_degree_zero, sol_00, is_field, &
     &    ntot_phys_rj, d_rj)
!
      end subroutine cal_sol_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_to_sol(nnod_rj, nri, jmax,          &
     &          inod_rj_center, idx_rj_degree_zero, is_field,           &
     &          ntot_phys_rj, d_rj, sol_00)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: nnod_rj,  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        sol_00(kr) = d_rj(inod,is_field)
      end do
!$omp end parallel do
      sol_00(0) = d_rj(inod_rj_center,is_field)
!
!       write(*,*) 'kr, Average RHS'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!       end do

      end subroutine copy_degree0_comps_to_sol
!
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_from_sol(nnod_rj, nri, jmax,        &
     &          inod_rj_center, idx_rj_degree_zero, sol_00, is_field,   &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(in) :: sol_00(0:nri)
!
      integer(kind = kint), intent(in) :: nnod_rj,  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        d_rj(inod,is_field) = sol_00(kr)
      end do
!$omp end parallel do
      d_rj(inod_rj_center,is_field) = sol_00(0)
!
!       write(*,*) 'kr, average Solution'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!      end do
!
      end subroutine copy_degree0_comps_from_sol
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_fluid_crank
