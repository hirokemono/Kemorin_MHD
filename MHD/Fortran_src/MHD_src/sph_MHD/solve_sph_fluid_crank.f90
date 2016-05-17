!>@file   solve_sph_fluid_crank.f90
!!@brief  module solve_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine solve_velo_by_vort_sph_crank(sph_rj, nri, jmax,      &
!!     &          vp_evo_lu, vt_evo_lu, i_vp_pivot, i_vt_pivot,         &
!!     &          is_velo, it_velo, n_point, ntot_phys_rj, d_rj)
!!        Input address:    is_velo, it_velo
!!        Solution address: is_velo, it_velo
!!
!!      subroutine solve_pressure_by_div_v                              &
!!     &         (sph_rj, nri, jmax, p_poisson_lu, i_p_pivot,           &
!!     &          is_press, n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine solve_magne_sph_crank(sph_rj, nri, jmax,             &
!!     &          bs_evo_lu, bt_evo_lu, i_bs_pivot, i_bt_pivot,         &
!!     &          is_magne, it_magne, n_point, ntot_phys_rj, d_rj)
!!        Input address:    is_magne, it_magne
!!        Solution address: is_magne, it_magne
!!
!!      subroutine solve_scalar_sph_crank(sph_rj, nri, jmax,            &
!!     &          evo_lu, i_pivot, s00_evo_lu, i_s00_pivot, is_field,   &
!!     &          n_point, ntot_phys_rj, d_rj, sol_00)
!!        Input address:    is_field
!!        Solution address: is_field
!!@endverbatim
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module solve_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_phys_address
      use t_spheric_rj_data
!
      use set_reference_sph_mhd
      use lubksb_357band_mul
!
      implicit none
!
      private :: copy_degree0_comps_to_sol, copy_degree0_comps_from_sol
      private :: check_temperature, check_NaN_temperature
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine solve_velo_by_vort_sph_crank(sph_rj, nri, jmax,        &
     &          vp_evo_lu, vt_evo_lu, i_vp_pivot, i_vt_pivot,           &
     &          is_velo, it_velo, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: vp_evo_lu(9,nri,jmax)
      real(kind = kreal), intent(in) :: vt_evo_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_vp_pivot(nri,jmax)
      integer(kind = kint), intent(in) :: i_vt_pivot(nri,jmax)
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_velo, it_velo
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!      integer(kind = kint) :: inod, k, j
!
!
!      write(my_rank+70,*) 'k, j, inod, vp_rhs, vt_rhs'
!      do j = 1, sph_rj%nidx_rj(2)
!        j = 3
!        do k = 1, sph_bc_U%kr_out
!          inod = (k-1) * sph_rj%nidx_rj(2) + j
!          write(my_rank+70,*) k, j, inod,                              &
!     &                 d_rj(inod,is_velo), d_rj(inod,it_velo)
!        end do
!      end do
!
      call lubksb_5band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, vp_evo_lu, i_vp_pivot, d_rj(1,is_velo) )
!
      call lubksb_3band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, vt_evo_lu, i_vt_pivot, d_rj(1,it_velo) )
!
!
      end subroutine solve_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine solve_pressure_by_div_v                                &
     &         (sph_rj, nri, jmax, p_poisson_lu, i_p_pivot,             &
     &          is_press, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: p_poisson_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_p_pivot(nri,jmax)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_press
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      call lubksb_3band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, p_poisson_lu, i_p_pivot, d_rj(1,is_press) )
!
      end subroutine solve_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine solve_magne_sph_crank(sph_rj, nri, jmax,               &
     &          bs_evo_lu, bt_evo_lu, i_bs_pivot, i_bt_pivot,           &
     &          is_magne, it_magne, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: bs_evo_lu(5,nri,jmax)
      real(kind = kreal), intent(in) :: bt_evo_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_bs_pivot(nri,jmax)
      integer(kind = kint), intent(in) :: i_bt_pivot(nri,jmax)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne, it_magne
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      call lubksb_3band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, bs_evo_lu, i_bs_pivot, d_rj(1,is_magne) )
!
      call lubksb_3band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, bt_evo_lu, i_bt_pivot, d_rj(1,it_magne) )
!
      end subroutine solve_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine solve_scalar_sph_crank(sph_rj, nri, jmax,              &
     &          evo_lu, i_pivot, s00_evo_lu, i_s00_pivot, is_field,     &
     &          n_point, ntot_phys_rj, d_rj, sol_00)
!
      use m_t_int_parameter
      use cal_sph_exp_center
      use lubksb_357band
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: evo_lu(5,nri,jmax)
      integer(kind = kint), intent(in) :: i_pivot(nri,jmax)
      real(kind = kreal), intent(in) :: s00_evo_lu(5,0:nri)
      integer(kind = kint), intent(in) :: i_s00_pivot(0:nri)
!
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
!
      if(sph_rj%inod_rj_center .gt. 0) then
        call copy_degree0_comps_to_sol(nri, jmax,                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_field, n_point, ntot_phys_rj, d_rj, sol_00)
      end if
!
!      j = find_local_sph_address(sph_rj, 30,-23)
!      if(j.gt.0) then
!        write(*,*) 'matrix'
!        call check_single_radial_3band_mat(my_rank, nri,               &
!     &      sph_rj%radius_1d_rj_r, evo_mat(1,1,j))
!      end if
!
      call lubksb_3band_mul(np_smp, sph_rj%istack_rj_j_smp,             &
     &    jmax, nri, evo_lu, i_pivot, d_rj(1,is_field) )
!
!       write(*,*) 'solution'
!       call check_temperature                                          &
!     &    (30,-23, sph_rj, is_field, ntot_phys_rj, d_rj)
!       write(*,*) 'check_NaN_temperature'
!       call check_NaN_temperature                                      &
!     &    (is_field, sph_rj, n_point, ntot_phys_rj, d_rj)
!
!   Solve l=m=0 including center
      if(sph_rj%inod_rj_center .eq. 0) return
      call lubksb_3band(nri+1, s00_evo_lu, i_s00_pivot, sol_00)
      call copy_degree0_comps_from_sol(nri, jmax,                       &
     &    sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero, sol_00,     &
     &    is_field, n_point, ntot_phys_rj, d_rj)
!
      end subroutine solve_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_to_sol(nri, jmax,                   &
     &          inod_rj_center, idx_rj_degree_zero, is_field,           &
     &          n_point, ntot_phys_rj, d_rj, sol_00)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point,  ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_from_sol(nri, jmax,                 &
     &          inod_rj_center, idx_rj_degree_zero, sol_00, is_field,   &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(in) :: sol_00(0:nri)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
! -----------------------------------------------------------------------
!
      subroutine check_temperature                                      &
     &          (l, m, sph_rj, is_field, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: l, m, is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: j,k,inod
      integer(kind = 4) :: l4, m4
!
!
      l4 = int(l)
      m4 = int(m)
      j = find_local_sph_address(sph_rj, l4, m4)
      if(j .eq. 0) return
!
      write(*,*) 'field ID, l, m: ', is_field, l, m
      do k = 1, sph_rj%nidx_rj(1)
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        write(*,*) k, d_rj(inod,is_field)
      end do
!
      end subroutine check_temperature
!
! -----------------------------------------------------------------------
!
      subroutine check_NaN_temperature                                  &
     &         (is_field, sph_rj, n_point, ntot_phys_rj, d_rj)
!
      use spherical_harmonics
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do inod = 1, n_point
        if(d_rj(inod,is_field) .ne. d_rj(inod,is_field)) then
          j = sph_rj%idx_global_rj(inod,2)
          k = sph_rj%idx_global_rj(inod,1)
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
      end module solve_sph_fluid_crank
