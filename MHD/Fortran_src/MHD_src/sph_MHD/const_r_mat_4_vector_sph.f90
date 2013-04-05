!const_r_mat_4_vector_sph.f90
!      module const_r_mat_4_vector_sph
!
!     Written by H. Matsui on Oct, 2009
!
!      subroutine const_radial_mat_vort_2step
!      subroutine const_radial_mat_4_magne_sph
!
      module const_r_mat_4_vector_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_t_int_parameter
      use m_spheric_param_smp
      use m_spheric_parameter
      use m_radial_matrices_sph
      use m_ludcmp_3band
      use set_radial_mat_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_vort_2step
!
      use m_control_params_sph_MHD
      use set_free_slip_sph_mat_bc
      use set_non_slip_sph_mat_bc
      use set_sph_mom_mat_bc
      use cal_inner_core_rotation
      use m_ludcmp_band
!
      integer(kind = kint) :: kst, ked, ip, jst, jed, j
!      integer(kind = kint) :: k
!
!
      kst = nlayer_ICB+1
      ked = nlayer_CMB-1
!$omp parallel
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    kst, ked, coef_imp_v, coef_d_velo, vt_evo_mat)
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    kst, ked, coef_imp_v, coef_d_velo, wt_evo_mat)
      call set_radial_vp_mat_sph(kst, ked)
      call set_radial_press_mat_sph(kst, ked)
!$omp end parallel
!
!   Boundary condition for ICB
!
      call set_icb_wt_sph_evo_mat
      call set_icb_p_sph_poisson_mat
!
      if(iflag_icb_velocity .eq. iflag_free_slip) then
        call set_free_slip_icb_vt_sph_mat
        call set_free_icb_vp_sph_poisson_mat
      else
        call set_non_slip_icb_vt_sph_mat
        call set_rgd_icb_vp_sph_poisson_mat
      end if
!
!   Rotation for inner core
!
      if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call set_icore_viscous_matrix
      end if
!
!   Boundary condition for CMB
!
      call set_cmb_wt_sph_evo_mat
      call set_cmb_p_sph_poisson_mat
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call set_free_slip_cmb_vt_sph_mat
        call set_free_cmb_vp_sph_poisson_mat
      else
        call set_non_slip_cmb_vt_sph_mat
        call set_rgd_cmb_vp_sph_poisson_mat
      end if
!
!
      call set_vp_evo_mat_sph_by_mat(nlayer_ICB, nlayer_CMB)
!
      if(i_debug .gt. 0) call check_vorticity_matrices_sph(my_rank)
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_band(nidx_rj(1), ifive, vp_evo_mat(1,1,j),        &
     &        vp_evo_lu(1,1,j) ,i_vp_pivot(1,j), vp_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), vt_evo_mat(1,1,j),              &
     &        i_vt_pivot(1,j), ierr, vt_evo_lu(1,1,j), vt_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), wt_evo_mat(1,1,j),              &
     &        i_wt_pivot(1,j), ierr, wt_evo_lu(1,1,j), wt_evo_det(1,j))
          call ludcmp_3band(nidx_rj(1), vs_poisson_mat(1,1,j),          &
     &        i_vs_pivot(1,j), ierr, vs_poisson_lu(1,1,j),              &
     &        vs_poisson_det(1,j) )
          call ludcmp_3band(nidx_rj(1), p_poisson_mat(1,1,j),           &
     &        i_p_pivot(1,j), ierr, p_poisson_lu(1,1,j),                &
     &        p_poisson_det(1,j) )
        end do
      end do
!$omp end parallel do
!
!      do j = 1, nidx_rj(2)
!        do k = 1, nidx_rj(1)
!          vp_evo_det(1,j) = vp_evo_det(1,j) * vp_evo_lu(5,k,j)
!          vt_evo_det(1,j) = vt_evo_det(1,j) * vt_evo_lu(3,k,j)
!        end do
!        write(my_rank+60,*) 'det vp', j,                               &
!     &                       vp_evo_det(1,j), vt_evo_det(1,j)
!      end do
!
      end subroutine const_radial_mat_vort_2step
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_magne_sph
!
      use m_control_params_sph_MHD
      use set_sph_magne_mat_bc
!
      integer(kind = kint) :: kst, ked, ip, jst, jed, j
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kst = itwo
        call set_magne_center_rmat_sph
      else if(iflag_icb_magne .eq. iflag_pseudo_vacuum) then
        kst = nlayer_ICB+1
        call set_qvacume_magne_icb_rmat_sph
      else
        kst = nlayer_ICB+1
        call set_ins_magne_icb_rmat_sph
      end if
!
      if(iflag_cmb_magne .eq. iflag_pseudo_vacuum) then
        call set_qvacume_magne_cmb_rmat_sph
      else
        call set_ins_magne_cmb_rmat_sph
      end if
!
      ked = nlayer_CMB-1
!$omp parallel
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    kst, ked, coef_imp_b, coef_d_magne, bs_evo_mat)
      call set_radial_vect_evo_mat_sph(nidx_rj(1), nidx_rj(2),          &
     &    kst, ked, coef_imp_b, coef_d_magne, bt_evo_mat)
!$omp end parallel
!
!
!$omp parallel do private(jst,jed,j)
      do ip = 1, np_smp
        jst = idx_rj_smp_stack(ip-1,2) + 1
        jed = idx_rj_smp_stack(ip,  2)
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), bs_evo_mat(1,1,j),              &
     &        i_bs_pivot(1,j), ierr, bs_evo_lu(1,1,j), bs_evo_det(1,j))
        end do
!
        do j = jst, jed
          call ludcmp_3band(nidx_rj(1), bt_evo_mat(1,1,j),              &
     &        i_bt_pivot(1,j), ierr, bt_evo_lu(1,1,j), bt_evo_det(1,j))
        end do
      end do
!$omp end parallel do
!
      end subroutine const_radial_mat_4_magne_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_vector_sph
