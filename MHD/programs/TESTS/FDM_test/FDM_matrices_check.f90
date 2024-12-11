!
      program FDM_matrices_check
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use chebyshev_radial_grid
!
      implicit none
!
      integer(kind = kint), parameter :: num_fluid_grid = 96
      real(kind = kreal), parameter :: rmin = 0.0d0
      real(kind = kreal), parameter :: rmax = 1.6d0
      real(kind = kreal), parameter :: r_ICB = 7.0d0 / 13.0d0
      real(kind = kreal), parameter :: r_CMB = 20.0d0 / 13.0d0
!
      type(sph_grids) :: sph1
      type(fdm_matrices) :: r_2nd_1
      type(fdm_matrices) :: r_n2e_3rd_1
      type(fdm_matrices) :: r_e2n_1st_1
      type(fdm_matrices) :: r_4th_1
!
      type(sph_MHD_boundary_data) :: sph_MHD_bc1
      character(len=kchara), parameter :: BC_label = 'Boundary'
!
      integer :: k
!
      iflag_debug = 0
      sph1%sph_rj%nidx_rj(2) =   6
      sph1%sph_params%radius_ICB = r_ICB
      sph1%sph_params%radius_CMB = r_CMB
      call count_chebyshev_ext_layers(num_fluid_grid,                   &
     &    sph1%sph_params%radius_ICB, sph1%sph_params%radius_CMB,       &
     &    rmin, rmax, sph1%sph_rj%nidx_rj(1),                           &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB)
      sph1%sph_rj%nnod_rj = sph1%sph_rj%nidx_rj(1)                      &
     &                     * sph1%sph_rj%nidx_rj(2)
!
      write(*,*) 'sph_rj%nidx_rj', sph1%sph_rj%nidx_rj(1:2)
      write(*,*) 'nlayer_ICB', sph1%sph_params%nlayer_ICB
      write(*,*) 'nlayer_CMB', sph1%sph_params%nlayer_CMB
!
      call alloc_sph_1d_index_rj(sph1%sph_rj)
!
      call set_chebyshev_distance_shell(sph1%sph_rj%nidx_rj(1),         &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_params%radius_ICB, sph1%sph_params%radius_CMB,       &
     &    sph1%sph_rj%radius_1d_rj_r)
!
      call init_FDM_coefs_for_test                                      &
     &   (sph1, r_2nd_1, r_n2e_3rd_1, r_e2n_1st_1, r_4th_1)
!
      sph_MHD_bc1%sph_bc_U%kr_in = sph1%sph_params%nlayer_ICB
      sph_MHD_bc1%sph_bc_U%kr_out = sph1%sph_params%nlayer_CMB
      call cal_fdm_coefs_4_BCs                                          &
     &   (sph1%sph_rj%nidx_rj(1), sph1%sph_rj%radius_1d_rj_r,           &
     &    sph_MHD_bc1%sph_bc_U)
      call check_fdm_coefs_4_BC2(6, BC_label, sph_MHD_bc1%sph_bc_U)
!
      call init_FDM_boundaries_for_test(sph1,  &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_CTR, &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_ICB, &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_free_ICB,        &
     &    sph_MHD_bc1%bc_fdms_U%fdm2_free_ICB,                          &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_CMB, &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_free_CMB,        &
     &    sph_MHD_bc1%bc_fdms_U%fdm2_free_CMB, sph_MHD_bc1%fdm2_center)
      call test_radial_FDM                                              &
     &   (sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_rj, r_2nd_1, r_n2e_3rd_1, r_e2n_1st_1,               &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_ICB, &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_free_ICB,        &
     &    sph_MHD_bc1%bc_fdms_U%fdm2_free_ICB,                          &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_CMB,   &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_free_CMB,        &
     &    sph_MHD_bc1%bc_fdms_U%fdm2_free_CMB)
!
      call check_sph_fdm_boundaries(6,                                  &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_rj%nidx_rj(1), sph1%sph_rj%radius_1d_rj_r,           &
     &    sph_MHD_bc1%bc_fdms_U)
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_FDM_coefs_for_test                                &
     &         (sph, r_2nd, r_n2e_3rd, r_e2n_1st, r_4th)
!
      use parallel_load_data_4_sph
      use init_radial_infos_sph_mhd
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
      use forth_fdm_node_coefs
!
      type(sph_grids), intent(inout) :: sph
      type(fdm_matrices), intent(inout) :: r_2nd
      type(fdm_matrices), intent(inout) :: r_n2e_3rd
      type(fdm_matrices), intent(inout) :: r_e2n_1st
      type(fdm_matrices), intent(inout) :: r_4th
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(sph%sph_params, sph%sph_rj, r_2nd)
      if (iflag_debug.gt.0) write(*,*) 'const_first_fdm_ele_to_node'
      call const_first_fdm_ele_to_node(sph%sph_rj, r_e2n_1st)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_node_to_ele'
      call const_third_fdm_node_to_ele(sph%sph_rj, r_n2e_3rd)
      if (iflag_debug.gt.0) write(*,*) 'const_forth_fdm_coefs'
      call const_forth_fdm_coefs(sph%sph_rj, r_4th)
!
      end subroutine init_FDM_coefs_for_test
!
!  -------------------------------------------------------------------
!
      subroutine init_FDM_boundaries_for_test(sph, fdm3e_CTR,           &
     &          fdm3e_vp0_ICB, fdm3e_free_ICB, fdm2_free_ICB,           &
     &          fdm3e_vp0_CMB, fdm3e_free_CMB, fdm2_free_CMB,           &
     &          fdm2_center)
!
      use t_coef_fdm2_centre
      use t_coef_fdm2_free_slip_ICB
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm2_free_slip_CMB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_free_vp_CMB
!
      type(sph_grids), intent(inout) :: sph
!
      type(fdm3_n2e_CTR_vpol), intent(inout) :: fdm3e_CTR
      type(fdm3_n2e_ICB_zero_vpol), intent(inout) :: fdm3e_vp0_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(inout) :: fdm3e_free_ICB
      type(fdm2_ICB_free_slip), intent(inout) :: fdm2_free_ICB
!
      type(fdm3_n2e_CMB_zero_vpol), intent(inout) :: fdm3e_vp0_CMB
      type(fdm3_n2e_CMB_free_vpol), intent(inout) :: fdm3e_free_CMB
      type(fdm2_CMB_free_slip), intent(inout) :: fdm2_free_CMB
!
      type(fdm2_center_mat), intent(inout) :: fdm2_center
!
      real(kind = kreal), allocatable :: h_rho(:)
      integer(kind = kint) :: kr_in, kr_out
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_2nd_to_center_fixed_fdm'
      call cal_2nd_to_center_fixed_fdm(sph%sph_rj%radius_1d_rj_r(1),    &
     &                                 fdm2_center)
      call cal_2nd_center_fix_df_fdm(sph%sph_rj%radius_1d_rj_r(1),      &
     &                               fdm2_center)
      call cal_2nd_center_fixed_fdm(sph%sph_rj%radius_1d_rj_r(1),       &
     &                              fdm2_center)
      call cal_fdm3e_CTR_hdiv_vp(sph%sph_rj%radius_1d_rj_r(1),          &
     &                           fdm3e_CTR)
!
      allocate(h_rho(sph%sph_rj%nidx_rj(1)))
      h_rho(:) = zero
!
      kr_in =  sph1%sph_params%nlayer_ICB
      call cal_fdm2_ICB_free_vt(h_rho(kr_in),                           &
     &    sph%sph_rj%radius_1d_rj_r(kr_in   ), fdm2_free_ICB)
      call cal_fdm2_ICB_free_vp(h_rho(kr_in),                           &
     &    sph%sph_rj%radius_1d_rj_r(kr_in   ), fdm2_free_ICB)
      call cal_fdm3e_ICB_hdiv_vp(sph%sph_rj%radius_1d_rj_r(kr_in   ),   &
     &                           fdm3e_vp0_ICB)
      call cal_fdm3e_ICB_free_hdiv_vp(fdm2_free_ICB%dmat_vp,            &
     &                                fdm3e_vp0_ICB, fdm3e_free_ICB)
!
      kr_out = sph1%sph_params%nlayer_CMB
      call cal_fdm2_CMB_free_vt(h_rho(kr_out),                          &
     &    sph%sph_rj%radius_1d_rj_r(kr_out-1), fdm2_free_CMB)
      call cal_fdm2_CMB_free_vp(h_rho(kr_out),                          &
     &    sph%sph_rj%radius_1d_rj_r(kr_out-1), fdm2_free_CMB)
      call cal_fdm3e_CMB_hdiv_vp(sph%sph_rj%radius_1d_rj_r(kr_out-2),   &
     &                           fdm3e_vp0_CMB)
      call cal_fdm3e_CMB_free_hdiv_vp(fdm2_free_CMB%dmat_vp,            &
     &                                fdm3e_vp0_CMB, fdm3e_free_CMB)
      deallocate(h_rho)
!
      end subroutine init_FDM_boundaries_for_test
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_FDM(kr_in, kr_out, sph_rj,                 &
     &          r_2nd, r_n2e_3rd, r_e2n_1st,                            &
     &          fdm3e_vp0_ICB, fdm3e_free_ICB, fdm2_free_ICB,           &
     &          fdm3e_vp0_CMB, fdm3e_free_CMB, fdm2_free_CMB)
!
      use t_coef_fdm2_free_slip_ICB
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm2_free_slip_CMB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_free_vp_CMB
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
!
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
!
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
      type(fdm3_n2e_CMB_free_vpol), intent(in) :: fdm3e_free_CMB
      type(fdm2_CMB_free_slip), intent(in) :: fdm2_free_CMB
!
      real(kind = kreal), allocatable :: r_ele(:)
!
      real(kind = kreal), allocatable :: d_rj(:)
      real(kind = kreal), allocatable :: dr_rj(:)
      real(kind = kreal), allocatable :: d2r_rj(:)
!
      real(kind = kreal), allocatable :: de_rj(:)
      real(kind = kreal), allocatable :: dre_rj(:)
      real(kind = kreal), allocatable :: d2re_rj(:)
      real(kind = kreal), allocatable :: d3re_rj(:)
!
      real(kind = kreal), allocatable :: dfdr_nod(:)
      real(kind = kreal), allocatable :: d2fdr2_nod(:)
!
      real(kind = kreal), allocatable :: d_ele(:)
      real(kind = kreal), allocatable :: dfdr_ele(:)
      real(kind = kreal), allocatable :: d2fdr2_ele(:)
      real(kind = kreal), allocatable :: d3fdr3_ele(:)
!
      real(kind = kreal), allocatable :: d_e2n(:)
      real(kind = kreal), allocatable :: dfdr_e2n(:)
!
      integer(kind = kint) :: inod, j, k, ist_in, ist_out
      real(kind = kreal) :: r,  dr_bc, c_dr, c_dr2, c_dr3
!
!
      allocate(r_ele(sph_rj%nidx_rj(1)))
      do k = 1, sph_rj%nidx_rj(1)
        if(k .eq. 1) then
          r_ele(k) = half * sph_rj%radius_1d_rj_r(k)
        else
          r_ele(k) = half * (sph_rj%radius_1d_rj_r(k-1)                 &
     &                      + sph_rj%radius_1d_rj_r(k))
        end if
      end do
!
      allocate(d_rj(sph_rj%nnod_rj))
      allocate(dr_rj(sph_rj%nnod_rj))
      allocate(d2r_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
        d_rj(inod) =   sph_rj%radius_1d_rj_r(k)**j
        dr_rj(inod) =  dble(j) * sph_rj%radius_1d_rj_r(k)**(j-1)
        d2r_rj(inod) = dble(j*(j-1)) * sph_rj%radius_1d_rj_r(k)**(j-2)
      end do
!
      allocate(de_rj(sph_rj%nnod_rj))
      allocate(dre_rj(sph_rj%nnod_rj))
      allocate(d2re_rj(sph_rj%nnod_rj))
      allocate(d3re_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        de_rj(inod) =   r_ele(k)**j
        dre_rj(inod) =  dble(j) * r_ele(k)**(j-1)
        d2re_rj(inod) = dble(j*(j-1)) * r_ele(k)**(j-2)
        d3re_rj(inod) = dble(j*(j-1)*(j-2)) * r_ele(k)**(j-3)
      end do
!
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(ione, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, dfdr_nod)
      write(*,*) '1st derivative'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (dfdr_nod(inod) - dr_rj(inod)),             &
     &            dfdr_nod(inod), dr_rj(inod)
       end do
      end do
!
      allocate(d2fdr2_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(itwo, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, d2fdr2_nod)
      write(*,*) '2nd derivative'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (d2fdr2_nod(inod) - d2r_rj(inod)),          &
     &            d2fdr2_nod(inod), d2r_rj(inod)
       end do
      end do
!
!
      ist_in =  1 + (kr_in- 1) * sph_rj%nidx_rj(2)
      ist_out = 1 + (kr_out-1) * sph_rj%nidx_rj(2)
!
      allocate(d_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(izero, kr_in, kr_out, sph_rj,      &
     &                               r_n2e_3rd, d_rj, d_ele)
      call cal_third_fdm_ICB_ele(izero, kr_in,                          &
     &                           sph_rj%nnod_rj, sph_rj%nidx_rj(2),     &
     &                            fdm3e_vp0_ICB, d_rj, dr_rj, d_ele)
      call cal_fdm3_zero_vp_CMB_ele                                     &
     &   (izero, kr_out, sph_rj%nnod_rj, sph_rj%nidx_rj(2),             &
     &    fdm3e_vp0_CMB, d_rj, dr_rj, d_ele)
!
      write(*,*) 'Interpolation to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d_ele(inod) - de_rj(inod)),                &
     &            d_ele(inod), de_rj(inod)
       end do
      end do
!
      allocate(dfdr_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(ione, kr_in, kr_out, sph_rj,       &
     &                               r_n2e_3rd, d_rj, dfdr_ele)
      call cal_third_fdm_ICB_ele(ione, kr_in,                           &
     &                           sph_rj%nnod_rj, sph_rj%nidx_rj(2),     &
     &                            fdm3e_vp0_ICB, d_rj, dr_rj, d_ele)
      call cal_fdm3_zero_vp_CMB_ele                                     &
     &   (ione, kr_out, sph_rj%nnod_rj, sph_rj%nidx_rj(2),              &
     &    fdm3e_vp0_CMB, d_rj, dr_rj, dfdr_ele)
      write(*,*) '1st derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (dfdr_ele(inod) - dre_rj(inod)),            &
     &            dfdr_ele(inod), dre_rj(inod)
       end do
      end do
!
      allocate(d2fdr2_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(itwo, kr_in, kr_out, sph_rj,       &
     &                               r_n2e_3rd, d_rj, d2fdr2_ele)
      call cal_third_fdm_ICB_ele(itwo, kr_in,                           &
     &                           sph_rj%nnod_rj, sph_rj%nidx_rj(2),     &
     &                            fdm3e_vp0_ICB, d_rj, dr_rj, d_ele)
      call cal_fdm3_zero_vp_CMB_ele                                     &
     &   (itwo, kr_out, sph_rj%nnod_rj, sph_rj%nidx_rj(2),              &
     &    fdm3e_vp0_CMB, d_rj, dr_rj, d2fdr2_ele)
      write(*,*) '2nd derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d2fdr2_ele(inod) - d2re_rj(inod)),         &
     &            d2fdr2_ele(inod), d2re_rj(inod)
       end do
      end do
!
      allocate(d3fdr3_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(ithree, kr_in, kr_out, sph_rj,     &
     &                               r_n2e_3rd, d_rj, d3fdr3_ele)
      call cal_third_fdm_ICB_ele(ithree, kr_in,                         &
     &                           sph_rj%nnod_rj, sph_rj%nidx_rj(2),     &
     &                           fdm3e_vp0_ICB, d_rj, dr_rj, d_ele)
      call cal_fdm3_zero_vp_CMB_ele                                     &
     &   (ithree, kr_out, sph_rj%nnod_rj, sph_rj%nidx_rj(2),            &
     &    fdm3e_vp0_CMB, d_rj, dr_rj, d3fdr3_ele)
      write(*,*) '3rd derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d3fdr3_ele(inod) - d3re_rj(inod)),         &
     &            d3fdr3_ele(inod), d3re_rj(inod)
       end do
      end do
!
!
      allocate(d_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(izero, kr_in, kr_out, sph_rj,      &
     &                               r_e2n_1st, de_rj, d_e2n)
      write(*,*) 'Interpolation from element to node'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (d_e2n(inod) - d_rj(inod)),                 &
     &            d_e2n(inod), d_rj(inod)
       end do
      end do
!
      allocate(dfdr_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(ione, kr_in, kr_out, sph_rj,       &
     &                               r_e2n_1st, de_rj, dfdr_e2n)
      write(*,*) '1st derivative from element to node'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (dfdr_e2n(inod) - dr_rj(inod)),             &
     &            dfdr_e2n(inod), dr_rj(inod)
       end do
      end do
!
      end subroutine test_radial_FDM
!
!  -------------------------------------------------------------------
!
      end program FDM_matrices_check
