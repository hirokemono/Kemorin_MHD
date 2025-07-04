!
      program FDM_matrices_check
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_schmidt_poly_on_rtm
      use t_physical_property
      use t_work_SPH_MHD
!
      use chebyshev_radial_grid
      use schmidt_poly_on_rtm_grid
      use const_r_mat_4_vector_sph
      use count_num_sph_smp
!
      implicit none
!
      integer(kind = kint), parameter :: id_file = 55
!
      integer(kind = kint), parameter :: num_fluid_grid = 96
      real(kind = kreal), parameter :: rmin = 0.0d0
      real(kind = kreal), parameter :: rmax = 1.6d0
      real(kind = kreal), parameter :: r_ICB = 7.0d0 / 13.0d0
      real(kind = kreal), parameter :: r_CMB = 20.0d0 / 13.0d0
!
      real(kind = kreal), parameter :: dt = 1.0d-5
!
      type(sph_grids) :: sph1
      type(work_SPH_MHD) :: SPH_WK1
      type(legendre_4_sph_trans) :: Plm_WK1
      type(fluid_property) :: fl_prop1
      type(phys_data) :: radial_variation1
!
      type(sph_MHD_boundary_data) :: sph_MHD_bc1
      character(len=kchara), parameter :: BC_label = 'Boundary'
!
      character(len=kchara), parameter                                  &
     &           :: vt_evo_name =  'toroidal_velocity_evolution'
      character(len=kchara), parameter                                  &
     &           :: wt_evo_name =  'toroidal_vorticity_evolution'
      character(len=kchara), parameter                                  &
     &           :: vp_evo_name =  'poloidal_velocity_evolution'
      character(len=kchara), parameter                                  &
     &           :: vsp_evo_name = 'velocity_pressure_evolution'
!
      type(band_matrices_type) :: band9_vsp_evo1
!
      integer ::  k, l, ierr
!
      iflag_debug = 0
!
      sph1%sph_rj%nidx_rj(2) =   9
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
      call count_num_rj_smp(sph1%sph_rj, ierr)
      do k = 1, sph1%sph_rj%nidx_rj(2)
        l = aint(sqrt(real(k)))
        sph1%sph_rj%idx_gl_1d_rj_j(k,1) = k
        sph1%sph_rj%idx_gl_1d_rj_j(k,2) = l
        sph1%sph_rj%idx_gl_1d_rj_j(k,3) = k - l*(l+1)
      end do
!
      call set_chebyshev_distance_shell(sph1%sph_rj%nidx_rj(1),         &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_params%radius_ICB, sph1%sph_params%radius_CMB,       &
     &    sph1%sph_rj%radius_1d_rj_r)
      call set_sph_one_over_radius_rj(sph1%sph_rj)

!
      iflag_debug = iflag_full_msg
      call init_FDM_coefs_for_test(sph1, SPH_WK1)
!
      sph_MHD_bc1%sph_bc_U%kr_in = sph1%sph_params%nlayer_ICB
      sph_MHD_bc1%sph_bc_U%kr_out = sph1%sph_params%nlayer_CMB
      call cal_fdm_coefs_4_BCs(sph1%sph_rj, sph_MHD_bc1%sph_bc_U)
      call check_fdm_coefs_4_BC2(6, BC_label, sph_MHD_bc1%sph_bc_U)
!
      call init_FDM_boundaries_for_test(sph1%sph_params, sph1%sph_rj,   &
     &   SPH_WK1%r_4th, sph_MHD_bc1%bc_fdms_U, sph_MHD_bc1%fdm2_center)
      open(id_file,file='FDM_BC.txt')
      call check_sph_fdm_boundaries(id_file,                            &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_rj, sph_MHD_bc1%bc_fdms_U)
      call check_sph_4th_fdm_boundaries(id_file, sph_MHD_bc1%bc_fdms_U)
      close(id_file)
!
      open(id_file,file='FDM_diff.dat')
      call test_radial_FDM(id_file,                                     &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_rj, SPH_WK1,                                         &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_ICB,                          &
     &    sph_MHD_bc1%bc_fdms_U%fdm3e_vp0_CMB)
      close(id_file)
!
!
      call alloc_schmidt_normalize                                      &
     &   (sph1%sph_rj%nidx_rj(2), sph1%sph_rj%nidx_rj(2), Plm_WK1)
      call copy_sph_normalization_2_rj(sph1%sph_rj,  Plm_WK1%g_sph_rj)
!      do k = 1, sph1%sph_rj%nidx_rj(2)
!        write(*,*) k, Plm_WK1%g_sph_rj(k,1:3)
!      end do
!
      fl_prop1%flag_viscous_variation =     .FALSE.
      fl_prop1%flag_ref_density_valiation = .FALSE.
      fl_prop1%coef_diffuse = 1.0d0
      fl_prop1%coef_press = 5.0d0
!
      sph_MHD_bc1%sph_bc_U%kr_in = 1
      sph_MHD_bc1%sph_bc_U%iflag_icb = iflag_sph_fill_center
!      sph_MHD_bc1%sph_bc_U%iflag_icb = iflag_free_slip
!      sph_MHD_bc1%sph_bc_U%iflag_icb = iflag_fixed_velo
!
!      sph_MHD_bc1%sph_bc_U%iflag_cmb = iflag_free_slip
      sph_MHD_bc1%sph_bc_U%iflag_cmb = iflag_fixed_velo 
!
      call const_radial_mat7_vpol_press                                 &
     &   (dt, sph1%sph_rj, Plm_WK1%g_sph_rj, fl_prop1,                  &
     &    SPH_WK1%r_2nd, SPH_WK1%r_n2e_3rd, SPH_WK1%r_e2n_1st,          &
     &    sph_MHD_bc1%sph_bc_U, sph_MHD_bc1%bc_fdms_U,                  &
     &    sph_MHD_bc1%fdm2_center, radial_variation1,                   &
     &    SPH_WK1%MHD_mats%band_vsp_evo)
!
      call const_radial_mat9_vpol_press                                 &
     &   (dt, sph1%sph_rj, Plm_WK1%g_sph_rj, fl_prop1,                  &
     &    SPH_WK1%r_4th, SPH_WK1%r_n2e_3rd, SPH_WK1%r_e2n_3rd,          &
     &    sph_MHD_bc1%sph_bc_U, sph_MHD_bc1%bc_fdms_U,                  &
     &    radial_variation1, band9_vsp_evo1)
!
!
      call const_radial_mat_toroidal_flow                               &
     &   (dt, sph1%sph_rj, SPH_WK1%r_2nd, fl_prop1,                     &
     &    sph_MHD_bc1%sph_bc_U, sph_MHD_bc1%bc_fdms_U,                  &
     &    sph_MHD_bc1%fdm2_center, Plm_WK1%g_sph_rj,                    &
     &    SPH_WK1%MHD_mats%band_vt_evo)
!
      call const_radial_mat_vort_2step(dt, sph1%sph_rj, SPH_WK1%r_2nd,  &
     &    fl_prop1, sph_MHD_bc1%sph_bc_U, sph_MHD_bc1%bc_fdms_U,        &
     &    sph_MHD_bc1%fdm2_center, Plm_WK1%g_sph_rj,                    &
     &    SPH_WK1%MHD_mats%band_vs_poisson,                             &
     &    SPH_WK1%MHD_mats%band_vp_evo, SPH_WK1%MHD_mats%band_wt_evo)
!
      if(iflag_debug .eq. iflag_full_msg) then
        open(id_file,file='FDM_MAT.txt')
        call check_velocity_matrices_sph(id_file, sph1%sph_rj,          &
     &                                   SPH_WK1%MHD_mats)
        call check_radial_band_mat(id_file, sph1%sph_rj,                &
     &                             SPH_WK1%MHD_mats%band_wt_evo)
        call check_radial_band_mat(id_file, sph1%sph_rj,                &
     &                             SPH_WK1%MHD_mats%band_vp_evo)
        call check_vpol_FDM4_matrix_sph(id_file, sph1%sph_rj,           &
     &                                  band9_vsp_evo1)
        close(id_file)
      end if
!
!      do j = 1, sph1%sph_rj%nidx_rj(2)
!        do k = 1, sph1%sph_rj%nidx_rj(1)
!          SPH_WK1%MHD_mats%band_vp_evo%det(j)                          &
!     &                = SPH_WK1%MHD_mats%band_vp_evo%det(j)            &
!     &                  * SPH_WK1%MHD_mats%band_vp_evo%lu(5,k,j)
!        end do
!        write(my_rank+60,*) 'det vp', j,                               &
!                           &    SPH_WK1%MHD_mats%band_vp_evo%det(j)
!      end do
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine check_vpol_FDM4_matrix_sph                             &
     &         (id_file, sph_rj, band9_vsp_evo)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type) :: band9_vsp_evo
!
      real(kind = kreal) :: rr(2*sph_rj%nidx_rj(1))
      integer(kind = kint) :: k
!
!
      do k = 1, sph_rj%nidx_rj(1)
        rr(2*k-1) = sph_rj%radius_1d_rj_r(k)
        rr(2*k) =   sph_rj%radius_1d_rj_r(k)
      end do
!
      call check_radial_9band_mat                                       &
     &   (id_file, (2*sph_rj%nidx_rj(1)), sph_rj%nidx_rj(2),            &
     &    sph_rj%idx_gl_1d_rj_j, rr, band9_vsp_evo%mat)
!
      end subroutine check_vpol_FDM4_matrix_sph
!
! -----------------------------------------------------------------------
!
      subroutine init_FDM_coefs_for_test(sph, SPH_WK)
!
      use parallel_load_data_4_sph
      use init_radial_infos_sph_mhd
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
      use forth_fdm_node_coefs
      use third_fdm_ele_to_node
!
      type(sph_grids), intent(inout) :: sph
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint), parameter :: id_check = 50
!
      write(*,*) '...',id_check, iflag_debug
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
      open(id_check, file='FDM.dat')
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(id_check, sph%sph_params, sph%sph_rj, &
     &                            SPH_WK%r_2nd)
      if (iflag_debug.gt.0) write(*,*) 'const_first_fdm_ele_to_node'
      call const_first_fdm_ele_to_node(id_check, sph%sph_rj,            &
     &                                 SPH_WK%r_e2n_1st)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_node_to_ele'
      call const_third_fdm_node_to_ele(id_check, sph%sph_rj,            &
     &                                 SPH_WK%r_n2e_3rd)
      if (iflag_debug.gt.0) write(*,*) 'const_forth_fdm_coefs'
      call const_forth_fdm_coefs(id_check, sph%sph_rj, SPH_WK%r_4th)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_ele_to_node'
      call const_third_fdm_ele_to_node(id_check, sph%sph_rj,            &
     &                                 SPH_WK%r_e2n_3rd)
      close(id_check)
!
      end subroutine init_FDM_coefs_for_test
!
!  -------------------------------------------------------------------
!
      subroutine init_FDM_boundaries_for_test(sph_params, sph_rj,       &
     &          fdm_4th, bc_fdms_U, fdm2_center)
!
      use t_fdm_coefs
      use t_coef_fdm2_centre
      use t_coef_sph_velocity_BCs
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: fdm_4th
!
      type(fdm2_center_mat), intent(inout) :: fdm2_center
      type(velocity_boundary_FDMs), intent(inout) :: bc_fdms_U
!
      real(kind = kreal), allocatable :: h_rho(:)
      integer(kind = kint) :: kr_in, kr_out
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_2nd_to_center_fixed_fdm'
      call cal_2nd_to_center_fixed_fdm(sph_rj%radius_1d_rj_r(1),        &
     &                                 fdm2_center)
      call cal_2nd_center_fix_df_fdm(sph_rj%radius_1d_rj_r(1),          &
     &                               fdm2_center)
      call cal_2nd_center_fixed_fdm(sph_rj%radius_1d_rj_r(1),           &
     &                              fdm2_center)
!
      allocate(h_rho(sph_rj%nidx_rj(1)))
      h_rho(1:sph_rj%nidx_rj(1)) = zero
!
      kr_in =  sph_params%nlayer_ICB
      kr_out = sph_params%nlayer_CMB
      call set_sph_fdm_velocity_bc                                      &
     &   (kr_in, kr_out, h_rho(kr_in), h_rho(kr_out),                   &
     &    sph_rj, bc_fdms_U)
      call set_boundary_sph_4th_fdm                                     &
     &   (kr_in, kr_out, h_rho(kr_in), h_rho(kr_out),                   &
     &    sph_rj, fdm_4th, bc_fdms_U)
      call set_boundary_sph_4th_fdm                                     &
     &   (kr_in, kr_out, h_rho(kr_in), h_rho(kr_out),                   &
     &    sph_rj, fdm_4th, bc_fdms_U)
      deallocate(h_rho)
!
      end subroutine init_FDM_boundaries_for_test
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_FDM(id_file, kr_in, kr_out, sph_rj,        &
     &                           SPH_WK, fdm3e_vp0_ICB, fdm3e_vp0_CMB)
!
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(work_SPH_MHD), intent(in) :: SPH_WK
!
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
!
      real(kind = kreal), allocatable :: r_ele(:)
!
      real(kind = kreal), allocatable :: d_rj(:)
      real(kind = kreal), allocatable :: dr_rj(:)
      real(kind = kreal), allocatable :: d2r_rj(:)
      real(kind = kreal), allocatable :: d3r_rj(:)
      real(kind = kreal), allocatable :: de_rj(:)
!
      integer(kind = kint) :: inod, j, k
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
      allocate(d3r_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
        d_rj(inod) =   sph_rj%radius_1d_rj_r(k)**j
        dr_rj(inod) =  dble(j) * sph_rj%radius_1d_rj_r(k)**(j-1)
        d2r_rj(inod) = dble(j*(j-1)) * sph_rj%radius_1d_rj_r(k)**(j-2)
        d3r_rj(inod) = dble(j*(j-1)) * sph_rj%radius_1d_rj_r(k)**(j-2)
        d3r_rj(inod) = dble(j*(j-1)*(j-2))                              &
     &                * sph_rj%radius_1d_rj_r(k)**(j-3)
      end do
!
      allocate(de_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        de_rj(inod) =   r_ele(k)**j
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '2nd-order FDM'
      call test_radial_2nd_FDM(id_file, kr_in, kr_out, sph_rj,          &
     &                         d_rj, dr_rj, d2r_rj, SPH_WK%r_2nd)
!
      write(id_file,*) '#'
      write(id_file,*) '3rd-order FDM from node to element'
      write(id_file,*) 'Interpolation to element'
      call test_radial_3rd_FDM_nod_to_ele(id_file, kr_in, kr_out,       &
     &    sph_rj, r_ele, d_rj, dr_rj, de_rj,                            &
     &    SPH_WK%r_n2e_3rd, fdm3e_vp0_ICB, fdm3e_vp0_CMB)
!
      write(id_file,*) '#'
      write(id_file,*) '1st-order FDM from element to node'
      call test_radial_1st_FDM_ele_to_nod(id_file, kr_in, kr_out,       &
     &    sph_rj, de_rj, d_rj, dr_rj, SPH_WK%r_e2n_1st)
!
      write(id_file,*) '#'
      write(id_file,*) '4th-orderr FDM'
      call test_radial_4th_FDM(id_file, kr_in, kr_out,                  &
     &    sph_rj, d_rj, dr_rj, d2r_rj, d3r_rj, SPH_WK%r_4th)
!
      write(id_file,*) '#'
      write(id_file,*) '3rd-order FDM from element to node'
      call test_radial_3rd_FDM_ele_to_nod(id_file, kr_in, kr_out,       &
     &    sph_rj, de_rj, d_rj, dr_rj, d2r_rj, d3r_rj, SPH_WK%r_e2n_3rd)
!
      deallocate(d3r_rj, d2r_rj, dr_rj, d_rj)
      deallocate(de_rj, r_ele)
!
      end subroutine test_radial_FDM
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_2nd_FDM(id_file, kr_in, kr_out, sph_rj,    &
     &                               d_rj, dr_rj, d2r_rj, r_2nd)
!
      use second_fdm_node_coefs
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dr_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d2r_rj(sph_rj%nnod_rj)
!
      type(fdm_matrices), intent(in) :: r_2nd
!
      real(kind = kreal), allocatable :: dfdr_nod(:)
      real(kind = kreal), allocatable :: d2fdr2_nod(:)
!
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(ione, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, dfdr_nod)
      write(id_file,*) '1st derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, dr_rj, dfdr_nod)
!
      allocate(d2fdr2_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(itwo, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, d2fdr2_nod)
      write(id_file,*) '2nd derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, d2r_rj, d2fdr2_nod)
!
      deallocate(dfdr_nod, d2fdr2_nod)
!
      end subroutine test_radial_2nd_FDM
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_3rd_FDM_nod_to_ele(id_file, kr_in, kr_out, &
     &          sph_rj, r_ele, d_rj, dr_rj, de_rj,                      &
     &          r_n2e_3rd, fdm3e_vp0_ICB, fdm3e_vp0_CMB)
!
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use third_fdm_node_to_ele
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: r_ele(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dr_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: de_rj(sph_rj%nnod_rj)
!
      type(fdm_matrices), intent(in) :: r_n2e_3rd
!
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
      type(fdm3_n2e_CMB_zero_vpol), intent(in) :: fdm3e_vp0_CMB
!
      real(kind = kreal), allocatable :: dre_rj(:)
      real(kind = kreal), allocatable :: d2re_rj(:)
      real(kind = kreal), allocatable :: d3re_rj(:)
!
      real(kind = kreal), allocatable :: d_ele(:)
      real(kind = kreal), allocatable :: dfdr_ele(:)
      real(kind = kreal), allocatable :: d2fdr2_ele(:)
      real(kind = kreal), allocatable :: d3fdr3_ele(:)
!
      integer(kind = kint) :: inod, j, k, ist_in, ist_out
!
!
      allocate(dre_rj(sph_rj%nnod_rj))
      allocate(d2re_rj(sph_rj%nnod_rj))
      allocate(d3re_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dre_rj(inod) =  dble(j) * r_ele(k)**(j-1)
        d2re_rj(inod) = dble(j*(j-1)) * r_ele(k)**(j-2)
        d3re_rj(inod) = dble(j*(j-1)*(j-2)) * r_ele(k)**(j-3)
      end do
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
      write(id_file,*) 'Interpolation to element'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           r_ele, de_rj, d_ele)
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
      write(id_file,*) '1st derivative from node to element'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           r_ele, dre_rj, dfdr_ele)
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
      write(id_file,*) '2nd derivative from node to element'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           r_ele, d2re_rj, d2fdr2_ele)
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
!
      write(id_file,*) '3rd derivative from node to element'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           r_ele, d3re_rj, d3fdr3_ele)
!
      deallocate(d_ele, dfdr_ele, d2fdr2_ele, d3fdr3_ele)
      deallocate(dre_rj, d2re_rj, d3re_rj)
!
      end subroutine test_radial_3rd_FDM_nod_to_ele
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_1st_FDM_ele_to_nod(id_file, kr_in, kr_out, &
     &          sph_rj, de_rj, d_rj, dr_rj, r_e2n_1st)
!
      use first_fdm_ele_to_node
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_e2n_1st
!
      real(kind = kreal), intent(in) :: de_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dr_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), allocatable :: d_e2n(:)
      real(kind = kreal), allocatable :: dfdr_e2n(:)
!
!
      allocate(d_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(izero, kr_in, kr_out, sph_rj,      &
     &                               r_e2n_1st, de_rj, d_e2n)
      write(id_file,*) 'Interpolation from element to node'
      write(id_file,*) 'order_of_reference, radius, r_ID, diff, ',      &
     &           'FDM, Reference'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           sph_rj%radius_1d_rj_r, d_rj, d_e2n)
!
      allocate(dfdr_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(ione, kr_in, kr_out, sph_rj,       &
     &                               r_e2n_1st, de_rj, dfdr_e2n)
      write(id_file,*) '1st derivative from element to node'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                          sph_rj%radius_1d_rj_r, dr_rj, dfdr_e2n)
!
      deallocate(d_e2n, dfdr_e2n)
!
      end subroutine test_radial_1st_FDM_ele_to_nod
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_4th_FDM(id_file, kr_in, kr_out, sph_rj,    &
     &          d_rj, dr_rj, d2r_rj, d3r_rj, r_4th)
!
      use forth_fdm_node_coefs
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dr_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d2r_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d3r_rj(sph_rj%nnod_rj)
!
      type(fdm_matrices), intent(in) :: r_4th
!
      real(kind = kreal), allocatable :: d4r_rj(:)
      real(kind = kreal), allocatable :: dfdr_nod(:)
!
      integer(kind = kint) :: inod, j, k
!
!
      allocate(d4r_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
        d4r_rj(inod) = dble(j*(j-1)*(j-2)*(j-3))                        &
     &                * sph_rj%radius_1d_rj_r(k)**(j-4)
      end do
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_forth_fdm_node(ione, kr_in, kr_out, sph_rj,              &
     &                         r_4th, d_rj, dfdr_nod)
      write(id_file,*) '1st derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, dr_rj, dfdr_nod)
      deallocate(dfdr_nod)
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_forth_fdm_node(itwo, kr_in, kr_out, sph_rj,              &
     &                        r_4th, d_rj, dfdr_nod)
      write(id_file,*) '2nd derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, d2r_rj, dfdr_nod)
      deallocate(dfdr_nod)
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_forth_fdm_node(ithree, kr_in, kr_out, sph_rj,            &
     &                        r_4th, d_rj, dfdr_nod)
      write(id_file,*) '3rd derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, d3r_rj, dfdr_nod)
      deallocate(dfdr_nod)
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_forth_fdm_node(ifour, kr_in, kr_out, sph_rj,             &
     &                        r_4th, d_rj, dfdr_nod)
      write(id_file,*) '4th derivative'
      call write_FDM_comparisons(id_file, kr_in, kr_out,                &
     &    sph_rj, sph_rj%radius_1d_rj_r, d4r_rj, dfdr_nod)
      deallocate(dfdr_nod)
      deallocate(d4r_rj)
!
      end subroutine test_radial_4th_FDM
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_3rd_FDM_ele_to_nod(id_file, kr_in, kr_out, &
     &          sph_rj, de_rj, d_rj, dr_rj, d2r_rj, d3r_rj, r_e2n_3rd)
!
      use third_fdm_ele_to_node
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_e2n_3rd
!
      real(kind = kreal), intent(in) :: de_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: dr_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d2r_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d3r_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), allocatable :: d_e2n(:)
      real(kind = kreal), allocatable :: dfdr_e2n(:)
!
!
      allocate(d_e2n(sph_rj%nnod_rj))
      call cal_third_fdm_ele_to_node(izero, kr_in, kr_out, sph_rj,      &
     &                               r_e2n_3rd, de_rj, d_e2n)
      write(id_file,*) 'Interpolation from element to node'
      write(id_file,*) 'order_of_reference, radius, r_ID, diff, ',      &
     &           'FDM, Reference'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                           sph_rj%radius_1d_rj_r, d_rj, d_e2n)
!
      allocate(dfdr_e2n(sph_rj%nnod_rj))
      call cal_third_fdm_ele_to_node(ione, kr_in, kr_out, sph_rj,       &
     &                               r_e2n_3rd, de_rj, dfdr_e2n)
      write(id_file,*) '1st derivative from element to node'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                          sph_rj%radius_1d_rj_r, dr_rj, dfdr_e2n)
!
      call cal_third_fdm_ele_to_node(itwo, kr_in, kr_out, sph_rj,       &
     &                               r_e2n_3rd, de_rj, dfdr_e2n)
      write(id_file,*) '2nd derivative from element to node'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                         sph_rj%radius_1d_rj_r, d2r_rj, dfdr_e2n)
!
      call cal_third_fdm_ele_to_node(ithree, kr_in, kr_out, sph_rj,     &
     &                               r_e2n_3rd, de_rj, dfdr_e2n)
      write(id_file,*) '3rd derivative from element to node'
      call write_FDM_comparisons(id_file, kr_in, kr_out, sph_rj,        &
     &                         sph_rj%radius_1d_rj_r, d3r_rj, dfdr_e2n)
      deallocate(d_e2n, dfdr_e2n)
!
      end subroutine test_radial_3rd_FDM_ele_to_nod
!
!  -------------------------------------------------------------------
!
      subroutine write_FDM_comparisons(id_file, kr_in, kr_out,          &
     &          sph_rj, radius, d_ref, d_FDM)
!
      integer(kind = kint) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: radius(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: d_ref(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: d_FDM(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, j, k
!
      write(id_file,'(2a)') 'order_of_reference, radius, r_ID, diff, ', &
     &                     'FDM, Reference'
      do j = 1, sph_rj%nidx_rj(2)
        do k = kr_in, kr_out
          inod = j + (k-1) * sph_rj%nidx_rj(2)
          write(id_file,'(i3,1pe23.15,i4,1p3e23.15)') j, radius(k), k,  &
     &           (d_FDM(inod) - d_ref(inod)), d_FDM(inod), d_ref(inod)
        end do
      end do
!
      end subroutine write_FDM_comparisons
!
!  -------------------------------------------------------------------
!
      end program FDM_matrices_check
