!>@file   t_radial_references.f90
!!@brief  module t_radial_references
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Refelence scalar by diffusive profile
!!
!!@verbatim
!!      subroutine const_diffusive_profiles                             &
!!     &         (sph, MHD_prop, sph_MHD_bc, r_2nd, trans_p,            &
!!     &          sph_MHD_mat, ipol, rj_fld, ref_field)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(radial_reference), intent(inout) :: ref_field
!!@endverbatim
      module t_radial_references
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_work_4_sph_trans
      use t_schmidt_poly_on_rtm
!
      use t_control_parameter
      use t_physical_property
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      use t_fdm_coefs
      use t_radial_matrices_sph_MHD
      use t_sph_matrix
      use t_sph_center_matrix
!
      implicit none
!
      type radial_reference
        integer(kind = kint) :: nri_w_ctr
        real(kind = kreal), allocatable :: ref_temp(:,:)
!
        real(kind = kreal), allocatable :: ref_comp(:,:)
        real(kind = kreal), allocatable :: ref_local(:,:)
      end type radial_reference
!
      private :: alloc_radial_reference
      private :: cal_sol_diffusive_profile
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_diffusive_profiles                               &
     &         (sph_rj, sph_bc_S, bcs_S, fdm2_center, r_2nd, trans_p,   &
     &          band_s00_poisson, diffusie_reduction_ICB, i_source,     &
     &          rj_fld, ref_field)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(phys_data), intent(in) :: rj_fld
      type(band_matrix_type), intent(in) :: band_s00_poisson
!
      integer(kind = kint), intent(in) :: i_source
      real(kind = kreal), intent(in) :: diffusie_reduction_ICB
!
      type(radial_reference), intent(inout) :: ref_field
!
      integer(kind = kint) :: k
!
      call alloc_radial_reference(sph_rj, ref_field)
!
        call cal_sol_diffusive_profile(sph_rj, r_2nd, trans_p%leg,      &
     &      sph_bc_S, bcs_S, fdm2_center, band_s00_poisson,             &
     &      diffusie_reduction_ICB, i_source, rj_fld,                   &
     &      ref_field%ref_local, ref_field%ref_temp)
!
        if(my_rank .eq. 0) then
          open(52,file='reference_temp.dat')
            write(52,'(a)')                                             &
     &         'Id, radius, reference_temperature, reference_grad_temp'
              write(52,'(i6,1p3E25.15e3)')                              &
     &             0, zero, ref_field%ref_temp(0,0:1)
            do k = 1, sph_rj%nidx_rj(1)
              write(52,'(i6,1p3E25.15e3)') k,                           &
     &          sph_rj%radius_1d_rj_r(k), ref_field%ref_temp(k,0:1)
            end do
          close(52)
        end if
!
      end subroutine const_diffusive_profiles
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_radial_reference(ref_field)
!
      type(radial_reference), intent(inout) :: ref_field
!
      deallocate(ref_field%ref_temp)
      deallocate(ref_field%ref_comp)
      deallocate(ref_field%ref_local)
!
      end subroutine dealloc_radial_reference
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_radial_reference(sph_rj, ref_field)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(radial_reference), intent(inout) :: ref_field
!
!
      ref_field%nri_w_ctr = sph_rj%nidx_rj(1)
!
      allocate(ref_field%ref_temp(0:ref_field%nri_w_ctr,0:1))
      allocate(ref_field%ref_comp(0:ref_field%nri_w_ctr,0:1))
      allocate(ref_field%ref_local(0:ref_field%nri_w_ctr,0:1))
!
!$omp parallel workshare
      ref_field%ref_temp(0:ref_field%nri_w_ctr,0:1) = 0.0d0
      ref_field%ref_comp(0:ref_field%nri_w_ctr,0:1) = 0.0d0
      ref_field%ref_local(0:ref_field%nri_w_ctr,0:1) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_radial_reference
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_diffusive_profile                              &
     &         (sph_rj, r_2nd, leg, sph_bc_S, bcs_S,          &
     &          fdm2_center, band_s00_poisson, diffusie_reduction_ICB, is_src, rj_fld,          &
     &          ref_local, ref_scalar)
!
      use calypso_mpi_real
      use copy_field_smp
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
      use cal_sph_exp_center
      use const_sph_radial_grad
      use fill_scalar_field
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(band_matrix_type), intent(in) :: band_s00_poisson
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: is_src
      real(kind = kreal), intent(in) :: diffusie_reduction_ICB
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ref_scalar(0:sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(inout)                                 &
     &                   :: ref_local(0:sph_rj%nidx_rj(1),0:1)
!
      integer :: k
      integer(kind = kint_gl) :: num64
      real(kind = kreal), allocatable :: x00(:)
      type(phys_data) :: tmp_fld
!
!
      allocate(x00(0:sph_rj%nidx_rj(1)))
      x00(0:sph_rj%nidx_rj(1)) = 0.0d0
!
      tmp_fld%num_phys = 3
      call alloc_phys_name(tmp_fld)
!
      tmp_fld%phys_name(1) = 'solution'
      tmp_fld%phys_name(2) = 'gradient'
      tmp_fld%phys_name(3) = 'source'
      tmp_fld%num_component(1) = 1
      tmp_fld%num_component(2) = 3
      tmp_fld%num_component(3) = 1
      tmp_fld%istack_component(0) = 0
      tmp_fld%istack_component(1) = 1
      tmp_fld%istack_component(2) = 4
      tmp_fld%istack_component(3) = 5
      tmp_fld%ntot_phys = tmp_fld%istack_component(3)
      call alloc_phys_data(rj_fld%n_point, tmp_fld)
!
      if(is_src .gt. 0) then
!$omp parallel
        call copy_nod_scalar_smp                                        &
     &     (rj_fld%n_point, rj_fld%d_fld(1,is_src), tmp_fld%d_fld(1,5))
!$omp end parallel
      else
!$omp parallel workshare
        tmp_fld%d_fld(1:tmp_fld%n_point,5) = 0.0d0
!$omp end parallel workshare
      end if
!
      call set_CMB_scalar_sph_crank                                     &
     &   (sph_rj, sph_bc_S, bcs_S%CMB_Sspec, zero, one, one, one,       &
     &    ifive, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld)
      call set_ICB_scalar_sph_crank(sph_rj, sph_bc_S, bcs_S%ICB_Sspec,  &
     &    zero, one, diffusie_reduction_ICB, one, one,                  &
     &    ifive, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld)
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        call copy_degree0_comps_to_sol                                  &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      ifive, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld,    &
     &      x00)
!
        call lubksb_3band_ctr(band_s00_poisson, x00)
!
        call copy_degree0_comps_from_sol                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero, x00,      &
     &      ione, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld)
!      end if
!
        call fill_scalar_at_external                                    &
     &     (sph_bc_S, sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero, &
     &      sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      ione, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld)
!
        call const_radial_grad_scalar(sph_rj, r_2nd, sph_bc_S, bcs_S,   &
       &    fdm2_center, leg%g_sph_rj, ione, itwo, tmp_fld)
!
!      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        call copy_degree0_comps_to_sol                                  &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      ione, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld,     &
     &      ref_local(0,0))
        call copy_degree0_comps_to_sol                                  &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      itwo, rj_fld%n_point, tmp_fld%ntot_phys, tmp_fld%d_fld,     &
     &      ref_local(0,1))
!
!$omp parallel do
        do k = sph_bc_S%kr_in, sph_bc_S%kr_out
          ref_local(k,1) = ref_local(k,1) * half                        &
     &                    * sph_rj%a_r_1d_rj_r(k)**2
        end do
!$omp end parallel do
!
      end if
      call dealloc_phys_data(tmp_fld)
      call dealloc_phys_name(tmp_fld)
      deallocate(x00)
!
!$omp parallel workshare
      ref_scalar(0:sph_rj%nidx_rj(1),0:1) = 0.0d0
!$omp end parallel workshare
!
      num64 = 2 * (sph_rj%nidx_rj(1) + 1)
      call calypso_mpi_allreduce_real(ref_local, ref_scalar,            &
     &                                num64, MPI_SUM)
!
      end subroutine cal_sol_diffusive_profile
!
! -----------------------------------------------------------------------
!
      end module t_radial_references
