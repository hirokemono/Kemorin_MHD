!>@file   init_sphrical_transform_MHD.f90
!!@brief  module init_sphrical_transform_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD                               &
!!     &         (SPH_model, iphys, trans_p, WK, SPH_MHD)
!!      subroutine init_leg_fourier_trans_MHD(sph_MHD_bc,               &
!!     &          sph, comms_sph, ncomp_max_trans, trans_p, WK)
!!      subroutine sel_sph_transform_MHD                                &
!!     &         (MHD_prop, sph_MHD_bc, sph, comms_sph, omega_sph,      &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,&
!!     &          trns_MHD, WK_sph, trans_p, gt_cor, cor_rlm, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!!
      module init_sphrical_transform_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_legendre_trans_select
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
!
      implicit  none
!
      integer(kind = kint), parameter :: num_test =  10
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_leg_symmetry,                                   &
     &            iflag_leg_sym_spin_loop,                              &
     &            iflag_leg_sym_matmul,                                 &
     &            iflag_leg_sym_dgemm,                                  &
     &            iflag_leg_sym_matmul_big,                             &
     &            iflag_leg_sym_dgemm_big,                              &
     &            iflag_leg_sym_mat_jt,                                 &
     &            iflag_leg_sym_dgemm_jt,                               &
     &            iflag_leg_sym_mat_tj,                                 &
     &            iflag_leg_sym_dgemm_tj/)
!
      private :: num_test, list_test
      private :: select_legendre_transform
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD                                 &
     &         (SPH_model, iphys, trans_p, WK, SPH_MHD)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_snap
!
      type(phys_address), intent(in) :: iphys
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans = 0
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans = 0
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans = 0
!
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD                                  &
     &   (SPH_MHD%ipol, iphys, WK%trns_MHD,                             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_snapshot_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_snap,                            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_ene_flux_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_eflux,                           &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_diff_vect_trans                                &
     &   (SPH_MHD%ipol, iphys, WK%trns_difv,                            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph%sph_rtp, WK)
!
      call init_leg_fourier_trans_MHD                                   &
     &   (SPH_model%sph_MHD_bc, SPH_MHD%sph, SPH_MHD%comms,             &
     &    ncomp_max_trans, trans_p, WK)
!
      call sel_sph_transform_MHD                                        &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%WK_sph, trans_p, WK%gt_cor, WK%cor_rlm,       &
     &    SPH_MHD%fld)
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_fourier_trans_MHD(sph_MHD_bc,                 &
     &          sph, comms_sph, ncomp_max_trans, trans_p, WK)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use const_wz_coriolis_rtp
      use pole_sph_transform
      use skip_comment_f
!
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans                                    &
     &   (trans_p%nvector_legendre, ncomp_max_trans, sph, comms_sph,    &
     &    trans_p%leg, trans_p%idx_trns, trans_p%iflag_SPH_recv)
      call init_fourier_transform_4_MHD                                 &
     &   (ncomp_max_trans, sph%sph_rtp, comms_sph%comm_rtp,             &
     &    WK%trns_MHD, WK%WK_sph, trans_p%iflag_FFT)
!
      if(my_rank .eq. 0)  call write_import_table_mode(trans_p)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_sphere_ave_coriolis'
      call alloc_sphere_ave_coriolis(sph%sph_rj)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm                                        &
     &   (sph%sph_params%l_truncation, sph%sph_rlm,                     &
     &    sph_MHD_bc%sph_bc_U, trans_p%leg, WK%gt_cor, WK%cor_rlm)
!
      end subroutine init_leg_fourier_trans_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_transform_MHD                                  &
     &         (MHD_prop, sph_MHD_bc, sph, comms_sph, omega_sph,        &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          trns_MHD, WK_sph, trans_p, gt_cor, cor_rlm, rj_fld)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'select_legendre_transform'
      call select_legendre_transform(sph, comms_sph, MHD_prop%fl_prop,  &
     &    sph_MHD_bc%sph_bc_U, omega_sph, trans_p, gt_cor,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    rj_fld, trns_MHD, WK_sph, cor_rlm)
!
      call sel_init_legendre_trans                                      &
     &   (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns,      &
     &    WK_sph%WK_leg)
!
      if(my_rank .ne. 0) return
      call display_selected_legendre_mode(WK_sph%WK_leg%id_legendre)
!
      end subroutine sel_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine select_legendre_transform(sph, comms_sph,              &
     &          fl_prop, sph_bc_U, omega_sph, trans_p, gt_cor,          &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, WK_sph, cor_rlm)
!
      use calypso_mpi_real
      use sph_transforms_4_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
!
      real(kind = kreal) :: starttime, etime_shortest
      real(kind = kreal) :: endtime(maxindex_Leg_trans_loop)
      real(kind = kreal) :: etime_trans(maxindex_Leg_trans_loop)
      real(kind = kreal) :: etime_max(maxindex_Leg_trans_loop)
!
      integer(kind = kint) :: id, iloop_type
      integer(kind = kint_gl) :: num64
!
!
      if(WK_sph%WK_leg%id_legendre .ne. iflag_leg_undefined) return
!
      endtime(1:maxindex_Leg_trans_loop) =     zero
      etime_trans(1:maxindex_Leg_trans_loop) = zero
      etime_max(1:maxindex_Leg_trans_loop) =   zero
      do iloop_type = 1, num_test
        WK_sph%WK_leg%id_legendre = list_test(iloop_type)
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'Test SPH transform for ', WK_sph%WK_leg%id_legendre
        call sel_init_legendre_trans                                    &
     &     (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,      &
     &      sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns,    &
     &      WK_sph%WK_leg)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, fl_prop, sph_bc_U,    &
     &      omega_sph, trans_p, gt_cor, rj_fld, trns_MHD%b_trns,        &
     &      trns_MHD%backward, WK_sph, cor_rlm)
        call sph_forward_trans_4_MHD(sph, comms_sph, fl_prop,           &
     &      trans_p, cor_rlm, trns_MHD%f_trns, trns_MHD%forward,        &
     &      WK_sph, rj_fld)
        endtime(WK_sph%WK_leg%id_legendre) = MPI_WTIME() - starttime
!
        call sel_finalize_legendre_trans(WK_sph%WK_leg)
      end do
!
      num64 = int(maxindex_Leg_trans_loop,KIND(num64))
      call calypso_mpi_allreduce_real                                   &
     &   (endtime, etime_trans, num64, MPI_SUM)
      call calypso_mpi_allreduce_real                                   &
     &   (endtime, etime_max, num64, MPI_MAX)
      etime_trans(1:maxindex_Leg_trans_loop)                            &
     &      = etime_trans(1:maxindex_Leg_trans_loop) / dble(nprocs)
!
      etime_shortest =  1.0d30
      do iloop_type = 1, num_test
        id = list_test(iloop_type)
        if(etime_max(id) .lt. etime_shortest) then
          WK_sph%WK_leg%id_legendre = id
          etime_shortest =       etime_max(id)
        end if
      end do
!
      if(my_rank .gt. 0) return
        write(*,'(a)') 'Loop ID: type, maximum time, average time'
        if(etime_trans(iflag_leg_symmetry) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_symmetry,                         &
     &          ': elapsed by original loop with symmetric'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_org_loop),  ':  ',      &
     &            etime_max(iflag_leg_symmetry),                        &
     &            etime_trans(iflag_leg_symmetry)
        end if
!
        if(etime_trans(iflag_leg_sym_spin_loop) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_spin_loop,                    &
     &            ': elapsed by sym. outer radius'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_spin_loop),  ':  ',     &
     &            etime_max(iflag_leg_sym_spin_loop),                   &
     &            etime_trans(iflag_leg_sym_spin_loop)
        end if
!
        if(etime_trans(iflag_leg_sym_matmul) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_matmul,                       &
     &          ': elapsed by using matmul with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_matmul),  ':  ',        &
     &            etime_max(iflag_leg_sym_matmul),                      &
     &            etime_trans(iflag_leg_sym_matmul)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm,                        &
     &          ': elapsed by BLAS with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_dgemm),  ':  ',         &
     &            etime_max(iflag_leg_sym_dgemm),                       &
     &            etime_trans(iflag_leg_sym_dgemm)
        end if
!
        if(etime_trans(iflag_leg_sym_matmul_big) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_matmul_big,                   &
     &          ': elapsed by big matmul with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_matmul_big),  ':  ',    &
     &            etime_max(iflag_leg_sym_matmul_big),                  &
     &            etime_trans(iflag_leg_sym_matmul_big)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm_big) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm_big,                    &
     &          ': elapsed by big BLAS with radial SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_dgemm_big),  ':  ',     &
     &            etime_max(iflag_leg_sym_dgemm_big),                   &
     &            etime_trans(iflag_leg_sym_dgemm_big)
        end if
!
        if(etime_trans(iflag_leg_sym_mat_jt) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_mat_jt,                       &
     &          ': elapsed by matmul for Pjt with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_mat_jt),  ':  ',        &
     &            etime_max(iflag_leg_sym_mat_jt),                      &
     &            etime_trans(iflag_leg_sym_mat_jt)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm_jt) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm_jt,                     &
     &          ': elapsed by BLAS for Pjt with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_dgemm_jt),  ':  ',      &
     &            etime_max(iflag_leg_sym_dgemm_jt),                    &
     &            etime_trans(iflag_leg_sym_dgemm_jt)
        end if
!
        if(etime_trans(iflag_leg_sym_mat_tj) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_mat_tj,                       &
     &          ': elapsed by matmul for Ptj with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_sym_mat_tj),  ':  ',        &
     &            etime_max(iflag_leg_sym_mat_tj),                      &
     &            etime_trans(iflag_leg_sym_mat_tj)
        end if
!
        if(etime_trans(iflag_leg_sym_dgemm_tj) .gt. zero) then
          write(*,'(i3,a)') iflag_leg_sym_dgemm_tj,                     &
     &          ': elapsed by BLAS for Ptj with theta SMP'
          write(*,'(2a,1p2e16.6)') trim(leg_dgemm_tj),  ':  ',          &
     &            etime_max(iflag_leg_sym_dgemm_tj),                    &
     &            etime_trans(iflag_leg_sym_dgemm_tj)
        end if
!
      end subroutine select_legendre_transform
!
!-----------------------------------------------------------------------
!
      end module init_sphrical_transform_MHD
