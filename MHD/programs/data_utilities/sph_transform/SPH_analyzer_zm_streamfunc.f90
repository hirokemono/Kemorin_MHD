!SPH_analyzer_zm_streamfunc.f90
!     module SPH_analyzer_zm_streamfunc
!
!      Written by H. Matsui
!
!      subroutine SPH_analyze_zm_streamfunc(i_step, visval, fld_IO)
!
      module SPH_analyzer_zm_streamfunc
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_SPH_transforms
      use calypso_mpi
!
      implicit none
!
      private :: set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_streamfunc(i_step, visval, fld_IO)
!
      use m_spheric_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_t_step_parameter
      use m_control_params_2nd_files
      use m_node_id_spherical_IO
      use m_sph_spectr_data
      use t_field_data_IO
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
      use set_exit_flag_4_visualizer
!
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: visval
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_udt
!
!
      call set_output_flag(i_udt, i_step, i_step_output_ucd)
      call set_output_flag_4_viz(i_step, visval)
      visval = visval * i_udt
!
      if(visval .eq. 0) then
!
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, fld_IO)
!
!    copy and extend magnetic field to outside
!
        if(iflag_org_sph_rj_head .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO                                 &
     &       (sph1%sph_rj%nnod_rj, fld_IO, rj_fld1)
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO                            &
     &       (fld_IO, sph1%sph_rj, rj_fld1)
        end if
!
        call set_rj_phys_for_zm_streamfunc(sph1%sph_rj%nidx_rj,         &
     &      rj_fld1%n_point, rj_fld1%ntot_phys, rj_fld1%d_fld)
        call zonal_mean_all_sph_spectr(sph1%sph_rj, rj_fld1)
!
!  spherical transform for vector
        call sph_b_trans_streamline                                     &
     &     (sph1%sph_params, sph1%sph_rtp, sph1%sph_rtm, sph1%sph_rlm,  &
     &      comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1,                  &
     &      femmesh_STR%mesh, rj_fld1, field_STR)
!
      end if
!
      end subroutine SPH_analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_streamline
!
      use m_ctl_data_4_fields
      use m_phys_labels
!
      integer(kind = kint) :: ifld, iflag_velo, iflag_magne
!
      iflag_velo =  0
      iflag_magne = 0
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. fhd_velo)  iflag_velo =  2
        if(field_ctl%c1_tbl(ifld) .eq. fhd_magne) iflag_magne = 2
      end do
!
      call deallocate_phys_control
!
      field_ctl%num = iflag_velo + iflag_magne
      call alloc_control_array_c3(field_ctl)
!
      ifld = 0
      if(iflag_velo .gt. 0) then
        field_ctl%c1_tbl(ifld+1) = fhd_velo
        field_ctl%c1_tbl(ifld+2) = fhd_vort
        ifld = ifld+2
      end if
      if(iflag_magne .gt. 0) then
        field_ctl%c1_tbl(ifld+1) = fhd_magne
        field_ctl%c1_tbl(ifld+2) = fhd_current
        ifld = ifld+2
      end if
!
      do ifld = 1, field_ctl%num
        field_ctl%c2_tbl(ifld) = 'Viz_On'
        field_ctl%c3_tbl(ifld) = 'Monitor_Off'
      end do
!
      end subroutine set_ctl_data_4_zm_streamline
!
! ----------------------------------------------------------------------
!
      subroutine sph_b_trans_streamline                                 &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm,                  &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj,                  &
     &          mesh, rj_fld, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_mesh_data
      use t_phys_data
!
      use m_solver_SR
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use sph_transforms
      use spherical_SRs_N
      use sph_transfer_all_field
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
!
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
      type(phys_data), intent(in) :: rj_fld
      type(mesh_geometry), intent(in) :: mesh
!
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: nscalar_trans
!
!
      if (num_vector_rtp .gt. 0) then
!
      nscalar_trans = num_scalar_rtp + 6*num_tensor_rtp
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comm_rj, comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comm_rtm, comm_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
        call set_all_vec_spec_to_sph_t                                  &
     &     (ncomp_sph_trans, comm_rj, rj_fld, n_WS, WS)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_backward_transforms',       &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_backward_transforms                                      &
     &   (ncomp_sph_trans, num_vector_rtp, nscalar_trans,               &
     &    sph_params, sph_rtp, sph_rtm, sph_rlm,                        &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj,                        &
     &    n_WS, n_WR, WS(1), WR(1), dall_rtp, dlcl_pole, dall_pole)
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
        call adjust_phi_comp_for_streamfunc                             &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%radius_1d_rtp_r, ncomp_sph_trans, dall_rtp(1,1))
        call set_xyz_vect_from_sph_trans                                &
     &     (sph_rtp, mesh%node, sph_params%m_folding, ncomp_sph_trans,  &
     &      dall_rtp(1,1), dall_pole(1,1), nod_fld)
      end if
!
      end subroutine sph_b_trans_streamline
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_zm_streamfunc                          &
     &         (nidx_rj, n_point, ntot_phys_rj, d_rj)
!
      use m_phys_labels
      use m_sph_phys_address
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, k, j
!
!
      if(ipol%i_velo .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, nidx_rj(1)
          do j = 1, nidx_rj(2)
            inod = (k-1)*nidx_rj(2) + j
            d_rj(inod,itor%i_vort ) =  d_rj(inod,itor%i_velo)
            d_rj(inod,ipol%i_vort ) =  zero
            d_rj(inod,idpdr%i_vort) =  zero
!
            d_rj(inod,itor%i_velo ) =  d_rj(inod,ipol%i_velo)
            d_rj(inod,ipol%i_velo ) =  zero
            d_rj(inod,idpdr%i_velo) =  zero
           end do
        end do
!$omp end parallel do
      end if
!
      if(ipol%i_magne .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, nidx_rj(1)
          do j = 1, nidx_rj(2)
            inod = (k-1)*nidx_rj(2) + j
            d_rj(inod,itor%i_current ) =  d_rj(inod,itor%i_magne)
            d_rj(inod,ipol%i_current ) =  zero
            d_rj(inod,idpdr%i_current) =  zero
!
            d_rj(inod,itor%i_magne ) =  d_rj(inod,ipol%i_magne)
            d_rj(inod,ipol%i_magne ) =  zero
            d_rj(inod,idpdr%i_magne) =  zero
           end do
        end do
!$omp end parallel do
      end if
!
      end subroutine set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine adjust_phi_comp_for_streamfunc                         &
     &         (nnod_rtp, nidx_rtp, radius_1d_rtp_r,                    &
     &          ncomp_trans, v_rtp)
!
      use m_phys_labels
      use m_work_4_sph_trans
      use set_phys_name_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: radius_1d_rtp_r(:)
!
      integer(kind = kint), intent(in) ::  ncomp_trans
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp,ncomp_trans)
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do j = 1, num_vector_rtp
!
!$omp parallel do private(k,l,m,inod)
          do m = 1, nidx_rtp(3)
            do l = 1, nidx_rtp(2)
              do k = 1, nidx_rtp(1)
                inod = k + (l-1)*nidx_rtp(1)                            &
     &                   + (m-1)*nidx_rtp(1)*nidx_rtp(2)
                v_rtp(inod,3*j-2) =  zero
                v_rtp(inod,3*j-1) =  zero
                v_rtp(inod,3*j  ) = -v_rtp(inod,3*j  )                  &
     &                      * radius_1d_rtp_r(k)*sin_theta_1d_rtp(l)
              end do
            end do
          end do
!$omp end parallel do
      end do
!
      end subroutine adjust_phi_comp_for_streamfunc
!
! ----------------------------------------------------------------------
!
     end module SPH_analyzer_zm_streamfunc
