!SPH_analyzer_zm_streamfunc.f90
!     module SPH_analyzer_zm_streamfunc
!
!      Written by H. Matsui
!
!!      subroutine SPH_analyze_zm_streamfunc                            &
!!     &         (i_step, files_param, viz_step,                        &
!!     &          SPH_MHD, t_IO, fld_IO, visval)
!!        type(SPH_TRNS_file_IO_params), intent(in) :: files_param
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(sph_grids), intent(in) :: sph_mesh
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine set_ctl_data_4_zm_streamline(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
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
      private :: sph_b_trans_streamline
      private :: set_rj_phys_for_zm_streamfunc
      private :: adjust_phi_comp_for_streamfunc
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_streamfunc                              &
     &         (i_step, files_param, viz_step,                          &
     &          SPH_MHD, t_IO, fld_IO, visval)
!
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_ctl_params_sph_trans
      use t_VIZ_step_parameter
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
!
!
      integer(kind = kint), intent(in) :: i_step
      type(SPH_TRNS_file_IO_params), intent(in) :: files_param
      type(VIZ_step_params), intent(in) :: viz_step
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      integer(kind = kint), intent(inout) :: visval
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)                  &
     &        * output_IO_flag(i_step, t_STR%ucd_step)
!
      if(visval .eq. 0) then
!
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      files_param%fst_file_IO, t_IO, fld_IO)
!
!    copy and extend magnetic field to outside
!
        if(files_param%org_rj_file_IO%iflag_IO .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO(fld_IO, SPH_MHD%fld)
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO                            &
     &       (fld_IO, SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
        end if
!
        call set_rj_phys_for_zm_streamfunc                              &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor,                  &
     &      SPH_MHD%sph%sph_rj, SPH_MHD%fld)
        call zonal_mean_all_sph_spectr(SPH_MHD%sph%sph_rj, SPH_MHD%fld)
!
!  spherical transform for vector
        call sph_b_trans_streamline(SPH_MHD%sph, SPH_MHD%comms,         &
     &      femmesh_STR%mesh, fld_rtp_TRNS, SPH_MHD%fld, field_STR)
      end if
!
      end subroutine SPH_analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_streamline(field_ctl)
!
      use t_control_array_character3
      use m_phys_labels
!
      type(ctl_array_c3), intent(inout) :: field_ctl
      integer(kind = kint) :: ifld, iflag_velo, iflag_magne
!
      iflag_velo =  0
      iflag_magne = 0
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. fhd_velo)  iflag_velo =  2
        if(field_ctl%c1_tbl(ifld) .eq. fhd_magne) iflag_magne = 2
      end do
!
      call dealloc_control_array_c3(field_ctl)
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
     &         (sph, comms_sph, mesh, fld_rtp, rj_fld, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_mesh_data
      use t_phys_data
      use t_sph_transforms
!
      use m_solver_SR
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use spherical_SRs_N
      use sph_transfer_all_field
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      type(phys_data), intent(in) :: rj_fld
      type(mesh_geometry), intent(in) :: mesh
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (fld_rtp%num_vector .gt. 0) then
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (fld_rtp%ncomp_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (fld_rtp%ncomp_trans, comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
      call set_all_vec_spec_to_sph_t                                    &
     &     (fld_rtp%ncomp_trans, comms_sph%comm_rj, fld_rtp,            &
     &      rj_fld, n_WS, WS)
!
      call sph_b_trans_w_poles(fld_rtp%ncomp_trans,                     &
     &    fld_rtp%num_vector, fld_rtp%nscalar_trans,                    &
     &    sph, comms_sph, trns_param, n_WS, n_WR, WS(1), WR(1),         &
     &    dall_rtp, dlcl_pole, dall_pole, WK_sph_TRNS)
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
        call adjust_phi_comp_for_streamfunc                             &
     &     (sph%sph_rtp%nnod_rtp, sph%sph_rtp%nidx_rtp,                 &
     &      sph%sph_rtp%radius_1d_rtp_r, sph%sph_rtp%sin_theta_1d_rtp,  &
     &      fld_rtp, dall_rtp(1,1))
        call set_xyz_vect_from_sph_trans                                &
     &     (sph%sph_rtp, mesh%node, fld_rtp, sph%sph_params%m_folding,  &
     &      fld_rtp%ncomp_trans, dall_rtp(1,1), dall_pole(1,1),         &
     &       nod_fld)
      end if
!
      end subroutine sph_b_trans_streamline
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_zm_streamfunc(ipol, idpdr, itor,       &
     &          sph_rj, rj_fld)
!
      use m_phys_labels
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, k, j
!
!
      if(ipol%i_velo .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, sph_rj%nidx_rj(1)
          do j = 1, sph_rj%nidx_rj(2)
            inod = (k-1)*sph_rj%nidx_rj(2) + j
            rj_fld%d_fld(inod,itor%i_vort )                             &
     &           =  rj_fld%d_fld(inod,itor%i_velo)
            rj_fld%d_fld(inod,ipol%i_vort ) =  zero
            rj_fld%d_fld(inod,idpdr%i_vort) =  zero
!
            rj_fld%d_fld(inod,itor%i_velo )                             &
     &           =  rj_fld%d_fld(inod,ipol%i_velo)
            rj_fld%d_fld(inod,ipol%i_velo ) =  zero
            rj_fld%d_fld(inod,idpdr%i_velo) =  zero
           end do
        end do
!$omp end parallel do
      end if
!
      if(ipol%i_magne .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, sph_rj%nidx_rj(1)
          do j = 1, sph_rj%nidx_rj(2)
            inod = (k-1)*sph_rj%nidx_rj(2) + j
            rj_fld%d_fld(inod,itor%i_current )                          &
     &           =  rj_fld%d_fld(inod,itor%i_magne)
            rj_fld%d_fld(inod,ipol%i_current ) =  zero
            rj_fld%d_fld(inod,idpdr%i_current) =  zero
!
            rj_fld%d_fld(inod,itor%i_magne )                            &
     &           =  rj_fld%d_fld(inod,ipol%i_magne)
            rj_fld%d_fld(inod,ipol%i_magne ) =  zero
            rj_fld%d_fld(inod,idpdr%i_magne) =  zero
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
     &         (nnod_rtp, nidx_rtp, radius_1d_rtp_r, sin_theta_1d_rtp,  &
     &          fld_rtp, v_rtp)
!
      use m_phys_labels
      use t_phys_name_4_sph_trans
!
      type(field_name_4_sph_trans), intent(in) :: fld_rtp
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: radius_1d_rtp_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_theta_1d_rtp(nidx_rtp(2))
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_rtp(nnod_rtp,fld_rtp%ncomp_trans)
!
      integer(kind = kint) :: inod, j, k, l, m
!
!
      do j = 1, fld_rtp%num_vector
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
